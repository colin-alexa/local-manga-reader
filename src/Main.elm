module Main exposing (main)

-- import Page.EditPost as EditPost

import Browser exposing (UrlRequest)
import Browser.Events exposing (onKeyPress)
import Browser.Navigation as Nav
import Html
import Page.AllSeries as AllSeries
import Page.AllSeries.View as AllSeriesView
import Page.Chapter as Chapter
import Page.Chapter.View as ChapterView
import Page.Reader as Reader
import Page.Reader.View as ReaderView
import Page.Series as Series
import Page.Series.View as SeriesView
import Platform.Cmd as Cmd
import Route exposing (Route)
import Url exposing (Url)


main : Program () Application ApplicationMsg
main =
    Browser.application
        { init = initApplication
        , view = \app -> { title = "Personal Manga Reader", body = [ viewApplication app ] }
        , update = updateApplication
        , subscriptions = subscriptionsForPage
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


type alias Application =
    { route : Route
    , page : Page
    , navKey : Nav.Key
    }


type Page
    = NotFoundPage
    | AllSeriesPage AllSeries.Model
    | SeriesPage Series.Model
    | ChapterPage Chapter.Model
    | ReaderPage Reader.Model


type ApplicationMsg
    = LinkClicked UrlRequest
    | UrlChanged Url
    | AllSeriesPageMsg AllSeries.Msg
    | SeriesPageMsg Series.Msg
    | ChapterPageMsg Chapter.Msg
    | ReaderPageMsg Reader.Msg


initApplication : () -> Url -> Nav.Key -> ( Application, Cmd ApplicationMsg )
initApplication _ url navKey =
    let
        model =
            { route = Route.parseUrl url
            , page = NotFoundPage
            , navKey = navKey
            }
    in
    initCurrentPage ( model, Cmd.none )


initCurrentPage : ( Application, Cmd ApplicationMsg ) -> ( Application, Cmd ApplicationMsg )
initCurrentPage ( app, appCommands ) =
    let
        ( currentPage, pageInitCommands ) =
            case app.route of
                Route.NotFound ->
                    ( NotFoundPage, Cmd.none )

                Route.AllSeries ->
                    let
                        ( pageModel, pageCommands ) =
                            AllSeries.init app.navKey
                    in
                    ( AllSeriesPage pageModel, Cmd.map AllSeriesPageMsg pageCommands )

                Route.Series id ->
                    let
                        ( pageModel, pageCommands ) =
                            Series.init app.navKey id
                    in
                    ( SeriesPage pageModel, Cmd.map SeriesPageMsg pageCommands )

                Route.Chapter seriesId chapterId ->
                    let
                        ( pageModel, pageCommands ) =
                            Chapter.init app.navKey seriesId chapterId
                    in
                    ( ChapterPage pageModel, Cmd.map ChapterPageMsg pageCommands )

                Route.Reader seriesId chapterId pageNumber ->
                    let
                        ( pageModel, pageCommands ) =
                            Reader.init app.navKey seriesId chapterId pageNumber
                    in
                    ( ReaderPage pageModel, Cmd.map ReaderPageMsg pageCommands )
    in
    ( { app | page = currentPage }, Cmd.batch [ appCommands, pageInitCommands ] )


updateApplication : ApplicationMsg -> Application -> ( Application, Cmd ApplicationMsg )
updateApplication msg app =
    case ( msg, app.page ) of
        ( AllSeriesPageMsg pageMsg, AllSeriesPage model ) ->
            let
                ( newModel, newCommand ) =
                    AllSeries.update pageMsg model
            in
            ( { app | page = AllSeriesPage newModel }
            , Cmd.map AllSeriesPageMsg newCommand
            )

        ( SeriesPageMsg pageMsg, SeriesPage model ) ->
            let
                ( newModel, newCommand ) =
                    Series.update pageMsg model
            in
            ( { app | page = SeriesPage newModel }
            , Cmd.map SeriesPageMsg newCommand
            )

        ( ChapterPageMsg pageMsg, ChapterPage model ) ->
            let
                ( newModel, newCommand ) =
                    Chapter.update pageMsg model
            in
            ( { app | page = ChapterPage newModel }
            , Cmd.map ChapterPageMsg newCommand
            )

        ( ReaderPageMsg pageMsg, ReaderPage model ) ->
            let
                ( newModel, newCommand ) =
                    Reader.update pageMsg model
            in
            ( { app | page = ReaderPage newModel }
            , Cmd.map ReaderPageMsg newCommand
            )

        ( LinkClicked req, _ ) ->
            case req of
                Browser.Internal url ->
                    ( app, Nav.pushUrl app.navKey (Url.toString url) )

                Browser.External url ->
                    ( app, Nav.load url )

        ( UrlChanged url, _ ) ->
            ( { app | route = Route.parseUrl url }, Cmd.none ) |> initCurrentPage

        ( _, _ ) ->
            ( app, Cmd.none )


viewApplication : Application -> Html.Html ApplicationMsg
viewApplication app =
    case app.page of
        NotFoundPage ->
            notFoundView

        SeriesPage model ->
            Html.map SeriesPageMsg <| SeriesView.view model

        ChapterPage model ->
            Html.map ChapterPageMsg <| ChapterView.view model

        AllSeriesPage model ->
            Html.map AllSeriesPageMsg <| AllSeriesView.view model

        ReaderPage model ->
            Html.map ReaderPageMsg <| ReaderView.view model


notFoundView : Html.Html ApplicationMsg
notFoundView =
    Html.h3 [] [ Html.text "Oops! Page not found!" ]


subscriptionsForPage : Application -> Sub ApplicationMsg
subscriptionsForPage app =
    case app.page of
        ReaderPage model ->
            Sub.map ReaderPageMsg (Reader.subscriptions model)

        ChapterPage model ->
            Sub.map ChapterPageMsg (Chapter.subscriptions model)

        _ ->
            Sub.none
