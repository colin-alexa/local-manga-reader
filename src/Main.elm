module Main exposing (main)

-- import Page.EditPost as EditPost

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Html
import Page.Chapter as Chapter
import Page.Chapters as Chapters
import Page.Chapters.View as ChaptersView
import Platform.Cmd as Cmd
import Route exposing (Route)
import Url exposing (Url)


main : Program () Application ApplicationMsg
main =
    Browser.application
        { init = initApplication
        , view = \app -> { title = "Personal Manga Reader", body = [ viewApplication app ] }
        , update = updateApplication
        , subscriptions = \_ -> Sub.none
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
    | ChaptersPage Chapters.Model
    | ChapterPage Chapter.Model


type ApplicationMsg
    = LinkClicked UrlRequest
    | UrlChanged Url
    | ChaptersPageMsg Chapters.Msg
    | ChapterPageMsg Chapter.Msg


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
        ( currentPage, pageCommands ) =
            case app.route of
                Route.NotFound ->
                    ( NotFoundPage, Cmd.none )

                Route.Chapters ->
                    let
                        ( pageModel, chaptersPageCommands ) =
                            Chapters.init app.navKey
                    in
                    ( ChaptersPage pageModel, Cmd.map ChaptersPageMsg chaptersPageCommands )

                Route.Chapter id ->
                    let
                        ( pageModel, chapterPageCommands ) =
                            Chapter.init app.navKey id
                    in
                    ( ChapterPage pageModel, Cmd.map ChapterPageMsg chapterPageCommands )
    in
    ( { app | page = currentPage }, Cmd.batch [ appCommands, pageCommands ] )


updateApplication : ApplicationMsg -> Application -> ( Application, Cmd ApplicationMsg )
updateApplication msg app =
    case ( msg, app.page ) of
        ( ChaptersPageMsg pageMsg, ChaptersPage model ) ->
            let
                ( newModel, newCommand ) =
                    Chapters.update pageMsg model
            in
            ( { app | page = ChaptersPage newModel }
            , Cmd.map ChaptersPageMsg newCommand
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

        ChaptersPage model ->
            Html.map ChaptersPageMsg <| ChaptersView.view model

        ChapterPage model ->
            Html.map ChapterPageMsg <| Chapter.view model


notFoundView : Html.Html ApplicationMsg
notFoundView =
    Html.h3 [] [ Html.text "Oops! Page not found!" ]
