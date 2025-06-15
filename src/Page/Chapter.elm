module Page.Chapter exposing (..)

import Browser.Navigation as Nav
import Data.ApiId exposing (ApiId)
import Data.Chapter exposing (Chapter)
import Html.Styled exposing (div, text)
import RemoteData exposing (WebData)
import Request exposing (getChapter)
import VirtualDom


type alias Model =
    { navKey : Nav.Key
    , chapter : WebData Chapter
    }


type Msg
    = ChapterReceived (WebData Chapter)


init : Nav.Key -> ApiId -> ( Model, Cmd Msg )
init key id =
    ( { navKey = key, chapter = RemoteData.Loading }
    , getChapter id ChapterReceived
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


view : Model -> VirtualDom.Node msg
view _ =
    Html.Styled.toUnstyled (div [] [ Debug.todo "implement chapter page" ])
