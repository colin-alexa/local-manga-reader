module Components exposing (..)

import Css exposing (..)
import Css.Animations as Anim exposing (keyframes)
import Css.Transitions exposing (transition)
import Error exposing (formatHttpError)
import Html.Styled as Styled exposing (Html, a, button, div, styled, text)
import Html.Styled.Attributes exposing (href, type_)
import Html.Styled.Events exposing (..)
import RemoteData exposing (WebData)
import Route exposing (Route)



-- PAGE HEADER


controls : List (Styled.Attribute msg) -> List (Html msg) -> Html msg
controls =
    styled div
        [ displayFlex
        , alignItems center
        ]


pageHeaderView : List (Styled.Attribute msg) -> List (Html msg) -> List (Html msg) -> Html msg
pageHeaderView attrs navItems pageControls =
    styled div
        [ displayFlex
        , justifyContent spaceBetween
        , backgroundColor (hex "#FFF")
        , boxShadow5 zero (px 4) (px 4) (px 2) (rgba 0 0 0 0.2)
        , border zero
        , padding2 (rem 0.25) (rem 1)
        , zIndex (int 100)
        , position sticky
        , top zero
        ]
        attrs
        [ div [] (List.intersperse (text " > ") navItems)
        , controls [] pageControls
        ]



-- NAV LINK


navLink : Route -> String -> Html msg
navLink r s =
    styled a
        [ color (hex "0268e5")
        , visited [ color (hex "0268e5") ]
        ]
        [ href <| Route.urlForRoute r ]
        [ text s ]



-- WEBDATA


webDataView : { loading : Html msg, error : String -> Html msg, view : a -> Html msg } -> WebData a -> Html msg
webDataView { loading, error, view } data =
    case data of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            loading

        RemoteData.Failure err ->
            error (formatHttpError err)

        RemoteData.Success resolvedData ->
            view resolvedData



-- LOADING SPINNER


spinning : Anim.Keyframes {}
spinning =
    keyframes
        [ ( 0, [ Anim.transform [ rotate (turn 0) ] ] )
        , ( 5, [ Anim.transform [ rotate (turn 0.02) ] ] )
        , ( 20, [ Anim.transform [ rotate (turn 0.15) ] ] )
        , ( 50, [ Anim.transform [ rotate (turn 0.5) ] ] )
        , ( 80, [ Anim.transform [ rotate (turn 0.85) ] ] )
        , ( 95, [ Anim.transform [ rotate (turn 0.98) ] ] )
        , ( 100, [ Anim.transform [ rotate (turn 1) ] ] )
        ]


loadingSpinner : LengthOrAuto c -> List (Styled.Attribute msg) -> List (Html msg) -> Html msg
loadingSpinner radius =
    styled div
        [ borderTop3 (px 4) solid (hex "000")
        , width radius
        , height radius
        , borderRadius (pct 50)
        , animationName spinning
        , animationDuration (sec 0.5)
        , animationIterationCount infinite
        ]



-- TOGGLE BUTTON


styledToggleButton : Bool -> List (Styled.Attribute msg) -> List (Html msg) -> Html msg
styledToggleButton isOn =
    styled button
        [ before
            [ property "content" "\"\""
            , display inlineBlock
            , backgroundColor (hex "#FFF")
            , borderRadius (pct 50)
            , border3 (px 1) solid (hex "#000")
            , width (px 15)
            , height (px 15)
            , transition [ Css.Transitions.marginLeft 60 ]
            , margin zero
            , marginLeft
                (if isOn then
                    pct 50

                 else
                    pct -50
                )
            ]
        , border3 (px 1) solid (hex "#000")
        , borderRadius (px 10)
        , height (px 21)
        , width (px 40)
        , backgroundColor
            (if isOn then
                hex "999"

             else
                hex "#FFF"
            )
        ]


toggle : Bool -> (Bool -> msg) -> List (Styled.Attribute msg) -> Html msg
toggle isOn callback attrs =
    styledToggleButton isOn (attrs ++ [ type_ "button", onClick (callback (not isOn)) ]) []
