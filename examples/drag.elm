module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D


-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { x : Int
    , y : Int
    , dragState : DragState
    }


type DragState
    = Static
    | Moving { startX : Int, startY : Int, endX : Int, endY : Int }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { x = 0, y = 0, dragState = Static }, Cmd.none )



-- UPDATE


type alias Point =
    { x : Int, y : Int }


type Msg
    = Start Point
    | Move Point
    | Stop Point


fromPoint : (Point -> Msg) -> Int -> Int -> Msg
fromPoint msg x y =
    Point x y |> msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start { x, y } ->
            ( { model
                | dragState =
                    Moving
                        { startX = x
                        , startY = y
                        , endX = x
                        , endY = y
                        }
              }
            , Cmd.none
            )

        Move { x, y } ->
            case model.dragState of
                Static ->
                    ( model, Cmd.none )

                Moving { startX, startY } ->
                    ( { model
                        | dragState =
                            Moving
                                { startX = startX
                                , startY = startY
                                , endX = x
                                , endY = y
                                }
                      }
                    , Cmd.none
                    )

        Stop { x, y } ->
            case model.dragState of
                Static ->
                    ( model, Cmd.none )

                Moving { startX, startY } ->
                    ( Model (model.x - startX + x) (model.y - startY + y) Static
                    , Cmd.none
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.dragState of
        Static ->
            Sub.none

        Moving _ ->
            Browser.Events.onMouseMove (D.map2 (fromPoint Move) pageX pageY)


pageX : D.Decoder Int
pageX =
    D.field "pageX" D.int


pageY : D.Decoder Int
pageY =
    D.field "pageY" D.int



-- VIEW


view : Model -> Html Msg
view model =
    let
        ( x, y ) =
            getPosition model
    in
    div
        [ style "width" "100vw"
        , style "height" "100vh"
        , on "mouseup" (D.map2 (fromPoint Stop) pageX pageY)
        ]
        [ div
            [ style "background-color" "rgb(104,216,239)"
            , style "position" "absolute"
            , style "left" (String.fromInt x ++ "px")
            , style "top" (String.fromInt y ++ "px")
            , style "width" "100px"
            , style "height" "100px"
            , style "padding" "20px"
            , style "text-align" "center"
            , on "mousedown" (D.map2 (fromPoint Start) pageX pageY)
            ]
            [ text "Drag me!"
            , br [] []
            , text <| "x: " ++ String.fromInt x
            , br [] []
            , text <| "y: " ++ String.fromInt y
            ]
        ]


getPosition : Model -> ( Int, Int )
getPosition model =
    case model.dragState of
        Static ->
            ( model.x, model.y )

        Moving m ->
            ( model.x - m.startX + m.endX, model.y - m.startY + m.endY )
