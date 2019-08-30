module Debugger.Main exposing
    ( cornerView
    , getUserModel
    , popoutView
    , wrapInit
    , wrapSubs
    , wrapUpdate
    )

import Bitwise
import Debugger.Expando as Expando exposing (Expando)
import Debugger.History as History exposing (History)
import Debugger.Metadata as Metadata exposing (Metadata)
import Debugger.Overlay as Overlay
import Debugger.Report as Report
import Dict exposing (Dict)
import Elm.Kernel.Debugger
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, onMouseDown, onMouseUp)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Task exposing (Task)



-- VIEW


getUserModel : Model model msg -> model
getUserModel model =
    getCurrentModel model.state



-- SUBSCRIPTIONS


wrapSubs : (model -> Sub msg) -> Model model msg -> Sub (Msg msg)
wrapSubs subscriptions model =
    Sub.map UserMsg (subscriptions (getLatestModel model.state))



-- MODEL


type alias Model model msg =
    { history : History model msg
    , state : State model msg
    , modelExpando : Expando
    , messageExpando : Maybe Expando
    , metadata : Result Metadata.Error Metadata
    , overlay : Overlay.State
    , popout : Popout
    , sidePanelResizable : Bool
    , sidePanelOffset : Int
    , layout : Layout
    , windowWidth : Int
    , windowHeight : Int
    }


type Popout
    = Popout Popout


type Layout
    = Vertical
    | Horizontal



-- STATE


type State model msg
    = Running model
    | Paused Int model model msg (History model msg)


getLatestModel : State model msg -> model
getLatestModel state =
    case state of
        Running model ->
            model

        Paused _ _ model _ _ ->
            model


getCurrentModel : State model msg -> model
getCurrentModel state =
    case state of
        Running model ->
            model

        Paused _ model _ _ _ ->
            model


isPaused : State model msg -> Bool
isPaused state =
    case state of
        Running _ ->
            False

        Paused _ _ _ _ _ ->
            True


cacheHistory : Model model msg -> History model msg
cacheHistory model =
    case model.state of
        Running _ ->
            model.history

        Paused _ _ _ _ cachedHistory ->
            cachedHistory



-- INIT


wrapInit : Encode.Value -> Popout -> (flags -> ( model, Cmd msg )) -> flags -> ( Model model msg, Cmd (Msg msg) )
wrapInit metadata popout init flags =
    let
        ( userModel, userCommands ) =
            init flags
    in
    ( { history = History.empty userModel
      , state = Running userModel
      , modelExpando = Expando.init userModel
      , messageExpando = Nothing
      , metadata = Metadata.decode metadata
      , overlay = Overlay.none
      , popout = popout
      , sidePanelResizable = False
      , sidePanelOffset = 300
      , layout = Vertical
      , windowWidth = 0
      , windowHeight = 0
      }
    , Cmd.map UserMsg userCommands
    )



-- UPDATE


type Msg msg
    = NoOp
    | Resize Int Int
    | UserMsg msg
    | ExpandoMsg ExpandoTarget Expando.Msg
    | Resume
    | Jump Int
    | SliderJump Int
    | Open
    | Up
    | Down
    | EnableSidePanelResizing Bool
    | ResizeSidePanel MouseProperties
    | Import
    | Export
    | Upload String
    | OverlayMsg Overlay.Msg


type alias UserUpdate model msg =
    msg -> model -> ( model, Cmd msg )


type ExpandoTarget
    = MessageExpando
    | ModelExpando


wrapUpdate : UserUpdate model msg -> Msg msg -> Model model msg -> ( Model model msg, Cmd (Msg msg) )
wrapUpdate update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Resize width height ->
            let
                newLayout =
                    if width < height then
                        Horizontal

                    else
                        Vertical
            in
            ( { model
                | windowWidth = width
                , windowHeight = height
                , layout = newLayout
                , sidePanelOffset =
                    case ( model.layout, newLayout ) of
                        ( Horizontal, Vertical ) ->
                            300

                        ( Vertical, Horizontal ) ->
                            height // 2

                        otherwise ->
                            model.sidePanelOffset
              }
            , Cmd.none
            )

        UserMsg userMsg ->
            let
                userModel =
                    getLatestModel model.state

                newHistory =
                    History.add userMsg userModel model.history

                ( newUserModel, userCmds ) =
                    update userMsg userModel

                commands =
                    Cmd.map UserMsg userCmds
            in
            case model.state of
                Running _ ->
                    ( { model
                        | history = newHistory
                        , state = Running newUserModel
                        , modelExpando = Expando.merge newUserModel model.modelExpando
                        , messageExpando = Just (Expando.init userMsg)
                      }
                    , Cmd.batch
                        [ commands
                        , scroll model.popout
                        ]
                    )

                Paused index indexModel _ _ cachedHistory ->
                    ( { model
                        | history = newHistory
                        , state = Paused index indexModel newUserModel userMsg cachedHistory
                      }
                    , commands
                    )

        ExpandoMsg target eMsg ->
            case target of
                MessageExpando ->
                    ( { model | messageExpando = Maybe.map (Expando.update eMsg) model.messageExpando }
                    , Cmd.none
                    )

                ModelExpando ->
                    ( { model | modelExpando = Expando.update eMsg model.modelExpando }
                    , Cmd.none
                    )

        Resume ->
            case model.state of
                Running _ ->
                    ( model, Cmd.none )

                Paused _ _ userModel userMsg _ ->
                    ( { model
                        | state = Running userModel
                        , modelExpando = Expando.merge userModel model.modelExpando
                        , messageExpando = Maybe.map (Expando.merge userMsg) model.messageExpando
                      }
                    , scroll model.popout
                    )

        Jump index ->
            let
                ( currentModel, currentMsg ) =
                    History.getRecent update model.history

                ( indexModel, indexMsg ) =
                    History.get update index model.history
            in
            ( { model
                | state = Paused index indexModel currentModel currentMsg (cacheHistory model)
                , modelExpando = Expando.merge indexModel model.modelExpando
                , messageExpando = Maybe.map (Expando.merge indexMsg) model.messageExpando
              }
            , Cmd.none
            )

        SliderJump index ->
            let
                ( modelAfterJump, jumpCmds ) =
                    wrapUpdate update (Jump index) model
            in
            ( modelAfterJump
            , scrollTo (History.idForMessageIndex index) model.popout
            )

        Open ->
            ( model
            , Task.perform (\_ -> NoOp) (Elm.Kernel.Debugger.open model.popout)
            )

        Up ->
            let
                index =
                    case model.state of
                        Paused i _ _ _ _ ->
                            i

                        Running _ ->
                            History.size model.history
            in
            if index > 0 then
                wrapUpdate update (Jump (index - 1)) model

            else
                ( model, Cmd.none )

        Down ->
            case model.state of
                Running _ ->
                    ( model, Cmd.none )

                Paused index _ userModel userMsg _ ->
                    if index == History.size model.history - 1 then
                        wrapUpdate update Resume model

                    else
                        wrapUpdate update (Jump (index + 1)) model

        EnableSidePanelResizing enabled ->
            ( { model | sidePanelResizable = enabled }
            , Cmd.none
            )

        ResizeSidePanel mouseProperties ->
            if mouseProperties.buttonIsPressed then
                let
                    minPanelSize =
                        150

                    dimensionSize =
                        case model.layout of
                            Horizontal ->
                                model.windowHeight

                            Vertical ->
                                model.windowWidth

                    upperBound =
                        dimensionSize - minPanelSize
                in
                ( { model
                    | sidePanelOffset =
                        mouseProperties.offset
                            |> Basics.max minPanelSize
                            |> Basics.min upperBound
                  }
                , Cmd.none
                )

            else
                ( { model | sidePanelResizable = False }
                , Cmd.none
                )

        Import ->
            withGoodMetadata model <|
                \_ ->
                    ( model, upload model.popout )

        Export ->
            withGoodMetadata model <|
                \metadata ->
                    ( model, download metadata model.history )

        Upload jsonString ->
            withGoodMetadata model <|
                \metadata ->
                    case Overlay.assessImport metadata jsonString of
                        Err newOverlay ->
                            ( { model | overlay = newOverlay }, Cmd.none )

                        Ok rawHistory ->
                            loadNewHistory rawHistory update model

        OverlayMsg overlayMsg ->
            case Overlay.close overlayMsg model.overlay of
                Nothing ->
                    ( { model | overlay = Overlay.none }, Cmd.none )

                Just rawHistory ->
                    loadNewHistory rawHistory update model



-- COMMANDS


scroll : Popout -> Cmd (Msg msg)
scroll popout =
    Task.perform (always NoOp) (Elm.Kernel.Debugger.scroll popout)


scrollTo : String -> Popout -> Cmd (Msg msg)
scrollTo id popout =
    Task.perform (always NoOp) (Elm.Kernel.Debugger.scrollTo id popout)


upload : Popout -> Cmd (Msg msg)
upload popout =
    Task.perform Upload (Elm.Kernel.Debugger.upload popout)


download : Metadata -> History model msg -> Cmd (Msg msg)
download metadata history =
    let
        historyLength =
            History.size history

        json =
            Encode.object
                [ ( "metadata", Metadata.encode metadata )
                , ( "history", History.encode history )
                ]
                |> Elm.Kernel.Json.unwrap
    in
    Task.perform (\_ -> NoOp) (Elm.Kernel.Debugger.download historyLength json)



-- UPDATE OVERLAY


withGoodMetadata :
    Model model msg
    -> (Metadata -> ( Model model msg, Cmd (Msg msg) ))
    -> ( Model model msg, Cmd (Msg msg) )
withGoodMetadata model func =
    case model.metadata of
        Ok metadata ->
            func metadata

        Err error ->
            ( { model | overlay = Overlay.badMetadata error }
            , Cmd.none
            )


loadNewHistory :
    Encode.Value
    -> UserUpdate model msg
    -> Model model msg
    -> ( Model model msg, Cmd (Msg msg) )
loadNewHistory rawHistory update model =
    let
        initialUserModel =
            History.getInitialModel model.history

        pureUserUpdate msg userModel =
            Tuple.first (update msg userModel)

        decoder =
            History.decoder initialUserModel pureUserUpdate
    in
    case Decode.decodeValue decoder rawHistory of
        Err _ ->
            ( { model | overlay = Overlay.corruptImport }
            , Cmd.none
            )

        Ok ( latestUserModel, newHistory ) ->
            ( { model
                | history = newHistory
                , state = Running latestUserModel
                , modelExpando = Expando.init latestUserModel
                , messageExpando = Nothing
                , overlay = Overlay.none
              }
            , Cmd.none
            )



-- CORNER VIEW


cornerView : Model model msg -> Html (Msg msg)
cornerView model =
    Overlay.view
        { resume = Resume
        , open = Open
        , importHistory = Import
        , exportHistory = Export
        , wrap = OverlayMsg
        }
        (isPaused model.state)
        (Elm.Kernel.Debugger.isOpen model.popout)
        (History.size model.history)
        model.overlay


toBlockerType : Model model msg -> Overlay.BlockerType
toBlockerType model =
    Overlay.toBlockerType (isPaused model.state) model.overlay



-- BIG DEBUG VIEW


popoutView : Model model msg -> Html (Msg msg)
popoutView model =
    let
        bodyAttributes =
            [ style "margin" "0"
            , style "padding" "0"
            , style "width" "100%"
            , style "height" "100%"
            , style "font-family" "monospace"
            , style "overflow" "auto"
            ]

        sidePanelOffset =
            String.fromInt model.sidePanelOffset ++ "px"
    in
    node "body"
        (if model.sidePanelResizable then
            List.append bodyAttributes
                [ onMouseMove model.layout ResizeSidePanel
                , onMouseUp (EnableSidePanelResizing False)

                -- Disable highlighting when dragging
                , style "-webkit-user-select" "none"
                , style "-moz-user-select" "none"
                , style "-ms-user-select" "none"
                , style "user-select" "none"
                ]

         else
            bodyAttributes
        )
        (case model.layout of
            Horizontal ->
                [ div
                    [ style "display" "block"
                    , style "width" "100%"
                    , style "height" sidePanelOffset
                    , style "margin" "0"
                    , style "overflow" "auto"
                    , style "cursor" "default"
                    ]
                    [ case model.messageExpando of
                        Just messageExpandoModel ->
                            div []
                                [ div [] [ text "Message:" ]
                                , Html.map (ExpandoMsg MessageExpando) <| Expando.view Nothing messageExpandoModel
                                ]

                        Nothing ->
                            text ""
                    , div []
                        [ div [] [ text "Model:" ]
                        , Html.map (ExpandoMsg ModelExpando) <| Expando.view Nothing model.modelExpando
                        ]
                    ]
                , viewSidebar model.state model.history model.layout sidePanelOffset
                ]

            Vertical ->
                [ viewSidebar model.state model.history model.layout sidePanelOffset
                , div
                    [ style "display" "block"
                    , style "float" "left"
                    , style "height" "100%"
                    , style "width" <| "calc(100% - " ++ sidePanelOffset ++ ")"
                    , style "margin" "0"
                    , style "overflow" "auto"
                    , style "cursor" "default"
                    ]
                    [ case model.messageExpando of
                        Just messageExpandoModel ->
                            div []
                                [ div [] [ text "Message:" ]
                                , Html.map (ExpandoMsg MessageExpando) <| Expando.view Nothing messageExpandoModel
                                ]

                        Nothing ->
                            text ""
                    , div []
                        [ div [] [ text "Model:" ]
                        , Html.map (ExpandoMsg ModelExpando) <| Expando.view Nothing model.modelExpando
                        ]
                    ]
                ]
        )


viewSidebar : State model msg -> History model msg -> Layout -> String -> Html (Msg msg)
viewSidebar state history layout offsetStyle =
    let
        maybeIndex =
            case state of
                Running _ ->
                    Nothing

                Paused index _ _ _ _ ->
                    Just index

        historyToRender =
            case state of
                Running _ ->
                    history

                Paused _ _ _ _ cachedHistory ->
                    cachedHistory
    in
    case layout of
        Horizontal ->
            div
                [ style "position" "relative"
                , style "display" "block"
                , style "width" "100%"
                , style "height" <| "calc(100% - " ++ offsetStyle ++ ")"
                , style "color" "white"
                , style "background-color" "rgb(61, 61, 61)"
                ]
                [ draggableBorderHorizontal
                , slider historyToRender maybeIndex
                , Html.map Jump (History.view maybeIndex historyToRender)
                , playButton maybeIndex
                ]

        Vertical ->
            div
                [ style "position" "relative"
                , style "display" "block"
                , style "float" "left"
                , style "width" offsetStyle
                , style "height" "100%"
                , style "color" "white"
                , style "background-color" "rgb(61, 61, 61)"
                ]
                [ draggableBorder
                , slider historyToRender maybeIndex
                , Html.map Jump (History.view maybeIndex historyToRender)
                , playButton maybeIndex
                ]


draggableBorder : Html (Msg msg)
draggableBorder =
    div
        [ style "position" "absolute"
        , style "top" "0px"
        , style "right" "0px"
        , style "height" "100%"
        , style "width" "5px"
        , style "background-color" "black"
        , style "cursor" "col-resize"
        , onMouseDown (EnableSidePanelResizing True)
        ]
        []


draggableBorderHorizontal : Html (Msg msg)
draggableBorderHorizontal =
    div
        [ style "position" "absolute"
        , style "top" "0px"
        , style "left" "0px"
        , style "height" "5px"
        , style "width" "100%"
        , style "background-color" "black"
        , style "cursor" "row-resize"
        , onMouseDown (EnableSidePanelResizing True)
        ]
        []


onMouseMove : Layout -> (MouseProperties -> Msg msg) -> Attribute (Msg msg)
onMouseMove layout toMsg =
    on "mousemove" (Decode.map toMsg (mouseMoveDecoder layout))


type alias MouseProperties =
    { offset : Int
    , buttonIsPressed : Bool
    }


mouseMoveDecoder : Layout -> Decoder MouseProperties
mouseMoveDecoder layout =
    let
        offsetFieldName =
            case layout of
                Horizontal ->
                    "pageY"

                Vertical ->
                    "pageX"

        primaryButtonDetector value =
            Bitwise.and value 1 == 1
    in
    Decode.map2 MouseProperties
        (Decode.field offsetFieldName Decode.int)
        (Decode.field "buttons" (Decode.map primaryButtonDetector Decode.int))


slider : History model msg -> Maybe Int -> Html (Msg msg)
slider history maybeIndex =
    let
        lastIndex =
            History.size history - 1

        selectedIndex =
            Maybe.withDefault lastIndex maybeIndex
    in
    div
        [ style "width" "100%"
        , style "height" "24px"
        , style "text-align" "center"
        , style "background-color" "rgb(50, 50, 50)"
        , style "padding" "0 20px"
        , style "box-sizing" "border-box"
        ]
        [ input
            [ type_ "range"
            , style "width" "100%"
            , style "height" "24px"
            , Html.Attributes.min "0"
            , Html.Attributes.max (String.fromInt lastIndex)
            , value (String.fromInt selectedIndex)
            , onInput (String.toInt >> Maybe.withDefault lastIndex >> SliderJump)
            ]
            []
        ]


playButton : Maybe Int -> Html (Msg msg)
playButton maybeIndex =
    div
        [ style "width" "100%"
        , style "text-align" "center"
        , style "background-color" "rgb(50, 50, 50)"
        ]
        [ viewResumeButton maybeIndex
        , div
            [ style "width" "100%"
            , style "height" "24px"
            , style "line-height" "24px"
            , style "font-size" "12px"
            ]
            [ viewTextButton Import "Import"
            , text " / "
            , viewTextButton Export "Export"
            ]
        ]


viewTextButton : msg -> String -> Html msg
viewTextButton msg label =
    span
        [ onClick msg
        , style "cursor" "pointer"
        ]
        [ text label ]


viewResumeButton : Maybe Int -> Html (Msg msg)
viewResumeButton maybeIndex =
    case maybeIndex of
        Nothing ->
            text ""

        Just _ ->
            div
                [ onClick Resume
                , class "elm-debugger-resume"
                ]
                [ text "Resume"
                , Html.node "style" [] [ text resumeStyle ]
                ]


resumeStyle : String
resumeStyle =
    """

.elm-debugger-resume {
  width: 100%;
  height: 30px;
  line-height: 30px;
  cursor: pointer;
}

.elm-debugger-resume:hover {
  background-color: rgb(41, 41, 41);
}

"""
