module Debugger.Main exposing
    ( cornerView
    , getUserModel
    , initialWindowHeight
    , initialWindowWidth
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



-- CONSTANTS


minimumPanelSize : Int
minimumPanelSize =
    150


initialWindowWidth : Int
initialWindowWidth =
    900


initialWindowHeight : Int
initialWindowHeight =
    360



-- SUBSCRIPTIONS


wrapSubs : (model -> Sub msg) -> Model model msg -> Sub (Msg msg)
wrapSubs subscriptions model =
    Sub.map UserMsg (subscriptions (getLatestModel model.state))



-- MODEL


type alias Model model msg =
    { history : History model msg
    , state : State model msg
    , modelExpando : Expando
    , messageExpando : Expando
    , metadata : Result Metadata.Error Metadata
    , overlay : Overlay.State
    , popout : Popout
    , messagePanelResizable : Bool
    , layout : Layout
    , windowWidth : Int
    , windowHeight : Int
    }


type Popout
    = Popout Popout


type Layout
    = Vertical Int
    | Horizontal Int


getUserModel : Model model msg -> model
getUserModel model =
    getCurrentModel model.state



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


cachedHistory : Model model msg -> History model msg
cachedHistory model =
    case model.state of
        Running _ ->
            model.history

        Paused _ _ _ _ history ->
            history



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
      , messageExpando = Expando.init ()
      , metadata = Metadata.decode metadata
      , overlay = Overlay.none
      , popout = popout
      , messagePanelResizable = False
      , layout = Vertical 300
      , windowWidth = initialWindowWidth
      , windowHeight = initialWindowHeight
      }
    , Cmd.map UserMsg userCommands
    )



-- UPDATE


type Msg msg
    = NoOp
    | Resize Int Int
    | UserMsg msg
    | MessageExpandoMsg Expando.Msg
    | ModelExpandoMsg Expando.Msg
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


wrapUpdate : UserUpdate model msg -> Msg msg -> Model model msg -> ( Model model msg, Cmd (Msg msg) )
wrapUpdate update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Resize width height ->
            ( { model
                | windowWidth = width
                , windowHeight = height
                , layout =
                    case model.layout of
                        Horizontal offset ->
                            if width < height then
                                model.layout

                            else
                                Vertical 300

                        Vertical offset ->
                            if width < height then
                                Horizontal (height // 2)

                            else
                                model.layout
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
                        , messageExpando = Expando.merge userMsg model.messageExpando
                      }
                    , Cmd.batch
                        [ commands
                        , scroll model.popout
                        ]
                    )

                Paused index indexModel _ _ history ->
                    ( { model
                        | history = newHistory
                        , state = Paused index indexModel newUserModel userMsg history
                      }
                    , commands
                    )

        MessageExpandoMsg eMsg ->
            ( { model | messageExpando = Expando.update eMsg model.messageExpando }
            , Cmd.none
            )

        ModelExpandoMsg eMsg ->
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
                        , messageExpando = Expando.merge userMsg model.messageExpando
                        , modelExpando = Expando.merge userModel model.modelExpando
                      }
                    , scroll model.popout
                    )

        Jump index ->
            ( jumpUpdate update index model
            , Cmd.none
            )

        SliderJump index ->
            ( jumpUpdate update index model
            , scrollTo (History.idForMessageIndex index) model.popout
            )

        Open ->
            ( model
            , Task.perform (always NoOp) (Elm.Kernel.Debugger.open model.popout)
            )

        Up ->
            case model.state of
                Paused i _ _ _ history ->
                    let
                        targetIndex =
                            i + 1
                    in
                    if targetIndex < History.size history then
                        wrapUpdate update (SliderJump targetIndex) model

                    else
                        wrapUpdate update Resume model

                Running _ ->
                    ( model, Cmd.none )

        Down ->
            case model.state of
                Running _ ->
                    wrapUpdate update (Jump (History.size model.history - 1)) model

                Paused index _ _ _ _ ->
                    if index > 0 then
                        wrapUpdate update (SliderJump (index - 1)) model

                    else
                        ( model, Cmd.none )

        EnableSidePanelResizing enabled ->
            ( { model | messagePanelResizable = enabled }
            , Cmd.none
            )

        ResizeSidePanel mouseProperties ->
            if mouseProperties.buttonIsPressed then
                let
                    updatedLayout =
                        case model.layout of
                            Horizontal _ ->
                                Horizontal <|
                                    Basics.clamp
                                        minimumPanelSize
                                        (model.windowHeight - minimumPanelSize)
                                        mouseProperties.pageY

                            Vertical _ ->
                                Vertical <|
                                    Basics.clamp
                                        minimumPanelSize
                                        (model.windowWidth - minimumPanelSize)
                                        mouseProperties.pageX
                in
                ( { model | layout = updatedLayout }
                , Cmd.none
                )

            else
                ( { model | messagePanelResizable = False }
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


jumpUpdate : UserUpdate model msg -> Int -> Model model msg -> Model model msg
jumpUpdate update index model =
    let
        history =
            cachedHistory model

        currentMsg =
            History.getRecentMsg history

        currentModel =
            getLatestModel model.state

        ( indexModel, indexMsg ) =
            History.get update index history
    in
    { model
        | state = Paused index indexModel currentModel currentMsg history
        , modelExpando = Expando.merge indexModel model.modelExpando
        , messageExpando = Expando.merge indexMsg model.messageExpando
    }



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
                , messageExpando = Expando.init (History.getRecentMsg newHistory)
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

        expandoContent =
            [ div []
                [ div [] [ text "Message:" ]
                , Html.map MessageExpandoMsg <| Expando.view Nothing model.messageExpando
                ]
            , div []
                [ div [] [ text "Model:" ]
                , Html.map ModelExpandoMsg <| Expando.view Nothing model.modelExpando
                ]
            ]

        maybeIndex =
            case model.state of
                Running _ ->
                    Nothing

                Paused index _ _ _ _ ->
                    Just index

        historyToRender =
            cachedHistory model
    in
    node "body"
        (if model.messagePanelResizable then
            List.append bodyAttributes
                [ onResizeSidePanel
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
            Horizontal offset ->
                let
                    offsetPxStr =
                        String.fromInt offset ++ "px"
                in
                [ div
                    [ style "display" "block"
                    , style "width" "100%"
                    , style "height" offsetPxStr
                    , style "margin" "0"
                    , style "overflow" "auto"
                    , style "cursor" "default"
                    ]
                    expandoContent
                , viewSidebarHorizontal maybeIndex historyToRender offsetPxStr
                ]

            Vertical offset ->
                let
                    offsetPxStr =
                        String.fromInt offset ++ "px"
                in
                [ viewSidebar maybeIndex historyToRender offsetPxStr
                , div
                    [ style "display" "block"
                    , style "float" "left"
                    , style "height" "100%"
                    , style "width" <| "calc(100% - " ++ offsetPxStr ++ ")"
                    , style "margin" "0"
                    , style "overflow" "auto"
                    , style "cursor" "default"
                    ]
                    expandoContent
                ]
        )


viewSidebar : Maybe Int -> History model msg -> String -> Html (Msg msg)
viewSidebar maybeIndex history offsetStyle =
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
        , slider history maybeIndex
        , Html.map Jump (History.view maybeIndex history)
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


viewSidebarHorizontal : Maybe Int -> History model msg -> String -> Html (Msg msg)
viewSidebarHorizontal maybeIndex history offsetStyle =
    div
        [ style "position" "relative"
        , style "display" "block"
        , style "width" "100%"
        , style "height" <| "calc(100% - " ++ offsetStyle ++ ")"
        , style "color" "white"
        , style "background-color" "rgb(61, 61, 61)"
        ]
        [ draggableBorderHorizontal
        , slider history maybeIndex
        , Html.map Jump (History.view maybeIndex history)
        , playButton maybeIndex
        ]


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


type alias MouseProperties =
    { pageX : Int
    , pageY : Int
    , buttonIsPressed : Bool
    }


onResizeSidePanel : Attribute (Msg msg)
onResizeSidePanel =
    on "mousemove" (Decode.map ResizeSidePanel mouseMoveDecoder)


mouseMoveDecoder : Decoder MouseProperties
mouseMoveDecoder =
    let
        primaryButtonDetector value =
            Bitwise.and value 1 == 1
    in
    Decode.map3 MouseProperties
        (Decode.field "pageX" Decode.int)
        (Decode.field "pageY" Decode.int)
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
