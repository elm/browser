module Debugger.Main exposing
    ( cornerView
    , getUserModel
    , popoutView
    , wrapInit
    , wrapSubs
    , wrapUpdate
    )

import Debugger.Expando as Expando exposing (Expando)
import Debugger.History as History exposing (History)
import Debugger.Metadata as Metadata exposing (Metadata)
import Debugger.Overlay as Overlay
import Debugger.Report as Report
import Elm.Kernel.Debugger
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, onMouseDown, onMouseUp)
import Json.Decode as Decode
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
    , expandTarget : ExpandTarget
    , expando : Expando
    , metadata : Result Metadata.Error Metadata
    , overlay : Overlay.State
    , popout : Popout
    , sidePanelResizable : Bool
    , sidePanelWidth : Int
    }


type Popout
    = Popout Popout


type ExpandTarget
    = ExpandModel
    | ExpandMsg



-- STATE


type State model msg
    = Running model
    | Paused Int model model msg


getLatestModel : State model msg -> model
getLatestModel state =
    case state of
        Running model ->
            model

        Paused _ _ model _ ->
            model


getCurrentModel : State model msg -> model
getCurrentModel state =
    case state of
        Running model ->
            model

        Paused _ model _ _ ->
            model


isPaused : State model msg -> Bool
isPaused state =
    case state of
        Running _ ->
            False

        Paused _ _ _ _ ->
            True



-- INIT


wrapInit : Encode.Value -> Popout -> (flags -> ( model, Cmd msg )) -> flags -> ( Model model msg, Cmd (Msg msg) )
wrapInit metadata popout init flags =
    let
        ( userModel, userCommands ) =
            init flags
    in
    ( { history = History.empty userModel
      , state = Running userModel
      , expandTarget = ExpandModel
      , expando = Expando.init userModel
      , metadata = Metadata.decode metadata
      , overlay = Overlay.none
      , popout = popout
      , sidePanelResizable = False
      , sidePanelWidth = 300
      }
    , Cmd.map UserMsg userCommands
    )



-- UPDATE


type Msg msg
    = NoOp
    | UserMsg msg
    | SetExpandTarget ExpandTarget
    | ExpandoMsg Expando.Msg
    | HistoryMsg History.Msg
    | Resume
    | Jump Int
    | Open
    | Up
    | Down
    | EnableSidePanelResizing Bool
    | ResizeSidePanel Int
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
                        , expando =
                            case model.expandTarget of
                                ExpandModel ->
                                    Expando.init newUserModel

                                ExpandMsg ->
                                    Expando.init userMsg
                      }
                    , Cmd.batch [ commands, scroll model.popout ]
                    )

                Paused index indexModel _ _ ->
                    ( { model
                        | history = newHistory
                        , state = Paused index indexModel newUserModel userMsg
                      }
                    , commands
                    )

        SetExpandTarget target ->
            let
                ( userModel, userMsg ) =
                    case model.state of
                        Running _ ->
                            History.getRecent model.history

                        Paused idx _ _ _ ->
                            History.get update idx model.history
            in
            ( { model
                | expandTarget = target
                , expando =
                    case target of
                        ExpandModel ->
                            Expando.init userModel

                        ExpandMsg ->
                            Expando.init userMsg
              }
            , Cmd.none
            )

        ExpandoMsg eMsg ->
            ( { model | expando = Expando.update eMsg model.expando }
            , Cmd.none
            )

        HistoryMsg historyMsg ->
            case historyMsg of
                History.SelectMsg idx ->
                    wrapUpdate update (Jump idx) model

                History.ToggleMulti id ->
                    let
                        currentHistory =
                            model.history
                    in
                    ( { model
                        | history =
                            { currentHistory
                                | messageHierarchy =
                                    History.openMultiContainer id currentHistory.messageHierarchy
                            }
                      }
                    , Cmd.none
                    )

        Resume ->
            case model.state of
                Running _ ->
                    ( model, Cmd.none )

                Paused _ _ userModel userMsg ->
                    ( { model
                        | state = Running userModel
                        , expando =
                            case model.expandTarget of
                                ExpandModel ->
                                    Expando.merge userModel model.expando

                                ExpandMsg ->
                                    Expando.merge userMsg model.expando
                      }
                    , scroll model.popout
                    )

        Jump index ->
            let
                ( currentModel, currentMsg ) =
                    History.getRecent model.history

                ( indexModel, indexMsg ) =
                    History.get update index model.history
            in
            ( { model
                | state = Paused index indexModel currentModel currentMsg
                , expando =
                    case model.expandTarget of
                        ExpandModel ->
                            Expando.merge indexModel model.expando

                        ExpandMsg ->
                            Expando.merge indexMsg model.expando
              }
            , Cmd.none
            )

        Open ->
            ( model
            , Task.perform (\_ -> NoOp) (Elm.Kernel.Debugger.open model.popout)
            )

        Up ->
            let
                index =
                    case model.state of
                        Paused i _ _ _ ->
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

                Paused index _ userModel userMsg ->
                    if index == History.size model.history - 1 then
                        wrapUpdate update Resume model

                    else
                        wrapUpdate update (Jump (index + 1)) model

        EnableSidePanelResizing enabled ->
            ( { model | sidePanelResizable = enabled }
            , Cmd.none
            )

        ResizeSidePanel currentX ->
            ( { model | sidePanelWidth = Basics.max 150 currentX }
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
                , expandTarget = ExpandModel
                , expando = Expando.init latestUserModel
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
popoutView { history, state, expandTarget, expando, sidePanelWidth, sidePanelResizable } =
    let
        bodyAttributes =
            [ style "margin" "0"
            , style "padding" "0"
            , style "width" "100%"
            , style "height" "100%"
            , style "font-family" "monospace"
            , style "overflow" "auto"
            ]

        sidePanelWidthStyle =
            String.fromInt sidePanelWidth ++ "px"
    in
    node "body"
        (if sidePanelResizable then
            List.append bodyAttributes
                [ onMouseMove ResizeSidePanel
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
        [ viewSidebar state history sidePanelWidthStyle
        , div
            [ style "display" "block"
            , style "float" "left"
            , style "height" "100%"
            , style "width" <| "calc(100% - " ++ sidePanelWidthStyle ++ ")"
            , style "margin" "0"
            , style "overflow" "auto"
            , style "cursor" "default"
            ]
            [ expandConfigPanel expandTarget
            , Html.map ExpandoMsg <| Expando.view Nothing expando
            ]
        ]


viewSidebar : State model msg -> History model msg -> String -> Html (Msg msg)
viewSidebar state history sidePanelWidthStyle =
    let
        maybeIndex =
            case state of
                Running _ ->
                    Nothing

                Paused index _ _ _ ->
                    Just index
    in
    div
        [ style "position" "relative"
        , style "display" "block"
        , style "float" "left"
        , style "width" sidePanelWidthStyle
        , style "height" "100%"
        , style "color" "white"
        , style "background-color" "rgb(61, 61, 61)"
        ]
        [ draggableBorder
        , slider history maybeIndex
        , Html.map HistoryMsg (History.view maybeIndex history)
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


onMouseMove : (Int -> Msg msg) -> Attribute (Msg msg)
onMouseMove toMsg =
    on "mousemove" (Decode.map toMsg (Decode.field "pageX" Decode.int))


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
            , onInput (String.toInt >> Maybe.withDefault lastIndex >> Jump)
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


expandConfigPanel : ExpandTarget -> Html (Msg msg)
expandConfigPanel target =
    div
        [ style "height" "24px"
        , style "width" "100%"
        , style "background-color" "rgb(50, 50, 50)"
        , style "color" "white"
        , style "text-align" "right"
        , style "box-sizing" "border-box"
        , style "padding" "2px 10px"
        ]
        [ text "Expand: "
        , selectableTextButton (target == ExpandMsg) (SetExpandTarget ExpandMsg) "Message"
        , text " / "
        , selectableTextButton (target == ExpandModel) (SetExpandTarget ExpandModel) "Model"
        ]


selectableTextButton : Bool -> msg -> String -> Html msg
selectableTextButton selected msg label =
    let
        baseAttributes =
            [ onClick msg
            , style "cursor" "pointer"
            ]
    in
    span
        (if selected then
            style "text-decoration" "underline" :: baseAttributes

         else
            baseAttributes
        )
        [ text label ]
