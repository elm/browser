module Debugger.Main exposing
  ( wrapInit
  , wrapUpdate
  , wrapSubs
  , wrapView
  , cornerView
  , popoutView
  )


import Elm.Kernel.Debugger
import Json.Decode as Decode
import Json.Encode as Encode
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Task exposing (Task)
import Debugger.Expando as Expando exposing (Expando)
import Debugger.History as History exposing (History)
import Debugger.Metadata as Metadata exposing (Metadata)
import Debugger.Overlay as Overlay
import Debugger.Report as Report



-- VIEW


wrapView : (model -> Html msg) -> Model model msg -> Html (Msg msg)
wrapView view model =
  Html.map UserMsg (view (getCurrentModel model.state))



-- SUBSCRIPTIONS


wrapSubs : (model -> Sub msg) -> Model model msg -> Sub (Msg msg)
wrapSubs subscriptions model =
  Sub.map UserMsg (subscriptions (getLatestModel model.state))



-- MODEL


type alias Model model msg =
  { history : History model msg
  , state : State model
  , expando : Expando
  , metadata : Result Metadata.Error Metadata
  , overlay : Overlay.State
  , popout : Popout
  }


type Popout = Popout Popout


type State model
  = Running model
  | Paused Int model model



-- INIT


wrapInit : Encode.Value -> Popout -> (flags -> (model, Cmd msg)) -> flags -> (Model model msg, Cmd (Msg msg))
wrapInit metadata popout init flags =
  let
    (userModel, userCommands) =
      init flags
  in
  ( { history = History.empty userModel
    , state = Running userModel
    , expando = Expando.init userModel
    , metadata = Metadata.decode metadata
    , overlay = Overlay.none
    , popout = popout
    }
  , Cmd.map UserMsg userCommands
  )



-- UPDATE


type Msg msg
  = NoOp
  | UserMsg msg
  | ExpandoMsg Expando.Msg
  | Resume
  | Jump Int
  | Open
  | Up
  | Down
  | Import
  | Export
  | Upload String
  | OverlayMsg Overlay.Msg


type alias UserUpdate model msg =
  msg -> model -> ( model, Cmd msg )


wrapUpdate : UserUpdate model msg -> Msg msg -> Model model msg -> (Model model msg, Cmd (Msg msg))
wrapUpdate update msg model =
  case msg of
    NoOp ->
      ( model, Cmd.none )

    UserMsg userMsg ->
      let
        userModel = getLatestModel model.state
        newHistory = History.add userMsg userModel model.history
        (newUserModel, userCmds) = update userMsg userModel
        commands = Cmd.map UserMsg userCmds
      in
      case model.state of
        Running _ ->
          ( { model
              | history = newHistory
              , state = Running newUserModel
              , expando = Expando.merge newUserModel model.expando
            }
          , Cmd.batch [ commands, scroll model.popout ]
          )

        Paused index indexModel _ ->
          ( { model
              | history = newHistory
              , state = Paused index indexModel newUserModel
            }
          , commands
          )

    ExpandoMsg eMsg ->
      ( { model | expando = Expando.update eMsg model.expando }
      , Cmd.none
      )

    Resume ->
      case model.state of
        Running _ ->
          ( model, Cmd.none )

        Paused _ _ userModel ->
          ( { model
              | state = Running userModel
              , expando = Expando.merge userModel model.expando
            }
          , scroll model.popout
          )

    Jump index ->
      let
        (indexModel, indexMsg) =
          History.get update index model.history
      in
      ( { model
          | state = Paused index indexModel (getLatestModel model.state)
          , expando = Expando.merge indexModel model.expando
        }
      , Cmd.none
      )

    Open ->
      ( { model | popout = Elm.Kernel.Debugger.open model.popout }
      , Cmd.none
      )

    Up ->
      let
        index =
          case model.state of
            Paused i _ _ ->
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

        Paused index _ userModel ->
          if index == History.size model.history - 1 then
            wrapUpdate update Resume model
          else
            wrapUpdate update (Jump (index + 1)) model

    Import ->
      withGoodMetadata model <| \_ ->
        ( model, upload )

    Export ->
      withGoodMetadata model <| \metadata ->
        ( model, download metadata model.history )

    Upload jsonString ->
      withGoodMetadata model <| \metadata ->
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


upload : Cmd (Msg msg)
upload =
  Task.perform Upload (Elm.Kernel.Debugger.upload ())


download : Metadata -> History model msg -> Cmd (Msg msg)
download metadata history =
  let
    historyLength =
      History.size history

    json =
      Encode.object
        [ ("metadata", Metadata.encode metadata)
        , ("history", History.encode history)
        ]
  in
    Task.perform (\_ -> NoOp) (Elm.Kernel.Debugger.download historyLength json)



-- UPDATE OVERLAY


withGoodMetadata
  : Model model msg
  -> (Metadata -> (Model model msg, Cmd (Msg msg)))
  -> (Model model msg, Cmd (Msg msg))
withGoodMetadata model func =
  case model.metadata of
    Ok metadata ->
      func metadata

    Err error ->
      ( { model | overlay = Overlay.badMetadata error }
      , Cmd.none
      )


loadNewHistory
  : Encode.Value
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

    Ok (latestUserModel, newHistory) ->
      ( { model
          | history = newHistory
          , state = Running latestUserModel
          , expando = Expando.init latestUserModel
          , overlay = Overlay.none
        }
      , Cmd.none
      )


runIf : Bool -> Task Never () -> Cmd (Msg msg)
runIf bool task =
  if bool then
    Task.perform (always NoOp) task
  else
    Cmd.none


getLatestModel : State model -> model
getLatestModel state =
  case state of
    Running model ->
      model

    Paused _ _ model ->
      model


getCurrentModel : State model -> model
getCurrentModel state =
  case state of
    Running model ->
      model

    Paused _ model _ ->
      model



-- CORNER VIEW


cornerView : Model model msg -> ( Overlay.Block, Html (Msg msg) )
cornerView { history, state, overlay, popout } =
  let
    isPaused =
      case state of
        Running _ ->
          False

        Paused _ _ _ ->
          True

    isDebuggerOpen =
      Elm.Kernel.Debugger.isOpen popout
  in
  Overlay.view overlayConfig isPaused isDebuggerOpen (History.size history) overlay


overlayConfig : Overlay.Config (Msg msg)
overlayConfig =
  { resume = Resume
  , open = Open
  , importHistory = Import
  , exportHistory = Export
  , wrap = OverlayMsg
  }



-- BIG DEBUG VIEW


popoutView : Model model msg -> Html (Msg msg)
popoutView { history, state, expando } =
  div
    [ id "debugger" ]
    [ styles
    , viewSidebar state history
    , Html.map ExpandoMsg <|
        div [ id "values" ] [ Expando.view Nothing expando ]
    ]


viewSidebar : State model -> History model msg -> Html (Msg msg)
viewSidebar state history =
  let
    maybeIndex =
      case state of
        Running _ ->
          Nothing

        Paused index _ _ ->
          Just index
  in
    div [ class "debugger-sidebar" ]
      [ Html.map Jump (History.view maybeIndex history)
      , playButton maybeIndex
      ]


playButton : Maybe Int -> Html (Msg msg)
playButton maybeIndex =
  div [ class "debugger-sidebar-controls" ]
    [ viewResumeButton maybeIndex
    , div [ class "debugger-sidebar-controls-import-export" ]
        [ button Import "Import"
        , text " / "
        , button Export "Export"
        ]
    ]


button msg label =
  span
    [ onClick msg
    , style "cursor" "pointer"
    ]
    [ text label ]


viewResumeButton maybeIndex =
  case maybeIndex of
    Nothing ->
      text ""

    Just _ ->
      resumeButton


resumeButton =
  div
    [ onClick Resume
    , class "debugger-sidebar-controls-resume"
    ]
    [ text "Resume"
    ]



-- STYLE


styles : Html msg
styles =
  Html.node "style" [] [ text """

html {
    overflow: hidden;
    height: 100%;
}

body {
    height: 100%;
    overflow: auto;
}

#debugger {
  width: 100%
  height: 100%;
  font-family: monospace;
}

#values {
  display: block;
  float: left;
  height: 100%;
  width: calc(100% - 30ch);
  margin: 0;
  overflow: auto;
  cursor: default;
}

.debugger-sidebar {
  display: block;
  float: left;
  width: 30ch;
  height: 100%;
  color: white;
  background-color: rgb(61, 61, 61);
}

.debugger-sidebar-controls {
  width: 100%;
  text-align: center;
  background-color: rgb(50, 50, 50);
}

.debugger-sidebar-controls-import-export {
  width: 100%;
  height: 24px;
  line-height: 24px;
  font-size: 12px;
}

.debugger-sidebar-controls-resume {
  width: 100%;
  height: 30px;
  line-height: 30px;
  cursor: pointer;
}

.debugger-sidebar-controls-resume:hover {
  background-color: rgb(41, 41, 41);
}

.debugger-sidebar-messages {
  width: 100%;
  overflow-y: auto;
  height: calc(100% - 24px);
}

.debugger-sidebar-messages-paused {
  width: 100%;
  overflow-y: auto;
  height: calc(100% - 54px);
}

.messages-entry {
  cursor: pointer;
  width: 100%;
}

.messages-entry:hover {
  background-color: rgb(41, 41, 41);
}

.messages-entry-selected, .messages-entry-selected:hover {
  background-color: rgb(10, 10, 10);
}

.messages-entry-content {
  width: calc(100% - 7ch);
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 1ch;
  text-overflow: ellipsis;
  white-space: nowrap;
  overflow: hidden;
  display: inline-block;
}

.messages-entry-index {
  color: #666;
  width: 5ch;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-right: 1ch;
  text-align: right;
  display: block;
  float: right;
}

""" ]
