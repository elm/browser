module Debugger.Main exposing
  ( init
  , update
  , subs
  , view
  , cornerView
  , popoutView
  )


import Elm.Kernel.Debugger
import Json.Decode as Decode
import Json.Encode as Encode
import Task exposing (Task)
import Debugger.Expando as Expando exposing (Expando)
import Debugger.Html as Html exposing (Html)
import Debugger.History as History exposing (History)
import Debugger.Metadata as Metadata exposing (Metadata)
import Debugger.Overlay as Overlay
import Debugger.Report as Report



-- MODEL


type alias Model model msg =
  { history : History model msg
  , state : State model
  , expando : Expando
  , metadata : Result Metadata.Error Metadata
  , overlay : Overlay.State
  , token : Token
  , isDebuggerOpen : Bool
  }


type Token = Token Token


type State model
  = Running model
  | Paused Int model model


init : Encode.Value -> Token -> (flags -> (model, Cmd msg)) -> flags -> (Model model msg, Cmd (Msg msg))
init metadata token userInit flags =
  let
    (userModel, userCommands) =
      userInit flags
  in
  ( { history = History.empty userModel
    , state = Running userModel
    , expando = Expando.init userModel
    , metadata = Metadata.decode metadata
    , overlay = Overlay.none
    , token = token
    , isDebuggerOpen = False
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
  | Close
  | Up
  | Down
  | Import
  | Export
  | Upload String
  | OverlayMsg Overlay.Msg


type alias UserUpdate model msg =
  msg -> model -> ( model, Cmd msg )


update : UserUpdate model msg -> Msg msg -> Model model msg -> (Model model msg, Cmd (Msg msg))
update userUpdate msg model =
  case msg of
    NoOp ->
      ( model, Cmd.none )

    UserMsg userMsg ->
      updateUserMsg userUpdate userMsg model

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
          , runIf model.isDebuggerOpen (scroll model.token)
          )

    Jump index ->
      let
        (indexModel, indexMsg) =
          History.get userUpdate index model.history
      in
        ( { model
            | state = Paused index indexModel (getLatestModel model.state)
            , expando = Expando.merge indexModel model.expando
          }
        , Cmd.none
        )

    Open ->
      ( { model | isDebuggerOpen = True }, Cmd.none )

    Close ->
      ( { model | isDebuggerOpen = False }, Cmd.none )

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
          update userUpdate (Jump (index - 1)) model
        else
          ( model, Cmd.none )

    Down ->
      case model.state of
        Running _ ->
          ( model, Cmd.none )

        Paused index _ userModel ->
          if index == History.size model.history - 1 then
            update userUpdate Resume model
          else
            update userUpdate (Jump (index + 1)) model

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
            loadNewHistory rawHistory userUpdate model

    OverlayMsg overlayMsg ->
      case Overlay.close overlayMsg model.overlay of
        Nothing ->
          ( { model | overlay = Overlay.none }, Cmd.none )

        Just rawHistory ->
          loadNewHistory rawHistory userUpdate model



-- COMMANDS


scroll : Token -> Task x ()
scroll =
  Elm.Kernel.Debugger.scroll


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
      ( { model | overlay = Overlay.badMetadata error }, Cmd.none )


loadNewHistory
  : Encode.Value
  -> UserUpdate model msg
  -> Model model msg
  -> ( Model model msg, Cmd (Msg msg) )
loadNewHistory rawHistory userUpdate model =
  let
    initialUserModel =
      History.getInitialModel model.history

    pureUserUpdate msg userModel =
      Tuple.first (userUpdate msg userModel)

    decoder =
      History.decoder initialUserModel pureUserUpdate
  in
    case Decode.decodeValue decoder rawHistory of
      Err _ ->
        ( { model | overlay = Overlay.corruptImport }, Cmd.none )

      Ok (latestUserModel, newHistory) ->
        ( { model
            | history = newHistory
            , state = Running latestUserModel
            , expando = Expando.init latestUserModel
            , overlay = Overlay.none
          }
        , Cmd.none
        )



-- UPDATE - USER MESSAGES


updateUserMsg : UserUpdate model msg -> msg -> Model model msg -> (Model model msg, Cmd (Msg msg))
updateUserMsg userUpdate userMsg model =
  let
    userModel =
      getLatestModel model.state

    newHistory =
      History.add userMsg userModel model.history

    (newUserModel, userCmds) =
      userUpdate userMsg userModel

    commands =
      Cmd.map UserMsg userCmds
  in
    case model.state of
      Running _ ->
        ( { model
            | history = newHistory
            , state = Running newUserModel
            , expando = Expando.merge newUserModel model.expando
          }
        , Cmd.batch [ commands, runIf model.isDebuggerOpen (scroll model.token) ]
        )

      Paused index indexModel _ ->
        ( { model
            | history = newHistory
            , state = Paused index indexModel newUserModel
          }
        , commands
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



-- SUBSCRIPTIONS


subs : (model -> Sub msg) -> Model model msg -> Sub (Msg msg)
subs userSubscriptions model =
  Sub.map UserMsg (userSubscriptions (getLatestModel model.state))



-- VIEW


view : (model -> Html msg) -> Model model msg -> Html (Msg msg)
view userView model =
  let
    currentModel =
      case model.state of
        Running userModel ->
          userModel

        Paused _ oldModel _ ->
          oldModel
  in
    Html.map UserMsg (userView currentModel)



-- CORNER VIEW


cornerView : Model model msg -> ( Overlay.Block, Html (Msg msg) )
cornerView { history, state, overlay, isDebuggerOpen } =
  let
    isPaused =
      case state of
        Running _ ->
          False

        Paused _ _ _ ->
          True
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
  Html.div
    [ Html.id "debugger" ]
    [ styles
    , viewSidebar state history
    , Html.map ExpandoMsg <|
        Html.div [ Html.id "values" ] [ Expando.view Nothing expando ]
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
    Html.div [ Html.class "debugger-sidebar" ]
      [ Html.map Jump (History.view maybeIndex history)
      , playButton maybeIndex
      ]


playButton : Maybe Int -> Html (Msg msg)
playButton maybeIndex =
  Html.div [ Html.class "debugger-sidebar-controls" ]
    [ viewResumeButton maybeIndex
    , Html.div [ Html.class "debugger-sidebar-controls-import-export" ]
        [ button Import "Import"
        , Html.text " / "
        , button Export "Export"
        ]
    ]


button msg label =
  Html.span
    [ Html.onClick msg
    , Html.style "cursor" "pointer"
    ]
    [ Html.text label ]


viewResumeButton maybeIndex =
  case maybeIndex of
    Nothing ->
      Html.text ""

    Just _ ->
      resumeButton


resumeButton =
  Html.div
    [ Html.onClick Resume
    , Html.class "debugger-sidebar-controls-resume"
    ]
    [ Html.text "Resume"
    ]



-- STYLE


styles : Html msg
styles =
  Html.inlineStyle """

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

"""
