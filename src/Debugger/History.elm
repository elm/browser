module Debugger.History exposing
  ( History
  , empty
  , size
  , getInitialModel
  , add
  , get
  , view
  , decoder
  , encode
  )


import Array exposing (Array)
import Elm.Kernel.Debugger
import Json.Decode as Decode
import Json.Encode as Encode
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Lazy exposing (..)
import Debugger.Metadata as Metadata



-- CONSTANTS


maxSnapshotSize : Int
maxSnapshotSize =
  64



-- HISTORY


type alias History model msg =
  { snapshots : Array (Snapshot model msg)
  , recent : RecentHistory model msg
  , numMessages : Int
  }


type alias RecentHistory model msg =
  { model : model
  , messages : List msg
  , numMessages : Int
  }


type alias Snapshot model msg =
  { model : model
  , messages : Array msg
  }


empty : model -> History model msg
empty model =
  History Array.empty (RecentHistory model [] 0) 0


size : History model msg -> Int
size history =
  history.numMessages


getInitialModel : History model msg -> model
getInitialModel  { snapshots, recent } =
  case Array.get 0 snapshots of
    Just { model } ->
      model

    Nothing ->
      recent.model



-- JSON


decoder : model -> (msg -> model -> model) -> Decode.Decoder (model, History model msg)
decoder initialModel update =
  let
    addMessage rawMsg (model, history) =
      let
        msg =
          jsToElm rawMsg
      in
        (update msg model, add msg model history)

    updateModel rawMsgs =
      List.foldl addMessage (initialModel, empty initialModel) rawMsgs
  in
    Decode.map updateModel (Decode.list Decode.value)


jsToElm : Encode.Value -> a
jsToElm =
  Elm.Kernel.Debugger.unsafeCoerce


encode : History model msg -> Encode.Value
encode { snapshots, recent } =
  Encode.list elmToJs <| Array.foldr encodeHelp (List.reverse recent.messages) snapshots


encodeHelp : Snapshot model msg -> List msg -> List msg
encodeHelp snapshot allMessages =
  Array.foldl (::) allMessages snapshot.messages


elmToJs : a -> Encode.Value
elmToJs =
  Elm.Kernel.Debugger.unsafeCoerce



-- ADD MESSAGES


add : msg -> model -> History model msg -> History model msg
add msg model { snapshots, recent, numMessages } =
  case addRecent msg model recent of
    (Just snapshot, newRecent) ->
      History (Array.push snapshot snapshots) newRecent (numMessages + 1)

    (Nothing, newRecent) ->
      History snapshots newRecent (numMessages + 1)


addRecent
  : msg
  -> model
  -> RecentHistory model msg
  -> ( Maybe (Snapshot model msg), RecentHistory model msg )
addRecent msg newModel { model, messages, numMessages } =
  if numMessages == maxSnapshotSize then
    ( Just (Snapshot model (Array.fromList messages))
    , RecentHistory newModel [msg] 1
    )

  else
    ( Nothing
    , RecentHistory model (msg :: messages) (numMessages + 1)
    )



-- GET SUMMARY


get : (msg -> model -> (model, a)) -> Int -> History model msg -> ( model, msg )
get update index { snapshots, recent, numMessages } =
  let
    snapshotMax =
      numMessages - recent.numMessages
  in
    if index >= snapshotMax then
      undone <|
        List.foldr (getHelp update) (Stepping (index - snapshotMax) recent.model) recent.messages

    else
      case Array.get (index // maxSnapshotSize) snapshots of
        Nothing ->
          Debug.todo "UI should only let you ask for real indexes!"

        Just { model, messages } ->
          undone <|
            Array.foldr (getHelp update) (Stepping (remainderBy maxSnapshotSize index) model) messages


type GetResult model msg
  = Stepping Int model
  | Done msg model


getHelp : (msg -> model -> (model, a)) -> msg -> GetResult model msg -> GetResult model msg
getHelp update msg getResult =
  case getResult of
    Done _ _ ->
      getResult

    Stepping n model ->
      if n == 0 then
        Done msg (Tuple.first (update msg model))

      else
        Stepping (n - 1) (Tuple.first (update msg model))


undone : GetResult model msg -> ( model, msg )
undone getResult =
  case getResult of
    Done msg model ->
      ( model, msg )

    Stepping _ _ ->
      Debug.todo "Bug in History.get"



-- VIEW


view : Maybe Int -> History model msg -> Html Int
view maybeIndex { snapshots, recent, numMessages } =
  let
    (index, className) =
      case maybeIndex of
        Nothing ->
          ( -1, "debugger-sidebar-messages" )
        Just i ->
          ( i, "debugger-sidebar-messages-paused" )

    oldStuff =
      lazy2 viewSnapshots index snapshots

    newStuff =
      Tuple.second <| List.foldl (consMsg index) (numMessages - 1, []) recent.messages
  in
    div [ class className ] (oldStuff :: newStuff)



-- VIEW SNAPSHOTS


viewSnapshots : Int -> Array (Snapshot model msg) -> Html Int
viewSnapshots currentIndex snapshots =
  let
    highIndex =
      maxSnapshotSize * Array.length snapshots
  in
    div [] <| Tuple.second <|
      Array.foldr (consSnapshot currentIndex) (highIndex, []) snapshots


consSnapshot : Int -> Snapshot model msg -> ( Int, List (Html Int) ) -> ( Int, List (Html Int) )
consSnapshot currentIndex snapshot (index, rest) =
  let
    nextIndex =
      index - maxSnapshotSize

    currentIndexHelp =
      if nextIndex <= currentIndex && currentIndex < index then currentIndex else -1
  in
    ( index - maxSnapshotSize
    , lazy3 viewSnapshot currentIndexHelp index snapshot :: rest
    )


viewSnapshot : Int -> Int -> Snapshot model msg -> Html Int
viewSnapshot currentIndex index { messages } =
  div [] <| Tuple.second <|
    Array.foldl (consMsg currentIndex) (index - 1, []) messages



-- VIEW MESSAGE


consMsg : Int -> msg -> ( Int, List (Html Int) ) -> ( Int, List (Html Int) )
consMsg currentIndex msg (index, rest) =
  ( index - 1
  , lazy3 viewMessage currentIndex index msg :: rest
  )


viewMessage : Int -> Int -> msg -> Html Int
viewMessage currentIndex index msg =
  let
    className =
      if currentIndex == index then
        "messages-entry messages-entry-selected"

      else
        "messages-entry"

    messageName =
      Elm.Kernel.Debugger.messageToString msg
  in
    div
      [ class className
      , onClick index
      ]
      [ span [class "messages-entry-content", title messageName ] [ text messageName ]
      , span [class "messages-entry-index"] [ text (String.fromInt index) ]
      ]
