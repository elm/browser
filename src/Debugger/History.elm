module Debugger.History exposing
    ( History
    , add
    , decoder
    , empty
    , encode
    , get
    , getInitialModel
    , getRecent
    , idForMessageIndex
    , size
    , view
    )

import Array exposing (Array)
import Debugger.Expando as Expando exposing (Expando)
import Debugger.Metadata as Metadata
import Elm.Kernel.Debugger
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Keyed
import Html.Lazy exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Set exposing (Set)



-- CONSTANTS


maxSnapshotSize : Int
maxSnapshotSize =
    31



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
getInitialModel { snapshots, recent } =
    case Array.get 0 snapshots of
        Just { model } ->
            model

        Nothing ->
            recent.model



-- JSON


decoder : model -> (msg -> model -> model) -> Decode.Decoder ( model, History model msg )
decoder initialModel update =
    let
        addMessage rawMsg ( model, history ) =
            let
                msg =
                    jsToElm rawMsg
            in
            ( update msg model, add msg model history )

        updateModel rawMsgs =
            List.foldl addMessage ( initialModel, empty initialModel ) rawMsgs
    in
    Decode.map updateModel (Decode.list Decode.value)


jsToElm : Encode.Value -> a
jsToElm =
    Elm.Kernel.Json.unwrap
        >> Elm.Kernel.Debugger.unsafeCoerce


encode : History model msg -> Encode.Value
encode { snapshots, recent } =
    Encode.list elmToJs <| Array.foldr encodeHelp (List.reverse recent.messages) snapshots


encodeHelp : Snapshot model msg -> List msg -> List msg
encodeHelp snapshot allMessages =
    Array.foldl (::) allMessages snapshot.messages


elmToJs : a -> Encode.Value
elmToJs =
    Elm.Kernel.Json.wrap
        >> Elm.Kernel.Debugger.unsafeCoerce



-- ADD MESSAGES


add : msg -> model -> History model msg -> History model msg
add msg model { snapshots, recent, numMessages } =
    case addRecent msg model recent of
        ( Just snapshot, newRecent ) ->
            History (Array.push snapshot snapshots) newRecent (numMessages + 1)

        ( Nothing, newRecent ) ->
            History snapshots newRecent (numMessages + 1)


addRecent :
    msg
    -> model
    -> RecentHistory model msg
    -> ( Maybe (Snapshot model msg), RecentHistory model msg )
addRecent msg newModel { model, messages, numMessages } =
    if numMessages == maxSnapshotSize then
        ( Just (Snapshot model (Array.fromList messages))
        , RecentHistory newModel [ msg ] 1
        )

    else
        ( Nothing
        , RecentHistory model (msg :: messages) (numMessages + 1)
        )



-- GET SUMMARY


get : (msg -> model -> ( model, a )) -> Int -> History model msg -> ( model, msg )
get update index history =
    let
        recent =
            history.recent

        snapshotMax =
            history.numMessages - recent.numMessages
    in
    if index >= snapshotMax then
        undone <|
            List.foldr (getHelp update) (Stepping (index - snapshotMax) recent.model) recent.messages

    else
        case Array.get (index // maxSnapshotSize) history.snapshots of
            Nothing ->
                get update index history

            -- Debug.crash "UI should only let you ask for real indexes!"
            Just { model, messages } ->
                undone <|
                    Array.foldr (getHelp update) (Stepping (remainderBy maxSnapshotSize index) model) messages


getRecent : (msg -> model -> ( model, a )) -> History model msg -> ( model, msg )
getRecent update history =
    get update (history.numMessages - 1) history


type GetResult model msg
    = Stepping Int model
    | Done msg model


getHelp : (msg -> model -> ( model, a )) -> msg -> GetResult model msg -> GetResult model msg
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
            undone getResult



-- Debug.crash "Bug in History.get"
-- VIEW


view : Maybe Int -> History model msg -> Html Int
view maybeIndex { snapshots, recent, numMessages } =
    let
        ( isPaused, index, height ) =
            case maybeIndex of
                Nothing ->
                    ( False, -1, "calc(100% - 48px)" )

                Just i ->
                    ( True, i, "calc(100% - 78px)" )

        highIndex =
            maxSnapshotSize * Array.length snapshots

        onlyRenderRecentMessages =
            isPaused || Array.length snapshots < 2

        oldStuff =
            if onlyRenderRecentMessages then
                lazy3 viewAllSnapshots index 0 snapshots

            else
                lazy3 viewRecentSnapshots index recent.numMessages snapshots

        newStuff =
            recent.messages
                |> List.foldr (consMsg index) ( highIndex, [] )
                |> Tuple.second
                |> Html.Keyed.node "div" []
    in
    div
        [ id "elm-debugger-sidebar"
        , style "width" "100%"
        , style "overflow-y" "auto"
        , style "height" height
        ]
        (styles
            :: newStuff
            :: oldStuff
            :: (if onlyRenderRecentMessages then
                    []

                else
                    [ showMoreButton (numMessages - 1 - maxSnapshotSize * 2) ]
               )
        )



-- VIEW SNAPSHOTS


viewAllSnapshots : Int -> Int -> Array (Snapshot model msg) -> Html Int
viewAllSnapshots selectedIndex startIndex snapshots =
    div [] <|
        Tuple.second <|
            Array.foldl (consSnapshot selectedIndex) ( startIndex, [] ) snapshots


viewRecentSnapshots : Int -> Int -> Array (Snapshot model msg) -> Html Int
viewRecentSnapshots selectedIndex recentMessagesNum snapshots =
    let
        arrayLength =
            Array.length snapshots

        messagesToFill =
            maxSnapshotSize - recentMessagesNum

        startingIndex =
            (arrayLength * maxSnapshotSize) - maxSnapshotSize - messagesToFill

        snapshotsToRender =
            case ( Array.get (arrayLength - 2) snapshots, Array.get (arrayLength - 1) snapshots ) of
                ( Just fillerSnapshot, Just recentSnapshot ) ->
                    Array.fromList
                        [ { model = fillerSnapshot.model
                          , messages = Array.slice 0 messagesToFill fillerSnapshot.messages
                          }
                        , recentSnapshot
                        ]

                _ ->
                    snapshots
    in
    viewAllSnapshots selectedIndex startingIndex snapshotsToRender


consSnapshot : Int -> Snapshot model msg -> ( Int, List (Html Int) ) -> ( Int, List (Html Int) )
consSnapshot selectedIndex snapshot ( index, rest ) =
    let
        nextIndex =
            index + Array.length snapshot.messages

        selectedIndexHelp =
            if nextIndex > selectedIndex && selectedIndex >= index then
                selectedIndex

            else
                -1
    in
    ( nextIndex
    , lazy3 viewSnapshot selectedIndexHelp index snapshot :: rest
    )


viewSnapshot : Int -> Int -> Snapshot model msg -> Html Int
viewSnapshot selectedIndex index { messages } =
    Html.Keyed.node "div" [] <|
        Tuple.second <|
            Array.foldr (consMsg selectedIndex) ( index, [] ) messages



-- VIEW MESSAGE


consMsg : Int -> msg -> ( Int, List ( String, Html Int ) ) -> ( Int, List ( String, Html Int ) )
consMsg currentIndex msg ( index, rest ) =
    ( index + 1
    , ( String.fromInt index, lazy3 viewMessage currentIndex index msg ) :: rest
    )


viewMessage : Int -> Int -> msg -> Html Int
viewMessage currentIndex index msg =
    let
        className =
            if currentIndex == index then
                "elm-debugger-entry elm-debugger-entry-selected"

            else
                "elm-debugger-entry"

        messageName =
            Elm.Kernel.Debugger.messageToString msg
    in
    div
        [ id (idForMessageIndex index)
        , class className
        , onClick index
        ]
        [ span
            [ title messageName
            , class "elm-debugger-entry-content"
            ]
            [ text messageName
            ]
        , span
            [ class "elm-debugger-entry-index"
            ]
            [ text (String.fromInt index)
            ]
        ]


showMoreButton : Int -> Html Int
showMoreButton nextIndex =
    let
        labelText =
            "View more messages"
    in
    div
        [ class "elm-debugger-entry"
        , onClick nextIndex
        ]
        [ span
            [ title labelText
            , class "elm-debugger-entry-content"
            ]
            [ text labelText
            ]
        , span
            [ class "elm-debugger-entry-index"
            ]
            []
        ]


idForMessageIndex : Int -> String
idForMessageIndex index =
    "msg-" ++ String.fromInt index



-- STYLES


styles : Html msg
styles =
    Html.node "style" [] [ text """

.elm-debugger-entry {
  cursor: pointer;
  width: 100%;
  box-sizing: border-box;
  padding: 8px;
}

.elm-debugger-entry:hover {
  background-color: rgb(41, 41, 41);
}

.elm-debugger-entry-selected, .elm-debugger-entry-selected:hover {
  background-color: rgb(10, 10, 10);
}

.elm-debugger-entry-content {
  width: calc(100% - 40px);
  padding: 0 5px;
  box-sizing: border-box;
  text-overflow: ellipsis;
  white-space: nowrap;
  overflow: hidden;
  display: inline-block;
}

.elm-debugger-entry-index {
  color: #666;
  width: 40px;
  text-align: right;
  display: block;
  float: right;
}

""" ]
