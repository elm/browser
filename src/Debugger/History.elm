module Debugger.History exposing
    ( History
    , Msg(..)
    , add
    , addLog
    , clearLogs
    , decoder
    , empty
    , encode
    , get
    , getInitialModel
    , getLogs
    , getRecent
    , openMultiContainer
    , size
    , view
    )

import Array exposing (Array)
import Debugger.Expando as Expando exposing (Expando)
import Debugger.Metadata as Metadata
import Dict exposing (Dict)
import Elm.Kernel.Debugger
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Lazy exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Set exposing (Set)



-- CONSTANTS


maxSnapshotSize : Int
maxSnapshotSize =
    64



-- HISTORY


type alias History model msg =
    { snapshots : Array (Snapshot model msg)
    , recent : RecentHistory model msg
    , numMessages : Int
    , messageHierarchy : MsgHierarchy msg
    , logs : Dict String Expando
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


type alias MsgHierarchy msg =
    { nextMultiID : Int
    , openMultis : Set Int
    , list : List (MsgContainer msg)
    }


type MsgContainer msg
    = Single (List String) msg
    | Multi Int String (List (MsgContainer msg))


empty : model -> History model msg
empty model =
    History Array.empty (RecentHistory model [] 0) 0 emptyHierarchy Dict.empty


emptyHierarchy : MsgHierarchy msg
emptyHierarchy =
    { nextMultiID = 0
    , openMultis = Set.empty
    , list = []
    }


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


addToHierarchy : msg -> MsgHierarchy msg -> MsgHierarchy msg
addToHierarchy msg hierarchy =
    let
        messagePath =
            Expando.messagePath msg
    in
    addToHierarchyWithPath msg (Expando.messagePath msg) hierarchy


addToHierarchyWithPath : msg -> List String -> MsgHierarchy msg -> MsgHierarchy msg
addToHierarchyWithPath msg path hierarchy =
    case hierarchy.list of
        [] ->
            { hierarchy
                | list =
                    [ Single path msg ]
            }

        (Single (lastPrefix :: lastRestPath) lastMsg) :: rest ->
            case path of
                prefix :: restPath ->
                    if lastPrefix == prefix then
                        let
                            subHierarchy =
                                addToHierarchyWithPath
                                    msg
                                    restPath
                                    { nextMultiID = hierarchy.nextMultiID + 1
                                    , openMultis = hierarchy.openMultis
                                    , list =
                                        [ Single lastRestPath lastMsg ]
                                    }
                        in
                        { nextMultiID = subHierarchy.nextMultiID
                        , openMultis = subHierarchy.openMultis
                        , list =
                            Multi hierarchy.nextMultiID prefix subHierarchy.list :: rest
                        }

                    else
                        { hierarchy
                            | list =
                                Single path msg :: hierarchy.list
                        }

                _ ->
                    { hierarchy
                        | list =
                            Single path msg :: hierarchy.list
                    }

        (Multi _ lastPrefix msgs) :: rest ->
            case path of
                prefix :: restPath ->
                    if lastPrefix == prefix then
                        let
                            subHierarchy =
                                addToHierarchyWithPath
                                    msg
                                    restPath
                                    { nextMultiID = hierarchy.nextMultiID + 1
                                    , openMultis = hierarchy.openMultis
                                    , list =
                                        msgs
                                    }
                        in
                        { nextMultiID = subHierarchy.nextMultiID
                        , openMultis = subHierarchy.openMultis
                        , list =
                            Multi hierarchy.nextMultiID lastPrefix subHierarchy.list :: rest
                        }

                    else
                        { hierarchy
                            | list =
                                Single path msg :: hierarchy.list
                        }

                _ ->
                    { hierarchy
                        | list =
                            Single path msg :: hierarchy.list
                    }

        _ ->
            { hierarchy
                | list =
                    Single path msg :: hierarchy.list
            }


openMultiContainer : Int -> MsgHierarchy msg -> MsgHierarchy msg
openMultiContainer id hierarchy =
    { hierarchy
        | openMultis =
            if Set.member id hierarchy.openMultis then
                Set.remove id hierarchy.openMultis

            else
                Set.insert id hierarchy.openMultis
    }



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
add msg model { snapshots, recent, numMessages, messageHierarchy, logs } =
    case addRecent msg model recent of
        ( Just snapshot, newRecent ) ->
            History (Array.push snapshot snapshots) newRecent (numMessages + 1) (addToHierarchy msg messageHierarchy) logs

        ( Nothing, newRecent ) ->
            History snapshots newRecent (numMessages + 1) (addToHierarchy msg messageHierarchy) logs


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


addLog : String -> a -> History model msg -> History model msg
addLog label value { snapshots, recent, numMessages, messageHierarchy, logs } =
    History snapshots recent numMessages messageHierarchy <|
        Dict.insert label (Expando.init value) logs



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


getLogs : Int -> History model msg -> Dict String Expando
getLogs index history =
    history.logs


clearLogs : History model msg -> History model msg
clearLogs { snapshots, recent, numMessages, messageHierarchy, logs } =
    History snapshots recent numMessages messageHierarchy Dict.empty


undone : GetResult model msg -> ( model, msg )
undone getResult =
    case getResult of
        Done msg model ->
            ( model, msg )

        Stepping _ _ ->
            undone getResult



-- Debug.crash "Bug in History.get"
-- VIEW


type Msg
    = SelectMsg Int
    | ToggleMulti Int


view : Maybe Int -> History model msg -> Html Msg
view maybeIndex { messageHierarchy, numMessages } =
    let
        ( selectedIndex, height ) =
            case maybeIndex of
                Nothing ->
                    ( -1, "calc(100% - 48px)" )

                Just i ->
                    ( i, "calc(100% - 78px)" )

        foldHelper container ( index, views ) =
            let
                ( nextIndex, div ) =
                    viewMessageContainer selectedIndex index messageHierarchy.openMultis container
            in
            ( nextIndex, div :: views )

        ( _, messageList ) =
            List.foldl foldHelper ( numMessages - 1, [] ) messageHierarchy.list
    in
    div
        [ id "elm-debugger-sidebar"
        , style "width" "100%"
        , style "overflow-y" "auto"
        , style "height" height
        ]
        (styles :: messageList)



-- VIEW MESSAGE


viewMessageContainer : Int -> Int -> Set Int -> MsgContainer msg -> ( Int, Html Msg )
viewMessageContainer selectedIndex index openMultis container =
    case container of
        Single _ msg ->
            ( index - 1
            , viewMessage selectedIndex index msg
            )

        Multi id prefix msgs ->
            viewMultiContainer selectedIndex index openMultis id prefix msgs


viewMessage : Int -> Int -> msg -> Html Msg
viewMessage selectedIndex currentIndex msg =
    let
        className =
            if selectedIndex == currentIndex then
                "elm-debugger-entry elm-debugger-entry-selected"

            else
                "elm-debugger-entry"

        messageName =
            Elm.Kernel.Debugger.messageToString msg
    in
    div
        [ class className
        , onClick (SelectMsg currentIndex)
        ]
        [ span [ class "elm-debugger-entry-arrow" ]
            [ text "" ]
        , span
            [ title messageName
            , class "elm-debugger-entry-content"
            ]
            [ text messageName
            ]
        , span
            [ class "elm-debugger-entry-index"
            ]
            [ text (String.fromInt currentIndex)
            ]
        ]


viewMultiContainer : Int -> Int -> Set Int -> Int -> String -> List (MsgContainer msg) -> ( Int, Html Msg )
viewMultiContainer selectedIndex currentIndex openMultis id prefix children =
    let
        isOpen =
            Set.member id openMultis

        foldHelper container ( childIndex, views ) =
            let
                ( nextChildIndex, div ) =
                    viewMessageContainer selectedIndex childIndex openMultis container
            in
            ( nextChildIndex, div :: views )

        ( nextIndex, messageList ) =
            List.foldl foldHelper ( currentIndex, [] ) children
    in
    ( nextIndex
    , div []
        [ div
            [ class "elm-debugger-entry"
            , onClick (ToggleMulti id)
            ]
            [ span [ class "elm-debugger-entry-arrow" ] <|
                if isOpen then
                    [ text "▾" ]

                else
                    [ text "▸" ]
            , span
                [ title prefix
                , class "elm-debugger-entry-content"
                ]
                [ text prefix
                ]
            , span
                [ class "elm-debugger-entry-index"
                ]
                [ if isOpen then
                    text ""

                  else
                    text (String.fromInt currentIndex)
                ]
            ]
        , if isOpen then
            div [ style "margin-left" "12px" ]
                messageList

          else
            text ""
        ]
    )



-- STYLES


styles : Html msg
styles =
    Html.node "style" [] [ text """

.elm-debugger-entry {
  cursor: pointer;
  width: 100%;
  box-sizing: border-box;
  padding: 4px;
}

.elm-debugger-entry:hover {
  background-color: rgb(41, 41, 41);
}

.elm-debugger-entry-selected, .elm-debugger-entry-selected:hover {
  background-color: rgb(10, 10, 10);
}

.elm-debugger-entry-arrow {
  display: inline-block;
  width: 10px;
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
  width: 20px;
  text-align: right;
  display: block;
  float: right;
}

""" ]
