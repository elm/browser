effect module Browser.Events.Manager where { subscription = MySub } exposing
  ( on
  , Node(..)
  )


import Dict
import Elm.Kernel.Browser
import Json.Decode as Decode
import Process
import Task exposing (Task)



-- SUBSCRIPTIONS


on : Node -> Bool -> String -> Decode.Decoder (msg, Bool) -> Sub msg
on node bool name decoder =
  subscription (MySub node bool name decoder)


type MySub msg =
  MySub Node Bool String (Decode.Decoder (msg, Bool))


type Node
  = Document
  | Window


subMap : (a -> b) -> MySub a -> MySub b
subMap func (MySub node passive name decoder) =
  MySub node passive name (Decode.map (Tuple.mapFirst func) decoder)



-- EFFECT MANAGER


type alias State msg =
  { subs : List (String, MySub msg)
  , pids : Dict.Dict String Process.Id
  }


init : Task Never (State msg)
init =
  Task.succeed (State [] Dict.empty)


type alias Event =
  { key : String
  , event : Decode.Value
  }


onSelfMsg : Platform.Router msg Event -> Event -> State msg -> Task Never (State msg)
onSelfMsg router { key, event } state =
  let
    toMessage (subKey, MySub node passive name decoder) =
      if subKey == key then
        Elm.Kernel.Browser.decodeEvent decoder event
      else
        Nothing

    messages =
      List.filterMap toMessage state.subs
  in
  Task.sequence (List.map (Platform.sendToApp router) messages)
    |> Task.andThen (\_ -> Task.succeed state)


onEffects : Platform.Router msg Event -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router subs state =
  let
    newSubs =
      List.map addKey subs

    stepLeft _ pid (deads, lives, news) =
      ( pid :: deads, lives, news )

    stepBoth key pid _ (deads, lives, news) =
      ( deads, Dict.insert key pid lives, news )

    stepRight key sub (deads, lives, news) =
      ( deads, lives, spawn router key sub :: news )

    (deadPids, livePids, makeNewPids) =
      Dict.merge stepLeft stepBoth stepRight state.pids (Dict.fromList newSubs) ([], Dict.empty, [])
  in
  Task.sequence (List.map Process.kill deadPids)
    |> Task.andThen (\_ -> Task.sequence makeNewPids)
    |> Task.andThen (\pids -> Task.succeed (State newSubs (Dict.union livePids (Dict.fromList pids))))



-- TO KEY


addKey : MySub msg -> ( String, MySub msg )
addKey (MySub node passive name _ as sub) =
  ( nodeToKey node ++ passiveToKey passive ++ name, sub )


nodeToKey : Node -> String
nodeToKey node =
  case node of
    Document ->
      "d"

    Window ->
      "w"


passiveToKey : Bool -> String
passiveToKey passive =
  if passive then "p" else "n"



-- SPAWN


spawn : Platform.Router msg Event -> String -> MySub msg -> Task Never (String, Process.Id)
spawn router key (MySub node passive name _) =
  let
    actualNode =
      case node of
        Document ->
          Elm.Kernel.Browser.document

        Window ->
          Elm.Kernel.Browser.window
  in
  Task.map (\value -> (key,value)) <|
    Elm.Kernel.Browser.on actualNode passive name <|
      \event -> Platform.sendToSelf router (Event key event)
