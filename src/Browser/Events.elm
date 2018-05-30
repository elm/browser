effect module Browser.Events where { subscription = MySub } exposing
  ( onDocument
  , onWindow
  , preventDefaultOnDocument
  , preventDefaultOnWindow
  )

{-|

# Document
@docs onDocument, preventDefaultOnDocument

# Window
@docs onWindow, preventDefaultOnWindow


-}


import Dict
import Elm.Kernel.Browser
import Json.Decode as Decode
import Process
import Task exposing (Task)



{-| Subscribe to events on `document`.

If you want to handle keyboard events check out [this document][kbd] all about
how to get what you need from different browsers.

[kbd]: https://github.com/elm/browser/blob/master/hints/keyboard.md

**Note:** This uses [passive][] event handlers, enabling optimizations for events
like `touchstart` and `touchmove`.

[passive]: https://github.com/WICG/EventListenerOptions/blob/gh-pages/explainer.md
-}
onDocument : String -> Decode.Decoder msg -> Sub msg
onDocument name decoder =
  on Document True name (Decode.map addFalse decoder)


{-| Subscribe to events on `window`.

It would make sense to use this with `"scroll"` and `"wheel"` events.

**Note:** This uses [passive][] event handlers, enabling optimizations for events
like `scroll` and `wheel`.

[passive]: https://github.com/WICG/EventListenerOptions/blob/gh-pages/explainer.md
-}
onWindow : String -> Decode.Decoder msg -> Sub msg
onWindow name decoder =
  on Window True name (Decode.map addFalse decoder)


{-| Subscribe to events on `document` and conditionally prevent the default
behavior. For example, pressing `SPACE` causes a “page down” normally, and
maybe you want it to do something different.

**Note:** This disables the [passive][] optimization, causing a performance
degredation for events like `touchstart` and `touchmove`.

[passive]: https://github.com/WICG/EventListenerOptions/blob/gh-pages/explainer.md
-}
preventDefaultOnDocument : String -> Decode.Decoder (msg, Bool) -> Sub msg
preventDefaultOnDocument =
  on Document False


{-| Subscribe to events on `window` and conditionally prevent the default
behavior.

**Note:** This disables the [passive][] optimization, causing a performance
degredation for events like `scroll` and `wheel`.

[passive]: https://github.com/WICG/EventListenerOptions/blob/gh-pages/explainer.md
-}
preventDefaultOnWindow : String -> Decode.Decoder (msg, Bool) -> Sub msg
preventDefaultOnWindow =
  on Window False


addFalse : msg -> (msg, Bool)
addFalse msg =
  (msg, False)



-- SUBSCRIPTIONS


type Node
  = Document
  | Window


on : Node -> Bool -> String -> Decode.Decoder (msg, Bool) -> Sub msg
on node bool name decoder =
  subscription (MySub node bool name decoder)


type MySub msg =
  MySub Node Bool String (Decode.Decoder (msg, Bool))


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
          Elm.Kernel.Browser.doc

        Window ->
          Elm.Kernel.Browser.window
  in
  Task.map (\value -> (key,value)) <|
    Elm.Kernel.Browser.on actualNode passive name <|
      \event -> Platform.sendToSelf router (Event key event)
