module Browser.Events exposing
  ( onDocument
  , onWindow
  , preventDefaultOnDocument
  , preventDefaultOnWindow
  )

{-|

# Global Events
@docs onDocument, onWindow, preventDefaultOnDocument, preventDefaultOnWindow


-}



import Browser.Events.Manager as Events
import Json.Decode as Decode



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
  Events.on Events.Document True name (Decode.map addFalse decoder)


{-| Subscribe to events on `window`.

It would make sense to use this with `"scroll"` and `"wheel"` events.

**Note:** This uses [passive][] event handlers, enabling optimizations for events
like `scroll` and `wheel`.

[passive]: https://github.com/WICG/EventListenerOptions/blob/gh-pages/explainer.md
-}
onWindow : String -> Decode.Decoder msg -> Sub msg
onWindow name decoder =
  Events.on Events.Window True name (Decode.map addFalse decoder)


{-| Subscribe to events on `document` and conditionally prevent the default
behavior. For example, pressing `SPACE` causes a “page down” normally, and
maybe you want it to do something different.

**Note:** This disables the [passive][] optimization, causing a performance
degredation for events like `touchstart` and `touchmove`.

[passive]: https://github.com/WICG/EventListenerOptions/blob/gh-pages/explainer.md
-}
preventDefaultOnDocument : String -> Decode.Decoder (msg, Bool) -> Sub msg
preventDefaultOnDocument =
  Events.on Events.Document False


{-| Subscribe to events on `window` and conditionally prevent the default
behavior.

**Note:** This disables the [passive][] optimization, causing a performance
degredation for events like `scroll` and `wheel`.

[passive]: https://github.com/WICG/EventListenerOptions/blob/gh-pages/explainer.md
-}
preventDefaultOnWindow : String -> Decode.Decoder (msg, Bool) -> Sub msg
preventDefaultOnWindow =
  Events.on Events.Window False


addFalse : msg -> (msg, Bool)
addFalse msg =
  (msg, False)

