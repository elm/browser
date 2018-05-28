module Browser.Dom exposing
  ( focus, blur, Error(..)
  , scrollIntoView
  , getScroll
  , setScrollTop, setScrollBottom
  , setScrollLeft, setScrollRight
  )

{-| This module helps you set up an Elm `Program` with functions like
[`sandbox`](#sandbox) and [`embed`](#embed).

It also has a bunch of miscellaneous helpers for global event listeners and
for focusing and scrolling DOM nodes.


# Static Pages
@docs staticPage


# Dynamic Pages
@docs sandbox, embed


# DOM Stuff

## Focus
@docs focus, blur, Error

## Scroll
@docs scrollIntoView, getScroll, setScrollTop, setScrollBottom, setScrollLeft, setScrollRight


# Global Events
@docs onDocument, onWindow, preventDefaultOnDocument, preventDefaultOnWindow


-}



import Elm.Kernel.Browser
import Task exposing (Task)


{-| All the DOM functions here look nodes up by their `id`. If you ask for an
`id` that is not in the DOM, you will get this error.
-}
type Error = NotFound String



-- FOCUS


{-| Find a DOM node by `id` and focus on it. So if you wanted to focus a node
like `<input type="text" id="search-box">` you could say:

    import Browser
    import Task

    type Msg = NoOp

    focusSearchBox : Cmd Msg
    focusSearchBox =
      Task.attempt (\_ -> NoOp) (Browser.focus "search-box")

Notice that this code ignores the possibility that `search-box` is not used
as an `id` by any node, failing silently in that case. It would be better to
log the failure with whatever error reporting software you use.
-}
focus : String -> Task Error ()
focus =
  Elm.Kernel.Browser.call "focus"


{-| Find a DOM node by `id` and make it lose focus. So if you wanted a node
like `<input type="text" id="search-box">` to lose focus you could say:

    import Browser
    import Task

    type Msg = NoOp

    unfocusSearchBox : Cmd Msg
    unfocusSearchBox =
      Task.attempt (\_ -> NoOp) (Browser.blur "search-box")
-}
blur : String -> Task Error ()
blur =
  Elm.Kernel.Browser.call "blur"




-- SCROLL


{-| Find a DOM node by `id` and scroll it into view. Maybe we want to scroll
to arbitrary headers in a long document? We could define a `scrollTo`
function like this:

    import Browser
    import Task

    type Msg = NoOp

    scrollTo : String -> Cmd Msg
    scrollTo id =
      Task.attempt (\_ -> NoOp) (Browser.scrollIntoView id)
-}
scrollIntoView : String -> Task Error ()
scrollIntoView =
  Elm.Kernel.Browser.call "scrollIntoView"


{-| Find a DOM node by `id` and get its `scrollLeft` and `scrollTop` values.
-}
getScroll : String -> Task Error ( Float, Float )
getScroll =
  Elm.Kernel.Browser.getScroll


{-| Find a DOM node by `id` and set the scroll offset from the top. If we want
to scroll to the top, we can say:

    import Browser
    import Task

    type Msg = NoOp

    scrollToTop : String -> Cmd Msg
    scrollToTop id =
      Task.attempt (\_ -> NoOp) (Browser.setScrollTop id 0)

So the offset from the top is zero. If we said `setScrollTop id 100` the
content would be scrolled down 100 pixels.
-}
setScrollTop : String -> Float -> Task Error ()
setScrollTop =
  Elm.Kernel.Browser.setPositiveScroll "scrollTop"


{-| Same as [`setScrollTop`](#setScrollTop), but it sets the scroll offset
from the bottom. So saying `setScrollBottom id 0` scrolls all the way down.
That can be useful in a chat room where messages keep appearing.

If you said `setScrollBottom id 200`, it is like you scrolled all the way to
the bottom and then scrolled up 200 pixels.
-}
setScrollBottom : String -> Float -> Task Error ()
setScrollBottom =
  Elm.Kernel.Browser.setNegativeScroll "scrollTop" "scrollHeight"


{-| Same as [`setScrollTop`](#setScrollTop), but it sets the horizontal scroll
offset from the left side.
-}
setScrollLeft : String -> Float -> Task Error ()
setScrollLeft =
  Elm.Kernel.Browser.setPositiveScroll "scrollLeft"


{-| Same as [`setScrollTop`](#setScrollTop), but it sets the horizontal scroll
offset from the right side.
-}
setScrollRight : String -> Float -> Task Error ()
setScrollRight =
  Elm.Kernel.Browser.setNegativeScroll "scrollLeft" "scrollWidth"

