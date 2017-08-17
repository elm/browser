module Browser.History exposing
  ( push
  , replace
  , back
  , forward
  )


{-| This module helps you manage browser history yourself with [`push`](#push),
[`replace`](#replace), [`back`](#back), and [`forward`](#forward).

The most important function is [`push`](#push) which changes the address bar
*without* starting a page load.


## What is a page load?

  1. Request a new HTML document. The page goes blank.
  2. As the HTML loads, request any `<script>` or `<link>` resources.
  3. A `<script>` may mutate the document, so these tags block rendering.
  4. When *all* of the assets are loaded, actually render the page.

That means the page will go blank for at least two round-trips to the servers!
You may have 90% of the data you need and be blocked on a font that is taking
a long time. Still blank!


## How does `push` help?

The `push` function changes the URL, but lets you keep the current HTML. This
means the page *never* goes blank. Instead of making two round-trips to the
server, you load whatever assets you want from within Elm. Maybe you do not
need any round-trips! Meanwhile, you retain full control over the UI, so you
can show a loading bar, show information as it loads, etc. Whatever you want!


# History
@docs push, replace, back, forward

-}


import Elm.Kernel.Browser
import Task



-- CHANGE HISTORY


{-| Change the URL, but do not trigger a page load.

This will add a new entry to the browser history. **(DEFAULT)**

Check out the [`elm-lang/url`][url] package for help building URLs. The
[`Url.absolute`][abs] and [`Url.relative`][rel] functions can be particularly
handy!

[url]: http://package.elm-lang.org/packages/elm-lang/url/latest
[abs]: http://package.elm-lang.org/packages/elm-lang/url/latest/Url#absolute
[rel]: http://package.elm-lang.org/packages/elm-lang/url/latest/Url#relative

**Note:** If the user has gone `back` a few pages, there will be &ldquo;future
pages&rdquo; that the user can go `forward` to. Adding a new URL in that
scenario will clear out any future pages. It is like going back in time and
making a different choice.
-}
push : String -> Cmd msg
push =
  Elm.Kernel.Browser.push


{-| Change the URL, but do not trigger a page load.

This *will not* add a new entry to the browser history. **(WEIRD)**

This can be useful if you have search box and you want the `?search=hats` in
the URL to match without adding a history entry for every single key stroke.
Imagine how annoying it would be to click `back` thirty times and still be on
the same page!
-}
replace : String -> Cmd msg
replace =
  Elm.Kernel.Browser.replace



-- NAVIGATE HISTORY


{-| Go back some number of pages. So `back 1` goes back one page, and `back 2`
goes back two pages.

**Note:** You only manage the browser history that *you* created. Think of this
library as letting you have access to a small part of the overall history. So
if you go back farther than the history you own, you will just go back to some
other website!
-}
back : Int -> Cmd msg
back n =
  Elm.Kernel.Browser.go -n


{-| Go forward some number of pages. So `forward 1` goes forward one page, and
`forward 2` goes forward two pages. If there are no more pages in the future,
this will do nothing.

**Note:** You only manage the browser history that *you* created. Think of this
library as letting you have access to a small part of the overall history. So
if you go forward farther than the history you own, the user will end up on
whatever website they visited next!
-}
forward : Int -> Cmd msg
forward =
  Elm.Kernel.Browser.go
