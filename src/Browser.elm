module Browser exposing
  ( sandbox
  , element
  , document
  , Document
  , application
  )

{-| This module helps you set up an Elm `Program` with functions like
[`sandbox`](#sandbox) and [`document`](#document).


# Sandboxes
@docs sandbox

# Elements
@docs element

# Documents
@docs document, Document

# Applications
@docs application

-}



import Dict
import Browser.Navigation.Manager as Navigation
import Debugger.Main
import Elm.Kernel.Browser
import Url
import Html exposing (Html)



-- SANDBOX


{-| Create a “sandboxed” program that cannot communicate with the outside
world.

This is great for learning the basics of [The Elm Architecture][tea]. You can
see sandboxes in action in tho following examples:

  - [Buttons](https://elm-lang.org/examples/buttons)
  - [Text Field](https://elm-lang.org/examples/field)
  - [Checkboxes](https://elm-lang.org/examples/checkboxes)

Those are nice, but **I very highly recommend reading [this guide][guide]
straight through** to really learn how Elm works. Understanding the
fundamentals actually pays off in this language!

[tea]: https://guide.elm-lang.org/architecture/
[guide]: https://guide.elm-lang.org/
-}
sandbox :
  { init : model
  , view : model -> Html msg
  , update : msg -> model -> model
  }
  -> Program () model msg
sandbox impl =
  Elm.Kernel.Browser.element
    { init = \() -> (impl.init, Cmd.none)
    , view = impl.view
    , update = \msg model -> (impl.update msg model, Cmd.none)
    , subscriptions = \_ -> Sub.none
    }



-- ELEMENT


{-| Create an HTML element managed by Elm. The resulting elements are easy to
embed in a larger JavaScript projects, and lots of companies that use Elm
started with this approach! Try it out on something small. If it works, great,
do more! If not, revert, no big deal.

Unlike a [`sandbox`](#sandbox), an `element` can talk to the outside world in
a couple ways:

  - `Cmd` &mdash; you can “command” the Elm runtime to do stuff, like HTTP.
  - `Sub` &mdash; you can “subscribe” to event sources, like clock ticks.
  - `flags` &mdash; JavaScript can pass in data when starting the Elm program
  - `ports` &mdash; set up a client-server relationship with JavaScript

As you read [the guide][guide] you will run into a bunch of examples of `element`
in [this section][fx]. You can learn more about flags and ports in [the interop
section][interop].

[guide]: https://guide.elm-lang.org/
[fx]: https://guide.elm-lang.org/effects/
[interop]: https://guide.elm-lang.org/interop/
-}
element :
  { init : flags -> (model, Cmd msg)
  , view : model -> Html msg
  , update : msg -> model -> ( model, Cmd msg )
  , subscriptions : model -> Sub msg
  }
  -> Program flags model msg
element =
  Elm.Kernel.Browser.element



-- DOCUMENT


{-| Create an HTML document managed by Elm. This expands upon what `element`
can do in that `view` now gives you control over the `<title>` and `<body>`.

-}
document :
  { init : flags -> (model, Cmd msg)
  , view : model -> Document msg
  , update : msg -> model -> ( model, Cmd msg )
  , subscriptions : model -> Sub msg
  }
  -> Program flags model msg
document =
  Elm.Kernel.Browser.document


{-| This data specifies the `<title>` and all of the nodes that should go in
the `<body>`. This means you can update the title as your application changes.
Maybe your "single-page app" navigates to a "different page", maybe a calendar
app shows an accurate date in the title, etc.

> **Note about CSS:** This looks similar to an `<html>` document, but this is
> not the place to manage CSS assets. If you want to work with CSS, there are
> a couple ways:
>
> 1. Packages like [`rtfeldman/elm-css`][elm-css] give all of the features
> of CSS without any CSS files. You can add all the styles you need in your
> `view` function, and there is no need to worry about class names matching.
>
> 2. Compile your Elm code to JavaScript with `elm make --output=elm.js` and
> then make your own HTML file that loads `elm.js` and the CSS file you want.
> With this approach, it does not matter where the CSS comes from. Write it
> by hand. Generate it. Whatever you want to do.
>
> 3. If you need to change `<link>` tags dynamically, you can send messages
> out a port to do it in JavaScript.
>
> The bigger point here is that loading assets involves touching the `<head>`
> as an implementation detail of browsers, but that does not mean it should be
> the responsibility of the `view` function in Elm. So we do it differently!

[elm-css]: /rtfeldman/elm-css/latest/
-}
type alias Document msg =
  { title : String
  , body : List (Html msg)
  }



-- APPLICATION


{-| Create an application that manages [`Url`][url] changes. It expands the
`document` functionality in that:

1. You get the initial `Url` in `init` so you can figure out what to show on
the first frame.
2. You provide an `onNavigation` function to turn URLs into messages for your
`update` function so you can show different things as the URL changes.

This allows you to create &ldquo;single-page apps&rdquo; (SPAs) when paired
with the [`Browser.Navigation`](Browser-Navigation) module!

**More Info:** Here are some example usages of `application` programs:

  - [RealWorld example app](https://github.com/rtfeldman/elm-spa-example)
  - [Elm’s package website](https://github.com/elm/package.elm-lang.org)

These are quite advanced Elm programs, so be sure to go through [the
guide](https://guide.elm-lang.org/) first to get a solid conceptual foundation
before diving in! If you start reading a calculus book from page 314, it might
seem confusing. Same here!

**Note:** Wait, but how can people use Elm in an [`element`](#element) and
still manage the URL? Read [this][]!

[url]: /packages/elm/url/latest/Url#Url
[this]: https://github.com/elm/browser/blob/1.0.0/notes/navigation-in-elements.md
-}
application :
  { init : flags -> Url.Url -> (model, Cmd msg)
  , view : model -> Document msg
  , update : msg -> model -> ( model, Cmd msg )
  , subscriptions : model -> Sub msg
  , onNavigation : Url.Url -> msg
  }
  -> Program flags model msg
application impl =
  Elm.Kernel.Browser.document
    { init = \flags -> impl.init flags (unsafeToUrl (Elm.Kernel.Browser.getUrl ()))
    , view = impl.view
    , update = impl.update
    , subscriptions = Navigation.addListen (impl.onNavigation << unsafeToUrl) impl.subscriptions
    }


unsafeToUrl : String -> Url.Url
unsafeToUrl string =
  case Url.fromString string of
    Just url ->
      url

    Nothing ->
      Elm.Kernel.Browser.invalidUrl string
