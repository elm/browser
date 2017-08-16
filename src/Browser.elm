module Browser exposing
  ( Env
  , program
  , Url
  , load
  , reload
  , reloadAndSkipCache
  )

{-|

# Create Programs
@docs Env, program, Url

# Page Loads
@docs load, reload, reloadAndSkipCache

-}


import Elm.Kernel.Browser
import Task
import VirtualDom



-- PROGRAMS


type alias Env flags =
  { url : Url
  , flags : flags
  }


program :
  { init : Env flags -> (model, Cmd msg)
  , view : model -> { title : String, html : VirtualDom.Node msg }
  , update : msg -> model -> ( model, Cmd msg )
  , onNavigation : Url -> msg
  , subscriptions : model -> Sub msg
  }
  -> Program flags model msg



-- URL


{-| A bunch of information about the URL in the address bar. You should always
be using the [`elm-lang/url`][url] package to turn these URLs into nice Elm
data. Check out the [`Url.Parser`][parser] module in particular.

**Note:** The fields correspond with the fields in `document.location` as
described [here](https://developer.mozilla.org/en-US/docs/Web/API/Url).
-}
type alias Url =
  { href : String
  , host : String
  , hostname : String
  , protocol : String
  , origin : String
  , port_ : String
  , pathname : String
  , search : String
  , hash : String
  , username : String
  , password : String
  }



-- PAGE LOADS


{-| Leave the current page and load the given URL. **This always results in a
page load**, even if the provided URL is the same as the current one.

    load "http://elm-lang.org"

Check out the [`elm-lang/url`][url] package for help building URLs. The
[`Url.absolute`][abs] and [`Url.relative`][rel] functions can be particularly
handy!

[url]: http://package.elm-lang.org/packages/elm-lang/url/latest
[abs]: http://package.elm-lang.org/packages/elm-lang/url/latest/Url#absolute
[rel]: http://package.elm-lang.org/packages/elm-lang/url/latest/Url#relative

Check out the [`Browser.History`](Browser-History) module if you want
page transitions that are faster and prettier, like in a “single-page app”.
-}
load : String -> Cmd msg
load url =
  Task.perform never (Elm.Kernel.Browser.load url)


{-| Reload the current page. **This always results in a page load!**
This may grab resources from the browser cache, so use
[`reloadAndSkipCache`](reloadAndSkipCache) if you want to be sure
that you are not loading any cached resources.
-}
reload : Cmd msg
reload =
  Task.perform never (Elm.Kernel.Browser.reload False)


{-| Reload the current page without using the browser cache. **This always
results in a page load!** It is more common to want [`reload`](reload).
-}
reloadAndSkipCache : Cmd msg
reloadAndSkipCache =
  Task.perform never (Elm.Kernel.Browser.reload True)



-- NEVER


type Never = JustOneMore Never


never : Never -> a
never (JustOneMore nvr) =
  never nvr
