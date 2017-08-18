effect module Browser.History where { command = MyCmd, subscription = MySub } exposing
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


import Browser exposing (Url)
import Dom.LowLevel as Dom
import Elm.Kernel.Browser
import Json.Decode as Decode
import Process
import Task exposing (Task)



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
push url =
  command (Push url)


{-| Change the URL, but do not trigger a page load.

This *will not* add a new entry to the browser history. **(WEIRD)**

This can be useful if you have search box and you want the `?search=hats` in
the URL to match without adding a history entry for every single key stroke.
Imagine how annoying it would be to click `back` thirty times and still be on
the same page!
-}
replace : String -> Cmd msg
replace url =
  command (Replace url)



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
  command (Go -n)


{-| Go forward some number of pages. So `forward 1` goes forward one page, and
`forward 2` goes forward two pages. If there are no more pages in the future,
this will do nothing.

**Note:** You only manage the browser history that *you* created. Think of this
library as letting you have access to a small part of the overall history. So
if you go forward farther than the history you own, the user will end up on
whatever website they visited next!
-}
forward : Int -> Cmd msg
forward n =
  command (Go n)




-- EFFECT MANAGER


type MyCmd msg
  = Go Int
  | Push String
  | Replace String


cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap _ myCmd =
  case myCmd of
    Go n ->
      Go n

    Push url ->
      Push url

    Replace url ->
      Replace url


type MySub msg =
  Monitor (Url -> msg)


subMap : (a -> b) -> MySub a -> MySub b
subMap func (Monitor tagger) =
  Monitor (tagger >> func)



-- STATE


type alias State msg =
  { subs : List (MySub msg)
  , popWatcher : Maybe PopWatcher
  }


type PopWatcher
  = Normal Process.Id
  | InternetExplorer Process.Id Process.Id



-- INIT


init : Task Never (State msg)
init =
  Task.succeed (State [] Nothing)



-- SELF MESSAGES


onSelfMsg : Platform.Router msg Url -> Url -> State msg -> Task Never (State msg)
onSelfMsg router location state =
  notify router state.subs location
    &> Task.succeed state


(&>) task1 task2 =
  Task.andThen (\_ -> task2) task1



-- APP MESSAGES


onEffects : Platform.Router msg Url -> List (MyCmd msg) -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router cmds subs {popWatcher} =
  let
    stepState =
      case (subs, popWatcher) of
        ([], Just watcher) ->
          killPopWatcher watcher
            &> Task.succeed (State subs Nothing)

        (_ :: _, Nothing) ->
          Task.map (State subs << Just) (spawnPopWatcher router)

        (_, _) ->
          Task.succeed (State subs popWatcher)

  in
    Task.sequence (List.map (cmdHelp router subs) cmds)
      &> stepState


cmdHelp : Platform.Router msg Url -> List (MySub msg) -> MyCmd msg -> Task Never ()
cmdHelp router subs cmd =
  case cmd of
    Go n ->
      go n

    Push url ->
      pushState url
        |> Task.andThen (notify router subs)

    Replace url ->
      replaceState url
        |> Task.andThen (notify router subs)



notify : Platform.Router msg Url -> List (MySub msg) -> Url -> Task x ()
notify router subs location =
  let
    send (Monitor tagger) =
      Platform.sendToApp router (tagger location)
  in
    Task.sequence (List.map send subs)
      &> Task.succeed ()


go : Int -> Task x ()
go =
  Elm.Kernel.Browser.go


pushState : String -> Task x Url
pushState =
  Elm.Kernel.Browser.pushState


replaceState : String -> Task x Url
replaceState =
  Elm.Kernel.Browser.replaceState



-- POP WATCHER STUFF


spawnPopWatcher : Platform.Router msg Url -> Task x PopWatcher
spawnPopWatcher router =
  let
    reportUrl _ =
      Platform.sendToSelf router (Elm.Kernel.Browser.getUrl ())
  in
    if Elm.Kernel.Browser.isInternetExplorer11 () then
      Task.map2 InternetExplorer
        (Process.spawn (onWindow "popstate" reportUrl))
        (Process.spawn (onWindow "hashchange" reportUrl))

    else
      Task.map Normal <|
        Process.spawn (onWindow "popstate" reportUrl)


onWindow : String -> (Decode.Value -> Task Never ()) -> Task Never Never
onWindow event onChange =
  Dom.windowBubble event (Dom.Normal Decode.value) onChange


killPopWatcher : PopWatcher -> Task x ()
killPopWatcher popWatcher =
  case popWatcher of
    Normal pid ->
      Process.kill pid

    InternetExplorer pid1 pid2 ->
      Process.kill pid1
        &> Process.kill pid2
