effect module Browser.NavigationManager where { command = MyCmd, subscription = MySub } exposing
  ( forward
  , pushUrl
  , replaceUrl
  , addListen
  )


import Elm.Kernel.Browser
import Json.Decode as Decode
import Process
import Task exposing (Task)



-- COMMANDS


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


forward : Int -> Cmd msg
forward n =
  command (Go n)


pushUrl : String -> Cmd msg
pushUrl url =
  command (Push url)


replaceUrl : String -> Cmd msg
replaceUrl url =
  command (Replace url)



-- SUBSCRIPTIONS


type MySub msg =
  Listen (String -> msg)


subMap : (a -> b) -> MySub a -> MySub b
subMap func (Listen tagger) =
  Listen (tagger >> func)


addListen : (String -> msg) -> (model -> Sub msg) -> model -> Sub msg
addListen toMsg toSubs model =
  Sub.batch [ subscription (Listen toMsg), toSubs model ]



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


onSelfMsg : Platform.Router msg String -> String -> State msg -> Task Never (State msg)
onSelfMsg router url state =
  ignore (notify router state.subs url) state



ignore : Task x a -> b -> Task x b
ignore task b =
  Task.andThen (\_ -> Task.succeed b) task



-- APP MESSAGES


onEffects : Platform.Router msg String -> List (MyCmd msg) -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router cmds subs {popWatcher} =
  let
    stepState =
      case (subs, popWatcher) of
        ([], Just watcher) ->
          ignore (killPopWatcher watcher) (State subs Nothing)

        (_ :: _, Nothing) ->
          Task.map (State subs << Just) (spawnPopWatcher router)

        (_, _) ->
          Task.succeed (State subs popWatcher)

  in
    Task.sequence (List.map (cmdHelp router subs) cmds)
      |> Task.andThen (\_ -> stepState)


cmdHelp : Platform.Router msg String -> List (MySub msg) -> MyCmd msg -> Task Never ()
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



notify : Platform.Router msg String -> List (MySub msg) -> String -> Task x ()
notify router subs url =
  let
    send (Listen tagger) =
      Platform.sendToApp router (tagger url)
  in
    ignore (Task.sequence (List.map send subs)) ()


go : Int -> Task x ()
go =
  Elm.Kernel.Browser.go


pushState : String -> Task x String
pushState =
  Elm.Kernel.Browser.pushState


replaceState : String -> Task x String
replaceState =
  Elm.Kernel.Browser.replaceState



-- POP WATCHER STUFF


spawnPopWatcher : Platform.Router msg String -> Task x PopWatcher
spawnPopWatcher router =
  if Elm.Kernel.Browser.isInternetExplorer11 () then
    Task.map2 InternetExplorer
      (reportUrl "popstate" router)
      (reportUrl "hashchange" router)

  else
    Task.map Normal (reportUrl "popstate" router)


reportUrl : String -> Platform.Router msg String -> Task x Process.Id
reportUrl name router =
  Elm.Kernel.Browser.on Elm.Kernel.Browser.window True name <|
    \_ -> Platform.sendToSelf router (Elm.Kernel.Browser.getUrl ())


killPopWatcher : PopWatcher -> Task x ()
killPopWatcher popWatcher =
  case popWatcher of
    Normal pid ->
      Process.kill pid

    InternetExplorer pid1 pid2 ->
      Process.kill pid1
        |> Task.andThen (\_ -> Process.kill pid2)
