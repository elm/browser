effect module Browser.Navigation.Manager where { command = MyCmd, subscription = MySub } exposing
  ( forward
  , pushUrl
  , replaceUrl
  , addListen
  )


import Dom.LowLevel as Dom
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
  Listen (Url -> msg)


subMap : (a -> b) -> MySub a -> MySub b
subMap func (Listen tagger) =
  Listen (tagger >> func)


addListen : (Url -> msg) -> (model -> Sub msg) -> model -> Sub msg
addListen toMsg toSubs model =
  Sub.batch [ subscription (Listen toMsg), toSubs model ]


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
    send (Listen tagger) =
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
