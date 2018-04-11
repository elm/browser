module Debugger.Html exposing
  ( Html
  , text
  , div, span, a, h1, code, p, ul, li, button, inlineStyle
  , Attribute
  , class, id, href, title, style
  , onClick
  , map
  , keyedNode, lazy, lazy2, lazy3, lazy4, lazy5
  )


import Json.Decode as Decode
import Json.Encode as Encode
import VirtualDom exposing (..)



-- HTML


type alias Html msg = VirtualDom.Node msg


text : String -> Html msg
text =
  VirtualDom.text


div : List (Attribute msg) -> List (Html msg) -> Html msg
div =
  node "div"


span : List (Attribute msg) -> List (Html msg) -> Html msg
span =
  node "span"


a : List (Attribute msg) -> List (Html msg) -> Html msg
a =
  node "a"


h1 : List (Attribute msg) -> List (Html msg) -> Html msg
h1 =
  node "h1"


code : List (Attribute msg) -> List (Html msg) -> Html msg
code =
  node "code"


p : List (Attribute msg) -> List (Html msg) -> Html msg
p =
  node "p"


ul : List (Attribute msg) -> List (Html msg) -> Html msg
ul =
  node "ul"


li : List (Attribute msg) -> List (Html msg) -> Html msg
li =
  node "li"


button : List (Attribute msg) -> List (Html msg) -> Html msg
button =
  node "button"


inlineStyle : String -> Html msg
inlineStyle css =
  node "style" [] [ text css ]



-- ATTRIBUTE


type alias Attribute msg = VirtualDom.Attribute msg


class : String -> Attribute msg
class name =
  property "className" (Encode.string name)


href : String -> Attribute msg
href name =
  property "href" (Encode.string name)


id : String -> Attribute msg
id =
  attribute "id"


title : String -> Attribute msg
title =
  attribute "title"


onClick : msg -> Attribute msg
onClick msg =
  on "click" (Normal (Decode.map Sync (Decode.succeed msg)))


style : String -> String -> Attribute msg
style =
  VirtualDom.style



-- FANCY STUFF


map : (a -> b) -> Html a -> Html b
map =
  VirtualDom.map


keyedNode : String -> List (Attribute msg) -> List (String, Html msg) -> Html msg
keyedNode =
  VirtualDom.keyedNode


lazy : (a -> Html msg) -> a -> Html msg
lazy =
  VirtualDom.lazy


lazy2 : (a -> b -> Html msg) -> a -> b -> Html msg
lazy2 =
  VirtualDom.lazy2


lazy3 : (a -> b -> c -> Html msg) -> a -> b -> c -> Html msg
lazy3 =
  VirtualDom.lazy3


lazy4 : (a -> b -> c -> d -> Html msg) -> a -> b -> c -> d -> Html msg
lazy4 =
  VirtualDom.lazy4


lazy5 : (a -> b -> c -> d -> e -> Html msg) -> a -> b -> c -> d -> e -> Html msg
lazy5 =
  VirtualDom.lazy5