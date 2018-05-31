# How do I manage URL from a `Browser.element`?

Many companies introduce Elm gradually. They use `Browser.element` to embed Elm in a larger codebase as a low-risk way to see if Elm is helpful. If so, great, do more! If not, just revert, no big deal.

But at some point the element has grown to manage _almost_ the whole page. Everything except the header and footer, which are produced by the server. It might be nice to stop doing that, but it is not always practical, especially if there is a lot of legacy code that needs it.

So what do you do?


## Managing the URL from `Browser.element`

You would initialize your element like this:

```javascript
var app = Elm.Main.init({
	flags: location.href,
	node: document.getElementById('elm-main')
});
document.addEventListener('popstate', function () {
	app.ports.navigation.send(location.href);
});
```

And then in your Elm code, you would have a `main` something like this:

```elm
import Browser
import Url
import Url.Parser as Url


main : Program String Model Msg
main =
  Browser.element
  	{ init = init
  	, update = update
  	, subscriptions = subscriptions
  	, view = view
  	}


-- INIT

init : String -> ( Model, Cmd Msg )
init locationHref =
  case locationHrefToRoute locationHref of
    Nothing -> ...
    Just _ -> ...


locationHrefToRoute : String -> Maybe Route
locationHrefToRoute locationHref =
  case Url.fromString locationHref of
    Nothing -> Nothing
    Just url -> Url.parse myParser url

-- myParser : Url.Parser (Route -> Route) Route


-- SUBSCRIPTION

type Msg = NewRoute (Maybe Route) | ...

port navigation : (String -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  navigation (NewRoute << locationHrefToRoute)
```

But a `Browser.application` sets some of this stuff up for us! Why not do that with `Browser.element` as well?!


## Justification

The justification is that (1) this will lead to more reliable programs overall and (2) other designs do not actually save significant amounts of code. We will explore both in order.

### Reliability

There are some Elm users that have many different technologies embedded in the same document. So imagine we have a header in React, charts in Elm, and a data entry interface in Angular.

For URL management to work here, all three of these things need to agree on what page they are on. So the most reliable design is to have one `popstate` listener on the very outside, and it would tell React, Elm, and Angular what to do. This gives you a guarantee that they are all in agreement about the current page.

If instead each project was reacting to the URL internally, synchronization bugs would inevitably arise. Maybe it was a static page, but it upgraded to have the URL change. You added that to your Elm, but what about the Angular and React nodes. What happens to them? Probably people forget and it is just a confusing bug. So having one `popstate` makes it obvious that there is a decision to make here. And what happens when React starts producing URLs that Angular and Elm have never heard of? Do those nodes show some sort of 404 page?

> **Note:** If you wanted you could send the `location.href` into a `Platform.worker` to do the URL parsing in Elm. Once you have nice data, you could send it out a port for all the different elements on your page.


### Lines of Code

So the decision is primarily motivated by the fact that **URL management should happen at the highest possible level for reliability**, but what if Elm is the only thing on screen? How many lines extra are those people paying?

Well, the JavaScript code would be something like this:

```javascript
var app = Elm.Main.init({
	flags: ...
});
```

And in Elm:

```elm
import Browser
import Url
import Url.Parser as Url


main : Program Flags Model Msg
main =
  Browser.application
  	{ init = init
  	, update = update
  	, subscriptions = subscriptions
  	, onNavigation = NewRoute << Url.parse myParser
  	, view = view
  	}


-- INIT

init : Flags -> Url.Url -> ( Model, Cmd Msg )
init flags url =
  case Url.parse myParser locationHref of
    Nothing -> ...
    Just _ -> ...

-- myParser : Url.Parser (Route -> Route) Route
```

So the main differences are:

1. You can drop the `port` declaration.
2. You move the `NewRoute` logic from `subscriptions` to `onNavigation`.
3. You do not need to call `Url.fromString`.
4. You do not need a `popstate` listener in JavaScript.

Depending on your whitespace usage, this is probably 10 or 20 lines that we are skipping in the `application` version.

The lines are very straight forward as well. Turn a `String` to a `Url` before using `myParser`, and add an event handler for `popstate`. Writing these functions yourself also gives you the leeway to customize things. Maybe you only want the hash because you support certain IE browsers? Maybe you only want the last two segments of the URL because the rest is managed in React? Just change `locationHrefToRoute` to be `whateverToRoute` based on what you need.


### Summary

Given the broad range of people and companies using Elm, this design seems like the best balance of concerns.