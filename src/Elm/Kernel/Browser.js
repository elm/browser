/*

import Json.Decode as Json exposing (map)
import Elm.Kernel.List exposing (Nil)
import Elm.Kernel.Platform exposing (initialize)
import Elm.Kernel.Scheduler exposing (binding, succeed)
import Elm.Kernel.Utils exposing (Tuple0)
import Elm.Kernel.VirtualDom exposing (appendChild, applyPatches, diff, doc, node, render)

*/



// FAKE NAVIGATION


function _Browser_go(n)
{
	return __Scheduler_binding(function(callback)
	{
		if (n !== 0)
		{
			history.go(n);
		}
		callback(__Scheduler_succeed(__Utils_Tuple0));
	});
}


function _Browser_pushState(url)
{
	return __Scheduler_binding(function(callback)
	{
		history.pushState({}, '', url);
		callback(__Scheduler_succeed(_Browser_getUrl()));
	});
}


function _Browser_replaceState(url)
{
	return __Scheduler_binding(function(callback)
	{
		history.replaceState({}, '', url);
		callback(__Scheduler_succeed(_Browser_getUrl()));
	});
}



// REAL NAVIGATION


function _Browser_reload(skipCache)
{
	return __Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	});
}

function _Browser_load(url)
{
	return __Scheduler_binding(function(callback)
	{
		try
		{
			window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	});
}



// GET URL


function _Browser_getUrl()
{
	var loc = _VirtualDom_doc.location;

	return {
		__$href: loc.href,
		__$host: loc.host,
		__$hostname: loc.hostname,
		__$protocol: loc.protocol,
		__$origin: loc.origin,
		__$port_: loc.port,
		__$pathname: loc.pathname,
		__$search: loc.search,
		__$hash: loc.hash,
		__$username: loc.username,
		__$password: loc.password
	};
}



// DETECT IE11 PROBLEMS


function _Browser_isInternetExplorer11()
{
	return window.navigator.userAgent.indexOf('Trident') !== -1;
}



// PROGRAMS


var _Browser_staticPage = F5(function(virtualNode, flagDecoder, object, moduleName, debugMetadata)
{
	object['staticPage'] = function staticPage(node)
	{
		node.parentNode.replaceChild(
			__VirtualDom_render(virtualNode, function() {}),
			node
		);
	};
});


var _Browser_embed = F5(function(impl, flagDecoder, object, moduleName, debugMetadata)
{
	object['embed'] = function embed(node, flags)
	{
		__Platform_initialize(
			moduleName,
			flagDecoder,
			flags,
			impl.__$init,
			impl.__$update,
			impl.__$subscriptions,
			_Browser_makeStepperBuilder(node, impl.__$view)
		);
	};
});


var _Browser_fullscreen = F5(function(impl, flagDecoder, object, moduleName, debugMetadata)
{
	object['fullscreen'] = function fullscreen(flags)
	{
		__Platform_initialize(
			moduleName,
			A2(__Json_map, _Browser_toEnv, flagDecoder),
			flags,
			impl.__$init,
			impl.__$update,
			impl.__$subscriptions,
			_Browser_makeStepperBuilder(_VirtualDom_doc.body, function(model) {
				var ui = impl.__$view(model);
				if (_VirtualDom_doc.title !== ui.__$title)
				{
					_VirtualDom_doc.title = ui.__$title;
				}
				return F3(__VirtualDom_node, 'body', __List_Nil, ui.__$body);
			})
		);
	};
});


function _Browser_toEnv(flags)
{
	return {
		__$url: _Browser_getUrl(),
		__$flags: flags
	};
}



// RENDERER


function _Browser_makeStepperBuilder(domNode, view)
{
	return function(sendToApp, initialModel)
	{
		var currNode = _VirtualDom_virtualize(domNode);

		return _Browser_makeAnimator(initialModel, function(model)
		{
			var nextNode = view(model);
			var patches = __VirtualDom_diff(currNode, nextNode);
			domNode = __VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
			currNode = nextNode;
		});
	};
}



// ANIMATION


var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = __4_NO_REQUEST;

	function updateIfNeeded()
	{
		state = state === __4_EXTRA_REQUEST
			? __4_NO_REQUEST
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), __4_EXTRA_REQUEST );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === __4_PENDING_REQUEST && state = __4_EXTRA_REQUEST
				)
			: ( state === __4_NO_REQUEST && _Browser_requestAnimationFrame(updateIfNeeded),
				state = __4_PENDING_REQUEST
				);
	};
}
