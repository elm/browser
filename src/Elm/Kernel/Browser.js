/*

import Browser.Dom as Dom exposing (NotFound)
import Elm.Kernel.Debug exposing (crash)
import Elm.Kernel.Debugger exposing (element, document)
import Elm.Kernel.Json exposing (runHelp)
import Elm.Kernel.List exposing (Nil)
import Elm.Kernel.Platform exposing (initialize)
import Elm.Kernel.Scheduler exposing (binding, fail, rawSpawn, succeed, spawn)
import Elm.Kernel.Utils exposing (Tuple0, Tuple2)
import Elm.Kernel.VirtualDom exposing (appendChild, applyPatches, diff, doc, node, passiveSupported, render)
import Json.Decode as Json exposing (map)
import Maybe exposing (Just, Nothing)
import Result exposing (isOk)

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
		__VirtualDom_doc.location.reload(skipCache);
	});
}

function _Browser_load(url)
{
	return __Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			__VirtualDom_doc.location.reload(false);
		}
	});
}



// GET URL


function _Browser_getUrl()
{
	return __VirtualDom_doc.location.href;
}



// DETECT IE11 PROBLEMS


function _Browser_isInternetExplorer11()
{
	return _Browser_window.navigator.userAgent.indexOf('Trident') !== -1;
}



// ELEMENT


var __Debugger_element;

var _Browser_element = __Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return __Platform_initialize(
		flagDecoder,
		args,
		impl.__$init,
		impl.__$update,
		impl.__$subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.__$view;
			/**__PROD/
			var domNode = args['node'];
			//*/
			/**__DEBUG/
			var domNode = args && args['node'] ? args['node'] : __Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = __VirtualDom_diff(currNode, nextNode);
				domNode = __VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var __Debugger_document;

var _Browser_document = __Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return __Platform_initialize(
		flagDecoder,
		args,
		impl.__$init,
		impl.__$update,
		impl.__$subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.__$view;
			var title = __VirtualDom_doc.title;
			var bodyNode = __VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				var doc = view(model);
				var nextNode = __VirtualDom_node('body')(__List_Nil)(doc.__$body);
				var patches = __VirtualDom_diff(currNode, nextNode);
				bodyNode = __VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				(title !== doc.__$title) && (__VirtualDom_doc.title = title = doc.__$title);
			});
		}
	);
});


function _Browser_invalidUrl(url)
{
	__Debug_crash(1, url);
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
				state === __4_PENDING_REQUEST && (state = __4_EXTRA_REQUEST)
				)
			: ( state === __4_NO_REQUEST && _Browser_requestAnimationFrame(updateIfNeeded),
				state = __4_PENDING_REQUEST
				);
	};
}



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return __Scheduler_spawn(__Scheduler_binding(function(callback)
	{
		function handler(event)	{ __Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, __VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = __Json_runHelp(decoder, event);
	return __Result_isOk(result) ? __Maybe_Just(result.a) : __Maybe_Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof __VirtualDom_doc.hidden !== 'undefined')
		? { __$hidden: 'hidden', __$change: 'visibilitychange' }
		:
	(typeof __VirtualDom_doc.mozHidden !== 'undefined')
		? { __$hidden: 'mozHidden', __$change: 'mozvisibilitychange' }
		:
	(typeof __VirtualDom_doc.msHidden !== 'undefined')
		? { __$hidden: 'msHidden', __$change: 'msvisibilitychange' }
		:
	(typeof __VirtualDom_doc.webkitHidden !== 'undefined')
		? { __$hidden: 'webkitHidden', __$change: 'webkitvisibilitychange' }
		: { __$hidden: 'hidden', __$change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return __Scheduler_binding(function(callback)
	{
		var id = requestAnimationFrame(function() {
			callback(__Scheduler_succeed(Date.now()));
		});

		return function() {
			cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return __Scheduler_binding(function(callback)
	{
		callback(__Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return __Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? __Scheduler_succeed(doStuff(node))
				: __Scheduler_fail(__Dom_NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return __Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(__Scheduler_succeed(doStuff())
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return __Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	var node = _Browser_doc.documentElement;
	return {
		__$scene: {
			__$width: node.scrollWidth,
			__$height: node.scrollHeight
		},
		__$viewport: {
			__$x: _Browser_window.pageXOffset,
			__$y: _Browser_window.pageYOffset,
			__$width: node.clientWidth,
			__$height: node.clientHeight
		}
	};
}


var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return __Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			__$scene: {
				__$width: node.scrollWidth,
				__$height: node.scrollHeight
			},
			__$viewport: {
				__$x: node.scrollLeft,
				__$y: node.scrollTop,
				__$width: node.clientWidth,
				__$height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return __Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var node = _Browser_doc.documentElement;
		var rect = node.getBoundingClientRect();
		return {
			__$scene: {
				__$width: node.scrollWidth,
				__$height: node.scrollHeight
			},
			__$viewport: {
				__$x: _Browser_window.pageXOffset,
				__$y: _Browser_window.pageYOffset,
				__$width: node.clientWidth,
				__$height: node.clientHeight
			},
			__$element: {
				__$x: rect.top,
				__$y: rect.left,
				__$width: rect.width,
				__$height: rect.height
			}
		};
	});
}
