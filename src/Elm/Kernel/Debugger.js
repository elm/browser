/*

import Debugger.Main as Main exposing (view, init, update, subs, cornerView, popoutView, Up, Down, Close)
import Elm.Kernel.Browser exposing (toEnv, makeAnimator)
import Elm.Kernel.List exposing (Cons, Nil)
import Elm.Kernel.Platform exposing (initialize)
import Elm.Kernel.Scheduler exposing (nativeBinding, succeed)
import Elm.Kernel.Utils exposing (Tuple0, Tuple2)
import Elm.Kernel.VirtualDom exposing (node, applyPatches, diff, doc, makeStepper, render)
import Json.Decode as Json exposing (map)
import List exposing (map, reverse)
import Maybe exposing (Just, Nothing)
import Set exposing (foldr)
import Dict exposing (foldr, empty, insert)
import Array exposing (foldr)

*/



// HELPERS


function _Debugger_unsafeCoerce(value)
{
	return value;
}



// DEBUG PROGRAMS


var _Browser_fullscreen = F4(function(impl, flagDecoder, debugMetadata, object)
{
	object['fullscreen'] = function(flags)
	{
 		var token = { __doc: undefined };
		return __Platform_initialize(
			A2(__Json_map, __Browser_toEnv, flagDecoder),
			flags,
			A3(__Main_init, debugMetadata, token, impl.__$init),
			__Main_update(impl.__$update),
			__Main_subs(impl.__$subscriptions),
			_Debugger_fullscreenRenderer(impl.__$view)
		);
	};
	return object;
});


function _Debugger_fullscreenRenderer(view)
{
	return function(sendToApp, initialModel)
	{
		var popoutCurrVNode;
		var popoutRootNode;

		var bodyNode = __VirtualDom_doc.body;
		var currVNode = _VirtualDom_virtualize(bodyNode);

		return __Browser_makeAnimator(initialModel, function(model)
		{
			// user view function
			var ui = A2(__Main_view, view, model);
			(__VirtualDom_doc.title !== ui.__$title) && __VirtualDom_doc.title = ui.__$title;

			// corner view function
			var cornerVNode = __Main_cornerView(model).b;

			// update <body>
			var nextVNode = __VirtualDom_node('body')(__List_Nil)(__List_Cons(cornerVNode, ui.__$body));
			var patches = __VirtualDom_diff(currVNode, nextVNode);
			bodyNode = __VirtualDom_applyPatches(bodyNode, currVNode, patches, sendToApp);
			currVNode = nextVNode;

			// update popout
			if (!model.isDebuggerOpen) return;
			if (!model.__$token.__doc)
			{
				popoutCurrVNode = __Main_popoutView(model);
				popoutRootNode = _Debugger_openWindow(model.__$token, popoutCurrVNode, sendToApp);
				return;
			}

			// switch to document of popout
			__VirtualDom_doc = model.__$token.__doc;

			var popoutNextVNode = __Main_popoutView(model);
			var patches = __VirtualDom_diff(popoutCurrVNode, popoutNextVNode);
			popoutRootNode = __VirtualDom_applyPatches(popoutRootNode, popoutCurrVNode, patches, sendToApp);
			popoutCurrVNode = popoutNextVNode;

			// switch back to normal document
			__VirtualDom_doc = document;
		});
	};
}



// EFFECTS


function _Debugger_scroll(token)
{
	return __Scheduler_nativeBinding(function(callback)
	{
		var doc = token.__doc;
		if (doc)
		{
			var msgs = doc.getElementsByClassName('debugger-sidebar-messages')[0];
			if (msgs)
			{
				msgs.scrollTop = msgs.scrollHeight;
			}
		}
		callback(__Scheduler_succeed(__Utils_Tuple0));
	});
}

function _Debugger_upload()
{
	return __Scheduler_nativeBinding(function(callback)
	{
		var element = document.createElement('input');
		element.setAttribute('type', 'file');
		element.setAttribute('accept', 'text/json');
		element.style.display = 'none';
		element.addEventListener('change', function(event)
		{
			var fileReader = new FileReader();
			fileReader.onload = function(e)
			{
				callback(__Scheduler_succeed(e.target.result));
			};
			fileReader.readAsText(event.target.files[0]);
			document.body.removeChild(element);
		});
		document.body.appendChild(element);
		element.click();
	});
}

var _Debugger_download = F2(function(historyLength, json)
{
	return __Scheduler_nativeBinding(function(callback)
	{
		var fileName = 'history-' + historyLength + '.txt';
		var jsonString = JSON.stringify(json);
		var mime = 'text/plain;charset=utf-8';
		var done = __Scheduler_succeed(__Utils_Tuple0);

		// for IE10+
		if (navigator.msSaveBlob)
		{
			navigator.msSaveBlob(new Blob([jsonString], {type: mime}), fileName);
			return callback(done);
		}

		// for HTML5
		var element = document.createElement('a');
		element.setAttribute('href', 'data:' + mime + ',' + encodeURIComponent(jsonString));
		element.setAttribute('download', fileName);
		element.style.display = 'none';
		document.body.appendChild(element);
		element.click();
		document.body.removeChild(element);
		callback(done);
	});
});



// POPOUT CONTENT


function _Debugger_messageToString(value)
{
	switch (typeof value)
	{
		case 'boolean':
			return value ? 'True' : 'False';
		case 'number':
			return value + '';
		case 'string':
			return '"' + _Debugger_addSlashes(value, false) + '"';
	}
	if (value instanceof String)
	{
		return '\'' + _Debugger_addSlashes(value, true) + '\'';
	}
	if (typeof value !== 'object' || value === null || !('ctor' in value))
	{
		return '…';
	}

	var ctorStarter = value.ctor.substring(0, 5);
	if (ctorStarter === '_Tupl' || ctorStarter === '_Task')
	{
		return '…'
	}
	if (['_Array', '<decoder>', '_Process', '::', '[]', 'Set_elm_builtin', 'RBNode_elm_builtin', 'RBEmpty_elm_builtin'].indexOf(value.ctor) >= 0)
	{
		return '…';
	}

	var keys = Object.keys(value);
	switch (keys.length)
	{
		case 1:
			return value.ctor;
		case 2:
			return value.ctor + ' ' + _Debugger_messageToString(value._0);
		default:
			return value.ctor + ' … ' + _Debugger_messageToString(value[keys[keys.length - 1]]);
	}
}


function _Debugger_primitive(str)
{
	return { ctor: 'Primitive', _0: str };
}


function _Debugger_init(value)
{
	var type = typeof value;

	if (type === 'boolean')
	{
		return {
			ctor: 'Constructor',
			_0: __Maybe_Just(value ? 'True' : 'False'),
			_1: true,
			_2: __List_Nil
		};
	}

	if (type === 'number')
	{
		return _Debugger_primitive(value + '');
	}

	if (type === 'string')
	{
		return { ctor: 'S', _0: '"' + _Debugger_addSlashes(value, false) + '"' };
	}

	if (value instanceof String)
	{
		return { ctor: 'S', _0: "'" + _Debugger_addSlashes(value, true) + "'" };
	}

	if (value instanceof Date)
	{
		return _Debugger_primitive('<' + value.toString() + '>');
	}

	if (value === null)
	{
		return _Debugger_primitive('XXX');
	}

	if (type === 'object' && 'ctor' in value)
	{
		var ctor = value.ctor;

		if (ctor === '::' || ctor === '[]')
		{
			return {
				ctor: 'Sequence',
				_0: {ctor: 'ListSeq'},
				_1: true,
				_2: A2(__List_map, _Debugger_init, value)
			};
		}

		if (ctor === 'Set_elm_builtin')
		{
			return {
				ctor: 'Sequence',
				_0: {ctor: 'SetSeq'},
				_1: true,
				_2: A3(__Set_foldr, _Debugger_initCons, __List_Nil, value)
			};
		}

		if (ctor === 'RBNode_elm_builtin' || ctor == 'RBEmpty_elm_builtin')
		{
			return {
				ctor: 'Dictionary',
				_0: true,
				_1: A3(__Dict_foldr, _Debugger_initKeyValueCons, __List_Nil, value)
			};
		}

		if (ctor === '_Array')
		{
			return {
				ctor: 'Sequence',
				_0: {ctor: 'ArraySeq'},
				_1: true,
				_2: A3(__Array_foldr, _Debugger_initCons, __List_Nil, value)
			};
		}

		var ctorStarter = value.ctor.substring(0, 5);
		if (ctorStarter === '_Task')
		{
			return _Debugger_primitive('<task>');
		}

		if (ctor === '<decoder>')
		{
			return _Debugger_primitive(ctor);
		}

		if (ctor === '_Process')
		{
			return _Debugger_primitive('<process>');
		}

		var list = __List_Nil;
		for (var i in value)
		{
			if (i === 'ctor') continue;
			list = __List_Cons(_Debugger_init(value[i]), list);
		}
		return {
			ctor: 'Constructor',
			_0: ctorStarter === '_Tupl' ? __Maybe_Nothing : __Maybe_Just(ctor),
			_1: true,
			_2: __List_reverse(list)
		};
	}

	if (type === 'object')
	{
		var dict = __Dict_empty;
		for (var i in value)
		{
			dict = A3(__Dict_insert, i, _Debugger_init(value[i]), dict);
		}
		return { ctor: 'Record', _0: true, _1: dict };
	}

	return _Debugger_primitive('XXX');
}

var _Debugger_initCons = F2(function initConsHelp(value, list)
{
	return __List_Cons(_Debugger_init(value), list);
});

var _Debugger_initKeyValueCons = F3(function(key, value, list)
{
	return __List_Cons(
		__Utils_Tuple2(_Debugger_init(key), _Debugger_init(value)),
		list
	);
});

function _Debugger_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');
	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}



// POPOUT WINDOW


function _Debugger_openWindow(token, virtualNode, sendToApp)
{
	var w = 900;
	var h = 360;
	var x = screen.width - w;
	var y = screen.height - h;
	var debugWindow = window.open('', '', 'width=' + w + ',height=' + h + ',left=' + x + ',top=' + y);

	// switch to window document
	var doc = debugWindow.document;
	token.__doc = __VirtualDom_doc = doc;

	doc.title = 'Elm Debugger';
	doc.body.style.margin = '0';
	doc.body.style.padding = '0';
	var domNode = __VirtualDom_render(virtualNode, sendToApp);
	doc.body.appendChild(domNode);

	doc.addEventListener('keydown', function(event) {
		event.metaKey && event.which === 82 && window.location.reload();
		event.which === 38 && (sendToApp(__Main_Up), event.preventDefault());
		event.which === 40 && (sendToApp(__Main_Down), event.preventDefault());
	});

	function close()
	{
		token.__doc = undefined;
		debugWindow.close();
	}
	window.addEventListener('unload', close);
	debugWindow.addEventListener('unload', function() {
		token.__doc = undefined;
		window.removeEventListener('unload', close);
		sendToApp(__Main_Close);
	});

	// switch back to the normal document
	__VirtualDom_doc = document;

	return domNode;
}


// BLOCK EVENTS

function _Debugger_wrapViewIn(appEventNode, overlayNode, viewIn)
{
	var ignorer = _Debugger_makeIgnorer(overlayNode);
	var blocking = 'Normal';
	var overflow;

	var normalTagger = appEventNode.tagger;
	var blockTagger = function() {};

	return function(model)
	{
		var tuple = viewIn(model);
		var newBlocking = tuple._0.ctor;
		appEventNode.tagger = newBlocking === 'Normal' ? normalTagger : blockTagger;
		if (blocking !== newBlocking)
		{
			_Debugger_traverse('removeEventListener', ignorer, blocking);
			_Debugger_traverse('addEventListener', ignorer, newBlocking);

			if (blocking === 'Normal')
			{
				overflow = document.body.style.overflow;
				document.body.style.overflow = 'hidden';
			}

			if (newBlocking === 'Normal')
			{
				document.body.style.overflow = overflow;
			}

			blocking = newBlocking;
		}
		return tuple._1;
	}
}

function _Debugger_traverse(verbEventListener, ignorer, blocking)
{
	switch(blocking)
	{
		case 'Normal':
			return;

		case 'Pause':
			return _Debugger_traverseHelp(verbEventListener, ignorer, _Debugger_mostEvents);

		case 'Message':
			return _Debugger_traverseHelp(verbEventListener, ignorer, _Debugger_allEvents);
	}
}

function _Debugger_traverseHelp(verbEventListener, handler, eventNames)
{
	for (var i = 0; i < eventNames.length; i++)
	{
		document.body[verbEventListener](eventNames[i], handler, true);
	}
}

function _Debugger_makeIgnorer(overlayNode)
{
	return function(event)
	{
		if (event.type === 'keydown' && event.metaKey && event.which === 82)
		{
			return;
		}

		var isScroll = event.type === 'scroll' || event.type === 'wheel';

		var node = event.target;
		while (node !== null)
		{
			if (node.className === 'elm-overlay-message-details' && isScroll)
			{
				return;
			}

			if (node === overlayNode && !isScroll)
			{
				return;
			}
			node = node.parentNode;
		}

		event.stopPropagation();
		event.preventDefault();
	}
}

var _Debugger_mostEvents = [
	'click', 'dblclick', 'mousemove',
	'mouseup', 'mousedown', 'mouseenter', 'mouseleave',
	'touchstart', 'touchend', 'touchcancel', 'touchmove',
	'pointerdown', 'pointerup', 'pointerover', 'pointerout',
	'pointerenter', 'pointerleave', 'pointermove', 'pointercancel',
	'dragstart', 'drag', 'dragend', 'dragenter', 'dragover', 'dragleave', 'drop',
	'keyup', 'keydown', 'keypress',
	'input', 'change',
	'focus', 'blur'
];

var _Debugger_allEvents = _Debugger_mostEvents.concat('wheel', 'scroll');

