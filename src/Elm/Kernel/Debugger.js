/*

import Debugger.Expando as Expando exposing (S, Primitive, Sequence, Dictionary, Record, Constructor, ListSeq, SetSeq, ArraySeq)
import Debugger.Main as Main exposing (wrapView, wrapInit, wrapUpdate, wrapSubs, cornerView, popoutView, NoOp, Up, Down, toBlockerType)
import Debugger.Overlay as Overlay exposing (BlockNone, BlockMost)
import Elm.Kernel.Browser exposing (toEnv, makeAnimator)
import Elm.Kernel.List exposing (Cons, Nil)
import Elm.Kernel.Platform exposing (initialize)
import Elm.Kernel.Scheduler exposing (binding, succeed)
import Elm.Kernel.Utils exposing (Tuple0, Tuple2)
import Elm.Kernel.VirtualDom exposing (node, applyPatches, diff, doc, makeStepper, render, virtualize)
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



// PROGRAMS


var _Debugger_element = F4(function(impl, flagDecoder, debugMetadata, object)
{
	object['element'] = function(node, flags)
	{
		return __Platform_initialize(
			flagDecoder,
			flags,
			A3(__Main_wrapInit, debugMetadata, _Debugger_popout(), impl.__$init),
			__Main_wrapUpdate(impl.__$update),
			__Main_wrapSubs(impl.__$subscriptions),
			_Debugger_makeStepperBuilder(node, __Main_wrapView(impl.__$view))
		);
	};
	return object;
});


var _Debugger_fullscreen = F4(function(impl, flagDecoder, debugMetadata, object)
{
	object['fullscreen'] = function(flags)
	{
		return __Platform_initialize(
			A2(__Json_map, __Browser_toEnv, flagDecoder),
			flags,
			A3(__Main_wrapInit, debugMetadata, _Debugger_popout(), impl.__$init),
			__Main_wrapUpdate(impl.__$update),
			__Main_wrapSubs(impl.__$subscriptions),
			_Debugger_makeStepperBuilder(__VirtualDom_doc.body, function(model) {
				var ui = A2(__Main_wrapView, impl.__$view, model);
				if (__VirtualDom_doc.title !== ui.__$title)
				{
					__VirtualDom_doc.title = ui.__$title;
				}
				return __VirtualDom_node('body')(__List_Nil)(ui.__$body);
			})
		);
	};
	return object;
});


function _Debugger_popout()
{
	return { __doc: undefined, __isClosed: true };
}

function _Debugger_isOpen(popout)
{
	return !popout.__isClosed;
}

function _Debugger_open(popout)
{
	popout.__isClosed = false;
	return popout
}


function _Debugger_makeStepperBuilder(appNode, view)
{
	return function(sendToApp, initialModel)
	{
		var currApp = __VirtualDom_virtualize(appNode);
		var currCorner = __Main_cornerView(initialModel);
		var currPopout;
		var currBlocker = __Main_toBlockerType(initialModel);

		var cornerNode = __VirtualDom_render(currCorner, sendToApp);

		return __Browser_makeAnimator(initialModel, function(model)
		{
			var nextApp = view(model);
			var appPatches = __VirtualDom_diff(currApp, nextApp);
			appNode = __VirtualDom_applyPatches(appNode, currApp, appPatches, sendToApp);
			currApp = nextApp;

			// view corner

			var nextBlocker = __Main_toBlockerType(model);
			_Debugger_updateBlocker(currBlocker, nextBlocker);
			currBlocker = nextBlocker;

			if (model.__$popout.__isClosed)
			{
				var nextCorner = __Main_cornerView(model);
				var cornerPatches = __VirtualDom_diff(currCorner, nextCorner);
				cornerNode = __VirtualDom_applyPatches(cornerNode, currCorner, cornerPatches, sendToApp);
				currCorner = nextCorner;
				cornerNode.parentNode === appNode || appNode.appendChild(cornerNode);
				return;
			}

			cornerNode.parentNode && cornerNode.parentNode.removeChild(cornerNode);

			// view popout

			model.__$popout.__doc || (currPopout = _Debugger_openWindow(model.__$popout, sendToApp));

			__VirtualDom_doc = model.__$popout.__doc; // SWITCH TO POPOUT DOC
			var nextPopout = __Main_popoutView(model);
			var popoutPatches = __VirtualDom_diff(currPopout, nextPopout);
			__VirtualDom_applyPatches(model.__$popout.__doc.body, currPopout, popoutPatches, sendToApp);
			currPopout = nextPopout;
			__VirtualDom_doc = document; // SWITCH BACK TO NORMAL DOC
		});
	};
}



// POPOUT


function _Debugger_openWindow(popout, sendToApp)
{
	var w = 900, h = 360, x = screen.width - w, y = screen.height - h;
	var debuggerWindow = window.open('', '', 'width=' + w + ',height=' + h + ',left=' + x + ',top=' + y);
	var doc = debuggerWindow.document;
	doc.title = 'Elm Debugger';

	// handle arrow keys
	doc.addEventListener('keydown', function(event) {
		event.metaKey && event.which === 82 && window.location.reload();
		event.which === 38 && (sendToApp(__Main_Up), event.preventDefault());
		event.which === 40 && (sendToApp(__Main_Down), event.preventDefault());
	});

	// handle window close
	window.addEventListener('unload', close);
	debuggerWindow.addEventListener('unload', function() {
		popout.__doc = undefined;
		popout.__isClosed = true;
		sendToApp(__Main_NoOp);
		window.removeEventListener('unload', close);
	});
	function close() {
		popout.__doc = undefined;
		popout.__isClosed = true;
		sendToApp(__Main_NoOp);
		debuggerWindow.close();
	}

	// register new window
	popout.__doc = doc;
	popout.__isClosed = false;
	return __VirtualDom_virtualize(doc.body);
}



// SCROLL


function _Debugger_scroll(popout)
{
	return __Scheduler_binding(function(callback)
	{
		if (popout.__doc)
		{
			var msgs = popout.__doc.getElementById('elm-debugger-sidebar');
			if (msgs)
			{
				msgs.scrollTop = msgs.scrollHeight;
			}
		}
		callback(__Scheduler_succeed(__Utils_Tuple0));
	});
}



// UPLOAD


function _Debugger_upload()
{
	return __Scheduler_binding(function(callback)
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



// DOWNLOAD


var _Debugger_download = F2(function(historyLength, json)
{
	return __Scheduler_binding(function(callback)
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
	if (typeof value === 'boolean')
	{
		return value ? 'True' : 'False';
	}

	if (typeof value === 'number')
	{
		return value + '';
	}

	if (typeof value === 'string')
	{
		return '"' + _Debugger_addSlashes(value, false) + '"';
	}

	if (value instanceof String)
	{
		return "'" + _Debugger_addSlashes(value, true) + "'";
	}

	if (typeof value !== 'object' || value === null || !('$' in value))
	{
		return '…';
	}

	if (typeof value.$ === 'number')
	{
		return '…';
	}

	var code = value.$.charCodeAt(0);
	if (code === 0x23 /* # */ || /* a */ 0x61 <= code && code <= 0x7A /* z */)
	{
		return '…';
	}

	if (['Array_elm_builtin', 'Set_elm_builtin', 'RBNode_elm_builtin', 'RBEmpty_elm_builtin'].indexOf(value.$) >= 0)
	{
		return '…';
	}

	var keys = Object.keys(value);
	switch (keys.length)
	{
		case 1:
			return value.$;
		case 2:
			return value.$ + ' ' + _Debugger_messageToString(value.a);
		default:
			return value.$ + ' … ' + _Debugger_messageToString(value[keys[keys.length - 1]]);
	}
}


function _Debugger_init(value)
{
	if (typeof value === 'boolean')
	{
		return A3(__Expando_Constructor, __Maybe_Just(value ? 'True' : 'False'), true, __List_Nil);
	}

	if (typeof value === 'number')
	{
		return __Expando_Primitive(value + '');
	}

	if (typeof value === 'string')
	{
		return __Expando_S('"' + _Debugger_addSlashes(value, false) + '"');
	}

	if (value instanceof String)
	{
		return __Expando_S("'" + _Debugger_addSlashes(value, true) + "'");
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (tag === '::' || tag === '[]')
		{
			return A3(__Expando_Sequence, __Expando_ListSeq, true,
				A2(__List_map, _Debugger_init, value)
			);
		}

		if (tag === 'Set_elm_builtin')
		{
			return A3(__Expando_Sequence, __Expando_SetSeq, true,
				A3(__Set_foldr, _Debugger_initCons, __List_Nil, value)
			);
		}

		if (tag === 'RBNode_elm_builtin' || tag == 'RBEmpty_elm_builtin')
		{
			return A2(__Expando_Dictionary, true,
				A3(__Dict_foldr, _Debugger_initKeyValueCons, __List_Nil, value)
			);
		}

		if (tag === 'Array_elm_builtin')
		{
			return A3(__Expando_Sequence, __Expando_ArraySeq, true,
				A3(__Array_foldr, _Debugger_initCons, __List_Nil, value)
			);
		}

		if (typeof tag === 'number')
		{
			return __Expando_Primitive('<internals>');
		}

		var char = tag.charCodeAt(0);
		if (char === 35 || 65 <= char && char <= 90)
		{
			var list = __List_Nil;
			for (var i in value)
			{
				if (i === '$') continue;
				list = __List_Cons(_Debugger_init(value[i]), list);
			}
			return A3(__Expando_Constructor, char === 35 ? __Maybe_Nothing : __Maybe_Just(tag), true, __List_reverse(list));
		}

		return __Expando_Primitive('<internals>');
	}

	if (typeof value === 'object')
	{
		var dict = __Dict_empty;
		for (var i in value)
		{
			dict = A3(__Dict_insert, i, _Debugger_init(value[i]), dict);
		}
		return A2(__Expando_Record, true, dict);
	}

	return __Expando_Primitive('<internals>');
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



// BLOCK EVENTS


function _Debugger_updateBlocker(oldBlocker, newBlocker)
{
	if (oldBlocker === newBlocker) return;

	var oldEvents = _Debugger_blockerToEvents(oldBlocker);
	var newEvents = _Debugger_blockerToEvents(newBlocker);

	// remove old blockers
	for (var i = 0; i < oldEvents.length; i++)
	{
		document.removeEventListener(oldEvents[i], _Debugger_blocker, true);
	}

	// add new blockers
	for (var i = 0; i < newEvents.length; i++)
	{
		document.addEventListener(newEvents[i], _Debugger_blocker, true);
	}
}


function _Debugger_blocker(event)
{
	if (event.type === 'keydown' && event.metaKey && event.which === 82)
	{
		return;
	}

	var isScroll = event.type === 'scroll' || event.type === 'wheel';
	for (var node = event.target; node; node = node.parentNode)
	{
		if (isScroll ? node.id === 'elm-debugger-details' : node.id === 'elm-debugger-overlay')
		{
			return;
		}
	}

	event.stopPropagation();
	event.preventDefault();
}

function _Debugger_blockerToEvents(blocker)
{
	return blocker === __Overlay_BlockNone
		? []
		: blocker === __Overlay_BlockMost
			? _Debugger_mostEvents
			: _Debugger_allEvents;
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

