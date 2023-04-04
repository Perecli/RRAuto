$.fn.tap = function (callBack) {
    var pointers = {};  // keep track of multiple touch points on each of the jQuery seleted elements

    $(this)
        .attr('touch-action', 'none')  // needed for PEP API
        .on('pointerdown', function (event) {                    
            pointers[event.originalEvent.pointerId] = {
                downX: event.pageX,
                downY: event.pageY
            };
        })
        .on('pointerup', function (event) {
            var pointer = pointers[event.originalEvent.pointerId];
            if (!pointer) return;
            var dist = Math.sqrt(Math.pow(event.pageX - pointer.downX, 2) + Math.pow(event.pageY - pointer.downY, 2));
            if (dist <= 10) { // down point must not travel more than 10px to be a tap
                callBack.call(this, event);  // first parameter becomes 'this' in callBack func's context
                delete pointers[event.originalEvent.pointerId];
            }
        });
};

$.fn.pushButton = function (callBack) {
    $(this)
        .attr('touch-action', 'none')   // needed for PEP API
        .on('pointerdown', function (event) {
            this.setPointerCapture(event.originalEvent.pointerId); // allows followup events to be raised even if no longer on target element
            event.action = 'down';
            callBack.call(this, event);
        })
        .on('pointerup', function (event) {
            event.action = 'up';
            callBack.call(this, event);
        });
};

$.fn.pointerDrag = function (callBack) {
    var pointers = {};  // keep track of multiple touch points on each of the jQuery seleted elements

    $(this)
        .attr('touch-action', 'none')   // needed for PEP API
        .on('pointerdown', function (event) {
            this.setPointerCapture(event.originalEvent.pointerId); // allows followup events to be raised even if no longer on target element
            var pointer = {
                downX: event.pageX,
                downY: event.pageY,
                firstMove: null
            };
            pointers[event.originalEvent.pointerId] = pointer;
            raiseEvent.call(this, 'down', pointer, event);
        })  
        .on('pointermove', function (event) {
            var pointer = pointers[event.originalEvent.pointerId];
            if (!pointer) return;
            pointer.firstMove = (pointer.firstMove == null) ? true : false;
            raiseEvent.call(this, 'move', pointer, event);
        })
        .on('pointerup', function (event) {
            var pointer = pointers[event.originalEvent.pointerId];
            if (!pointer) return;
            raiseEvent.call(this, 'up', pointer, event);
            delete pointers[event.originalEvent.pointerId];
        });

    function raiseEvent(name, pointer, event) {
        var me = this;
        event.action = name;
        event.deltaX = event.pageX - pointer.downX;
        event.deltaY = event.pageY - pointer.downY;
        event.firstMove = pointer.firstMove;
        event.elem = function () { // used function so calculation is performed on demand instead of automatically
            var rect = me.getBoundingClientRect();
            return { x: event.clientX - rect.left, y: event.clientY - rect.top }
        };
        callBack.call(this, event);
    }
};