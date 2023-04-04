function Throttle(idx) {

    // private instance fields
    var engineProxy = $.connection.EngineHub;   // server's EngineHub SignalR proxy     
    var index = idx;                            // throttle index ID used by HTML to reference this throttle
    var throtID;                                // client throttle GUID
    var engineID;                               // Engine object GUID known by RRA
    var name;                                   // to display name when being controlled
    var address;                                // to display DCC address when being controlled
    var speed;
    var maxSpeed;
    var direction;
    var functions;
   
    // private method
    function resetState() {
        engineID = null;
        name = null;
        address = null;
        speed = 0;
        maxSpeed = 16;
        direction = 'F';
        functions = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
    };
    
    resetState();

    // public instance fields
    this.ptrDownSpeed;                  // keeps track of start point of a speed finger swipe
    this.rampTimerId;                   // ID of the setInterval() timer function running the auto speed ramp 
    this.rampDeltaAmt;                  // speed amount changed per timer tick for the auto speed ramp 
    this.spdRendSubmitted = false;      // keeps track that a request to render the speed for this frame has already been submitted
    
    // static public memebrs                
    Throttle.settings;                  // settings saved/loaded from client cookie
    Throttle.stateChanged;              // client defined function called whenever the throttle state changes
    Throttle.errorOccured;              // client defined function called when an error occurs
    

    // public properties --------------------------------------------

    Object.defineProperty(this, 'index', {
        get: function () { return index; }
    });

    Object.defineProperty(this, 'engineID', {
        get: function () { return engineID; }
    });

    Object.defineProperty(this, 'name', {
        get: function () { return name; }
    });

    Object.defineProperty(this, 'address', {
        get: function () { return address; }
    });

    Object.defineProperty(this, 'speed', {
        get: function () { return speed; },
        set: function (value) {
            if (!engineID) return;
            value = util.capVal(value, 0, maxSpeed);  // prevent speed out of accepted range
            if (speed != value) {
                speed = value;
                Throttle.stateChanged('Spd', this);
                engineProxy.server.SetSpeed(engineID, speed)
                    .fail(function (error) { Throttle.errorOccured(error.message); });
            };
        }
    });

    Object.defineProperty(this, 'maxSpeed', {
        get: function () { return maxSpeed; }
    });

    Object.defineProperty(this, 'direction', {
        get: function () { return direction; },
        set: function (value) {
            if (!engineID) return;
            if (direction != value) {
                direction = value;
                Throttle.stateChanged('Dir', this);
                engineProxy.server.SetDir(engineID, direction)
                    .fail(function (error) { Throttle.errorOccured(error.message); });
            };
        }
    });
      
    // public methods -----------------------------------------------

    this.control = function (engID) {
        var me = this;  // reference to this class instance through "this" is lost when we need it
        engineProxy.server.Control(engID)
            .done(function (init) {
                engineID = engID;
                name = init.Name;
                address = init.Address;
                speed = init.Speed;
                maxSpeed = init.MaxSpeed;
                direction = init.Direction;
                functions = init.Functions;
                Throttle.stateChanged('All', me);
            })
            .fail(function (error) { Throttle.errorOccured(error.message); });
    };

    this.release = function () {
        var me = this;  // reference to this class instance through "this" is lost when we need it
        engineProxy.server.Release(engineID)
            .done(function () {
                resetState();
                Throttle.stateChanged('All', me);
            })
            .fail(function (error) { Throttle.errorOccured(error.message); });
    };

    this.eStop = function () {
        if (!engineID) return;
        speed = 0;
        Throttle.stateChanged('Spd', this);
        engineProxy.server.EStop(engineID)
            .fail(function (error) { Throttle.errorOccured(error.message); });
    };

    this.function = function (idx, value) {
        if (value == undefined) {   //getter
            return functions[idx];
        } else {                    //setter
            if (!engineID) return;
            if (functions[idx] != value) {
                functions[idx] = value;
                Throttle.stateChanged('Func', this);
                engineProxy.server.SetFunc(engineID, idx, value)
                    .fail(function (error) { Throttle.errorOccured(error.message); });
            };
        };
    };

    this.functions = function () {
        return functions.slice(0);  // return clone
    };
    
    // server event handlers ----------------------------------------

    this.onSpeedChanged = function (spd, thrID) {
        // client already changes the local value of speed so we don't want to set it again as the event loops back to us
        // from the server which could interfere with rapid subsequent client speed changes;
        // this is not so important for direction and function changes so we don't perform this check for those

        if (throtID != thrID) speed = spd;
        Throttle.stateChanged('Spd', this);
    };

    this.onDirChanged = function (dir) {
        direction = dir;
        Throttle.stateChanged('Dir', this);
    };

    this.onFuncChanged = function (func) {
        functions = func;
        Throttle.stateChanged('Func', this);
    };

    // --------------------------------------------------------------

    // called at the end of the instance initialization so all public members are available
    Throttle.stateChanged('All', this);
}

