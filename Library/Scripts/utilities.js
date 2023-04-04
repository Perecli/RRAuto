var util = {
    randHexStr: function (length) {      
        // length: number of 2 digit hex numbers making up the result string

        var arr = new Uint8Array(length);
        window.crypto.getRandomValues(arr);
        return [].map.call(arr, function (n) { return ("0" + n.toString(16)).slice(-2).toUpperCase(); }).join('');
    },

    capVal: function (value, low, high) {
        if (value < low) return low;
        if (value > high) return high;
        return value;
    },

    phoneDeviceSize: function () {
        // checks width in case device is in portrait and height if in landscape
        return ($(window).width() <= 480 || $(window).height() <= 480) 
    },

    selCache: function () {
        var cache = {};

        function getFromCache(selector) {
            if (cache[selector] === undefined) {
                cache[selector] = $(selector);
            };
            return cache[selector];
        }

        return { $: getFromCache };
    }
}
