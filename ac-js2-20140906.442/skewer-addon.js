/**
 * @fileOverview Completion request handler for skewer.js
 * @requires skewer
 * @version 1.0
 */

/**
 * Handles a completion request from Emacs.
 * @param request The request object sent by Emacs
 * @returns The completions and init values to be returned to Emacs
 */
skewer.fn.complete = function(request) {
    var result =  {
        type : request.type,
        id : request.id,
        strict : request.strict,
        status : "success"
    },

        /**
         * Methods for generating candidates
         */
        METHOD = {
            EVAL : 0,
            GLOBAL : 1
        },

        /**
         * Add the properties from object to extendObject. Properties
         * may be from the prototype but we still want to add them.
         */
        extend = function(extendObject, object) {
            for(var key in object) {
                extendObject[key] = object[key];
            }
        },

        globalCompletion = function() {
            var global = Function('return this')(),
                keys = Object.keys(global);
            candidates = buildCandidates(global, keys);
        },

        evalCompletion = function(evalObject) {
            var obj = (eval, eval)(evalObject);
            if (typeof obj === "object") {
                candidates = buildCandidates(obj) || {};
                while (request.prototypes && (obj = Object.getPrototypeOf(obj)) !== null) {
                    extend(candidates, buildCandidates(obj));
                }
            } else if (typeof obj === "function"){
                candidates = buildCandidates(obj) || {};
                extend(candidates, buildCandidates(Object.getPrototypeOf(obj)));
                if (request.prototypes) {
                    var protoObject = Object.getPrototypeOf(obj.prototype);
                    if (protoObject !== null) {
                        extend(candidates, buildCandidates(protoObject));
                    } else {
                        extend(candidates, buildCandidates(obj.prototype));
                    }
                }
            }
        },

        /**
         * Completion candidates sent back to Emacs. Keys are
         * completion candidates the values are the inital items or
         * function interfaces.
         */
        candidates = {},

        /**
         * Build the candiates to return to Emacs.
         * @param obj The object to get candidates from
         * @param items The selected keys from obj to create candidates for
         * @return object containing completion candidates and documentation strings
         */
        buildCandidates = function(obj, items) {
            var keys = items || Object.getOwnPropertyNames(obj), values = {};
            for (var i = 0; i < keys.length; i++) {
                var key = keys[i];
                if (key === "callee" || key === "caller" || key === "arguments") continue;
                if (Object.prototype.toString.call(obj[key]) === "[object Function]") {
                    values[key] = obj[key].toString();
                } else if (typeof obj[key] === "object"){
                    values[key] = "[object Object]";
                } else if (typeof obj[key] === "number") {
                    if (!(obj instanceof Array)) {
                        values[key] = obj[key].toString();
                    }
                } else if (typeof obj[key] === "string") {
                    values[key] = obj[key].toString();
                } else if(obj[key] === true) {
                    values[key] = "true";
                } else if (obj[key] === false) {
                    values[key] = "false";
                } else {
                    values[key] = "";
                }
            }
            return values;
        };
    try {
        switch (request.method) {
        case METHOD.GLOBAL:
            globalCompletion();
            break;
        default:
            evalCompletion(request.eval);
        }
        result.value = candidates;
    } catch (error){
        skewer.errorResult(error, result, request);
    }
    return result;
};
