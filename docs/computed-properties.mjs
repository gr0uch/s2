
/* (DEFPARAMETER *OBSERVABLE-CONTEXT-MAP* (NEW (*WEAK-MAP))) */
var OBSERVABLECONTEXTMAP = new WeakMap();
/* (DEFPARAMETER *TARGET-OBSERVABLES-MAP* (NEW (*WEAK-MAP))) */
var TARGETOBSERVABLESMAP = new WeakMap();
/* (DEFPARAMETER *READ-STACK* (LIST)) */
var READSTACK = [];
/* (DEFPARAMETER *CLEAR-STACK-TIMEOUT* NIL) */
var CLEARSTACKTIMEOUT = null;
/* (DEFPARAMETER *STACK-DELIMITER-SYMBOL* (*SYMBOL 'STACK-DELIMITER)) */
var STACKDELIMITERSYMBOL = Symbol('stackDelimiter');
/* (DEFPARAMETER *PROXY-OBSERVABLE*
     (LET ((SET-PROPERTY (MAKE-SET-PROPERTY)))
       (CREATE GET GET-PROPERTY SET SET-PROPERTY DELETE-PROPERTY SET-PROPERTY))) */
var PROXYOBSERVABLE = (function () {
    var setProperty = makeSetProperty();
    
    return { get : getProperty,
             set : setProperty,
             deleteProperty : setProperty
           };
})();
/* (DEFPARAMETER *PROXY-DEEP-OBSERVABLE*
     (LET ((SET-PROPERTY (MAKE-SET-PROPERTY T)))
       (CREATE GET GET-PROPERTY SET SET-PROPERTY DELETE-PROPERTY SET-PROPERTY))) */
var PROXYDEEPOBSERVABLE = (function () {
    var setProperty = makeSetProperty(true);
    
    return { get : getProperty,
             set : setProperty,
             deleteProperty : setProperty
           };
})();
/* (DEFUN CLEAR-STACK ()
     (SETF *CLEAR-STACK-TIMEOUT* NIL)
     (LOOP WHILE (LENGTH *READ-STACK*)
           DO (CHAIN *READ-STACK* (POP)))) */
function clearStack() {
    CLEARSTACKTIMEOUT = null;
    while (READSTACK.length) {
        READSTACK.pop();
    };
};
/* (DEFUN POP-STACK ()
     (LOOP FOR I FROM (- (LENGTH *READ-STACK*) 1) DOWNTO 0
           DO (WHEN (EQ (CHAIN *READ-STACK* (POP)) *STACK-DELIMITER-SYMBOL*)
                (BREAK)))) */
function popStack() {
    for (var i = READSTACK.length - 1; i >= 0; i -= 1) {
        if (READSTACK.pop() === STACKDELIMITERSYMBOL) {
            break;
        };
    };
};
/* (DEFUN GET-PROPERTY (TARGET KEY RECEIVER)
     (WHEN
         (NOT
          (CHAIN *READ-STACK*
                 (FIND
                  (LAMBDA (TUPLE)
                    (AND (EQ (ELT TUPLE 0) TARGET) (EQ (ELT TUPLE 1) KEY))))))
       (CHAIN *READ-STACK* (PUSH (LIST TARGET KEY)))
       (WHEN (NOT *CLEAR-STACK-TIMEOUT*)
         (SETF *CLEAR-STACK-TIMEOUT* (SET-TIMEOUT CLEAR-STACK 0))))
     (CHAIN *REFLECT (GET TARGET KEY RECEIVER))) */
function getProperty(target, key, receiver) {
    if (!READSTACK.find(function (tuple) {
        return tuple[0] === target && tuple[1] === key;
    })) {
        READSTACK.push([target, key]);
        if (!CLEARSTACKTIMEOUT) {
            CLEARSTACKTIMEOUT = setTimeout(clearStack, 0);
        };
    };
    
    return Reflect.get(target, key, receiver);
};
/* (DEFUN IS-OBJECT (OBJ) (AND OBJ (EQ (TYPEOF OBJ) 'OBJECT))) */
function isObject(obj) {
    return obj && typeof obj === 'object';
};
/* (DEFUN MAKE-SET-PROPERTY (IS-DEEP)
     (DEFUN SET-PROPERTY (TARGET KEY VALUE RECEIVER)
       (LET ((OLD-VALUE (GETPROP TARGET KEY)))
         (WHEN (EQ OLD-VALUE VALUE) (RETURN-FROM SET-PROPERTY T))
         (WHEN (AND IS-DEEP (IS-OBJECT VALUE))
           (IF (IS-OBJECT OLD-VALUE)
               (PROGN
                (DEEP-REPLACE (GETPROP RECEIVER KEY) VALUE)
                (RETURN-FROM SET-PROPERTY T))
               (SETF VALUE (CREATE-SOURCE VALUE T)))))
       (IF (NOT (EQ VALUE UNDEFINED))
           (CHAIN *REFLECT (SET TARGET KEY VALUE RECEIVER))
           (CHAIN *REFLECT (DELETE-PROPERTY TARGET KEY)))
       (LET ((CONTEXT (CHAIN *OBSERVABLE-CONTEXT-MAP* (GET TARGET)))
             (KEY-BINDINGS NIL))
         (WHEN (NOT CONTEXT) (RETURN-FROM SET-PROPERTY T))
         (SETF KEY-BINDINGS (OR (GETPROP CONTEXT KEY) (LIST)))
         (LOOP FOR KEY-BINDING IN KEY-BINDINGS
               DO (LET* ((OBJ (@ KEY-BINDING 0))
                         (OBJ-KEY (@ KEY-BINDING 1))
                         (FN (@ KEY-BINDING 2)))
                    (COMPUTE-DEPENDENCIES OBJ OBJ-KEY FN))))
       T)
     SET-PROPERTY) */
function makeSetProperty(isDeep) {
    function setProperty(target, key, value, receiver) {
        var oldValue = target[key];
        if (oldValue === value) {
            return true;
        };
        if (isDeep && isObject(value)) {
            if (isObject(oldValue)) {
                deepReplace(receiver[key], value);
                
                return true;
            } else {
                value = createSource(value, true);
            };
        };
        if (value !== undefined) {
            Reflect.set(target, key, value, receiver);
        } else {
            Reflect.deleteProperty(target, key);
        };
        var context = OBSERVABLECONTEXTMAP.get(target);
        var keyBindings = null;
        if (!context) {
            
            return true;
        };
        keyBindings = context[key] || [];
        var _js6 = keyBindings.length;
        for (var _js5 = 0; _js5 < _js6; _js5 += 1) {
            var keyBinding = keyBindings[_js5];
            var obj = keyBinding[0];
            var objKey = keyBinding[1];
            var fn = keyBinding[2];
            computeDependencies(obj, objKey, fn);
        };
        
        return true;
    };
    return setProperty;
};
/* (DEFUN DEEP-REPLACE (PROXY OBJ)
     (LOOP FOR KEY OF OBJ
           DO (LET ((VALUE (GETPROP OBJ KEY)) (OLD-VALUE (GETPROP PROXY KEY)))
                (IF (AND (IS-OBJECT VALUE) (IS-OBJECT OLD-VALUE))
                    (DEEP-REPLACE OLD-VALUE VALUE)
                    (SETF (GETPROP PROXY KEY) VALUE))))
     (LOOP FOR KEY OF PROXY
           DO (LET ((OLD-VALUE (GETPROP PROXY KEY)))
                (WHEN (NOT (CHAIN OBJ (HAS-OWN-PROPERTY KEY)))
                  (DELETE (GETPROP PROXY KEY)))))) */
function deepReplace(proxy, obj) {
    for (var key in obj) {
        var value = obj[key];
        var oldValue = proxy[key];
        if (isObject(value) && isObject(oldValue)) {
            deepReplace(oldValue, value);
        } else {
            proxy[key] = value;
        };
    };
    for (var key in proxy) {
        var oldValue7 = proxy[key];
        if (!obj.hasOwnProperty(key)) {
            delete proxy[key];
        };
    };
};
/* (DEFUN CREATE-SOURCE (OBJ IS-DEEP)
     (WHEN (NOT OBJ) (SETF OBJ (CREATE)))
     (LET ((PROXY
            (NEW
             (*PROXY OBJ
              (IF IS-DEEP
                  *PROXY-DEEP-OBSERVABLE*
                  *PROXY-OBSERVABLE*)))))
       (WHEN IS-DEEP
         (LOOP FOR KEY OF OBJ
               DO (LET ((VALUE (GETPROP OBJ KEY)))
                    (WHEN (IS-OBJECT VALUE)
                      (SETF (GETPROP OBJ KEY) (CREATE-SOURCE VALUE T))))))
       PROXY)) */
function createSource(obj, isDeep) {
    if (!obj) {
        obj = {  };
    };
    var proxy = new Proxy(obj, isDeep ? PROXYDEEPOBSERVABLE : PROXYOBSERVABLE);
    if (isDeep) {
        for (var key in obj) {
            var value = obj[key];
            if (isObject(value)) {
                obj[key] = createSource(value, true);
            };
        };
    };
    
    return proxy;
};
/* (DEFUN MOUNT-OBJECT (OBJ)
     (LOOP FOR KEY OF OBJ
           DO (LET* ((VALUE (GETPROP OBJ KEY))
                     (IS-FUNCTION (EQ (TYPEOF VALUE) 'FUNCTION)))
                (WHEN IS-FUNCTION
                  (WHEN (@ VALUE IS-EVENT-LISTENER) (CONTINUE))
                  (COMPUTE-DEPENDENCIES OBJ KEY VALUE))))) */
function mountObject(obj) {
    for (var key in obj) {
        var value = obj[key];
        var isFunction = typeof value === 'function';
        if (isFunction) {
            if (value.isEventListener) {
                continue;
            };
            computeDependencies(obj, key, value);
        };
    };
};
/* (DEFUN COMPUTE-DEPENDENCIES (OBJ KEY FN)
     (CHAIN *READ-STACK* (PUSH *STACK-DELIMITER-SYMBOL*))
     (LET ((RETURN-VALUE (CHAIN FN (CALL OBJ))))
       (IF (NOT (EQ RETURN-VALUE UNDEFINED))
           (SETF (GETPROP OBJ KEY) RETURN-VALUE)
           (DELETE (GETPROP OBJ KEY)))
       (LOOP FOR I FROM (- (LENGTH *READ-STACK*) 1) DOWNTO 0
             DO (WHEN (EQ (TYPEOF (GETPROP *READ-STACK* I)) 'SYMBOL)
                  BREAK) (LET* ((TUPLE (GETPROP *READ-STACK* I))
                                (OBSERVABLE (@ TUPLE 0))
                                (OBSERVABLE-KEY (@ TUPLE 1))
                                (OBSERVABLE-CONTEXT NIL))
                           (WHEN
                               (NOT (CHAIN *TARGET-OBSERVABLES-MAP* (HAS OBJ)))
                             (CHAIN *TARGET-OBSERVABLES-MAP* (SET OBJ (LIST))))
                           (LET ((OBSERVABLES
                                  (CHAIN *TARGET-OBSERVABLES-MAP* (GET OBJ))))
                             (WHEN
                                 (NOT
                                  (CHAIN OBSERVABLES (INCLUDES OBSERVABLE)))
                               (CHAIN OBSERVABLES (PUSH OBSERVABLE))))
                           (WHEN
                               (NOT
                                (CHAIN *OBSERVABLE-CONTEXT-MAP*
                                       (HAS OBSERVABLE)))
                             (CHAIN *OBSERVABLE-CONTEXT-MAP*
                                    (SET OBSERVABLE (CREATE))))
                           (SETF OBSERVABLE-CONTEXT
                                   (CHAIN *OBSERVABLE-CONTEXT-MAP*
                                          (GET OBSERVABLE)))
                           (WHEN
                               (NOT
                                (GETPROP OBSERVABLE-CONTEXT OBSERVABLE-KEY))
                             (SETF (GETPROP OBSERVABLE-CONTEXT OBSERVABLE-KEY)
                                     (LIST)))
                           (LET ((KEY-BINDINGS
                                  (GETPROP OBSERVABLE-CONTEXT OBSERVABLE-KEY)))
                             (WHEN
                                 (NOT
                                  (CHAIN KEY-BINDINGS
                                         (FIND
                                          (LAMBDA (ENTRY)
                                            (AND (EQ (ELT ENTRY 0) OBJ)
                                                 (EQ (ELT ENTRY 1) KEY))))))
                               (CHAIN KEY-BINDINGS
                                      (PUSH (LIST OBJ KEY FN))))))))
     (POP-STACK)) */
function computeDependencies(obj, key, fn) {
    READSTACK.push(STACKDELIMITERSYMBOL);
    var returnValue = fn.call(obj);
    if (returnValue !== undefined) {
        obj[key] = returnValue;
    } else {
        delete obj[key];
    };
    for (var i = READSTACK.length - 1; i >= 0; i -= 1) {
        if (typeof READSTACK[i] === 'symbol') {
            break;
        };
        var tuple = READSTACK[i];
        var observable = tuple[0];
        var observableKey = tuple[1];
        var observableContext = null;
        if (!TARGETOBSERVABLESMAP.has(obj)) {
            TARGETOBSERVABLESMAP.set(obj, []);
        };
        var observables = TARGETOBSERVABLESMAP.get(obj);
        if (!observables.includes(observable)) {
            observables.push(observable);
        };
        if (!OBSERVABLECONTEXTMAP.has(observable)) {
            OBSERVABLECONTEXTMAP.set(observable, {  });
        };
        observableContext = OBSERVABLECONTEXTMAP.get(observable);
        if (!observableContext[observableKey]) {
            observableContext[observableKey] = [];
        };
        var keyBindings = observableContext[observableKey];
        if (!keyBindings.find(function (entry) {
            return entry[0] === obj && entry[1] === key;
        })) {
            keyBindings.push([obj, key, fn]);
        };
    };
    
    return popStack();
};
/* (DEFUN UNMOUNT-OBJECT (OBJ)
     (LET ((OBSERVABLES (CHAIN *TARGET-OBSERVABLES-MAP* (GET OBJ))))
       (WHEN (NOT OBSERVABLES) (RETURN-FROM UNMOUNT-OBJECT))
       (LOOP FOR OBSERVABLE IN OBSERVABLES
             DO (LET ((CONTEXT
                       (CHAIN *OBSERVABLE-CONTEXT-MAP* (GET OBSERVABLE))))
                  (LOOP FOR KEY OF CONTEXT
                        DO (LET ((KEY-BINDINGS (GETPROP CONTEXT KEY)))
                             (LOOP FOR I FROM (- (LENGTH KEY-BINDINGS)
                                                 1) DOWNTO 0
                                   DO (LET* ((KEY-BINDING
                                              (GETPROP KEY-BINDINGS I))
                                             (TARGET (@ KEY-BINDING 0)))
                                        (WHEN (EQ TARGET OBJ)
                                          (CHAIN KEY-BINDINGS
                                                 (SPLICE I 1))))))))))) */
function unmountObject(obj) {
    var observables = TARGETOBSERVABLESMAP.get(obj);
    if (!observables) {
        return;
    };
    var _js8 = observables.length;
    for (var _js7 = 0; _js7 < _js8; _js7 += 1) {
        var observable = observables[_js7];
        var context = OBSERVABLECONTEXTMAP.get(observable);
        for (var key in context) {
            var keyBindings = context[key];
            for (var i = keyBindings.length - 1; i >= 0; i -= 1) {
                var keyBinding = keyBindings[i];
                var target = keyBinding[0];
                if (target === obj) {
                    keyBindings.splice(i, 1);
                };
            };
        };
    };
};
/* (DEFUN CREATE-COMPUTED (MOUNT-SYMBOL UNMOUNT-SYMBOL)
     (DEFUN COMPUTED (OBJ)
       (LET ((MOUNT (GETPROP OBJ MOUNT-SYMBOL))
             (UNMOUNT (GETPROP OBJ UNMOUNT-SYMBOL)))
         (SETF (GETPROP OBJ MOUNT-SYMBOL)
                 (LAMBDA ()
                   (WHEN MOUNT (CHAIN MOUNT (CALL THIS)))
                   (MOUNT-OBJECT THIS))
               (GETPROP OBJ UNMOUNT-SYMBOL)
                 (LAMBDA ()
                   (WHEN UNMOUNT (CHAIN UNMOUNT (CALL THIS)))
                   (UNMOUNT-OBJECT THIS))))
       OBJ)
     COMPUTED) */
function createComputed(mountSymbol, unmountSymbol) {
    function computed(obj) {
        var mount = obj[mountSymbol];
        var unmount = obj[unmountSymbol];
        obj[mountSymbol] = function () {
            if (mount) {
                mount.call(this);
            };
            
            return mountObject(this);
        };
        obj[unmountSymbol] = function () {
            if (unmount) {
                unmount.call(this);
            };
            
            return unmountObject(this);
        };
        return obj;
    };
    return computed;
};
/* (EXPORT NAMES ((CREATE-SOURCE OBSERVABLE) CREATE-SOURCE CREATE-COMPUTED)) */
export { createSource as observable, createSource, createComputed, };

