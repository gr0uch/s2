
/* (DEFPARAMETER *OBSERVABLE-CONTEXT-MAP* (NEW (*WEAK-MAP))) */
var OBSERVABLECONTEXTMAP = new WeakMap();
/* (DEFPARAMETER *TARGET-OBSERVABLES-MAP* (NEW (*WEAK-MAP))) */
var TARGETOBSERVABLESMAP = new WeakMap();
/* (DEFPARAMETER *READ-STACK* (LIST)) */
var READSTACK = [];
/* (DEFPARAMETER *WILL-CLEAR-STACK* FALSE) */
var WILLCLEARSTACK = false;
/* (DEFPARAMETER *STACK-DELIMITER-SYMBOL* (*SYMBOL 'STACK-DELIMITER)) */
var STACKDELIMITERSYMBOL = Symbol('stackDelimiter');
/* (DEFPARAMETER *REF-SYMBOL* (*SYMBOL 'REF)) */
var REFSYMBOL = Symbol('ref');
/* (DEFPARAMETER *HAS-UNMOUNTED-SYMBOL* (*SYMBOL 'HAS-UNMOUNTED)) */
var HASUNMOUNTEDSYMBOL = Symbol('hasUnmounted');
/* (DEFPARAMETER *PROXY-TARGET-SYMBOL* (*SYMBOL 'PROXY-TARGET)) */
var PROXYTARGETSYMBOL = Symbol('proxyTarget');
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
/* (DEFPARAMETER *PROXY-PARTIAL-OBSERVABLE*
     (LET ((SET-PROPERTY (MAKE-SET-PROPERTY T)))
       (CREATE GET GET-PROPERTY SET SET-PROPERTY DELETE-PROPERTY SET-PROPERTY))) */
var PROXYPARTIALOBSERVABLE = (function () {
    var setProperty = makeSetProperty(true);
    
    return { get : getProperty,
             set : setProperty,
             deleteProperty : setProperty
           };
})();
/* (DEFUN CLEAR-STACK ()
     (LOOP WHILE (LENGTH *READ-STACK*)
           DO (CHAIN *READ-STACK* (POP)))
     (SETF *WILL-CLEAR-STACK* FALSE)) */
function clearStack() {
    while (READSTACK.length) {
        READSTACK.pop();
    };
    return WILLCLEARSTACK = false;
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
     (WHEN (EQ KEY *PROXY-TARGET-SYMBOL*) (RETURN-FROM GET-PROPERTY TARGET))
     (LET ((HAS-READ FALSE))
       (LOOP FOR I FROM (- (LENGTH *READ-STACK*) 1) DOWNTO 0
             DO (LET ((TUPLE (ELT *READ-STACK* I)))
                  (WHEN (EQ TUPLE *STACK-DELIMITER-SYMBOL*) (BREAK))
                  (WHEN (AND (EQ (ELT TUPLE 0) TARGET) (EQ (ELT TUPLE 1) KEY))
                    (SETF HAS-READ T))))
       (WHEN (NOT HAS-READ)
         (CHAIN *READ-STACK* (PUSH (LIST TARGET KEY)))
         (WHEN (NOT *WILL-CLEAR-STACK*)
           (SETF *WILL-CLEAR-STACK* T)
           (QUEUE-MICROTASK CLEAR-STACK))))
     (CHAIN *REFLECT (GET TARGET KEY RECEIVER))) */
function getProperty(target, key, receiver) {
    if (key === PROXYTARGETSYMBOL) {
        return target;
    };
    var hasRead = false;
    for (var i = READSTACK.length - 1; i >= 0; i -= 1) {
        var tuple = READSTACK[i];
        if (tuple === STACKDELIMITERSYMBOL) {
            break;
        };
        if (tuple[0] === target && tuple[1] === key) {
            hasRead = true;
        };
    };
    if (!hasRead) {
        READSTACK.push([target, key]);
        if (!WILLCLEARSTACK) {
            WILLCLEARSTACK = true;
            queueMicrotask(clearStack);
        };
    };
    
    return Reflect.get(target, key, receiver);
};
/* (DEFUN IS-OBJECT-PROXYABLE (OBJ)
     (LET ((CTOR (AND OBJ (@ OBJ CONSTRUCTOR))))
       (OR (EQ CTOR *OBJECT) (EQ CTOR *ARRAY)))) */
function isObjectProxyable(obj) {
    var ctor = obj && obj.constructor;
    return ctor === Object || ctor === Array;
};
/* (DEFUN MAKE-SET-PROPERTY (SHOULD-PARTIALLY-REPLACE)
     (DEFUN SET-PROPERTY (TARGET KEY VALUE RECEIVER)
       (LET ((OLD-VALUE (GETPROP TARGET KEY)))
         (WHEN (EQ OLD-VALUE VALUE) (RETURN-FROM SET-PROPERTY T))
         (WHEN
             (AND SHOULD-PARTIALLY-REPLACE (IS-OBJECT-PROXYABLE VALUE)
                  (NOT (GETPROP VALUE *REF-SYMBOL*)))
           (IF (AND (IS-OBJECT-PROXYABLE OLD-VALUE)
                    (NOT (GETPROP OLD-VALUE *REF-SYMBOL*)))
               (PROGN
                (PARTIAL-REPLACE OLD-VALUE VALUE)
                (RETURN-FROM SET-PROPERTY T))
               (SETF VALUE (CREATE-SOURCE VALUE T)))))
       (IF (NOT (EQ VALUE UNDEFINED))
           (CHAIN *REFLECT (SET TARGET KEY VALUE RECEIVER))
           (CHAIN *REFLECT (DELETE-PROPERTY TARGET KEY)))
       (LET ((CONTEXT (CHAIN *OBSERVABLE-CONTEXT-MAP* (GET TARGET)))
             (KEY-BINDINGS NIL))
         (WHEN (NOT CONTEXT) (RETURN-FROM SET-PROPERTY T))
         (SETF KEY-BINDINGS (OR (GETPROP CONTEXT KEY) (LIST)))
         (LOOP FOR I FROM (- (LENGTH KEY-BINDINGS) 1) DOWNTO 0
               DO (LET ((KEY-BINDING (GETPROP KEY-BINDINGS I)))
                    (WHEN (NOT KEY-BINDING) (CONTINUE))
                    (LET* ((OBJ (@ KEY-BINDING 0))
                           (OBJ-KEY (@ KEY-BINDING 1))
                           (FN (@ KEY-BINDING 2)))
                      (WHEN (GETPROP OBJ *HAS-UNMOUNTED-SYMBOL*) (CONTINUE))
                      (COMPUTE-DEPENDENCIES OBJ OBJ-KEY FN)))))
       T)
     SET-PROPERTY) */
function makeSetProperty(shouldPartiallyReplace) {
    function setProperty(target, key, value, receiver) {
        var oldValue = target[key];
        if (oldValue === value) {
            return true;
        };
        if (shouldPartiallyReplace && isObjectProxyable(value) && !value[REFSYMBOL]) {
            if (isObjectProxyable(oldValue) && !oldValue[REFSYMBOL]) {
                partialReplace(oldValue, value);
                
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
        for (var i = keyBindings.length - 1; i >= 0; i -= 1) {
            var keyBinding = keyBindings[i];
            if (!keyBinding) {
                continue;
            };
            var obj = keyBinding[0];
            var objKey = keyBinding[1];
            var fn = keyBinding[2];
            if (obj[HASUNMOUNTEDSYMBOL]) {
                continue;
            };
            computeDependencies(obj, objKey, fn);
        };
        
        return true;
    };
    return setProperty;
};
/* (DEFUN PARTIAL-REPLACE (PROXY OBJ)
     (LET ((OLD-TARGET (GETPROP PROXY *PROXY-TARGET-SYMBOL*)))
       (LOOP FOR KEY OF OBJ
             DO (LET ((VALUE (GETPROP OBJ KEY))
                      (OLD-VALUE (GETPROP OLD-TARGET KEY)))
                  (IF (AND (IS-OBJECT-PROXYABLE VALUE)
                           (IS-OBJECT-PROXYABLE OLD-VALUE)
                           (NOT (GETPROP OLD-VALUE *REF-SYMBOL*)))
                      (PARTIAL-REPLACE OLD-VALUE VALUE)
                      (SETF (GETPROP PROXY KEY) VALUE))))
       (LOOP FOR KEY OF OLD-TARGET
             DO (LET ((OLD-VALUE (GETPROP OLD-TARGET KEY)))
                  (WHEN
                      (NOT
                       (CHAIN *OBJECT PROTOTYPE HAS-OWN-PROPERTY
                              (CALL OBJ KEY)))
                    (DELETE (GETPROP PROXY KEY))))))) */
function partialReplace(proxy, obj) {
    var oldTarget = proxy[PROXYTARGETSYMBOL];
    for (var key in obj) {
        var value = obj[key];
        var oldValue = oldTarget[key];
        if (isObjectProxyable(value) && isObjectProxyable(oldValue) && !oldValue[REFSYMBOL]) {
            partialReplace(oldValue, value);
        } else {
            proxy[key] = value;
        };
    };
    for (var key in oldTarget) {
        var oldValue5 = oldTarget[key];
        if (!Object.prototype.hasOwnProperty.call(obj, key)) {
            delete proxy[key];
        };
    };
};
/* (DEFUN CREATE-SOURCE (OBJ SHOULD-PARTIALLY-REPLACE)
     (WHEN (NOT OBJ) (SETF OBJ (CREATE)))
     (LET* ((PROXY
             (NEW
              (*PROXY OBJ
               (IF SHOULD-PARTIALLY-REPLACE
                   *PROXY-PARTIAL-OBSERVABLE*
                   *PROXY-OBSERVABLE*))))
            (SYMBOLS (CHAIN *OBJECT (GET-OWN-PROPERTY-SYMBOLS OBJ)))
            (MOUNT-FN (GETPROP OBJ (ELT SYMBOLS 0))))
       (WHEN SHOULD-PARTIALLY-REPLACE
         (LOOP FOR KEY OF OBJ
               DO (LET ((VALUE (GETPROP OBJ KEY)))
                    (WHEN
                        (AND (IS-OBJECT-PROXYABLE VALUE)
                             (NOT (GETPROP VALUE *REF-SYMBOL*)))
                      (SETF (GETPROP OBJ KEY) (CREATE-SOURCE VALUE T))))))
       (WHEN (EQ (TYPEOF MOUNT-FN) 'FUNCTION) (CHAIN MOUNT-FN (CALL PROXY)))
       PROXY)) */
function createSource(obj, shouldPartiallyReplace) {
    if (!obj) {
        obj = {  };
    };
    var proxy = new Proxy(obj, shouldPartiallyReplace ? PROXYPARTIALOBSERVABLE : PROXYOBSERVABLE);
    var symbols = Object.getOwnPropertySymbols(obj);
    var mountFn = obj[symbols[0]];
    if (shouldPartiallyReplace) {
        for (var key in obj) {
            var value = obj[key];
            if (isObjectProxyable(value) && !value[REFSYMBOL]) {
                obj[key] = createSource(value, true);
            };
        };
    };
    if (typeof mountFn === 'function') {
        mountFn.call(proxy);
    };
    
    return proxy;
};
/* (DEFUN COMPUTE-DEPENDENCIES (OBJ KEY FN)
     (CHAIN *READ-STACK* (PUSH *STACK-DELIMITER-SYMBOL*))
     (LET ((DELIMITER-INDEX (- (LENGTH *READ-STACK*) 1))
           (UPPER-INDEX NIL)
           (RETURN-VALUE (CHAIN FN (CALL OBJ)))
           (OBSERVABLES (LIST)))
       (IF (NOT (EQ RETURN-VALUE UNDEFINED))
           (SETF (GETPROP OBJ KEY) RETURN-VALUE)
           (DELETE (GETPROP OBJ KEY)))
       (CHAIN *TARGET-OBSERVABLES-MAP* (SET OBJ OBSERVABLES))
       (LOOP FOR I FROM (+ DELIMITER-INDEX 1) TO (- (LENGTH *READ-STACK*) 1)
             DO (WHEN (EQ (GETPROP *READ-STACK* I) *STACK-DELIMITER-SYMBOL*)
                  BREAK) (SETF UPPER-INDEX I))
       (LOOP FOR I FROM UPPER-INDEX DOWNTO (+ DELIMITER-INDEX 1)
             DO (LET* ((TUPLE (GETPROP *READ-STACK* I))
                       (OBSERVABLE (@ TUPLE 0))
                       (OBSERVABLE-KEY (@ TUPLE 1))
                       (CONTEXT NIL))
                  (WHEN (NOT (CHAIN OBSERVABLES (INCLUDES OBSERVABLE)))
                    (CHAIN OBSERVABLES (PUSH OBSERVABLE)))
                  (WHEN (NOT (CHAIN *OBSERVABLE-CONTEXT-MAP* (HAS OBSERVABLE)))
                    (CHAIN *OBSERVABLE-CONTEXT-MAP* (SET OBSERVABLE (CREATE))))
                  (SETF CONTEXT
                          (CHAIN *OBSERVABLE-CONTEXT-MAP* (GET OBSERVABLE)))
                  (WHEN
                      (NOT
                       (CHAIN *OBJECT PROTOTYPE HAS-OWN-PROPERTY
                              (CALL CONTEXT OBSERVABLE-KEY)))
                    (SETF (GETPROP CONTEXT OBSERVABLE-KEY) (LIST)))
                  (LET* ((KEY-BINDINGS (GETPROP CONTEXT OBSERVABLE-KEY))
                         (MATCH-INDEX
                          (CHAIN KEY-BINDINGS
                                 (FIND-INDEX
                                  (LAMBDA (ENTRY)
                                    (AND (EQ (ELT ENTRY 0) OBJ)
                                         (EQ (ELT ENTRY 1) KEY)))))))
                    (WHEN (NOT (EQ MATCH-INDEX -1))
                      (CHAIN KEY-BINDINGS (SPLICE MATCH-INDEX 1)))
                    (CHAIN KEY-BINDINGS (PUSH (LIST OBJ KEY FN))))))
       (POP-STACK)
       RETURN-VALUE)) */
function computeDependencies(obj, key, fn) {
    READSTACK.push(STACKDELIMITERSYMBOL);
    var delimiterIndex = READSTACK.length - 1;
    var upperIndex = null;
    var returnValue = fn.call(obj);
    var observables = [];
    if (returnValue !== undefined) {
        obj[key] = returnValue;
    } else {
        delete obj[key];
    };
    TARGETOBSERVABLESMAP.set(obj, observables);
    var _js5 = READSTACK.length - 1;
    for (var i = delimiterIndex + 1; i <= _js5; i += 1) {
        if (READSTACK[i] === STACKDELIMITERSYMBOL) {
            break;
        };
        upperIndex = i;
    };
    var _js6 = delimiterIndex + 1;
    for (var i = upperIndex; i >= _js6; i -= 1) {
        var tuple = READSTACK[i];
        var observable = tuple[0];
        var observableKey = tuple[1];
        var context = null;
        if (!observables.includes(observable)) {
            observables.push(observable);
        };
        if (!OBSERVABLECONTEXTMAP.has(observable)) {
            OBSERVABLECONTEXTMAP.set(observable, {  });
        };
        context = OBSERVABLECONTEXTMAP.get(observable);
        if (!Object.prototype.hasOwnProperty.call(context, observableKey)) {
            context[observableKey] = [];
        };
        var keyBindings = context[observableKey];
        var matchIndex = keyBindings.findIndex(function (entry) {
            return entry[0] === obj && entry[1] === key;
        });
        if (matchIndex !== -1) {
            keyBindings.splice(matchIndex, 1);
        };
        keyBindings.push([obj, key, fn]);
    };
    popStack();
    
    return returnValue;
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
                                                 (SPLICE I 1))))))))))
     (SETF (GETPROP OBJ *HAS-UNMOUNTED-SYMBOL*) T)) */
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
    return obj[HASUNMOUNTEDSYMBOL] = true;
};
/* (DEFUN CREATE-COMPUTED (MOUNT-SYMBOL UNMOUNT-SYMBOL)
     (DEFUN COMPUTED (OBJ)
       (LET ((MOUNT (GETPROP OBJ MOUNT-SYMBOL))
             (UNMOUNT (GETPROP OBJ UNMOUNT-SYMBOL)))
         (DEFUN COMPUTED-MOUNT ()
           (WHEN MOUNT (CHAIN MOUNT (APPLY THIS ARGUMENTS)))
           (WHEN (NOT (LENGTH ARGUMENTS))
             (LOOP FOR KEY OF OBJ
                   DO (LET ((FN (GETPROP OBJ KEY)))
                        (WHEN (EQ (TYPEOF FN) 'FUNCTION)
                          (CHAIN FN (CALL THIS)))))))
         (DEFUN COMPUTED-UNMOUNT ()
           (WHEN UNMOUNT (CHAIN UNMOUNT (APPLY THIS ARGUMENTS)))
           (UNMOUNT-OBJECT THIS))
         (SETF (GETPROP OBJ MOUNT-SYMBOL) COMPUTED-MOUNT
               (GETPROP OBJ UNMOUNT-SYMBOL) COMPUTED-UNMOUNT))
       (CHAIN *OBJECT (KEYS OBJ)
              (FOR-EACH
               (LAMBDA (KEY)
                 (LET ((FN (GETPROP OBJ KEY)))
                   (WHEN (EQ (TYPEOF FN) 'FUNCTION)
                     (DEFUN COMPUTED-PROPERTY ()
                       (LET ((VALUE (GETPROP THIS KEY)))
                         (IF (AND VALUE (@ VALUE IS-EVENT-LISTENER))
                             (CHAIN FN (APPLY THIS ARGUMENTS))
                             (COMPUTE-DEPENDENCIES THIS KEY FN))))
                     (SETF (GETPROP OBJ KEY) COMPUTED-PROPERTY))))))
       OBJ)
     COMPUTED) */
function createComputed(mountSymbol, unmountSymbol) {
    function computed(obj) {
        var mount = obj[mountSymbol];
        var unmount = obj[unmountSymbol];
        function computedMount() {
            if (mount) {
                mount.apply(this, arguments);
            };
            if (!arguments.length) {
                for (var key in obj) {
                    var fn = obj[key];
                    if (typeof fn === 'function') {
                        fn.call(this);
                    };
                };
            };
        };
        function computedUnmount() {
            if (unmount) {
                unmount.apply(this, arguments);
            };
            
            return unmountObject(this);
        };
        obj[mountSymbol] = computedMount;
        obj[unmountSymbol] = computedUnmount;
        Object.keys(obj).forEach(function (key) {
            var fn = obj[key];
            if (typeof fn === 'function') {
                function computedProperty() {
                    var value = this[key];
                    
                    return value && value.isEventListener ? fn.apply(this, arguments) : computeDependencies(this, key, fn);
                };
                return obj[key] = computedProperty;
            };
        });
        return obj;
    };
    return computed;
};
/* (DEFUN REF (OBJ) (SETF (GETPROP OBJ *REF-SYMBOL*) T) OBJ) */
function ref(obj) {
    obj[REFSYMBOL] = true;
    return obj;
};
/* (EXPORT NAMES ((CREATE-SOURCE OBSERVABLE) CREATE-SOURCE CREATE-COMPUTED REF)) */
export { createSource as observable, createSource, createComputed, ref, };

