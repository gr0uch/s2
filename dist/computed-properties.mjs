
/* (DEFPARAMETER *SOURCE-CONTEXT-MAP* (NEW (*WEAK-MAP))) */
var SOURCECONTEXTMAP = new WeakMap();
/* (DEFPARAMETER *TARGET-SOURCES-MAP* (NEW (*WEAK-MAP))) */
var TARGETSOURCESMAP = new WeakMap();
/* (DEFPARAMETER *READ-STACK* (LIST)) */
var READSTACK = [];
/* (DEFPARAMETER *CLEAR-STACK-TIMEOUT* NIL) */
var CLEARSTACKTIMEOUT = null;
/* (DEFPARAMETER *STACK-DELIMITER-SYMBOL* (*SYMBOL 'STACK-DELIMITER)) */
var STACKDELIMITERSYMBOL = Symbol('stackDelimiter');
/* (DEFPARAMETER *PROXY-SOURCE*
     (CREATE GET GET-PROPERTY SET SET-PROPERTY DELETE-PROPERTY SET-PROPERTY)) */
var PROXYSOURCE = { get : getProperty,
                    set : setProperty,
                    deleteProperty : setProperty
                  };
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
     (CHAIN *READ-STACK* (PUSH (LIST TARGET KEY)))
     (WHEN (NOT *CLEAR-STACK-TIMEOUT*)
       (SETF *CLEAR-STACK-TIMEOUT* (SET-TIMEOUT CLEAR-STACK 0)))
     (CHAIN *REFLECT (GET TARGET KEY RECEIVER))) */
function getProperty(target, key, receiver) {
    READSTACK.push([target, key]);
    if (!CLEARSTACKTIMEOUT) {
        CLEARSTACKTIMEOUT = setTimeout(clearStack, 0);
    };
    
    return Reflect.get(target, key, receiver);
};
/* (DEFUN SET-PROPERTY (TARGET KEY VALUE RECEIVER)
     (WHEN (EQ (GETPROP TARGET KEY) VALUE) (RETURN-FROM SET-PROPERTY T))
     (IF (NOT (EQ VALUE UNDEFINED))
         (CHAIN *REFLECT (SET TARGET KEY VALUE RECEIVER))
         (CHAIN *REFLECT (DELETE-PROPERTY TARGET KEY)))
     (LET ((CONTEXT (CHAIN *SOURCE-CONTEXT-MAP* (GET TARGET)))
           (KEY-BINDINGS NIL))
       (WHEN (NOT CONTEXT) (RETURN-FROM SET-PROPERTY T))
       (SETF KEY-BINDINGS (OR (GETPROP CONTEXT KEY) (LIST)))
       (LOOP FOR KEY-BINDING IN KEY-BINDINGS
             DO (LET* ((OBJ (@ KEY-BINDING 0))
                       (OBJ-KEY (@ KEY-BINDING 1))
                       (FN (@ KEY-BINDING 2))
                       (RETURN-VALUE (CHAIN FN (CALL OBJ))))
                  (SETF (GETPROP OBJ OBJ-KEY) RETURN-VALUE))))
     T) */
function setProperty(target, key, value, receiver) {
    if (target[key] === value) {
        return true;
    };
    if (value !== undefined) {
        Reflect.set(target, key, value, receiver);
    } else {
        Reflect.deleteProperty(target, key);
    };
    var context = SOURCECONTEXTMAP.get(target);
    var keyBindings = null;
    if (!context) {
        return true;
    };
    keyBindings = context[key] || [];
    var _js2 = keyBindings.length;
    for (var _js1 = 0; _js1 < _js2; _js1 += 1) {
        var keyBinding = keyBindings[_js1];
        var obj = keyBinding[0];
        var objKey = keyBinding[1];
        var fn = keyBinding[2];
        var returnValue = fn.call(obj);
        obj[objKey] = returnValue;
    };
    return true;
};
/* (DEFUN CREATE-SOURCE (OBJ)
     (LET ((PROXY (NEW (*PROXY OBJ *PROXY-SOURCE*))))
       PROXY)) */
function createSource(obj) {
    var proxy = new Proxy(obj, PROXYSOURCE);
    
    return proxy;
};
/* (DEFUN MOUNT-OBJECT (OBJ)
     (LOOP FOR KEY OF OBJ
           DO (LET* ((VALUE (GETPROP OBJ KEY))
                     (IS-FUNCTION (EQ (TYPEOF VALUE) 'FUNCTION)))
                (WHEN IS-FUNCTION
                  (WHEN (@ VALUE IS-EVENT-LISTENER) (CONTINUE))
                  (CHAIN *READ-STACK* (PUSH *STACK-DELIMITER-SYMBOL*))
                  (LET ((RETURN-VALUE (CHAIN VALUE (CALL OBJ))))
                    (WHEN (NOT (EQ RETURN-VALUE UNDEFINED))
                      (SETF (GETPROP OBJ KEY) RETURN-VALUE))
                    (LOOP FOR I FROM (- (LENGTH *READ-STACK*) 1) DOWNTO 0
                          DO (WHEN
                                 (EQ (TYPEOF (GETPROP *READ-STACK* I)) 'SYMBOL)
                               BREAK) (LET* ((TUPLE (GETPROP *READ-STACK* I))
                                             (SOURCE (@ TUPLE 0))
                                             (SOURCE-KEY (@ TUPLE 1))
                                             (SOURCE-CONTEXT NIL))
                                        (WHEN
                                            (NOT
                                             (CHAIN *TARGET-SOURCES-MAP*
                                                    (HAS OBJ)))
                                          (CHAIN *TARGET-SOURCES-MAP*
                                                 (SET OBJ (LIST))))
                                        (CHAIN *TARGET-SOURCES-MAP* (GET OBJ)
                                               (PUSH SOURCE))
                                        (WHEN
                                            (NOT
                                             (CHAIN *SOURCE-CONTEXT-MAP*
                                                    (HAS SOURCE)))
                                          (CHAIN *SOURCE-CONTEXT-MAP*
                                                 (SET SOURCE (CREATE))))
                                        (SETF SOURCE-CONTEXT
                                                (CHAIN *SOURCE-CONTEXT-MAP*
                                                       (GET SOURCE)))
                                        (WHEN
                                            (NOT
                                             (GETPROP SOURCE-CONTEXT
                                              SOURCE-KEY))
                                          (SETF (GETPROP SOURCE-CONTEXT
                                                 SOURCE-KEY)
                                                  (LIST)))
                                        (LET ((KEY-BINDINGS
                                               (GETPROP SOURCE-CONTEXT
                                                SOURCE-KEY)))
                                          (CHAIN KEY-BINDINGS
                                                 (PUSH
                                                  (LIST OBJ KEY VALUE)))))))
                  (POP-STACK))))) */
function mountObject(obj) {
    for (var key in obj) {
        var value = obj[key];
        var isFunction = typeof value === 'function';
        if (isFunction) {
            if (value.isEventListener) {
                continue;
            };
            READSTACK.push(STACKDELIMITERSYMBOL);
            var returnValue = value.call(obj);
            if (returnValue !== undefined) {
                obj[key] = returnValue;
            };
            for (var i = READSTACK.length - 1; i >= 0; i -= 1) {
                if (typeof READSTACK[i] === 'symbol') {
                    break;
                };
                var tuple = READSTACK[i];
                var source = tuple[0];
                var sourceKey = tuple[1];
                var sourceContext = null;
                if (!TARGETSOURCESMAP.has(obj)) {
                    TARGETSOURCESMAP.set(obj, []);
                };
                TARGETSOURCESMAP.get(obj).push(source);
                if (!SOURCECONTEXTMAP.has(source)) {
                    SOURCECONTEXTMAP.set(source, {  });
                };
                sourceContext = SOURCECONTEXTMAP.get(source);
                if (!sourceContext[sourceKey]) {
                    sourceContext[sourceKey] = [];
                };
                var keyBindings = sourceContext[sourceKey];
                keyBindings.push([obj, key, value]);
            };
            popStack();
        };
    };
};
/* (DEFUN UNMOUNT-OBJECT (OBJ)
     (LET ((SOURCES (CHAIN *TARGET-SOURCES-MAP* (GET OBJ))))
       (WHEN (NOT SOURCES) (RETURN-FROM UNMOUNT-OBJECT))
       (LOOP FOR SOURCE IN SOURCES
             DO (LET ((CONTEXT (CHAIN *SOURCE-CONTEXT-MAP* (GET SOURCE))))
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
    var sources = TARGETSOURCESMAP.get(obj);
    if (!sources) {
        return;
    };
    var _js4 = sources.length;
    for (var _js3 = 0; _js3 < _js4; _js3 += 1) {
        var source = sources[_js3];
        var context = SOURCECONTEXTMAP.get(source);
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
/* (EXPORT NAMES (CREATE-SOURCE CREATE-COMPUTED)) */
export { createSource, createComputed, };

