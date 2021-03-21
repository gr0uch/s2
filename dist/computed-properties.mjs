var __PS_MV_REG;
/* (DEFVAR *SOURCE-CONTEXT-MAP* (NEW (*WEAK-MAP))) */
if ('undefined' === typeof SOURCECONTEXTMAP) {
    var SOURCECONTEXTMAP = new WeakMap();
};
/* (DEFVAR *TARGET-SOURCES-MAP* (NEW (*WEAK-MAP))) */
if ('undefined' === typeof TARGETSOURCESMAP) {
    var TARGETSOURCESMAP = new WeakMap();
};
/* (DEFVAR *READ-STACK* (LIST)) */
if ('undefined' === typeof READSTACK) {
    var READSTACK = [];
};
/* (DEFVAR *PROXY-SOURCE*
     (CREATE GET GET-PROPERTY SET SET-PROPERTY DELETE-PROPERTY SET-PROPERTY)) */
if ('undefined' === typeof PROXYSOURCE) {
    var PROXYSOURCE = { get : getProperty,
                        set : setProperty,
                        deleteProperty : setProperty
                      };
};
/* (DEFUN GET-PROPERTY (TARGET KEY RECEIVER)
     (CHAIN *READ-STACK* (PUSH (LIST RECEIVER KEY)))
     (CHAIN *REFLECT (GET TARGET KEY RECEIVER))) */
function getProperty(target, key, receiver) {
    READSTACK.push([receiver, key]);
    return Reflect.get(target, key, receiver);
};
/* (DEFUN SET-PROPERTY (TARGET KEY VALUE RECEIVER)
     (CHAIN *REFLECT (SET TARGET KEY VALUE RECEIVER))
     (LET ((CONTEXT (CHAIN *SOURCE-CONTEXT-MAP* (GET RECEIVER)))
           (KEY-BINDINGS NIL))
       (WHEN (NOT CONTEXT) (RETURN-FROM SET-PROPERTY T))
       (SETF KEY-BINDINGS (OR (GETPROP CONTEXT KEY) (LIST)))
       (LOOP FOR KEY-BINDING IN KEY-BINDINGS
             DO (LET ((OBJ (@ KEY-BINDING 0))
                      (OBJ-KEY (@ KEY-BINDING 1))
                      (FN (@ KEY-BINDING 2)))
                  (SETF (GETPROP OBJ OBJ-KEY) (FN))
                  (LOOP WHILE (LENGTH *READ-STACK*)
                        DO (CHAIN *READ-STACK* (SHIFT))))))
     T) */
function setProperty(target, key, value, receiver) {
    Reflect.set(target, key, value, receiver);
    var context = SOURCECONTEXTMAP.get(receiver);
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
        obj[objKey] = fn();
        while (READSTACK.length) {
            READSTACK.shift();
        };
    };
    __PS_MV_REG = [];
    return true;
};
/* (DEFUN CREATE-CONTEXT (OBJ)
     (LET ((PROXY (NEW (*PROXY OBJ *PROXY-SOURCE*))))
       PROXY)) */
function createContext(obj) {
    var proxy = new Proxy(obj, PROXYSOURCE);
    __PS_MV_REG = [];
    return proxy;
};
/* (DEFUN MOUNT-OBJECT (OBJ)
     (LOOP FOR KEY OF OBJ
           DO (LET* ((VALUE (GETPROP OBJ KEY))
                     (IS-FUNCTION (EQ (TYPEOF VALUE) 'FUNCTION)))
                (WHEN IS-FUNCTION
                  (LOOP WHILE (LENGTH *READ-STACK*)
                        DO (CHAIN *READ-STACK* (SHIFT)))
                  (LET ((RETURN-VALUE (VALUE)))
                    (WHEN (NOT (EQ RETURN-VALUE UNDEFINED))
                      (SETF (GETPROP OBJ KEY) RETURN-VALUE))
                    (LOOP FOR TUPLE IN *READ-STACK*
                          DO (LET ((SOURCE (@ TUPLE 0))
                                   (SOURCE-KEY (@ TUPLE 1))
                                   (SOURCE-CONTEXT NIL))
                               (WHEN
                                   (NOT (CHAIN *TARGET-SOURCES-MAP* (HAS OBJ)))
                                 (CHAIN *TARGET-SOURCES-MAP* (SET OBJ (LIST))))
                               (CHAIN *TARGET-SOURCES-MAP* (GET OBJ)
                                      (PUSH SOURCE))
                               (WHEN
                                   (NOT
                                    (CHAIN *SOURCE-CONTEXT-MAP* (HAS SOURCE)))
                                 (CHAIN *SOURCE-CONTEXT-MAP*
                                        (SET SOURCE (CREATE))))
                               (SETF SOURCE-CONTEXT
                                       (CHAIN *SOURCE-CONTEXT-MAP*
                                              (GET SOURCE)))
                               (WHEN (NOT (GETPROP SOURCE-CONTEXT SOURCE-KEY))
                                 (SETF (GETPROP SOURCE-CONTEXT SOURCE-KEY)
                                         (LIST)))
                               (LET ((KEY-BINDINGS
                                      (GETPROP SOURCE-CONTEXT SOURCE-KEY)))
                                 (CHAIN KEY-BINDINGS
                                        (PUSH (LIST OBJ KEY VALUE))))))))))) */
function mountObject(obj) {
    for (var key in obj) {
        var value = obj[key];
        var isFunction = typeof value === 'function';
        if (isFunction) {
            while (READSTACK.length) {
                READSTACK.shift();
            };
            var returnValue = value();
            if (returnValue !== undefined) {
                obj[key] = returnValue;
            };
            var _js4 = READSTACK.length;
            for (var _js3 = 0; _js3 < _js4; _js3 += 1) {
                var tuple = READSTACK[_js3];
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
        };
    };
};
/* (DEFUN UNMOUNT-OBJECT (OBJ)
     (LET ((SOURCES (CHAIN *TARGET-SOURCES-MAP* (GET OBJ))))
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
            __PS_MV_REG = [];
            return mountObject(this);
        };
        obj[unmountSymbol] = function () {
            if (unmount) {
                unmount.call(this);
            };
            __PS_MV_REG = [];
            return unmountObject(this);
        };
        return obj;
    };
    return computed;
};
/* (EXPORT NAMES (CREATE-CONTEXT CREATE-COMPUTED)) */
export { createContext, createComputed, };
