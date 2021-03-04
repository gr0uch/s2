var __PS_MV_REG;
/* (DEFVAR *SYMBOL-SLOT* (*SYMBOL 'SLOT)) */
if ('undefined' === typeof SYMBOLSLOT) {
    var SYMBOLSLOT = Symbol('slot');
};
/* (DEFVAR *SYMBOL-TEXT* (*SYMBOL 'TEXT)) */
if ('undefined' === typeof SYMBOLTEXT) {
    var SYMBOLTEXT = Symbol('text');
};
/* (DEFVAR *SYMBOL-HTML* (*SYMBOL 'HTML)) */
if ('undefined' === typeof SYMBOLHTML) {
    var SYMBOLHTML = Symbol('html');
};
/* (DEFVAR *SYMBOL-VALUE* (*SYMBOL 'VALUE)) */
if ('undefined' === typeof SYMBOLVALUE) {
    var SYMBOLVALUE = Symbol('value');
};
/* (DEFVAR *SYMBOL-CLASS* (*SYMBOL 'CLASS)) */
if ('undefined' === typeof SYMBOLCLASS) {
    var SYMBOLCLASS = Symbol('class');
};
/* (DEFVAR *SYMBOL-ATTRIBUTE* (*SYMBOL 'ATTRIBUTE)) */
if ('undefined' === typeof SYMBOLATTRIBUTE) {
    var SYMBOLATTRIBUTE = Symbol('attribute');
};
/* (DEFVAR *SYMBOL-EVENT* (*SYMBOL 'EVENT)) */
if ('undefined' === typeof SYMBOLEVENT) {
    var SYMBOLEVENT = Symbol('event');
};
/* (DEFVAR *SYMBOL-MOUNT* (*SYMBOL 'MOUNT)) */
if ('undefined' === typeof SYMBOLMOUNT) {
    var SYMBOLMOUNT = Symbol('mount');
};
/* (DEFVAR *SYMBOL-UNMOUNT* (*SYMBOL 'UNMOUNT)) */
if ('undefined' === typeof SYMBOLUNMOUNT) {
    var SYMBOLUNMOUNT = Symbol('unmount');
};
/* (DEFVAR *TAG-SLOT* '*SLOT*) */
if ('undefined' === typeof TAGSLOT) {
    var TAGSLOT = 'SLOT';
};
/* (DEFMACRO CONSOLE-LOG (&BODY FORMS) `(CHAIN CONSOLE (LOG ,@FORMS))) */

/* (DEFVAR *TARGET-CONTEXT-MAP* (NEW (*WEAK-MAP))) */
if ('undefined' === typeof TARGETCONTEXTMAP) {
    var TARGETCONTEXTMAP = new WeakMap();
};
/* (DEFVAR *TARGET-EVENT-MAP* (NEW (*WEAK-MAP))) */
if ('undefined' === typeof TARGETEVENTMAP) {
    var TARGETEVENTMAP = new WeakMap();
};
/* (DEFVAR *TARGET-DELIMITER-MAP* (NEW (*WEAK-MAP))) */
if ('undefined' === typeof TARGETDELIMITERMAP) {
    var TARGETDELIMITERMAP = new WeakMap();
};
/* (DEFVAR *PROXY-UNMOUNT-MAP* (NEW (*WEAK-MAP))) */
if ('undefined' === typeof PROXYUNMOUNTMAP) {
    var PROXYUNMOUNTMAP = new WeakMap();
};
/* (DEFVAR *PROXY-DELIMITER-MAP* (NEW (*WEAK-MAP))) */
if ('undefined' === typeof PROXYDELIMITERMAP) {
    var PROXYDELIMITERMAP = new WeakMap();
};
/* (DEFVAR *PROXY-TEMPLATE-MAP* (NEW (*WEAK-MAP))) */
if ('undefined' === typeof PROXYTEMPLATEMAP) {
    var PROXYTEMPLATEMAP = new WeakMap();
};
/* (DEFVAR *PROXY-ANCHOR-MAP* (NEW (*WEAK-MAP))) */
if ('undefined' === typeof PROXYANCHORMAP) {
    var PROXYANCHORMAP = new WeakMap();
};
/* (DEFVAR *TEMPLATE-PROCESSED-MAP* (NEW (*WEAK-MAP))) */
if ('undefined' === typeof TEMPLATEPROCESSEDMAP) {
    var TEMPLATEPROCESSEDMAP = new WeakMap();
};
/* (DEFVAR *TEMPLATE-CONTEXT-MAP* (NEW (*WEAK-MAP))) */
if ('undefined' === typeof TEMPLATECONTEXTMAP) {
    var TEMPLATECONTEXTMAP = new WeakMap();
};
/* (DEFVAR *PROXY-OBJECT*
     (CREATE SET SET-PROPERTY DELETE-PROPERTY SET-PROPERTY)) */
if ('undefined' === typeof PROXYOBJECT) {
    var PROXYOBJECT = { set : setProperty, deleteProperty : setProperty };
};
/* (DEFVAR *PROXY-ARRAY* (CREATE SET SET-INDEX DELETE-PROPERTY SET-INDEX)) */
if ('undefined' === typeof PROXYARRAY) {
    var PROXYARRAY = { set : setIndex, deleteProperty : setIndex };
};
/* (DEFUN SET-INDEX (TARGET KEY VALUE RECEIVER)
     (WHEN (@ MAIN DEBUG) (CONSOLE-LOG 'SET-INDEX ARGUMENTS))
     (LET* ((NUMKEY (CHAIN *NUMBER (PARSE-INT KEY 10)))
            (IS-INDEX (NOT (CHAIN *NUMBER (IS-NA-N NUMKEY))))
            (IS-SETTER (EQ (LENGTH ARGUMENTS) 4))
            (IS-DELETE (EQ (LENGTH ARGUMENTS) 2)))
       (WHEN (EQ KEY 'LENGTH)
         (LOOP FOR I FROM VALUE TO (- (LENGTH TARGET) 1)
               DO (LET* ((PROXY (GETPROP TARGET I))
                         (NODES (CHAIN *PROXY-DELIMITER-MAP* (GET PROXY)))
                         (UNMOUNT (CHAIN *PROXY-UNMOUNT-MAP* (GET PROXY))))
                    (WHEN NODES
                      (REMOVE-BETWEEN-DELIMITERS (@ NODES 0) (@ NODES 1)
                       UNMOUNT PROXY))) (DELETE (GETPROP TARGET I))))
       (WHEN (AND IS-DELETE (GETPROP TARGET KEY))
         (LET* ((PROXY (GETPROP TARGET KEY))
                (NODES (CHAIN *PROXY-DELIMITER-MAP* (GET PROXY)))
                (UNMOUNT (CHAIN *PROXY-UNMOUNT-MAP* (GET PROXY))))
           (REMOVE-BETWEEN-DELIMITERS (@ NODES 0) (@ NODES 1) UNMOUNT PROXY))
         (DELETE (GETPROP TARGET KEY)))
       (WHEN (AND IS-SETTER (NOT IS-INDEX))
         (RETURN-FROM SET-INDEX
           (CHAIN *REFLECT (SET TARGET KEY VALUE RECEIVER))))
       (WHEN IS-SETTER
         (IF (NOT (CHAIN TARGET (INCLUDES VALUE)))
             (LET* ((ANCHOR (CHAIN *PROXY-ANCHOR-MAP* (GET RECEIVER)))
                    (PARENT-NODE (@ ANCHOR PARENT-NODE))
                    (TEMPLATE (CHAIN *PROXY-TEMPLATE-MAP* (GET RECEIVER)))
                    (RESULT (CREATE-BINDING VALUE TEMPLATE))
                    (PROXY (@ RESULT 0))
                    (NODE (@ RESULT 1))
                    (PREVIOUS-PROXY (GETPROP TARGET KEY))
                    (NEXT-PROXY NIL))
               (LOOP FOR I FROM (+ NUMKEY 1) TO (- (LENGTH TARGET) 1)
                     DO (SETF NEXT-PROXY (GETPROP TARGET I)) (WHEN NEXT-PROXY
                                                               (BREAK)))
               (WHEN PREVIOUS-PROXY
                 (WHEN
                     (NOT
                      (CHAIN TARGET
                             (FIND
                              (LAMBDA (P I)
                                (AND (EQ P PREVIOUS-PROXY)
                                     (NOT (EQ I NUMKEY)))))))
                   (LET ((NODES
                          (CHAIN *PROXY-DELIMITER-MAP* (GET PREVIOUS-PROXY)))
                         (UNMOUNT
                          (CHAIN *PROXY-UNMOUNT-MAP* (GET PREVIOUS-PROXY))))
                     (REMOVE-BETWEEN-DELIMITERS (@ NODES 0) (@ NODES 1) UNMOUNT
                      PROXY))))
               (IF NEXT-PROXY
                   (LET ((NEXT-ANCHOR
                          (@ (CHAIN *PROXY-DELIMITER-MAP* (GET NEXT-PROXY)) 0)))
                     (CHAIN PARENT-NODE (INSERT-BEFORE NODE NEXT-ANCHOR)))
                   (LET ((END-NODE
                          (@ (CHAIN *PROXY-DELIMITER-MAP* (GET RECEIVER)) 1)))
                     (CHAIN PARENT-NODE (INSERT-BEFORE NODE END-NODE))))
               (RETURN-FROM SET-INDEX
                 (CHAIN *REFLECT (SET TARGET KEY PROXY RECEIVER))))
             (LET* ((OTHER-INDEX
                     (CHAIN TARGET
                            (FIND-INDEX
                             (LAMBDA (P I)
                               (AND (EQ P VALUE) (NOT (EQ I NUMKEY)))))))
                    (OTHER-PROXY (GETPROP TARGET KEY)))
               (WHEN (AND OTHER-PROXY (NOT (EQ VALUE OTHER-PROXY)))
                 (LET* ((OTHER-NODES
                         (CHAIN *PROXY-DELIMITER-MAP* (GET OTHER-PROXY)))
                        (OTHER-START-NODE (@ OTHER-NODES 0))
                        (OTHER-END-NODE (@ OTHER-NODES 1))
                        (NODES (CHAIN *PROXY-DELIMITER-MAP* (GET VALUE)))
                        (START-NODE (@ NODES 0))
                        (END-NODE (@ NODES 1))
                        (ANCHOR (@ NODES 1 NEXT-SIBLING))
                        (PARENT-NODE (@ ANCHOR PARENT-NODE)))
                   (LET ((NODE START-NODE))
                     (LOOP WHILE (NOT (CHAIN NODE (IS-SAME-NODE END-NODE)))
                           DO (LET ((OLD-NODE NODE))
                                (SETF NODE (@ NODE NEXT-SIBLING))
                                (CHAIN PARENT-NODE
                                       (INSERT-BEFORE OLD-NODE
                                        OTHER-START-NODE))))
                     (CHAIN PARENT-NODE
                            (INSERT-BEFORE END-NODE OTHER-START-NODE)))
                   (LET ((NODE OTHER-START-NODE))
                     (LOOP WHILE (NOT
                                  (CHAIN NODE (IS-SAME-NODE OTHER-END-NODE)))
                           DO (LET ((OLD-NODE NODE))
                                (SETF NODE (@ NODE NEXT-SIBLING))
                                (CHAIN PARENT-NODE
                                       (INSERT-BEFORE OLD-NODE ANCHOR))))
                     (CHAIN PARENT-NODE
                            (INSERT-BEFORE OTHER-END-NODE ANCHOR)))))
               (WHEN (~ OTHER-INDEX)
                 (CHAIN *REFLECT
                        (SET TARGET OTHER-INDEX OTHER-PROXY RECEIVER)))))
         (CHAIN *REFLECT (SET TARGET KEY VALUE RECEIVER))))
     T) */
function setIndex(target, key, value, receiver) {
    if (main.debug) {
        console.log('setIndex', arguments);
    };
    var numkey = Number.parseInt(key, 10);
    var isIndex = !Number.isNaN(numkey);
    var isSetter = arguments.length === 4;
    var isDelete = arguments.length === 2;
    if (key === 'length') {
        var _js1 = target.length - 1;
        for (var i = value; i <= _js1; i += 1) {
            var proxy = target[i];
            var nodes = PROXYDELIMITERMAP.get(proxy);
            var unmount = PROXYUNMOUNTMAP.get(proxy);
            if (nodes) {
                removeBetweenDelimiters(nodes[0], nodes[1], unmount, proxy);
            };
            delete target[i];
        };
    };
    if (isDelete && target[key]) {
        var proxy2 = target[key];
        var nodes3 = PROXYDELIMITERMAP.get(proxy2);
        var unmount4 = PROXYUNMOUNTMAP.get(proxy2);
        removeBetweenDelimiters(nodes3[0], nodes3[1], unmount4, proxy2);
        delete target[key];
    };
    if (isSetter && !isIndex) {
        __PS_MV_REG = [];
        return Reflect.set(target, key, value, receiver);
    };
    if (isSetter) {
        if (!target.includes(value)) {
            var anchor = PROXYANCHORMAP.get(receiver);
            var parentNode5 = anchor.parentNode;
            var template = PROXYTEMPLATEMAP.get(receiver);
            var result = createBinding(value, template);
            var proxy6 = result[0];
            var node = result[1];
            var previousProxy = target[key];
            var nextProxy = null;
            var _js7 = target.length - 1;
            for (var i = numkey + 1; i <= _js7; i += 1) {
                nextProxy = target[i];
                if (nextProxy) {
                    break;
                };
            };
            if (previousProxy) {
                if (!target.find(function (p, i) {
                    return p === previousProxy && i !== numkey;
                })) {
                    var nodes8 = PROXYDELIMITERMAP.get(previousProxy);
                    var unmount9 = PROXYUNMOUNTMAP.get(previousProxy);
                    removeBetweenDelimiters(nodes8[0], nodes8[1], unmount9, proxy6);
                };
            };
            if (nextProxy) {
                var nextAnchor = PROXYDELIMITERMAP.get(nextProxy)[0];
                parentNode5.insertBefore(node, nextAnchor);
            } else {
                var endNode = PROXYDELIMITERMAP.get(receiver)[1];
                parentNode5.insertBefore(node, endNode);
            };
            __PS_MV_REG = [];
            return Reflect.set(target, key, proxy6, receiver);
        } else {
            var otherIndex = target.findIndex(function (p, i) {
                return p === value && i !== numkey;
            });
            var otherProxy = target[key];
            if (otherProxy && value !== otherProxy) {
                var otherNodes = PROXYDELIMITERMAP.get(otherProxy);
                var otherStartNode = otherNodes[0];
                var otherEndNode = otherNodes[1];
                var nodes10 = PROXYDELIMITERMAP.get(value);
                var startNode = nodes10[0];
                var endNode11 = nodes10[1];
                var anchor12 = nodes10[1].nextSibling;
                var parentNode13 = anchor12.parentNode;
                var node14 = startNode;
                while (!node14.isSameNode(endNode11)) {
                    var oldNode = node14;
                    node14 = node14.nextSibling;
                    parentNode13.insertBefore(oldNode, otherStartNode);
                };
                parentNode13.insertBefore(endNode11, otherStartNode);
                var node15 = otherStartNode;
                while (!node15.isSameNode(otherEndNode)) {
                    var oldNode16 = node15;
                    node15 = node15.nextSibling;
                    parentNode13.insertBefore(oldNode16, anchor12);
                };
                parentNode13.insertBefore(otherEndNode, anchor12);
            };
            if (~(otherIndex)) {
                Reflect.set(target, otherIndex, otherProxy, receiver);
            };
        };
        Reflect.set(target, key, value, receiver);
    };
    __PS_MV_REG = [];
    return true;
};
/* (DEFUN SET-PROPERTY (TARGET KEY VALUE RECEIVER)
     (LET* ((CONTEXT (CHAIN *TARGET-CONTEXT-MAP* (GET TARGET)))
            (IS-SETTER (EQ (LENGTH ARGUMENTS) 4))
            (IS-DELETE (EQ (LENGTH ARGUMENTS) 2))
            (IS-CHANGED (NOT (EQ (GETPROP TARGET KEY) VALUE)))
            (DESCRIPTOR (GETPROP CONTEXT KEY))
            (NODE (AND DESCRIPTOR (@ DESCRIPTOR NODE)))
            (TYPE (AND DESCRIPTOR (@ DESCRIPTOR TYPE)))
            (NORMALIZED-VALUE
             (IF (OR (EQ VALUE UNDEFINED) (EQ VALUE NIL))

                 VALUE)))
       (WHEN
           (AND (CHAIN *OBJECT PROTOTYPE HAS-OWN-PROPERTY (CALL CONTEXT KEY))
                IS-CHANGED)
         (WHEN
             (AND (EQ TYPE *SYMBOL-TEXT*)
                  (NOT (EQ VALUE (@ NODE TEXT-CONTENT))))
           (SETF (@ NODE TEXT-CONTENT) NORMALIZED-VALUE))
         (WHEN
             (AND (EQ TYPE *SYMBOL-HTML*)
                  (NOT (EQ VALUE (@ NODE INNER-H-T-M-L))))
           (SETF (@ NODE INNER-H-T-M-L) NORMALIZED-VALUE))
         (WHEN (AND (EQ TYPE *SYMBOL-VALUE*) (NOT (EQ VALUE (@ NODE VALUE))))
           (SETF (@ NODE VALUE) NORMALIZED-VALUE))
         (WHEN (EQ TYPE *SYMBOL-CLASS*) (SET-CLASS NODE VALUE))
         (WHEN (EQ TYPE *SYMBOL-ATTRIBUTE*)
           (SET-ATTRIBUTE NODE (@ DESCRIPTOR NAME) VALUE))
         (WHEN (EQ TYPE *SYMBOL-EVENT*)
           (SET-EVENT TARGET VALUE DESCRIPTOR RECEIVER))
         (WHEN (EQ TYPE *SYMBOL-SLOT*)
           (LET ((PROXY (SET-SLOT TARGET KEY VALUE DESCRIPTOR)))
             (WHEN PROXY
               (RETURN-FROM SET-PROPERTY
                 (CHAIN *REFLECT (SET TARGET KEY PROXY RECEIVER)))))))
       (WHEN (AND (EQ TYPE *SYMBOL-VALUE*) (NOT (@ DESCRIPTOR IS-LISTENING)))
         (CHAIN NODE
                (ADD-EVENT-LISTENER input
                 (LAMBDA (EVENT)
                   (CHAIN *REFLECT
                          (SET TARGET KEY (@ EVENT TARGET VALUE) RECEIVER)))))
         (SETF (@ DESCRIPTOR IS-LISTENING) T))
       (WHEN IS-DELETE (DELETE (GETPROP TARGET KEY)))
       (WHEN IS-SETTER (CHAIN *REFLECT (SET TARGET KEY VALUE RECEIVER))))
     T) */
function setProperty(target, key, value, receiver) {
    var context = TARGETCONTEXTMAP.get(target);
    var isSetter = arguments.length === 4;
    var isDelete = arguments.length === 2;
    var isChanged = target[key] !== value;
    var descriptor = context[key];
    var node16 = descriptor && descriptor.node;
    var type17 = descriptor && descriptor.type;
    var normalizedValue = value === undefined || value === null ? '' : value;
    if (Object.prototype.hasOwnProperty.call(context, key) && isChanged) {
        if (type17 === SYMBOLTEXT && value !== node16.textContent) {
            node16.textContent = normalizedValue;
        };
        if (type17 === SYMBOLHTML && value !== node16.innerHTML) {
            node16.innerHTML = normalizedValue;
        };
        if (type17 === SYMBOLVALUE && value !== node16.value) {
            node16.value = normalizedValue;
        };
        if (type17 === SYMBOLCLASS) {
            setClass(node16, value);
        };
        if (type17 === SYMBOLATTRIBUTE) {
            setAttribute(node16, descriptor.name, value);
        };
        if (type17 === SYMBOLEVENT) {
            setEvent(target, value, descriptor, receiver);
        };
        if (type17 === SYMBOLSLOT) {
            var proxy = setSlot(target, key, value, descriptor);
            if (proxy) {
                __PS_MV_REG = [];
                return Reflect.set(target, key, proxy, receiver);
            };
        };
    };
    if (type17 === SYMBOLVALUE && !descriptor.isListening) {
        node16.addEventListener('input', function (event) {
            return Reflect.set(target, key, event.target.value, receiver);
        });
        descriptor.isListening = true;
    };
    if (isDelete) {
        delete target[key];
    };
    if (isSetter) {
        Reflect.set(target, key, value, receiver);
    };
    __PS_MV_REG = [];
    return true;
};
/* (DEFUN SET-ATTRIBUTE (NODE NAME VALUE)
     (IF (NOT (OR (EQ VALUE NIL) (EQ VALUE UNDEFINED)))
         (CHAIN NODE (SET-ATTRIBUTE NAME VALUE))
         (CHAIN NODE (REMOVE-ATTRIBUTE NAME)))) */
function setAttribute(node, name, value) {
    return !(value === null || value === undefined) ? node.setAttribute(name, value) : node.removeAttribute(name);
};
/* (DEFUN SET-CLASS (NODE VALUE)
     (IF VALUE
         (LET* ((CLASS-LIST (@ NODE CLASS-LIST))
                (PROTOTYPE (CHAIN *OBJECT (GET-PROTOTYPE-OF VALUE)))
                (ARRAY
                 (IF (EQ PROTOTYPE (@ *SET PROTOTYPE))
                     (CHAIN VALUE (VALUES))
                     (IF (EQ PROTOTYPE (@ *ARRAY PROTOTYPE))
                         VALUE
                         (IF (EQ PROTOTYPE (@ *STRING PROTOTYPE))
                             (CHAIN VALUE (SPLIT  ))
                             (LIST))))))
           (LOOP FOR CLS IN (CHAIN CLASS-LIST (VALUES))
                 DO (WHEN (NOT (CHAIN ARRAY (INCLUDES CLS)))
                      (CHAIN CLASS-LIST (REMOVE CLS))))
           (LOOP FOR CLS IN ARRAY
                 DO (WHEN (NOT (CHAIN CLASS-LIST (CONTAINS CLS)))
                      (CHAIN CLASS-LIST (ADD CLS)))))
         (CHAIN NODE (REMOVE-ATTRIBUTE 'CLASS)))
     NIL) */
function setClass(node, value) {
    if (value) {
        var classList18 = node.classList;
        var prototype = Object.getPrototypeOf(value);
        var array = prototype === Set.prototype ? value.values() : (prototype === Array.prototype ? value : (prototype === String.prototype ? value.split(' ') : []));
        var _js19 = classList18.values();
        var _js21 = _js19.length;
        for (var _js20 = 0; _js20 < _js21; _js20 += 1) {
            var cls = _js19[_js20];
            if (!array.includes(cls)) {
                classList18.remove(cls);
            };
        };
        var _js23 = array.length;
        for (var _js22 = 0; _js22 < _js23; _js22 += 1) {
            var cls24 = array[_js22];
            if (!classList18.contains(cls24)) {
                classList18.add(cls24);
            };
        };
    } else {
        node.removeAttribute('class');
    };
    return null;
};
/* (DEFUN REMOVE-BETWEEN-DELIMITERS (START-NODE END-NODE UNMOUNT SELF)
     (LET* ((NODE START-NODE) (FIRST-NODE NODE))
       (SETF NODE (@ NODE NEXT-SIBLING))
       (CHAIN FIRST-NODE (REMOVE))
       (LOOP WHILE (NOT (CHAIN NODE (IS-SAME-NODE END-NODE)))
             DO (LET ((OLD-NODE NODE))
                  (SETF NODE (@ NODE NEXT-SIBLING))
                  (IF UNMOUNT
                      (CHAIN *PROMISE
                             (RESOLVE (CHAIN UNMOUNT (CALL SELF OLD-NODE)))
                             (THEN (LAMBDA () (CHAIN OLD-NODE (REMOVE)))))
                      (CHAIN OLD-NODE (REMOVE)))))
       (CHAIN END-NODE (REMOVE)))) */
function removeBetweenDelimiters(startNode, endNode, unmount, self) {
    var node = startNode;
    var firstNode = node;
    node = node.nextSibling;
    firstNode.remove();
    while (!node.isSameNode(endNode)) {
        (function () {
            var oldNode = node;
            node = node.nextSibling;
            return unmount ? Promise.resolve(unmount.call(self, oldNode)).then(function () {
                return oldNode.remove();
            }) : oldNode.remove();
        })();
    };
    __PS_MV_REG = [];
    return endNode.remove();
};
/* (DEFUN SET-SLOT (TARGET KEY VALUE DESCRIPTOR)
     (WHEN (NOT (OR (GETPROP TARGET KEY) VALUE)) (RETURN-FROM SET-SLOT))
     (LET* ((ANCHOR (@ DESCRIPTOR NODE))
            (SLOT (@ DESCRIPTOR SLOT))
            (TEMPLATE (@ DESCRIPTOR TEMPLATE))
            (HASH (CHAIN *TARGET-DELIMITER-MAP* (GET TARGET)))
            (NODES (GETPROP HASH KEY))
            (PARENT-NODE (@ ANCHOR PARENT-NODE))
            (START-NODE (CREATE-ANCHOR 0 KEY))
            (END-NODE (CREATE-ANCHOR 1 KEY))
            (PREVIOUS-VALUE (GETPROP TARGET KEY))
            (IS-PREVIOUS-ARRAY (CHAIN *ARRAY (IS-ARRAY PREVIOUS-VALUE)))
            (IS-VALUE-ARRAY (CHAIN *ARRAY (IS-ARRAY VALUE)))
            (IS-TYPE-MISMATCH (NOT (EQ IS-PREVIOUS-ARRAY IS-VALUE-ARRAY)))
            (RETURN-VALUE NIL))
       (WHEN
           (AND NODES
                (OR (NOT VALUE) (AND VALUE (NOT PREVIOUS-VALUE))
                    IS-TYPE-MISMATCH))
         (IF (CHAIN *ARRAY (IS-ARRAY PREVIOUS-VALUE))
             (LOOP FOR PROXY IN PREVIOUS-VALUE
                   DO (LET ((UNMOUNT (CHAIN *PROXY-UNMOUNT-MAP* (GET PROXY)))
                            (NODES (CHAIN *PROXY-DELIMITER-MAP* (GET PROXY))))
                        (WHEN NODES
                          (REMOVE-BETWEEN-DELIMITERS (@ NODES 0) (@ NODES 1)
                           UNMOUNT PROXY))))
             (WHEN PREVIOUS-VALUE
               (LET ((UNMOUNT (CHAIN *PROXY-UNMOUNT-MAP* (GET PREVIOUS-VALUE)))
                     (NODES (CHAIN *PROXY-DELIMITER-MAP* (GET PREVIOUS-VALUE))))
                 (REMOVE-BETWEEN-DELIMITERS (@ NODES 0) (@ NODES 1) UNMOUNT
                  PREVIOUS-VALUE))))
         (REMOVE-BETWEEN-DELIMITERS (@ NODES 0) (@ NODES 1))
         (DELETE (GETPROP HASH KEY)))
       (SETF (GETPROP HASH KEY) (LIST START-NODE END-NODE))
       (CHAIN PARENT-NODE (INSERT-BEFORE START-NODE ANCHOR))
       (IF VALUE
           (IF (OR (NOT PREVIOUS-VALUE) IS-TYPE-MISMATCH)
               (IF (CHAIN *ARRAY (IS-ARRAY VALUE))
                   (LET* ((RESULT (CREATE-ARRAY VALUE TEMPLATE))
                          (NODES (@ RESULT 0))
                          (PROXY (@ RESULT 1)))
                     (LOOP FOR NODE IN NODES
                           DO (CHAIN PARENT-NODE (INSERT-BEFORE NODE ANCHOR)))
                     (CHAIN *PROXY-ANCHOR-MAP* (SET PROXY ANCHOR))
                     (CHAIN *PROXY-DELIMITER-MAP*
                            (SET PROXY (GETPROP HASH KEY)))
                     (SETF RETURN-VALUE PROXY))
                   (LET* ((RESULT (CREATE-BINDING VALUE TEMPLATE))
                          (PROXY (@ RESULT 0))
                          (NODE (@ RESULT 1)))
                     (CHAIN PARENT-NODE (INSERT-BEFORE NODE ANCHOR))
                     (SETF RETURN-VALUE PROXY)))
               (LET* ((PREVIOUS-VALUES
                       (IF IS-PREVIOUS-ARRAY
                           PREVIOUS-VALUE
                           (LIST PREVIOUS-VALUE)))
                      (VALUES
                       (IF IS-VALUE-ARRAY
                           VALUE
                           (LIST VALUE))))
                 (LOOP FOR I FROM 0 TO (- (LENGTH VALUES) 1)
                       DO (LET ((PREV (GETPROP PREVIOUS-VALUES I))
                                (OBJ (GETPROP VALUES I)))
                            (IF PREV
                                (PROGN
                                 (LOOP FOR KEY OF OBJ
                                       DO (SETF (GETPROP PREV KEY)
                                                  (GETPROP OBJ KEY)))
                                 (LOOP FOR KEY OF PREV
                                       DO (WHEN
                                              (NOT
                                               (CHAIN OBJ
                                                      (HAS-OWN-PROPERTY KEY)))
                                            (DELETE (GETPROP PREV KEY)))))
                                (SETF (GETPROP PREVIOUS-VALUES I) OBJ))))
                 (WHEN (CHAIN *ARRAY (IS-ARRAY PREVIOUS-VALUE))
                   (SETF (LENGTH PREVIOUS-VALUES) (LENGTH VALUES)))
                 (SETF RETURN-VALUE PREVIOUS-VALUE)))
           (LOOP FOR NODE IN (@ SLOT CHILD-NODES)
                 DO (CHAIN PARENT-NODE
                           (INSERT-BEFORE (CHAIN NODE (CLONE-NODE T))
                            ANCHOR))))
       (CHAIN PARENT-NODE (INSERT-BEFORE END-NODE ANCHOR))
       RETURN-VALUE)) */
function setSlot(target, key, value, descriptor) {
    if (!(target[key] || value)) {
        return;
    };
    var anchor = descriptor.node;
    var slot24 = descriptor.slot;
    var template25 = descriptor.template;
    var hash = TARGETDELIMITERMAP.get(target);
    var nodes = hash[key];
    var parentNode26 = anchor.parentNode;
    var startNode = createAnchor(0, key);
    var endNode = createAnchor(1, key);
    var previousValue = target[key];
    var isPreviousArray = Array.isArray(previousValue);
    var isValueArray = Array.isArray(value);
    var isTypeMismatch = isPreviousArray !== isValueArray;
    var returnValue = null;
    if (nodes && (!value || value && !previousValue || isTypeMismatch)) {
        if (Array.isArray(previousValue)) {
            var _js28 = previousValue.length;
            for (var _js27 = 0; _js27 < _js28; _js27 += 1) {
                var proxy = previousValue[_js27];
                var unmount = PROXYUNMOUNTMAP.get(proxy);
                var nodes29 = PROXYDELIMITERMAP.get(proxy);
                if (nodes29) {
                    removeBetweenDelimiters(nodes29[0], nodes29[1], unmount, proxy);
                };
            };
        } else {
            if (previousValue) {
                var unmount29 = PROXYUNMOUNTMAP.get(previousValue);
                var nodes30 = PROXYDELIMITERMAP.get(previousValue);
                removeBetweenDelimiters(nodes30[0], nodes30[1], unmount29, previousValue);
            };
        };
        removeBetweenDelimiters(nodes[0], nodes[1]);
        delete hash[key];
    };
    hash[key] = [startNode, endNode];
    parentNode26.insertBefore(startNode, anchor);
    if (value) {
        if (!previousValue || isTypeMismatch) {
            if (Array.isArray(value)) {
                var result = createArray(value, template25);
                var nodes31 = result[0];
                var proxy32 = result[1];
                var _js34 = nodes31.length;
                for (var _js33 = 0; _js33 < _js34; _js33 += 1) {
                    var node = nodes31[_js33];
                    parentNode26.insertBefore(node, anchor);
                };
                PROXYANCHORMAP.set(proxy32, anchor);
                PROXYDELIMITERMAP.set(proxy32, hash[key]);
                returnValue = proxy32;
            } else {
                var result35 = createBinding(value, template25);
                var proxy36 = result35[0];
                var node37 = result35[1];
                parentNode26.insertBefore(node37, anchor);
                returnValue = proxy36;
            };
        } else {
            var previousValues = isPreviousArray ? previousValue : [previousValue];
            var values = isValueArray ? value : [value];
            var _js38 = values.length - 1;
            for (var i = 0; i <= _js38; i += 1) {
                var prev = previousValues[i];
                var obj = values[i];
                if (prev) {
                    for (var key in obj) {
                        prev[key] = obj[key];
                    };
                    for (var key in prev) {
                        if (!obj.hasOwnProperty(key)) {
                            delete prev[key];
                        };
                    };
                } else {
                    previousValues[i] = obj;
                };
            };
            if (Array.isArray(previousValue)) {
                previousValues.length = values.length;
            };
            returnValue = previousValue;
        };
    } else {
        var _js39 = slot24.childNodes;
        var _js41 = _js39.length;
        for (var _js40 = 0; _js40 < _js41; _js40 += 1) {
            var node42 = _js39[_js40];
            parentNode26.insertBefore(node42.cloneNode(true), anchor);
        };
    };
    parentNode26.insertBefore(endNode, anchor);
    __PS_MV_REG = [];
    return returnValue;
};
/* (DEFUN SET-EVENT (TARGET VALUE DESCRIPTOR RECEIVER)
     (LET* ((NODE (@ DESCRIPTOR NODE))
            (EVENT (@ DESCRIPTOR EVENT))
            (HASH (CHAIN *TARGET-EVENT-MAP* (GET TARGET)))
            (LISTENER (GETPROP HASH EVENT)))
       (WHEN LISTENER
         (CHAIN NODE
                (REMOVE-EVENT-LISTENER EVENT LISTENER (@ LISTENER OPTIONS))))
       (WHEN VALUE
         (LET ((BOUND-LISTENER (CHAIN VALUE (BIND RECEIVER))))
           (SETF (@ BOUND-LISTENER OPTIONS) (@ VALUE OPTIONS))
           (CHAIN NODE
                  (ADD-EVENT-LISTENER EVENT BOUND-LISTENER
                   (@ BOUND-LISTENER OPTIONS)))
           (SETF (GETPROP HASH EVENT) BOUND-LISTENER))))) */
function setEvent(target, value, descriptor, receiver) {
    var node42 = descriptor.node;
    var event43 = descriptor.event;
    var hash = TARGETEVENTMAP.get(target);
    var listener = hash[event43];
    if (listener) {
        node42.removeEventListener(event43, listener, listener.options);
    };
    if (value) {
        var boundListener = value.bind(receiver);
        boundListener.options = value.options;
        node42.addEventListener(event43, boundListener, boundListener.options);
        return hash[event43] = boundListener;
    };
};
/* (DEFUN PROCESS-TEMPLATE (TEMPLATE)
     (LET* ((ROOT (OR (@ TEMPLATE CONTENT) TEMPLATE))
            (CLONE (CHAIN ROOT (CLONE-NODE T)))
            (CONTEXT (CREATE)))
       (DEFUN WALK (PARENT-NODE PATH)
         (LOOP FOR I FROM 0 TO (- (LENGTH (@ PARENT-NODE CHILD-NODES)) 1)
               DO (LET ((NODE (GETPROP (@ PARENT-NODE CHILD-NODES) I)))
                    (WHEN (NOT (EQ (@ NODE NODE-TYPE) (@ *NODE ELEMENT_NODE)))
                      (CONTINUE))
                    (WHEN (LENGTH (@ NODE CHILDREN))
                      (WALK NODE (CHAIN PATH (CONCAT I))))
                    (WHEN
                        (OR (EQ (@ NODE TAG-NAME) *TAG-SLOT*)
                            (@ NODE DATASET TEMPLATE))
                      (LET* ((SLOT-NAME
                              (OR (@ NODE DATASET KEY) (@ NODE NAME)))
                             (ANCHOR (CREATE-ANCHOR 2 SLOT-NAME))
                             (TEMPLATE-NODE
                              (CHAIN DOCUMENT
                                     (QUERY-SELECTOR
                                      (@ NODE DATASET TEMPLATE)))))
                        (WHEN (EQ SLOT-NAME UNDEFINED)
                          (THROW
                              (NEW
                               (*TYPE-ERROR
                                Missing `name` or `data-key` for slot.))))
                        (CHAIN PARENT-NODE (INSERT-BEFORE ANCHOR NODE))
                        (SETF (GETPROP CONTEXT SLOT-NAME)
                                (CREATE PATH (CHAIN PATH (CONCAT I)) SLOT NODE
                                 TEMPLATE TEMPLATE-NODE TYPE *SYMBOL-SLOT*))
                        (CHAIN NODE (REMOVE)))
                      (CONTINUE))
                    (LOOP FOR KEY OF (@ NODE DATASET)
                          DO (LET ((VALUE (GETPROP (@ NODE DATASET) KEY))
                                   (RESULT NIL))
                               (CASE KEY
                                 (text
                                  (SETF RESULT (CREATE TYPE *SYMBOL-TEXT*)))
                                 (value
                                  (SETF RESULT (CREATE TYPE *SYMBOL-VALUE*)))
                                 (class
                                  (SETF RESULT (CREATE TYPE *SYMBOL-CLASS*)))
                                 (unsafeHtml
                                  (SETF RESULT (CREATE TYPE *SYMBOL-HTML*))))
                               (WHEN (CHAIN KEY (STARTS-WITH 'ATTRIBUTE))
                                 (SETF RESULT
                                         (CREATE TYPE *SYMBOL-ATTRIBUTE* NAME
                                          (CHAIN KEY (SLICE 9)
                                                 (TO-LOWER-CASE)))))
                               (WHEN (CHAIN KEY (STARTS-WITH 'EVENT))
                                 (SETF RESULT
                                         (CREATE TYPE *SYMBOL-EVENT* EVENT
                                          (CHAIN KEY (SLICE 5)
                                                 (TO-LOWER-CASE)))))
                               (WHEN RESULT
                                 (DELETE (GETPROP (@ NODE DATASET) KEY))
                                 (CHAIN NODE (REMOVE-ATTRIBUTE KEY))
                                 (SETF (@ RESULT PATH) (CHAIN PATH (CONCAT I))
                                       (GETPROP CONTEXT VALUE) RESULT)))))))
       (WALK CLONE (LIST))
       (CHAIN *TEMPLATE-PROCESSED-MAP* (SET TEMPLATE CLONE))
       (CHAIN *TEMPLATE-CONTEXT-MAP* (SET TEMPLATE CONTEXT)))) */
function processTemplate(template) {
    var root = template.content || template;
    var clone = root.cloneNode(true);
    var context = {  };
    function walk(parentNode, path) {
        var _js44 = parentNode.childNodes.length - 1;
        for (var i = 0; i <= _js44; i += 1) {
            var node = parentNode.childNodes[i];
            if (node.nodeType !== Node['ELEMENT_NODE']) {
                continue;
            };
            if (node.children.length) {
                walk(node, path.concat(i));
            };
            if (node.tagName === TAGSLOT || node.dataset.template) {
                var slotName = node.dataset.key || node.name;
                var anchor = createAnchor(2, slotName);
                var templateNode = document.querySelector(node.dataset.template);
                if (slotName === undefined) {
                    throw new TypeError('Missing `name` or `data-key` for slot.');
                };
                parentNode.insertBefore(anchor, node);
                context[slotName] = { path : path.concat(i),
                                   slot : node,
                                   template : templateNode,
                                   type : SYMBOLSLOT
                                 };
                node.remove();
                continue;
            };
            for (var key in node.dataset) {
                var value = node.dataset[key];
                var result = null;
                switch (key) {
                case 'text':
                    result = { type : SYMBOLTEXT };
                    break;
                case 'value':
                    result = { type : SYMBOLVALUE };
                    break;
                case 'class':
                    result = { type : SYMBOLCLASS };
                    break;
                case 'unsafeHtml':
                    result = { type : SYMBOLHTML };
                };
                if (key.startsWith('attribute')) {
                    result = { type : SYMBOLATTRIBUTE, name : key.slice(9).toLowerCase() };
                };
                if (key.startsWith('event')) {
                    result = { type : SYMBOLEVENT, event : key.slice(5).toLowerCase() };
                };
                if (result) {
                    delete node.dataset[key];
                    node.removeAttribute(key);
                    result.path = path.concat(i);
                    context[value] = result;
                };
            };
        };
    };
    walk(clone, []);
    TEMPLATEPROCESSEDMAP.set(template, clone);
    __PS_MV_REG = [];
    return TEMPLATECONTEXTMAP.set(template, context);
};
/* (DEFUN CREATE-CONTEXT (CLONE TEMPLATE)
     (LET ((CONTEXT (CHAIN *TEMPLATE-CONTEXT-MAP* (GET TEMPLATE)))
           (CLONED-CONTEXT (CREATE)))
       (LOOP FOR KEY OF CONTEXT
             DO (LET* ((DESCRIPTOR (GETPROP CONTEXT KEY))
                       (PATH (@ DESCRIPTOR PATH))
                       (NODE (GET-PATH CLONE PATH)))
                  (SETF (GETPROP CLONED-CONTEXT KEY)
                          (CHAIN *OBJECT
                                 (ASSIGN (CREATE NODE NODE) DESCRIPTOR)))))
       CLONED-CONTEXT)) */
function createContext(clone, template) {
    var context = TEMPLATECONTEXTMAP.get(template);
    var clonedContext = {  };
    for (var key in context) {
        var descriptor = context[key];
        var path45 = descriptor.path;
        var node = getPath(clone, path45);
        clonedContext[key] = Object.assign({ node : node }, descriptor);
    };
    __PS_MV_REG = [];
    return clonedContext;
};
/* (DEFUN GET-PATH (NODE PATH)
     (LET ((RESULT NODE))
       (LOOP FOR I FROM 0 TO (- (LENGTH PATH) 1)
             DO (LET ((J (GETPROP PATH I)))
                  (SETF RESULT (GETPROP (@ RESULT CHILD-NODES) J))))
       RESULT)) */
function getPath(node, path) {
    var result = node;
    var _js45 = path.length - 1;
    for (var i = 0; i <= _js45; i += 1) {
        var j = path[i];
        result = result.childNodes[j];
    };
    return result;
};
/* (DEFUN CREATE-ARRAY (ARRAY TEMPLATE)
     (LET* ((NODES (LIST)) (PROXIES (LIST)) (PROXY NIL))
       (LOOP FOR ITEM IN ARRAY
             DO (LET ((RESULT (CREATE-BINDING ITEM TEMPLATE)))
                  (CHAIN PROXIES (PUSH (@ RESULT 0)))
                  (CHAIN NODES (PUSH (@ RESULT 1)))))
       (SETF PROXY (NEW (*PROXY PROXIES *PROXY-ARRAY*)))
       (CHAIN *PROXY-TEMPLATE-MAP* (SET PROXY TEMPLATE))
       (LIST NODES PROXY))) */
function createArray(array, template) {
    var nodes = [];
    var proxies = [];
    var proxy = null;
    var _js47 = array.length;
    for (var _js46 = 0; _js46 < _js47; _js46 += 1) {
        var item = array[_js46];
        var result = createBinding(item, template);
        proxies.push(result[0]);
        nodes.push(result[1]);
    };
    proxy = new Proxy(proxies, PROXYARRAY);
    PROXYTEMPLATEMAP.set(proxy, template);
    __PS_MV_REG = [];
    return [nodes, proxy];
};
/* (DEFUN CREATE-BINDING (OBJ TEMPLATE)
     (WHEN (NOT (CHAIN *TEMPLATE-PROCESSED-MAP* (GET TEMPLATE)))
       (PROCESS-TEMPLATE TEMPLATE))
     (LET* ((CLONE
             (CHAIN (CHAIN *TEMPLATE-PROCESSED-MAP* (GET TEMPLATE))
                    (CLONE-NODE T)))
            (TARGET (CREATE))
            (PROXY (NEW (*PROXY TARGET *PROXY-OBJECT*)))
            (CONTEXT (CREATE-CONTEXT CLONE TEMPLATE))
            (START-NODE (CREATE-ANCHOR 0 'PROXY))
            (END-NODE (CREATE-ANCHOR 1 'PROXY))
            (NODES (LIST START-NODE END-NODE))
            (MOUNT (GETPROP OBJ *SYMBOL-MOUNT*))
            (UNMOUNT (GETPROP OBJ *SYMBOL-UNMOUNT*))
            (FRAGMENT (CHAIN DOCUMENT (CREATE-DOCUMENT-FRAGMENT))))
       (WHEN MOUNT (CHAIN MOUNT (CALL PROXY CLONE)))
       (WHEN UNMOUNT (CHAIN *PROXY-UNMOUNT-MAP* (SET PROXY UNMOUNT)))
       (CHAIN FRAGMENT (APPEND-CHILD START-NODE))
       (CHAIN FRAGMENT (APPEND-CHILD CLONE))
       (CHAIN FRAGMENT (APPEND-CHILD END-NODE))
       (CHAIN *PROXY-DELIMITER-MAP* (SET PROXY NODES))
       (CHAIN *TARGET-CONTEXT-MAP* (SET TARGET CONTEXT))
       (CHAIN *TARGET-EVENT-MAP* (SET TARGET (CREATE)))
       (CHAIN *TARGET-DELIMITER-MAP* (SET TARGET (CREATE)))
       (LOOP FOR KEY OF CONTEXT
             DO (SET-PROPERTY TARGET KEY (GETPROP OBJ KEY) PROXY))
       (LIST PROXY FRAGMENT))) */
function createBinding(obj, template) {
    if (!TEMPLATEPROCESSEDMAP.get(template)) {
        processTemplate(template);
    };
    var clone = TEMPLATEPROCESSEDMAP.get(template).cloneNode(true);
    var target = {  };
    var proxy = new Proxy(target, PROXYOBJECT);
    var context = createContext(clone, template);
    var startNode = createAnchor(0, 'proxy');
    var endNode = createAnchor(1, 'proxy');
    var nodes = [startNode, endNode];
    var mount = obj[SYMBOLMOUNT];
    var unmount = obj[SYMBOLUNMOUNT];
    var fragment = document.createDocumentFragment();
    if (mount) {
        mount.call(proxy, clone);
    };
    if (unmount) {
        PROXYUNMOUNTMAP.set(proxy, unmount);
    };
    fragment.appendChild(startNode);
    fragment.appendChild(clone);
    fragment.appendChild(endNode);
    PROXYDELIMITERMAP.set(proxy, nodes);
    TARGETCONTEXTMAP.set(target, context);
    TARGETEVENTMAP.set(target, {  });
    TARGETDELIMITERMAP.set(target, {  });
    for (var key in context) {
        setProperty(target, key, obj[key], proxy);
    };
    __PS_MV_REG = [];
    return [proxy, fragment];
};
/* (DEFUN CREATE-ANCHOR (TYPE KEY)
     (IF (@ MAIN DEBUG)
         (LET ((COMMENT
                (+
                 (IF (EQ TYPE 0)
                     'START
                     (IF (EQ TYPE 1)
                         'END
                         'ANCHOR))
                   KEY)))
           (CHAIN DOCUMENT (CREATE-COMMENT COMMENT)))
         (CHAIN DOCUMENT (CREATE-TEXT-NODE )))) */
function createAnchor(type, key) {
    if (main.debug) {
        var comment = (type === 0 ? 'start' : (type === 1 ? 'end' : 'anchor')) + ' ' + key;
        return document.createComment(comment);
    } else {
        return document.createTextNode('');
    };
};
/* (DEFUN MAIN (ORIGIN TEMPLATE) (CREATE-BINDING ORIGIN TEMPLATE)) */
function main(origin, template) {
    __PS_MV_REG = [];
    return createBinding(origin, template);
};
/* (EXPORT DEFAULT MAIN NAMES
           (*SYMBOL-MOUNT* AS MOUNT *SYMBOL-UNMOUNT* AS UNMOUNT)) */
export { SYMBOLMOUNT as mount, SYMBOLUNMOUNT as unmount, };
export default main;

