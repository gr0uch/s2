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
                    (NODE (@ RESULT 0))
                    (PROXY (@ RESULT 1))
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
            var node = result[0];
            var proxy6 = result[1];
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
            (IS-DELETE (EQ (LENGTH ARGUMENTS) 2)))
       (WHEN (CHAIN *OBJECT PROTOTYPE HAS-OWN-PROPERTY (CALL CONTEXT KEY))
         (LET* ((DESCRIPTOR (GETPROP CONTEXT KEY))
                (NODE (@ DESCRIPTOR NODE))
                (TYPE (@ DESCRIPTOR TYPE)))
           (WHEN
               (AND (EQ TYPE *SYMBOL-TEXT*)
                    (NOT (EQ VALUE (@ NODE TEXT-CONTENT))))
             (SETF (@ NODE TEXT-CONTENT) VALUE))
           (WHEN
               (AND (EQ TYPE *SYMBOL-HTML*)
                    (NOT (EQ VALUE (@ NODE INNER-H-T-M-L))))
             (SETF (@ NODE INNER-H-T-M-L) VALUE))
           (WHEN (EQ TYPE *SYMBOL-CLASS*) (SET-CLASS NODE VALUE))
           (WHEN (EQ TYPE *SYMBOL-ATTRIBUTE*)
             (SET-ATTRIBUTE NODE (@ DESCRIPTOR NAME) VALUE))
           (WHEN (EQ TYPE *SYMBOL-EVENT*)
             (SET-EVENT TARGET VALUE DESCRIPTOR RECEIVER))
           (WHEN (EQ TYPE *SYMBOL-SLOT*)
             (LET ((PROXY (SET-SLOT TARGET KEY VALUE DESCRIPTOR)))
               (WHEN PROXY
                 (RETURN-FROM SET-PROPERTY
                   (CHAIN *REFLECT (SET TARGET KEY PROXY RECEIVER))))))))
       (WHEN IS-DELETE (DELETE (GETPROP TARGET KEY)))
       (WHEN IS-SETTER (CHAIN *REFLECT (SET TARGET KEY VALUE RECEIVER))))
     T) */
function setProperty(target, key, value, receiver) {
    var context = TARGETCONTEXTMAP.get(target);
    var isSetter = arguments.length === 4;
    var isDelete = arguments.length === 2;
    if (Object.prototype.hasOwnProperty.call(context, key)) {
        var descriptor = context[key];
        var node16 = descriptor.node;
        var type17 = descriptor.type;
        if (type17 === SYMBOLTEXT && value !== node16.textContent) {
            node16.textContent = value;
        };
        if (type17 === SYMBOLHTML && value !== node16.innerHTML) {
            node16.innerHTML = value;
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
     (IF (OR (EQ VALUE NIL) (EQ VALUE UNDEFINED))
         (CHAIN NODE (SET-ATTRIBUTE NAME VALUE))
         (CHAIN NODE (REMOVE-ATTRIBUTE NAME)))) */
function setAttribute(node, name, value) {
    return value === null || value === undefined ? node.setAttribute(name, value) : node.removeAttribute(name);
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
     (LET* ((ANCHOR (@ DESCRIPTOR ANCHOR))
            (SLOT (@ DESCRIPTOR SLOT))
            (TEMPLATE (@ DESCRIPTOR TEMPLATE))
            (HASH (CHAIN *TARGET-DELIMITER-MAP* (GET TARGET)))
            (NODES (GETPROP HASH KEY))
            (PARENT-NODE (@ ANCHOR PARENT-NODE))
            (START-NODE (CREATE-ANCHOR 0 KEY))
            (END-NODE (CREATE-ANCHOR 1 KEY))
            (RETURN-VALUE NIL))
       (WHEN NODES
         (LET ((PREVIOUS-VALUE (GETPROP TARGET KEY)))
           (IF (CHAIN *ARRAY (IS-ARRAY PREVIOUS-VALUE))
               (LOOP FOR PROXY IN PREVIOUS-VALUE
                     DO (LET ((UNMOUNT (CHAIN *PROXY-UNMOUNT-MAP* (GET PROXY)))
                              (NODES (CHAIN *PROXY-DELIMITER-MAP* (GET PROXY))))
                          (REMOVE-BETWEEN-DELIMITERS (@ NODES 0) (@ NODES 1)
                           UNMOUNT PROXY)))
               (IF PREVIOUS-VALUE
                   (LET ((UNMOUNT
                          (CHAIN *PROXY-UNMOUNT-MAP* (GET PREVIOUS-VALUE)))
                         (NODES
                          (CHAIN *PROXY-DELIMITER-MAP* (GET PREVIOUS-VALUE))))
                     (REMOVE-BETWEEN-DELIMITERS (@ NODES 0) (@ NODES 1) UNMOUNT
                      PREVIOUS-VALUE))
                   (REMOVE-BETWEEN-DELIMITERS (@ NODES 0) (@ NODES 1)))))
         (DELETE (GETPROP HASH KEY)))
       (SETF (GETPROP HASH KEY) (LIST START-NODE END-NODE))
       (CHAIN PARENT-NODE (INSERT-BEFORE START-NODE ANCHOR))
       (IF VALUE
           (IF (CHAIN *ARRAY (IS-ARRAY VALUE))
               (LET* ((RESULT (CREATE-ARRAY VALUE TEMPLATE))
                      (NODES (@ RESULT 0))
                      (PROXY (@ RESULT 1)))
                 (CHAIN PARENT-NODE (INSERT-BEFORE START-NODE ANCHOR))
                 (LOOP FOR NODE IN NODES
                       DO (CHAIN PARENT-NODE (INSERT-BEFORE NODE ANCHOR)))
                 (CHAIN PARENT-NODE (INSERT-BEFORE END-NODE ANCHOR))
                 (CHAIN *PROXY-ANCHOR-MAP* (SET PROXY ANCHOR))
                 (CHAIN *PROXY-DELIMITER-MAP* (SET PROXY (GETPROP HASH KEY)))
                 (SETF RETURN-VALUE PROXY))
               (LET* ((RESULT (CREATE-BINDING VALUE TEMPLATE))
                      (NODE (@ RESULT 0))
                      (PROXY (@ RESULT 1)))
                 (CHAIN PARENT-NODE (INSERT-BEFORE NODE ANCHOR))
                 (SETF RETURN-VALUE PROXY)))
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
    var anchor24 = descriptor.anchor;
    var slot25 = descriptor.slot;
    var template26 = descriptor.template;
    var hash = TARGETDELIMITERMAP.get(target);
    var nodes = hash[key];
    var parentNode27 = anchor24.parentNode;
    var startNode = createAnchor(0, key);
    var endNode = createAnchor(1, key);
    var returnValue = null;
    if (nodes) {
        var previousValue = target[key];
        if (Array.isArray(previousValue)) {
            var _js29 = previousValue.length;
            for (var _js28 = 0; _js28 < _js29; _js28 += 1) {
                var proxy = previousValue[_js28];
                var unmount = PROXYUNMOUNTMAP.get(proxy);
                var nodes30 = PROXYDELIMITERMAP.get(proxy);
                removeBetweenDelimiters(nodes30[0], nodes30[1], unmount, proxy);
            };
        } else {
            if (previousValue) {
                var unmount30 = PROXYUNMOUNTMAP.get(previousValue);
                var nodes31 = PROXYDELIMITERMAP.get(previousValue);
                removeBetweenDelimiters(nodes31[0], nodes31[1], unmount30, previousValue);
            } else {
                removeBetweenDelimiters(nodes[0], nodes[1]);
            };
        };
        delete hash[key];
    };
    hash[key] = [startNode, endNode];
    parentNode27.insertBefore(startNode, anchor24);
    if (value) {
        if (Array.isArray(value)) {
            var result = createArray(value, template26);
            var nodes32 = result[0];
            var proxy33 = result[1];
            parentNode27.insertBefore(startNode, anchor24);
            var _js35 = nodes32.length;
            for (var _js34 = 0; _js34 < _js35; _js34 += 1) {
                var node = nodes32[_js34];
                parentNode27.insertBefore(node, anchor24);
            };
            parentNode27.insertBefore(endNode, anchor24);
            PROXYANCHORMAP.set(proxy33, anchor24);
            PROXYDELIMITERMAP.set(proxy33, hash[key]);
            returnValue = proxy33;
        } else {
            var result36 = createBinding(value, template26);
            var node37 = result36[0];
            var proxy38 = result36[1];
            parentNode27.insertBefore(node37, anchor24);
            returnValue = proxy38;
        };
    } else {
        var _js39 = slot25.childNodes;
        var _js41 = _js39.length;
        for (var _js40 = 0; _js40 < _js41; _js40 += 1) {
            var node42 = _js39[_js40];
            parentNode27.insertBefore(node42.cloneNode(true), anchor24);
        };
    };
    parentNode27.insertBefore(endNode, anchor24);
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
/* (DEFUN CREATE-CONTEXT (CLONE)
     (LET ((NODE NIL)
           (ITER
            (CHAIN DOCUMENT
                   (CREATE-NODE-ITERATOR CLONE (@ *NODE-FILTER SHOW_ELEMENT))))
           (CONTEXT (CREATE)))
       (LOOP WHILE (SETF NODE (CHAIN ITER (NEXT-NODE)))
             DO (WHEN (EQ (@ NODE TAG-NAME) *TAG-SLOT*)
                  (LET* ((SLOT-NAME (@ NODE NAME))
                         (ANCHOR (CREATE-ANCHOR 2 SLOT-NAME))
                         (TEMPLATE-NODE
                          (CHAIN DOCUMENT
                                 (QUERY-SELECTOR (@ NODE DATASET TEMPLATE)))))
                    (CHAIN NODE PARENT-NODE (INSERT-BEFORE ANCHOR NODE))
                    (SETF (GETPROP CONTEXT SLOT-NAME)
                            (CREATE ANCHOR ANCHOR SLOT NODE TEMPLATE
                             TEMPLATE-NODE TYPE *SYMBOL-SLOT*))
                    (CHAIN NODE (REMOVE)))
                  (CONTINUE)) (LOOP FOR KEY OF (@ NODE DATASET)
                                    DO (LET ((VALUE
                                              (GETPROP (@ NODE DATASET) KEY))
                                             (RESULT NIL))
                                         (CASE KEY
                                           (text
                                            (SETF RESULT
                                                    (CREATE NODE NODE TYPE
                                                     *SYMBOL-TEXT*)))
                                           (class
                                            (SETF RESULT
                                                    (CREATE NODE NODE TYPE
                                                     *SYMBOL-CLASS*)))
                                           (unsafeHtml
                                            (SETF RESULT
                                                    (CREATE NODE NODE TYPE
                                                     *SYMBOL-HTML*))))
                                         (WHEN
                                             (CHAIN KEY
                                                    (STARTS-WITH 'ATTRIBUTE))
                                           (SETF RESULT
                                                   (CREATE NODE NODE TYPE
                                                    *SYMBOL-ATTRIBUTE* NAME
                                                    (CHAIN KEY (SLICE 9)
                                                           (TO-LOWER-CASE)))))
                                         (WHEN (CHAIN KEY (STARTS-WITH 'EVENT))
                                           (SETF RESULT
                                                   (CREATE NODE NODE TYPE
                                                    *SYMBOL-EVENT* EVENT
                                                    (CHAIN KEY (SLICE 5)
                                                           (TO-LOWER-CASE)))))
                                         (WHEN RESULT
                                           (DELETE
                                            (GETPROP (@ NODE DATASET) KEY))
                                           (SETF (GETPROP CONTEXT VALUE)
                                                   RESULT)))))
       CONTEXT)) */
function createContext(clone) {
    var node = null;
    var iter = document.createNodeIterator(clone, NodeFilter['SHOW_ELEMENT']);
    var context = {  };
    while (node = iter.nextNode()) {
        if (node.tagName === TAGSLOT) {
            var slotName = node.name;
            var anchor = createAnchor(2, slotName);
            var templateNode = document.querySelector(node.dataset.template);
            node.parentNode.insertBefore(anchor, node);
            context[slotName] = { anchor : anchor,
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
                result = { node : node, type : SYMBOLTEXT };
                break;
            case 'class':
                result = { node : node, type : SYMBOLCLASS };
                break;
            case 'unsafeHtml':
                result = { node : node, type : SYMBOLHTML };
            };
            if (key.startsWith('attribute')) {
                result = { node : node,
                        type : SYMBOLATTRIBUTE,
                        name : key.slice(9).toLowerCase()
                      };
            };
            if (key.startsWith('event')) {
                result = { node : node,
                        type : SYMBOLEVENT,
                        event : key.slice(5).toLowerCase()
                      };
            };
            if (result) {
                delete node.dataset[key];
                context[value] = result;
            };
        };
    };
    __PS_MV_REG = [];
    return context;
};
/* (DEFUN CREATE-ARRAY (ARRAY TEMPLATE)
     (LET* ((NODES (LIST)) (PROXIES (LIST)) (PROXY NIL))
       (LOOP FOR ITEM IN ARRAY
             DO (LET ((RESULT (CREATE-BINDING ITEM TEMPLATE)))
                  (CHAIN NODES (PUSH (@ RESULT 0)))
                  (CHAIN PROXIES (PUSH (@ RESULT 1)))))
       (SETF PROXY (NEW (*PROXY PROXIES *PROXY-ARRAY*)))
       (CHAIN *PROXY-TEMPLATE-MAP* (SET PROXY TEMPLATE))
       (LIST NODES PROXY))) */
function createArray(array, template) {
    var nodes = [];
    var proxies = [];
    var proxy = null;
    var _js45 = array.length;
    for (var _js44 = 0; _js44 < _js45; _js44 += 1) {
        var item = array[_js44];
        var result = createBinding(item, template);
        nodes.push(result[0]);
        proxies.push(result[1]);
    };
    proxy = new Proxy(proxies, PROXYARRAY);
    PROXYTEMPLATEMAP.set(proxy, template);
    __PS_MV_REG = [];
    return [nodes, proxy];
};
/* (DEFUN CREATE-BINDING (OBJ TEMPLATE)
     (LET* ((ROOT (OR (@ TEMPLATE CONTENT) TEMPLATE))
            (CLONE (CHAIN ROOT (CLONE-NODE T)))
            (PROXY (NEW (*PROXY OBJ *PROXY-OBJECT*)))
            (CONTEXT (CREATE-CONTEXT CLONE))
            (START-NODE (CREATE-ANCHOR 0 'PROXY))
            (END-NODE (CREATE-ANCHOR 1 'PROXY))
            (NODES (LIST START-NODE END-NODE))
            (MOUNT (GETPROP OBJ *SYMBOL-MOUNT*))
            (UNMOUNT (GETPROP OBJ *SYMBOL-UNMOUNT*)))
       (CHAIN CLONE (INSERT-BEFORE START-NODE (@ CLONE FIRST-CHILD)))
       (CHAIN CLONE (APPEND-CHILD END-NODE))
       (CHAIN *PROXY-DELIMITER-MAP* (SET PROXY NODES))
       (CHAIN *TARGET-CONTEXT-MAP* (SET OBJ CONTEXT))
       (CHAIN *TARGET-EVENT-MAP* (SET OBJ (CREATE)))
       (CHAIN *TARGET-DELIMITER-MAP* (SET OBJ (CREATE)))
       (CHAIN *OBJECT (ASSIGN PROXY OBJ))
       (WHEN MOUNT (CHAIN MOUNT (CALL PROXY CLONE)))
       (WHEN UNMOUNT (CHAIN *PROXY-UNMOUNT-MAP* (SET PROXY UNMOUNT)))
       (LIST CLONE PROXY))) */
function createBinding(obj, template) {
    var root = template.content || template;
    var clone = root.cloneNode(true);
    var proxy = new Proxy(obj, PROXYOBJECT);
    var context = createContext(clone);
    var startNode = createAnchor(0, 'proxy');
    var endNode = createAnchor(1, 'proxy');
    var nodes = [startNode, endNode];
    var mount = obj[SYMBOLMOUNT];
    var unmount = obj[SYMBOLUNMOUNT];
    clone.insertBefore(startNode, clone.firstChild);
    clone.appendChild(endNode);
    PROXYDELIMITERMAP.set(proxy, nodes);
    TARGETCONTEXTMAP.set(obj, context);
    TARGETEVENTMAP.set(obj, {  });
    TARGETDELIMITERMAP.set(obj, {  });
    Object.assign(proxy, obj);
    if (mount) {
        mount.call(proxy, clone);
    };
    if (unmount) {
        PROXYUNMOUNTMAP.set(proxy, unmount);
    };
    __PS_MV_REG = [];
    return [clone, proxy];
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

