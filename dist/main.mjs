
/* (DEFPARAMETER *CONTEXT-SLOT* 'SLOT) */
var CONTEXTSLOT = 'slot';
/* (DEFPARAMETER *CONTEXT-TEXT* 'TEXT) */
var CONTEXTTEXT = 'text';
/* (DEFPARAMETER *CONTEXT-HTML* 'HTML) */
var CONTEXTHTML = 'html';
/* (DEFPARAMETER *CONTEXT-VALUE* 'VALUE) */
var CONTEXTVALUE = 'value';
/* (DEFPARAMETER *CONTEXT-CLASS* 'CLASS) */
var CONTEXTCLASS = 'class';
/* (DEFPARAMETER *CONTEXT-CLASSLIST* 'CLASSLIST) */
var CONTEXTCLASSLIST = 'classlist';
/* (DEFPARAMETER *CONTEXT-ATTRIBUTE* 'ATTRIBUTE) */
var CONTEXTATTRIBUTE = 'attribute';
/* (DEFPARAMETER *CONTEXT-STYLE-PROPERTY* 'STYLE-PROPERTY) */
var CONTEXTSTYLEPROPERTY = 'styleProperty';
/* (DEFPARAMETER *CONTEXT-DATA* 'DATA) */
var CONTEXTDATA = 'data';
/* (DEFPARAMETER *CONTEXT-EVENT* 'EVENT) */
var CONTEXTEVENT = 'event';
/* (DEFPARAMETER *SYMBOL-MOUNT* (*SYMBOL 'MOUNT)) */
var SYMBOLMOUNT = Symbol('mount');
/* (DEFPARAMETER *SYMBOL-UNMOUNT* (*SYMBOL 'UNMOUNT)) */
var SYMBOLUNMOUNT = Symbol('unmount');
/* (DEFPARAMETER *SYMBOL-MOVE* (*SYMBOL 'MOVE)) */
var SYMBOLMOVE = Symbol('move');
/* (DEFPARAMETER *SYMBOL-ROOT* (*SYMBOL 'ROOT)) */
var SYMBOLROOT = Symbol('root');
/* (DEFPARAMETER *SYMBOL-TARGET* (*SYMBOL 'TARGET)) */
var SYMBOLTARGET = Symbol('target');
/* (DEFPARAMETER *TAG-SLOT* '*SLOT*) */
var TAGSLOT = 'SLOT';
/* (DEFMACRO CONSOLE-LOG (&BODY FORMS) `(CHAIN CONSOLE (LOG ,@FORMS))) */

/* (DEFMACRO CONSOLE-WARN (&BODY FORMS) `(CHAIN CONSOLE (WARN ,@FORMS))) */

/* (DEFPARAMETER *TARGET-CONTEXT-MAP* (NEW (*WEAK-MAP))) */
var TARGETCONTEXTMAP = new WeakMap();
/* (DEFPARAMETER *TARGET-EVENT-MAP* (NEW (*WEAK-MAP))) */
var TARGETEVENTMAP = new WeakMap();
/* (DEFPARAMETER *TARGET-DELIMITER-MAP* (NEW (*WEAK-MAP))) */
var TARGETDELIMITERMAP = new WeakMap();
/* (DEFPARAMETER *PROXY-UNMOUNT-MAP* (NEW (*WEAK-MAP))) */
var PROXYUNMOUNTMAP = new WeakMap();
/* (DEFPARAMETER *PROXY-MOVE-MAP* (NEW (*WEAK-MAP))) */
var PROXYMOVEMAP = new WeakMap();
/* (DEFPARAMETER *PROXY-DELIMITER-MAP* (NEW (*WEAK-MAP))) */
var PROXYDELIMITERMAP = new WeakMap();
/* (DEFPARAMETER *PROXY-TEMPLATE-MAP* (NEW (*WEAK-MAP))) */
var PROXYTEMPLATEMAP = new WeakMap();
/* (DEFPARAMETER *PROXY-ANCHOR-MAP* (NEW (*WEAK-MAP))) */
var PROXYANCHORMAP = new WeakMap();
/* (DEFPARAMETER *TEMPLATE-PROCESSED-MAP* (NEW (*WEAK-MAP))) */
var TEMPLATEPROCESSEDMAP = new WeakMap();
/* (DEFPARAMETER *TEMPLATE-CONTEXT-MAP* (NEW (*WEAK-MAP))) */
var TEMPLATECONTEXTMAP = new WeakMap();
/* (DEFPARAMETER *PROXY-OBJECT*
     (CREATE SET SET-PROPERTY DELETE-PROPERTY SET-PROPERTY)) */
var PROXYOBJECT = { set : setProperty, deleteProperty : setProperty };
/* (DEFPARAMETER *PROXY-ARRAY* (CREATE SET SET-INDEX DELETE-PROPERTY SET-INDEX)) */
var PROXYARRAY = { set : setIndex, deleteProperty : setIndex };
/* (DEFPARAMETER *TEMPLATES-HASH* (CREATE)) */
var TEMPLATESHASH = {  };
/* (DEFPARAMETER *PROPERTY-HANDLERS* (CREATE)) */
var PROPERTYHANDLERS = {  };
/* (SETF (GETPROP *PROPERTY-HANDLERS* *CONTEXT-TEXT*)
           (LAMBDA (NODE KEY VALUE)
             (WHEN (NOT (EQ VALUE (@ NODE TEXT-CONTENT)))
               (SETF (@ NODE TEXT-CONTENT) VALUE)))
         (GETPROP *PROPERTY-HANDLERS* *CONTEXT-HTML*)
           (LAMBDA (NODE KEY VALUE)
             (WHEN (NOT (EQ VALUE (@ NODE INNER-H-T-M-L)))
               (SETF (@ NODE INNER-H-T-M-L) (OR VALUE ))))
         (GETPROP *PROPERTY-HANDLERS* *CONTEXT-VALUE*)
           (LAMBDA (NODE KEY VALUE)
             (WHEN (NOT (EQ VALUE (@ NODE VALUE)))
               (IF (EQ VALUE UNDEFINED)
                   (PROGN
                    (CHAIN NODE (REMOVE-ATTRIBUTE 'VALUE))
                    (SETF (@ NODE VALUE) ))
                   (PROGN
                    (CHAIN NODE (SET-ATTRIBUTE 'VALUE VALUE))
                    (SETF (@ NODE VALUE) VALUE)))))
         (GETPROP *PROPERTY-HANDLERS* *CONTEXT-STYLE-PROPERTY*)
           (LAMBDA (NODE KEY VALUE)
             (CHAIN NODE STYLE (SET-PROPERTY KEY VALUE)))
         (GETPROP *PROPERTY-HANDLERS* *CONTEXT-CLASS*) SET-CLASS
         (GETPROP *PROPERTY-HANDLERS* *CONTEXT-CLASSLIST*) SET-CLASSLIST
         (GETPROP *PROPERTY-HANDLERS* *CONTEXT-ATTRIBUTE*) SET-ATTRIBUTE
         (GETPROP *PROPERTY-HANDLERS* *CONTEXT-DATA*) SET-DATA) */
PROPERTYHANDLERS[CONTEXTTEXT] = function (node, key, value) {
    return value !== node.textContent ? (node.textContent = value) : null;
};
PROPERTYHANDLERS[CONTEXTHTML] = function (node, key, value) {
    return value !== node.innerHTML ? (node.innerHTML = value || '') : null;
};
PROPERTYHANDLERS[CONTEXTVALUE] = function (node, key, value) {
    if (value !== node.value) {
        if (value === undefined) {
            node.removeAttribute('value');
            return node.value = '';
        } else {
            node.setAttribute('value', value);
            return node.value = value;
        };
    };
};
PROPERTYHANDLERS[CONTEXTSTYLEPROPERTY] = function (node, key, value) {
    return node.style.setProperty(key, value);
};
PROPERTYHANDLERS[CONTEXTCLASS] = setClass;
PROPERTYHANDLERS[CONTEXTCLASSLIST] = setClasslist;
PROPERTYHANDLERS[CONTEXTATTRIBUTE] = setAttribute;
PROPERTYHANDLERS[CONTEXTDATA] = setData;
/* (DEFUN SET-INDEX (TARGET KEY VALUE RECEIVER IS-INITIALIZING)
     (WHEN (AND (@ MAIN IS-DEFERRED) (NOT IS-INITIALIZING))
       (QUEUE-MICROTASK (LAMBDA () (SET-INDEX TARGET KEY VALUE RECEIVER T)))
       (RETURN-FROM SET-INDEX T))
     (WHEN (@ MAIN DEBUG) (CONSOLE-LOG 'SET-INDEX ARGUMENTS))
     (LET* ((NUMKEY
             (CHAIN *NUMBER
                    (PARSE-INT
                     (IF (EQ (TYPEOF KEY) 'STRING)
                         KEY
                         NIL)
                     10)))
            (IS-INDEX (NOT (CHAIN *NUMBER (IS-NA-N NUMKEY))))
            (IS-SETTER (NOT (EQ VALUE UNDEFINED))))
       (WHEN (EQ KEY 'LENGTH)
         (LOOP FOR I FROM VALUE TO (- (LENGTH TARGET) 1)
               DO (LET* ((PROXY (GETPROP TARGET I))
                         (NODES (CHAIN *PROXY-DELIMITER-MAP* (GET PROXY)))
                         (UNMOUNT (CHAIN *PROXY-UNMOUNT-MAP* (GET PROXY))))
                    (WHEN NODES
                      (REMOVE-BETWEEN-DELIMITERS (@ NODES 0) (@ NODES 1)
                       UNMOUNT PROXY))) (DELETE (GETPROP TARGET I))))
       (WHEN (AND (NOT IS-SETTER) (GETPROP TARGET KEY))
         (LET* ((PROXY (GETPROP TARGET KEY))
                (NODES (CHAIN *PROXY-DELIMITER-MAP* (GET PROXY)))
                (UNMOUNT (CHAIN *PROXY-UNMOUNT-MAP* (GET PROXY))))
           (REMOVE-BETWEEN-DELIMITERS (@ NODES 0) (@ NODES 1) UNMOUNT PROXY))
         (DELETE (GETPROP TARGET KEY)))
       (WHEN IS-SETTER
         (WHEN (NOT IS-INDEX)
           (RETURN-FROM SET-INDEX
             (CHAIN *REFLECT (SET TARGET KEY VALUE RECEIVER))))
         (IF (NOT (CHAIN TARGET (INCLUDES VALUE)))
             (LET* ((ANCHOR (CHAIN *PROXY-ANCHOR-MAP* (GET RECEIVER)))
                    (PARENT-NODE (@ ANCHOR PARENT-NODE))
                    (TEMPLATE (CHAIN *PROXY-TEMPLATE-MAP* (GET RECEIVER)))
                    (RESULT
                     (CREATE-BINDING VALUE TEMPLATE
                      (GETPROP RECEIVER *SYMBOL-ROOT*)))
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
                    (OTHER-PROXY (GETPROP TARGET KEY))
                    (MOVE (CHAIN *PROXY-MOVE-MAP* (GET VALUE)))
                    (NODES (CHAIN *PROXY-DELIMITER-MAP* (GET VALUE)))
                    (START-NODE (@ NODES 0))
                    (END-NODE (@ NODES 1)))
               (IF OTHER-PROXY
                   (LET* ((OTHER-NODES
                           (CHAIN *PROXY-DELIMITER-MAP* (GET OTHER-PROXY)))
                          (OTHER-START-NODE (@ OTHER-NODES 0))
                          (OTHER-END-NODE (@ OTHER-NODES 1))
                          (OTHER-MOVE
                           (CHAIN *PROXY-MOVE-MAP* (GET OTHER-PROXY)))
                          (ANCHOR (@ NODES 1 NEXT-SIBLING))
                          (PARENT-NODE (@ ANCHOR PARENT-NODE)))
                     (WHEN (NOT (EQ VALUE OTHER-PROXY))
                       (WHEN MOVE
                         (LET ((NODE (@ START-NODE NEXT-SIBLING)))
                           (LOOP WHILE (NOT
                                        (CHAIN NODE (IS-SAME-NODE END-NODE)))
                                 DO (LET ((OLD-NODE NODE))
                                      (SETF NODE (@ NODE NEXT-SIBLING))
                                      (CHAIN MOVE (CALL VALUE OLD-NODE))))))
                       (WHEN OTHER-MOVE
                         (LET ((NODE (@ OTHER-START-NODE NEXT-SIBLING)))
                           (LOOP WHILE (NOT
                                        (CHAIN NODE
                                               (IS-SAME-NODE OTHER-END-NODE)))
                                 DO (LET ((OLD-NODE NODE))
                                      (SETF NODE (@ NODE NEXT-SIBLING))
                                      (CHAIN OTHER-MOVE
                                             (CALL OTHER-PROXY OLD-NODE))))))
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
                                      (CHAIN NODE
                                             (IS-SAME-NODE OTHER-END-NODE)))
                               DO (LET ((OLD-NODE NODE))
                                    (SETF NODE (@ NODE NEXT-SIBLING))
                                    (CHAIN PARENT-NODE
                                           (INSERT-BEFORE OLD-NODE ANCHOR))))
                         (CHAIN PARENT-NODE
                                (INSERT-BEFORE OTHER-END-NODE ANCHOR)))))
                   (WHEN MOVE
                     (LET ((NODE (@ START-NODE NEXT-SIBLING)))
                       (LOOP WHILE (NOT (CHAIN NODE (IS-SAME-NODE END-NODE)))
                             DO (LET ((OLD-NODE NODE))
                                  (SETF NODE (@ NODE NEXT-SIBLING))
                                  (CHAIN MOVE (CALL VALUE OLD-NODE)))))))
               (WHEN (~ OTHER-INDEX)
                 (CHAIN *REFLECT
                        (SET TARGET OTHER-INDEX OTHER-PROXY RECEIVER)))))
         (CHAIN *REFLECT (SET TARGET KEY VALUE RECEIVER))))
     T) */
function setIndex(target, key, value, receiver, isInitializing) {
    if (main.isDeferred && !isInitializing) {
        queueMicrotask(function () {
            
            return setIndex(target, key, value, receiver, true);
        });
        
        return true;
    };
    if (main.debug) {
        console.log('setIndex', arguments);
    };
    var numkey = Number.parseInt(typeof key === 'string' ? key : null, 10);
    var isIndex = !Number.isNaN(numkey);
    var isSetter = value !== undefined;
    if (key === 'length') {
        var _js5 = target.length - 1;
        for (var i = value; i <= _js5; i += 1) {
            var proxy = target[i];
            var nodes = PROXYDELIMITERMAP.get(proxy);
            var unmount = PROXYUNMOUNTMAP.get(proxy);
            if (nodes) {
                removeBetweenDelimiters(nodes[0], nodes[1], unmount, proxy);
            };
            delete target[i];
        };
    };
    if (!isSetter && target[key]) {
        var proxy6 = target[key];
        var nodes7 = PROXYDELIMITERMAP.get(proxy6);
        var unmount8 = PROXYUNMOUNTMAP.get(proxy6);
        removeBetweenDelimiters(nodes7[0], nodes7[1], unmount8, proxy6);
        delete target[key];
    };
    if (isSetter) {
        if (!isIndex) {
            
            return Reflect.set(target, key, value, receiver);
        };
        if (!target.includes(value)) {
            var anchor = PROXYANCHORMAP.get(receiver);
            var parentNode9 = anchor.parentNode;
            var template = PROXYTEMPLATEMAP.get(receiver);
            var result = createBinding(value, template, receiver[SYMBOLROOT]);
            var proxy10 = result[0];
            var node = result[1];
            var previousProxy = target[key];
            var nextProxy = null;
            var _js11 = target.length - 1;
            for (var i = numkey + 1; i <= _js11; i += 1) {
                nextProxy = target[i];
                if (nextProxy) {
                    break;
                };
            };
            if (previousProxy) {
                if (!target.find(function (p, i) {
                    return p === previousProxy && i !== numkey;
                })) {
                    var nodes12 = PROXYDELIMITERMAP.get(previousProxy);
                    var unmount13 = PROXYUNMOUNTMAP.get(previousProxy);
                    removeBetweenDelimiters(nodes12[0], nodes12[1], unmount13, proxy10);
                };
            };
            if (nextProxy) {
                var nextAnchor = PROXYDELIMITERMAP.get(nextProxy)[0];
                parentNode9.insertBefore(node, nextAnchor);
            } else {
                var endNode = PROXYDELIMITERMAP.get(receiver)[1];
                parentNode9.insertBefore(node, endNode);
            };
            
            return Reflect.set(target, key, proxy10, receiver);
        } else {
            var otherIndex = target.findIndex(function (p, i) {
                return p === value && i !== numkey;
            });
            var otherProxy = target[key];
            var move = PROXYMOVEMAP.get(value);
            var nodes14 = PROXYDELIMITERMAP.get(value);
            var startNode = nodes14[0];
            var endNode15 = nodes14[1];
            if (otherProxy) {
                var otherNodes = PROXYDELIMITERMAP.get(otherProxy);
                var otherStartNode = otherNodes[0];
                var otherEndNode = otherNodes[1];
                var otherMove = PROXYMOVEMAP.get(otherProxy);
                var anchor16 = nodes14[1].nextSibling;
                var parentNode17 = anchor16.parentNode;
                if (value !== otherProxy) {
                    if (move) {
                        var node18 = startNode.nextSibling;
                        while (!node18.isSameNode(endNode15)) {
                            var oldNode = node18;
                            node18 = node18.nextSibling;
                            move.call(value, oldNode);
                        };
                    };
                    if (otherMove) {
                        var node19 = otherStartNode.nextSibling;
                        while (!node19.isSameNode(otherEndNode)) {
                            var oldNode20 = node19;
                            node19 = node19.nextSibling;
                            otherMove.call(otherProxy, oldNode20);
                        };
                    };
                    var node20 = startNode;
                    while (!node20.isSameNode(endNode15)) {
                        var oldNode21 = node20;
                        node20 = node20.nextSibling;
                        parentNode17.insertBefore(oldNode21, otherStartNode);
                    };
                    parentNode17.insertBefore(endNode15, otherStartNode);
                    var node21 = otherStartNode;
                    while (!node21.isSameNode(otherEndNode)) {
                        var oldNode22 = node21;
                        node21 = node21.nextSibling;
                        parentNode17.insertBefore(oldNode22, anchor16);
                    };
                    parentNode17.insertBefore(otherEndNode, anchor16);
                };
            } else {
                if (move) {
                    var node22 = startNode.nextSibling;
                    while (!node22.isSameNode(endNode15)) {
                        var oldNode23 = node22;
                        node22 = node22.nextSibling;
                        move.call(value, oldNode23);
                    };
                };
            };
            if (~(otherIndex)) {
                Reflect.set(target, otherIndex, otherProxy, receiver);
            };
        };
        Reflect.set(target, key, value, receiver);
    };
    
    return true;
};
/* (DEFUN SET-PROPERTY (TARGET KEY VALUE RECEIVER IS-INITIALIZING)
     (WHEN (AND (@ MAIN IS-DEFERRED) (NOT IS-INITIALIZING))
       (QUEUE-MICROTASK (LAMBDA () (SET-PROPERTY TARGET KEY VALUE RECEIVER T)))
       (RETURN-FROM SET-PROPERTY T))
     (WHEN (@ MAIN DEBUG) (CONSOLE-LOG 'SET-PROPERTY ARGUMENTS))
     (LET* ((CONTEXT (CHAIN *TARGET-CONTEXT-MAP* (GET TARGET)))
            (IS-SETTER (NOT (EQ VALUE UNDEFINED)))
            (IS-CHANGED (NOT (EQ (GETPROP TARGET KEY) VALUE)))
            (DESCRIPTORS (GETPROP CONTEXT KEY))
            (NODE NIL)
            (TYPE NIL)
            (RETURN-VALUE NIL)
            (HAS-RETURN-VALUE FALSE))
       (WHEN DESCRIPTORS
         (LOOP FOR DESCRIPTOR IN DESCRIPTORS
               DO (SETF NODE (@ DESCRIPTOR NODE)
                        TYPE (@ DESCRIPTOR TYPE)) (WHEN
                                                      (OR IS-CHANGED
                                                          IS-INITIALIZING)
                                                    (WHEN
                                                        (AND
                                                         (EQ (TYPEOF VALUE)
                                                             'FUNCTION)
                                                         (NOT
                                                          (EQ TYPE
                                                              *CONTEXT-EVENT*))
                                                         (NOT
                                                          HAS-RETURN-VALUE))
                                                      (SETF HAS-RETURN-VALUE T
                                                            RETURN-VALUE
                                                              (CHAIN VALUE
                                                                     (CALL
                                                                      RECEIVER))))
                                                    (IF (CHAIN
                                                         *PROPERTY-HANDLERS*
                                                         (HAS-OWN-PROPERTY
                                                          TYPE))
                                                        ((GETPROP
                                                          *PROPERTY-HANDLERS*
                                                          TYPE)
                                                         NODE
                                                         (@ DESCRIPTOR NAME)
                                                         (IF HAS-RETURN-VALUE
                                                             RETURN-VALUE
                                                             VALUE))
                                                        (PROGN
                                                         (WHEN
                                                             (EQ TYPE
                                                                 *CONTEXT-EVENT*)
                                                           (SET-EVENT TARGET
                                                            VALUE DESCRIPTOR
                                                            RECEIVER))
                                                         (WHEN
                                                             (EQ TYPE
                                                                 *CONTEXT-SLOT*)
                                                           (LET ((PROXY
                                                                  (SET-SLOT
                                                                   TARGET KEY
                                                                   (IF HAS-RETURN-VALUE
                                                                       RETURN-VALUE
                                                                       VALUE)
                                                                   RECEIVER
                                                                   DESCRIPTOR
                                                                   IS-INITIALIZING)))
                                                             (WHEN PROXY
                                                               (RETURN-FROM
                                                                   SET-PROPERTY
                                                                 (CHAIN
                                                                  *REFLECT
                                                                  (SET TARGET
                                                                       KEY
                                                                       PROXY
                                                                       RECEIVER))))))))) (WHEN
                                                                                             (AND
                                                                                              (EQ
                                                                                               TYPE
                                                                                               *CONTEXT-VALUE*)
                                                                                              (NOT
                                                                                               (@
                                                                                                DESCRIPTOR
                                                                                                IS-LISTENING)))
                                                                                           (CHAIN
                                                                                            NODE
                                                                                            (ADD-EVENT-LISTENER
                                                                                             'INPUT
                                                                                             (LAMBDA
                                                                                                 (
                                                                                                  EVENT)
                                                                                               (CHAIN
                                                                                                *REFLECT
                                                                                                (SET
                                                                                                 TARGET
                                                                                                 KEY
                                                                                                 (@
                                                                                                  EVENT
                                                                                                  TARGET
                                                                                                  VALUE)
                                                                                                 RECEIVER)))))
                                                                                           (SETF (@
                                                                                                  DESCRIPTOR
                                                                                                  IS-LISTENING)
                                                                                                   T))))
       (IF IS-SETTER
           (CHAIN *REFLECT (SET TARGET KEY VALUE RECEIVER))
           (CHAIN *REFLECT (DELETE-PROPERTY TARGET KEY))))) */
function setProperty(target, key, value, receiver, isInitializing) {
    if (main.isDeferred && !isInitializing) {
        queueMicrotask(function () {
            
            return setProperty(target, key, value, receiver, true);
        });
        
        return true;
    };
    if (main.debug) {
        console.log('setProperty', arguments);
    };
    var context = TARGETCONTEXTMAP.get(target);
    var isSetter = value !== undefined;
    var isChanged = target[key] !== value;
    var descriptors = context[key];
    var node = null;
    var type = null;
    var returnValue = null;
    var hasReturnValue = false;
    if (descriptors) {
        var _js24 = descriptors.length;
        for (var _js23 = 0; _js23 < _js24; _js23 += 1) {
            var descriptor = descriptors[_js23];
            node = descriptor.node;
            type = descriptor.type;
            if (isChanged || isInitializing) {
                if (typeof value === 'function' && type !== CONTEXTEVENT && !hasReturnValue) {
                    hasReturnValue = true;
                    returnValue = value.call(receiver);
                };
                if (PROPERTYHANDLERS.hasOwnProperty(type)) {
                    PROPERTYHANDLERS[type](node, descriptor.name, hasReturnValue ? returnValue : value);
                } else {
                    if (type === CONTEXTEVENT) {
                        setEvent(target, value, descriptor, receiver);
                    };
                    if (type === CONTEXTSLOT) {
                        var proxy = setSlot(target, key, hasReturnValue ? returnValue : value, receiver, descriptor, isInitializing);
                        if (proxy) {
                            
                            return Reflect.set(target, key, proxy, receiver);
                        };
                    };
                };
            };
            if (type === CONTEXTVALUE && !descriptor.isListening) {
                node.addEventListener('input', function (event) {
                    return Reflect.set(target, key, event.target.value, receiver);
                });
                descriptor.isListening = true;
            };
        };
    };
    
    return isSetter ? Reflect.set(target, key, value, receiver) : Reflect.deleteProperty(target, key);
};
/* (DEFUN SET-ATTRIBUTE (NODE NAME VALUE)
     (IF (NOT (OR (EQ VALUE NIL) (EQ VALUE UNDEFINED) (EQ VALUE FALSE)))
         (CHAIN NODE
                (SET-ATTRIBUTE NAME
                 (IF (EQ VALUE T)

                     VALUE)))
         (CHAIN NODE (REMOVE-ATTRIBUTE NAME)))) */
function setAttribute(node, name, value) {
    if (!(value === null || value === undefined || value === false)) {
        return node.setAttribute(name, value === true ? '' : value);
    } else {
        return node.removeAttribute(name);
    };
};
/* (DEFUN SET-DATA (NODE NAME VALUE)
     (IF (NOT (OR (EQ VALUE NIL) (EQ VALUE UNDEFINED)))
         (SETF (GETPROP (@ NODE DATASET) NAME) VALUE)
         (DELETE (GETPROP (@ NODE DATASET) NAME)))) */
function setData(node, name, value) {
    return !(value === null || value === undefined) ? (node.dataset[name] = value) : delete node.dataset[name];
};
/* (DEFUN SET-CLASS (NODE NAME VALUE)
     (IF VALUE
         (SETF (@ NODE CLASS-NAME)
                 (IF (CHAIN *ARRAY (IS-ARRAY VALUE))
                     (CHAIN VALUE (SPLIT  ))
                     VALUE))
         (CHAIN NODE (REMOVE-ATTRIBUTE 'CLASS)))) */
function setClass(node, name, value) {
    if (value) {
        return node.className = Array.isArray(value) ? value.split(' ') : value;
    } else {
        return node.removeAttribute('class');
    };
};
/* (DEFUN SET-CLASSLIST (NODE NAME VALUE)
     (IF VALUE
         (CHAIN NODE CLASS-LIST (ADD NAME))
         (CHAIN NODE CLASS-LIST (REMOVE NAME)))) */
function setClasslist(node, name, value) {
    return value ? node.classList.add(name) : node.classList.remove(name);
};
/* (DEFUN REMOVE-BETWEEN-DELIMITERS (START-NODE END-NODE UNMOUNT SELF)
     (LET* ((NODE START-NODE) (FIRST-NODE NODE))
       (SETF NODE (@ NODE NEXT-SIBLING))
       (CHAIN FIRST-NODE (REMOVE))
       (LOOP WHILE (NOT (CHAIN NODE (IS-SAME-NODE END-NODE)))
             DO (LET ((OLD-NODE NODE))
                  (SETF NODE (@ NODE NEXT-SIBLING))
                  (IF UNMOUNT
                      (LET ((UNMOUNT-VALUE
                             (CHAIN UNMOUNT (CALL SELF OLD-NODE))))
                        (IF (AND UNMOUNT-VALUE
                                 (EQ (TYPEOF (@ UNMOUNT-VALUE THEN))
                                     'FUNCTION))
                            (CHAIN UNMOUNT-VALUE
                                   (THEN
                                    (LAMBDA () (CHAIN OLD-NODE (REMOVE)))))
                            (CHAIN OLD-NODE (REMOVE))))
                      (CHAIN OLD-NODE (REMOVE)))))
       (CHAIN END-NODE (REMOVE)))
     (WHEN SELF (RECURSIVE-UNMOUNT SELF FALSE))
     NIL) */
function removeBetweenDelimiters(startNode, endNode, unmount, self) {
    var node = startNode;
    var firstNode = node;
    node = node.nextSibling;
    firstNode.remove();
    while (!node.isSameNode(endNode)) {
        (function () {
            var oldNode = node;
            node = node.nextSibling;
            if (unmount) {
                var unmountValue = unmount.call(self, oldNode);
                return unmountValue && typeof unmountValue.then === 'function' ? unmountValue.then(function () {
                    return oldNode.remove();
                }) : oldNode.remove();
            } else {
                return oldNode.remove();
            };
        })();
    };
    endNode.remove();
    if (self) {
        recursiveUnmount(self, false);
    };
    
    return null;
};
/* (DEFUN RECURSIVE-UNMOUNT (SELF SHOULD-UNMOUNT CYCLE-SET)
     (WHEN (NOT CYCLE-SET) (SETF CYCLE-SET (NEW (*WEAK-SET))))
     (CHAIN CYCLE-SET (ADD SELF))
     (LOOP FOR KEY OF SELF
           DO (LET ((VALUE (GETPROP SELF KEY)))
                (WHEN
                    (AND (EQ (TYPEOF VALUE) 'OBJECT) (NOT (EQ VALUE NIL))
                         (NOT (CHAIN CYCLE-SET (HAS VALUE))))
                  (RECURSIVE-UNMOUNT VALUE T CYCLE-SET))))
     (WHEN SHOULD-UNMOUNT
       (LET ((UNMOUNT (CHAIN *PROXY-UNMOUNT-MAP* (GET SELF))))
         (WHEN UNMOUNT (CHAIN UNMOUNT (CALL SELF)))))) */
function recursiveUnmount(self, shouldUnmount, cycleSet) {
    if (!cycleSet) {
        cycleSet = new WeakSet();
    };
    cycleSet.add(self);
    for (var key in self) {
        var value = self[key];
        if (typeof value === 'object' && value !== null && !cycleSet.has(value)) {
            recursiveUnmount(value, true, cycleSet);
        };
    };
    if (shouldUnmount) {
        var unmount = PROXYUNMOUNTMAP.get(self);
        
        return unmount ? unmount.call(self) : null;
    };
};
/* (DEFUN SET-SLOT (TARGET KEY VALUE RECEIVER DESCRIPTOR IS-INITIALIZING)
     (WHEN (@ MAIN DEBUG) (CONSOLE-LOG 'SET-SLOT ARGUMENTS))
     (WHEN
         (OR (NOT (OR (GETPROP TARGET KEY) VALUE IS-INITIALIZING))
             (AND (NOT (EQ VALUE UNDEFINED))
                  (NOT (EQ (TYPEOF VALUE) 'OBJECT))))
       (RETURN-FROM SET-SLOT))
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
            (IS-PREVIOUS-OBJECT
             (AND PREVIOUS-VALUE (EQ (TYPEOF PREVIOUS-VALUE) 'OBJECT)))
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
                 (WHEN NODES
                   (REMOVE-BETWEEN-DELIMITERS (@ NODES 0) (@ NODES 1) UNMOUNT
                    PREVIOUS-VALUE)))))
         (IF (AND VALUE (NOT PREVIOUS-VALUE))
             (REMOVE-BETWEEN-DELIMITERS (@ NODES 0) (@ NODES 1))
             (PROGN (CHAIN NODES 0 (REMOVE)) (CHAIN NODES 1 (REMOVE)))))
       (SETF (GETPROP HASH KEY) (LIST START-NODE END-NODE))
       (CHAIN PARENT-NODE (INSERT-BEFORE START-NODE ANCHOR))
       (IF VALUE
           (IF (OR (NOT PREVIOUS-VALUE) (NOT IS-PREVIOUS-OBJECT)
                   IS-TYPE-MISMATCH)
               (IF (CHAIN *ARRAY (IS-ARRAY VALUE))
                   (LET* ((RESULT
                           (CREATE-ARRAY VALUE TEMPLATE
                            (GETPROP RECEIVER *SYMBOL-ROOT*)))
                          (NODES (@ RESULT 0))
                          (PROXY (@ RESULT 1)))
                     (LOOP FOR NODE IN NODES
                           DO (CHAIN PARENT-NODE (INSERT-BEFORE NODE ANCHOR)))
                     (CHAIN *PROXY-ANCHOR-MAP* (SET PROXY ANCHOR))
                     (CHAIN *PROXY-DELIMITER-MAP*
                            (SET PROXY (GETPROP HASH KEY)))
                     (SETF RETURN-VALUE PROXY))
                   (LET* ((RESULT
                           (CREATE-BINDING VALUE TEMPLATE
                            (GETPROP RECEIVER *SYMBOL-ROOT*)))
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
                            (IF (AND PREV OBJ)
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
function setSlot(target, key, value, receiver, descriptor, isInitializing) {
    if (main.debug) {
        console.log('setSlot', arguments);
    };
    if (!(target[key] || value || isInitializing) || value !== undefined && typeof value !== 'object') {
        return;
    };
    var anchor = descriptor.node;
    var slot25 = descriptor.slot;
    var template26 = descriptor.template;
    var hash = TARGETDELIMITERMAP.get(target);
    var nodes = hash[key];
    var parentNode27 = anchor.parentNode;
    var startNode = createAnchor(0, key);
    var endNode = createAnchor(1, key);
    var previousValue = target[key];
    var isPreviousArray = Array.isArray(previousValue);
    var isPreviousObject = previousValue && typeof previousValue === 'object';
    var isValueArray = Array.isArray(value);
    var isTypeMismatch = isPreviousArray !== isValueArray;
    var returnValue = null;
    if (nodes && (!value || value && !previousValue || isTypeMismatch)) {
        if (Array.isArray(previousValue)) {
            var _js29 = previousValue.length;
            for (var _js28 = 0; _js28 < _js29; _js28 += 1) {
                var proxy = previousValue[_js28];
                var unmount = PROXYUNMOUNTMAP.get(proxy);
                var nodes30 = PROXYDELIMITERMAP.get(proxy);
                if (nodes30) {
                    removeBetweenDelimiters(nodes30[0], nodes30[1], unmount, proxy);
                };
            };
        } else {
            if (previousValue) {
                var unmount30 = PROXYUNMOUNTMAP.get(previousValue);
                var nodes31 = PROXYDELIMITERMAP.get(previousValue);
                if (nodes31) {
                    removeBetweenDelimiters(nodes31[0], nodes31[1], unmount30, previousValue);
                };
            };
        };
        if (value && !previousValue) {
            removeBetweenDelimiters(nodes[0], nodes[1]);
        } else {
            nodes[0].remove();
            nodes[1].remove();
        };
    };
    hash[key] = [startNode, endNode];
    parentNode27.insertBefore(startNode, anchor);
    if (value) {
        if (!previousValue || !isPreviousObject || isTypeMismatch) {
            if (Array.isArray(value)) {
                var result = createArray(value, template26, receiver[SYMBOLROOT]);
                var nodes32 = result[0];
                var proxy33 = result[1];
                var _js35 = nodes32.length;
                for (var _js34 = 0; _js34 < _js35; _js34 += 1) {
                    var node = nodes32[_js34];
                    parentNode27.insertBefore(node, anchor);
                };
                PROXYANCHORMAP.set(proxy33, anchor);
                PROXYDELIMITERMAP.set(proxy33, hash[key]);
                returnValue = proxy33;
            } else {
                var result36 = createBinding(value, template26, receiver[SYMBOLROOT]);
                var proxy37 = result36[0];
                var node38 = result36[1];
                parentNode27.insertBefore(node38, anchor);
                returnValue = proxy37;
            };
        } else {
            var previousValues = isPreviousArray ? previousValue : [previousValue];
            var values = isValueArray ? value : [value];
            var _js39 = values.length - 1;
            for (var i = 0; i <= _js39; i += 1) {
                var prev = previousValues[i];
                var obj = values[i];
                if (prev && obj) {
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
        var _js40 = slot25.childNodes;
        var _js42 = _js40.length;
        for (var _js41 = 0; _js41 < _js42; _js41 += 1) {
            var node43 = _js40[_js41];
            parentNode27.insertBefore(node43.cloneNode(true), anchor);
        };
    };
    parentNode27.insertBefore(endNode, anchor);
    
    return returnValue;
};
/* (DEFUN SET-EVENT (TARGET VALUE DESCRIPTOR RECEIVER)
     (LET* ((NODE (@ DESCRIPTOR NODE))
            (EVENT (@ DESCRIPTOR EVENT))
            (HASH (CHAIN *TARGET-EVENT-MAP* (GET TARGET)))
            (LISTENERS (GETPROP HASH EVENT)))
       (WHEN (NOT LISTENERS)
         (SETF LISTENERS (LIST)
               (GETPROP HASH EVENT) LISTENERS))
       (LOOP FOR LISTENER IN LISTENERS
             DO (CHAIN NODE
                       (REMOVE-EVENT-LISTENER EVENT LISTENER
                        (@ LISTENER OPTIONS))))
       (WHEN VALUE
         (SETF (@ VALUE IS-EVENT-LISTENER) T)
         (LET ((BOUND-LISTENER (CHAIN VALUE (BIND RECEIVER))))
           (SETF (@ BOUND-LISTENER OPTIONS) (@ VALUE OPTIONS))
           (CHAIN NODE
                  (ADD-EVENT-LISTENER EVENT BOUND-LISTENER
                   (@ BOUND-LISTENER OPTIONS)))
           (CHAIN LISTENERS (PUSH BOUND-LISTENER)))))) */
function setEvent(target, value, descriptor, receiver) {
    var node43 = descriptor.node;
    var event44 = descriptor.event;
    var hash = TARGETEVENTMAP.get(target);
    var listeners = hash[event44];
    if (!listeners) {
        listeners = [];
        hash[event44] = listeners;
    };
    var _js46 = listeners.length;
    for (var _js45 = 0; _js45 < _js46; _js45 += 1) {
        var listener = listeners[_js45];
        node43.removeEventListener(event44, listener, listener.options);
    };
    if (value) {
        value.isEventListener = true;
        var boundListener = value.bind(receiver);
        boundListener.options = value.options;
        node43.addEventListener(event44, boundListener, boundListener.options);
        return listeners.push(boundListener);
    };
};
/* (DEFUN PROCESS-TEMPLATE (TEMPLATE)
     (LET* ((ROOT (OR (@ TEMPLATE CONTENT) TEMPLATE))
            (CLONE (CHAIN ROOT (CLONE-NODE T)))
            (CONTEXT (CREATE)))
       (DEFUN WALK (PARENT-NODE PATH)
         (LOOP FOR I FROM 0 TO (- (LENGTH (@ PARENT-NODE CHILD-NODES)) 1)
               DO (LET ((NODE (GETPROP (@ PARENT-NODE CHILD-NODES) I)))
                    (WHEN
                        (NOT
                         (EQ (@ NODE NODE-TYPE)
                             (@ MAIN WINDOW *NODE ELEMENT_NODE)))
                      (CONTINUE))
                    (WHEN
                        (OR (EQ (@ NODE TAG-NAME) *TAG-SLOT*)
                            (@ NODE DATASET KEY))
                      (LET* ((SLOT-NAME
                              (OR (@ NODE DATASET KEY)
                                  (CHAIN NODE (GET-ATTRIBUTE 'NAME))))
                             (ANCHOR (CREATE-ANCHOR 2 SLOT-NAME))
                             (TEMPLATE-SELECTOR (@ NODE DATASET TEMPLATE))
                             (TEMPLATE-NODE
                              (IF TEMPLATE-SELECTOR
                                  (OR
                                   (GETPROP *TEMPLATES-HASH* TEMPLATE-SELECTOR)
                                   (CHAIN MAIN WINDOW DOCUMENT
                                          (QUERY-SELECTOR TEMPLATE-SELECTOR)))
                                  NODE)))
                        (WHEN (EQ SLOT-NAME UNDEFINED)
                          (THROW
                              (NEW
                               (*TYPE-ERROR
                                Missing `name` or `data-key` for slot.))))
                        (DELETE (@ NODE DATASET KEY))
                        (WHEN
                            (AND (NOT TEMPLATE-SELECTOR)
                                 (EQ (@ NODE TAG-NAME) *TAG-SLOT*))
                          (SETF TEMPLATE-NODE
                                  (CHAIN MAIN WINDOW DOCUMENT
                                         (CREATE-DOCUMENT-FRAGMENT)))
                          (LOOP WHILE (@ NODE FIRST-CHILD)
                                DO (LET ((CHILD-NODE (@ NODE FIRST-CHILD)))
                                     (CHAIN TEMPLATE-NODE
                                            (APPEND-CHILD CHILD-NODE)))))
                        (CHAIN PARENT-NODE (INSERT-BEFORE ANCHOR NODE))
                        (IF (NOT (CHAIN CONTEXT (HAS-OWN-PROPERTY SLOT-NAME)))
                            (SETF (GETPROP CONTEXT SLOT-NAME) (LIST))
                            (THROW
                                (NEW
                                 (*ERROR
                                  (+ The key " SLOT-NAME
                                     " was used in a template
                                     more than once, which is not allowed.)))))
                        (CHAIN (GETPROP CONTEXT SLOT-NAME)
                               (PUSH
                                (CREATE PATH (CHAIN PATH (CONCAT I)) SLOT
                                 (IF TEMPLATE-SELECTOR
                                     NODE
                                     (CHAIN MAIN WINDOW DOCUMENT
                                            (CREATE-ELEMENT 'DIV)))
                                 TEMPLATE TEMPLATE-NODE TYPE *CONTEXT-SLOT*)))
                        (CHAIN NODE (REMOVE)))
                      (CONTINUE))
                    (WHEN (@ NODE FIRST-CHILD)
                      (WALK NODE (CHAIN PATH (CONCAT I))))
                    (LOOP FOR KEY OF (@ NODE DATASET)
                          DO (LET ((VALUE (GETPROP (@ NODE DATASET) KEY))
                                   (RESULT NIL))
                               (CASE KEY
                                 (text
                                  (SETF RESULT (CREATE TYPE *CONTEXT-TEXT*)))
                                 (value
                                  (SETF RESULT (CREATE TYPE *CONTEXT-VALUE*)))
                                 (class
                                  (SETF RESULT (CREATE TYPE *CONTEXT-CLASS*)))
                                 (unsafeHtml
                                  (SETF RESULT (CREATE TYPE *CONTEXT-HTML*))))
                               (WHEN (CHAIN KEY (STARTS-WITH 'ATTRIBUTE))
                                 (SETF RESULT
                                         (CREATE TYPE *CONTEXT-ATTRIBUTE* NAME
                                          (CHAIN KEY (SLICE 9)
                                                 (TO-LOWER-CASE)))))
                               (WHEN (CHAIN KEY (STARTS-WITH 'STYLE))
                                 (SETF RESULT
                                         (CREATE TYPE *CONTEXT-STYLE-PROPERTY*
                                          NAME
                                          (CHAIN KEY (SLICE 5)
                                                 (REPLACE-ALL (REGEX /[A-Z]/g)
                                                  (LAMBDA (V)
                                                    (+ -
                                                       (CHAIN V
                                                              (TO-LOWER-CASE)))))
                                                 (SLICE 1)))))
                               (WHEN (CHAIN KEY (STARTS-WITH 'CLASSLIST))
                                 (SETF RESULT
                                         (CREATE TYPE *CONTEXT-CLASSLIST* NAME
                                          (+ (CHAIN KEY 9 (TO-LOWER-CASE))
                                             (CHAIN KEY (SLICE 10))))))
                               (WHEN (CHAIN KEY (STARTS-WITH 'EVENT))
                                 (SETF RESULT
                                         (CREATE TYPE *CONTEXT-EVENT* EVENT
                                          (CHAIN KEY (SLICE 5)
                                                 (TO-LOWER-CASE)))))
                               (WHEN (NOT RESULT)
                                 (SETF RESULT
                                         (CREATE TYPE *CONTEXT-DATA* NAME KEY))
                                 (WHEN (NOT VALUE) (SETF VALUE KEY)))
                               (WHEN RESULT
                                 (DELETE (GETPROP (@ NODE DATASET) KEY))
                                 (CHAIN NODE (REMOVE-ATTRIBUTE KEY))
                                 (SETF (@ RESULT PATH) (CHAIN PATH (CONCAT I)))
                                 (WHEN
                                     (NOT
                                      (CHAIN CONTEXT (HAS-OWN-PROPERTY VALUE)))
                                   (SETF (GETPROP CONTEXT VALUE) (LIST)))
                                 (CHAIN (GETPROP CONTEXT VALUE)
                                        (PUSH RESULT))))))))
       (WALK CLONE (LIST))
       (CHAIN *TEMPLATE-PROCESSED-MAP* (SET TEMPLATE CLONE))
       (CHAIN *TEMPLATE-CONTEXT-MAP* (SET TEMPLATE CONTEXT)))) */
function processTemplate(template) {
    var root = template.content || template;
    var clone = root.cloneNode(true);
    var context = {  };
    function walk(parentNode, path) {
        var _js47 = parentNode.childNodes.length - 1;
        for (var i = 0; i <= _js47; i += 1) {
            var node = parentNode.childNodes[i];
            if (node.nodeType !== main.window.Node['ELEMENT_NODE']) {
                continue;
            };
            if (node.tagName === TAGSLOT || node.dataset.key) {
                var slotName = node.dataset.key || node.getAttribute('name');
                var anchor = createAnchor(2, slotName);
                var templateSelector = node.dataset.template;
                var templateNode = templateSelector ? TEMPLATESHASH[templateSelector] || main.window.document.querySelector(templateSelector) : node;
                if (slotName === undefined) {
                    throw new TypeError('Missing `name` or `data-key` for slot.');
                };
                delete node.dataset.key;
                if (!templateSelector && node.tagName === TAGSLOT) {
                    templateNode = main.window.document.createDocumentFragment();
                    while (node.firstChild) {
                        var childNode = node.firstChild;
                        templateNode.appendChild(childNode);
                    };
                };
                parentNode.insertBefore(anchor, node);
                if (!context.hasOwnProperty(slotName)) {
                    context[slotName] = [];
                } else {
                    throw new Error('The key \"' + slotName + '\" was used in a template ' + 'more than once, which is not allowed.');
                };
                context[slotName].push({ path : path.concat(i),
                                         slot : templateSelector ? node : main.window.document.createElement('div'),
                                         template : templateNode,
                                         type : CONTEXTSLOT
                                       });
                node.remove();
                continue;
            };
            if (node.firstChild) {
                walk(node, path.concat(i));
            };
            for (var key in node.dataset) {
                var value = node.dataset[key];
                var result = null;
                switch (key) {
                case 'text':
                    result = { type : CONTEXTTEXT };
                    break;
                case 'value':
                    result = { type : CONTEXTVALUE };
                    break;
                case 'class':
                    result = { type : CONTEXTCLASS };
                    break;
                case 'unsafeHtml':
                    result = { type : CONTEXTHTML };
                };
                if (key.startsWith('attribute')) {
                    result = { type : CONTEXTATTRIBUTE, name : key.slice(9).toLowerCase() };
                };
                if (key.startsWith('style')) {
                    result = { type : CONTEXTSTYLEPROPERTY, name : key.slice(5).replaceAll(/[A-Z]/g, function (v) {
                        return '-' + v.toLowerCase();
                    }).slice(1) };
                };
                if (key.startsWith('classlist')) {
                    result = { type : CONTEXTCLASSLIST, name : key[9].toLowerCase() + key.slice(10) };
                };
                if (key.startsWith('event')) {
                    result = { type : CONTEXTEVENT, event : key.slice(5).toLowerCase() };
                };
                if (!result) {
                    result = { type : CONTEXTDATA, name : key };
                    if (!value) {
                        value = key;
                    };
                };
                if (result) {
                    delete node.dataset[key];
                    node.removeAttribute(key);
                    result.path = path.concat(i);
                    if (!context.hasOwnProperty(value)) {
                        context[value] = [];
                    };
                    context[value].push(result);
                };
            };
        };
    };
    walk(clone, []);
    TEMPLATEPROCESSEDMAP.set(template, clone);
    
    return TEMPLATECONTEXTMAP.set(template, context);
};
/* (DEFUN CREATE-CONTEXT (CLONE TEMPLATE)
     (LET ((CONTEXT (CHAIN *TEMPLATE-CONTEXT-MAP* (GET TEMPLATE)))
           (CLONED-CONTEXT (CREATE)))
       (LOOP FOR KEY OF CONTEXT
             DO (SETF (GETPROP CLONED-CONTEXT KEY)
                        (LIST)) (LOOP FOR DESCRIPTOR IN (GETPROP CONTEXT KEY)
                                      DO (LET* ((PATH (@ DESCRIPTOR PATH))
                                                (NODE (GET-PATH CLONE PATH)))
                                           (CHAIN (GETPROP CLONED-CONTEXT KEY)
                                                  (PUSH
                                                   (CHAIN *OBJECT
                                                          (ASSIGN
                                                           (CREATE NODE NODE)
                                                           DESCRIPTOR)))))))
       CLONED-CONTEXT)) */
function createContext(clone, template) {
    var context = TEMPLATECONTEXTMAP.get(template);
    var clonedContext = {  };
    for (var key in context) {
        clonedContext[key] = [];
        var _js48 = context[key];
        var _js50 = _js48.length;
        for (var _js49 = 0; _js49 < _js50; _js49 += 1) {
            var descriptor = _js48[_js49];
            var path51 = descriptor.path;
            var node = getPath(clone, path51);
            clonedContext[key].push(Object.assign({ node : node }, descriptor));
        };
    };
    
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
    var _js48 = path.length - 1;
    for (var i = 0; i <= _js48; i += 1) {
        var j = path[i];
        result = result.childNodes[j];
    };
    return result;
};
/* (DEFUN CREATE-ARRAY (ARRAY TEMPLATE ROOT)
     (LET* ((NODES (LIST)) (PROXIES (LIST)) (PROXY NIL))
       (LOOP FOR ITEM IN ARRAY
             DO (LET ((RESULT (CREATE-BINDING ITEM TEMPLATE ROOT)))
                  (CHAIN PROXIES (PUSH (@ RESULT 0)))
                  (CHAIN NODES (PUSH (@ RESULT 1)))))
       (SETF PROXY (NEW (*PROXY PROXIES *PROXY-ARRAY*)))
       (SETF (GETPROP PROXY *SYMBOL-ROOT*) ROOT)
       (CHAIN *PROXY-TEMPLATE-MAP* (SET PROXY TEMPLATE))
       (LIST NODES PROXY))) */
function createArray(array, template, root) {
    var nodes = [];
    var proxies = [];
    var proxy = null;
    var _js50 = array.length;
    for (var _js49 = 0; _js49 < _js50; _js49 += 1) {
        var item = array[_js49];
        var result = createBinding(item, template, root);
        proxies.push(result[0]);
        nodes.push(result[1]);
    };
    proxy = new Proxy(proxies, PROXYARRAY);
    proxy[SYMBOLROOT] = root;
    PROXYTEMPLATEMAP.set(proxy, template);
    
    return [nodes, proxy];
};
/* (DEFUN CREATE-BINDING (OBJ TEMPLATE ROOT)
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
            (MOVE (GETPROP OBJ *SYMBOL-MOVE*))
            (FRAGMENT (CHAIN MAIN WINDOW DOCUMENT (CREATE-DOCUMENT-FRAGMENT))))
       (WHEN UNMOUNT (CHAIN *PROXY-UNMOUNT-MAP* (SET PROXY UNMOUNT)))
       (WHEN MOVE (CHAIN *PROXY-MOVE-MAP* (SET PROXY MOVE)))
       (CHAIN *TARGET-CONTEXT-MAP* (SET TARGET CONTEXT))
       (CHAIN *TARGET-EVENT-MAP* (SET TARGET (CREATE)))
       (CHAIN *TARGET-DELIMITER-MAP* (SET TARGET (CREATE)))
       (SETF (GETPROP PROXY *SYMBOL-ROOT*) (OR ROOT PROXY))
       (SETF (GETPROP PROXY *SYMBOL-TARGET*) OBJ)
       (LOOP FOR KEY OF OBJ
             DO (WHEN (CHAIN CONTEXT (HAS-OWN-PROPERTY KEY))
                  (CONTINUE)) (SETF (GETPROP TARGET KEY) (GETPROP OBJ KEY)))
       (LOOP FOR KEY OF CONTEXT
             DO (SET-PROPERTY TARGET KEY (GETPROP OBJ KEY) PROXY T))
       (WHEN MOUNT (CHAIN MOUNT (CALL PROXY CLONE)))
       (CHAIN FRAGMENT (APPEND-CHILD START-NODE))
       (CHAIN FRAGMENT (APPEND-CHILD CLONE))
       (CHAIN FRAGMENT (APPEND-CHILD END-NODE))
       (CHAIN *PROXY-DELIMITER-MAP* (SET PROXY NODES))
       (LIST PROXY FRAGMENT))) */
function createBinding(obj, template, root) {
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
    var move = obj[SYMBOLMOVE];
    var fragment = main.window.document.createDocumentFragment();
    if (unmount) {
        PROXYUNMOUNTMAP.set(proxy, unmount);
    };
    if (move) {
        PROXYMOVEMAP.set(proxy, move);
    };
    TARGETCONTEXTMAP.set(target, context);
    TARGETEVENTMAP.set(target, {  });
    TARGETDELIMITERMAP.set(target, {  });
    proxy[SYMBOLROOT] = root || proxy;
    proxy[SYMBOLTARGET] = obj;
    for (var key in obj) {
        if (context.hasOwnProperty(key)) {
            continue;
        };
        target[key] = obj[key];
    };
    for (var key in context) {
        setProperty(target, key, obj[key], proxy, true);
    };
    if (mount) {
        mount.call(proxy, clone);
    };
    fragment.appendChild(startNode);
    fragment.appendChild(clone);
    fragment.appendChild(endNode);
    PROXYDELIMITERMAP.set(proxy, nodes);
    
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
           (CHAIN MAIN WINDOW DOCUMENT (CREATE-COMMENT COMMENT)))
         (CHAIN MAIN WINDOW DOCUMENT (CREATE-TEXT-NODE )))) */
function createAnchor(type, key) {
    if (main.debug) {
        var comment = (type === 0 ? 'start' : (type === 1 ? 'end' : 'anchor')) + ' ' + key;
        return main.window.document.createComment(comment);
    } else {
        return main.window.document.createTextNode('');
    };
};
/* (DEFUN REGISTER-TEMPLATE (NAME TEMPLATE)
     (WHEN (EQ (TYPEOF TEMPLATE) 'STRING)
       (LET ((ELEMENT (CHAIN MAIN WINDOW DOCUMENT (CREATE-ELEMENT 'TEMPLATE))))
         (SETF (@ ELEMENT INNER-H-T-M-L) TEMPLATE
               TEMPLATE ELEMENT)))
     (SETF (GETPROP *TEMPLATES-HASH* NAME) TEMPLATE)) */
function registerTemplate(name, template) {
    if (typeof template === 'string') {
        var element = main.window.document.createElement('template');
        element.innerHTML = template;
        template = element;
    };
    return TEMPLATESHASH[name] = template;
};
/* (DEFUN MAIN (ROOT-STATE TEMPLATE)
     (WHEN (CHAIN *TEMPLATES-HASH* (HAS-OWN-PROPERTY TEMPLATE))
       (SETF TEMPLATE (GETPROP *TEMPLATES-HASH* TEMPLATE)))
     (LET ((BINDINGS (CREATE-BINDING ROOT-STATE TEMPLATE)))
       (WHEN
           (AND (GETPROP ROOT-STATE *SYMBOL-UNMOUNT*)
                (@ MAIN SHOULD-UNMOUNT-ROOT))
         (OBSERVE-UNMOUNT BINDINGS))
       BINDINGS)) */
function main(rootState, template) {
    if (TEMPLATESHASH.hasOwnProperty(template)) {
        template = TEMPLATESHASH[template];
    };
    var bindings = createBinding(rootState, template);
    if (rootState[SYMBOLUNMOUNT] && main.shouldUnmountRoot) {
        observeUnmount(bindings);
    };
    
    return bindings;
};
/* (DEFUN OBSERVE-UNMOUNT (BINDINGS)
     (LET* ((PROXY (ELT BINDINGS 0))
            (FRAGMENT (ELT BINDINGS 1))
            (FRAGMENT-NODES (CHAIN *ARRAY (FROM (@ FRAGMENT CHILD-NODES))))
            (OBSERVE-FN
             (LAMBDA (MUTATIONS OBSERVER)
               (LOOP FOR MUTATION IN MUTATIONS
                     DO (LOOP FOR NODE IN (@ MUTATION REMOVED-NODES)
                              DO (LOOP FOR I FROM (- (LENGTH FRAGMENT-NODES)
                                                     1) DOWNTO 0
                                       DO (LET ((FRAGMENT-NODE
                                                 (ELT FRAGMENT-NODES I)))
                                            (WHEN
                                                (CHAIN NODE
                                                       (CONTAINS
                                                        FRAGMENT-NODE))
                                              (CHAIN FRAGMENT-NODES
                                                     (SPLICE I 1)))))))
               (WHEN (EQ (LENGTH FRAGMENT-NODES) 0)
                 (RECURSIVE-UNMOUNT PROXY T)
                 (CHAIN OBSERVER (DISCONNECT)))))
            (OBSERVER
             (NEW (CHAIN MAIN WINDOW (*MUTATION-OBSERVER OBSERVE-FN)))))
       (CHAIN OBSERVER
              (OBSERVE (@ MAIN WINDOW DOCUMENT DOCUMENT-ELEMENT)
               (CREATE CHILD-LIST T SUBTREE T))))) */
function observeUnmount(bindings) {
    var proxy = bindings[0];
    var fragment = bindings[1];
    var fragmentNodes = Array.from(fragment.childNodes);
    var observeFn = function (mutations, observer) {
        var _js52 = mutations.length;
        for (var _js51 = 0; _js51 < _js52; _js51 += 1) {
            var mutation = mutations[_js51];
            var _js53 = mutation.removedNodes;
            var _js55 = _js53.length;
            for (var _js54 = 0; _js54 < _js55; _js54 += 1) {
                var node = _js53[_js54];
                for (var i = fragmentNodes.length - 1; i >= 0; i -= 1) {
                    var fragmentNode = fragmentNodes[i];
                    if (node.contains(fragmentNode)) {
                        fragmentNodes.splice(i, 1);
                    };
                };
            };
        };
        if (fragmentNodes.length === 0) {
            recursiveUnmount(proxy, true);
            
            return observer.disconnect();
        };
    };
    var observer = new main.window.MutationObserver(observeFn);
    return observer.observe(main.window.document.documentElement, { childList : true, subtree : true });
};
/* (SETF (@ MAIN DEBUG) FALSE
         (@ MAIN IS-DEFERRED) FALSE
         (@ MAIN SHOULD-UNMOUNT-ROOT) T
         (@ MAIN WINDOW)
           (IF (NOT (EQ (TYPEOF WINDOW) 'UNDEFINED))
               WINDOW
               NIL)) */
main.debug = false;
main.isDeferred = false;
main.shouldUnmountRoot = true;
main.window = typeof window !== 'undefined' ? window : null;
/* (EXPORT DEFAULT MAIN NAMES
           ((*SYMBOL-MOUNT* MOUNT) (*SYMBOL-UNMOUNT* UNMOUNT)
            (*SYMBOL-MOVE* MOVE) (*SYMBOL-ROOT* ROOT) (*SYMBOL-TARGET* TARGET)
            REGISTER-TEMPLATE)) */
export { SYMBOLMOUNT as mount, SYMBOLUNMOUNT as unmount, SYMBOLMOVE as move, SYMBOLROOT as root, SYMBOLTARGET as target, registerTemplate, };
export default main;

