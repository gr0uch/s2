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
/* (DEFVAR *SYMBOL-EVENT* (*SYMBOL 'EVENT)) */
if ('undefined' === typeof SYMBOLEVENT) {
    var SYMBOLEVENT = Symbol('event');
};
/* (DEFVAR *TAG-SLOT* '*SLOT*) */
if ('undefined' === typeof TAGSLOT) {
    var TAGSLOT = 'SLOT';
};
/* (DEFVAR *TARGET-CONTEXT-MAP* (NEW (*WEAK-MAP))) */
if ('undefined' === typeof TARGETCONTEXTMAP) {
    var TARGETCONTEXTMAP = new WeakMap();
};
/* (DEFVAR *TARGET-EVENT-MAP* (NEW (*WEAK-MAP))) */
if ('undefined' === typeof TARGETEVENTMAP) {
    var TARGETEVENTMAP = new WeakMap();
};
/* (DEFVAR *TARGET-NODE-MAP* (NEW (*WEAK-MAP))) */
if ('undefined' === typeof TARGETNODEMAP) {
    var TARGETNODEMAP = new WeakMap();
};
/* (DEFVAR *PROXY-NODE-MAP* (NEW (*WEAK-MAP))) */
if ('undefined' === typeof PROXYNODEMAP) {
    var PROXYNODEMAP = new WeakMap();
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
     (WHEN (@ MAIN DEBUG) (CHAIN CONSOLE (LOG 'SET-INDEX ARGUMENTS)))
     (LET* ((NUMKEY (CHAIN *NUMBER (PARSE-INT KEY 10)))
            (IS-INDEX (NOT (CHAIN *NUMBER (IS-NA-N NUMKEY))))
            (IS-SETTER (EQ (LENGTH ARGUMENTS) 4))
            (IS-DELETE (EQ (LENGTH ARGUMENTS) 2)))
       (WHEN (EQ KEY 'LENGTH)
         (LOOP FOR I FROM VALUE TO (- (LENGTH TARGET) 1)
               DO (LET ((NODES
                         (CHAIN *PROXY-NODE-MAP* (GET (GETPROP TARGET I)))))
                    (WHEN NODES
                      (REMOVE-BETWEEN-DELIMITERS (@ NODES 0)
                       (@ NODES 1)))) (DELETE (GETPROP TARGET I))))
       (WHEN IS-DELETE
         (LET ((NODES (CHAIN *PROXY-NODE-MAP* (GET (GETPROP TARGET KEY)))))
           (REMOVE-BETWEEN-DELIMITERS (@ NODES 0) (@ NODES 1)))
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
                   (LET ((NODES (CHAIN *PROXY-NODE-MAP* (GET PREVIOUS-PROXY))))
                     (REMOVE-BETWEEN-DELIMITERS (@ NODES 0) (@ NODES 1)))))
               (IF NEXT-PROXY
                   (LET ((NEXT-ANCHOR
                          (@ (CHAIN *PROXY-NODE-MAP* (GET NEXT-PROXY)) 0)))
                     (CHAIN PARENT-NODE (INSERT-BEFORE NODE NEXT-ANCHOR)))
                   (LET ((END-NODE
                          (@ (CHAIN *PROXY-NODE-MAP* (GET RECEIVER)) 1)))
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
                         (CHAIN *PROXY-NODE-MAP* (GET OTHER-PROXY)))
                        (OTHER-START-NODE (@ OTHER-NODES 0))
                        (OTHER-END-NODE (@ OTHER-NODES 1))
                        (NODES (CHAIN *PROXY-NODE-MAP* (GET VALUE)))
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
            var nodes = PROXYNODEMAP.get(target[i]);
            if (nodes) {
                removeBetweenDelimiters(nodes[0], nodes[1]);
            };
            delete target[i];
        };
    };
    if (isDelete) {
        var nodes2 = PROXYNODEMAP.get(target[key]);
        removeBetweenDelimiters(nodes2[0], nodes2[1]);
        delete target[key];
    };
    if (isSetter && !isIndex) {
        __PS_MV_REG = [];
        return Reflect.set(target, key, value, receiver);
    };
    if (isSetter) {
        if (!target.includes(value)) {
            var anchor = PROXYANCHORMAP.get(receiver);
            var parentNode3 = anchor.parentNode;
            var template = PROXYTEMPLATEMAP.get(receiver);
            var result = createBinding(value, template);
            var node = result[0];
            var proxy = result[1];
            var previousProxy = target[key];
            var nextProxy = null;
            var _js4 = target.length - 1;
            for (var i = numkey + 1; i <= _js4; i += 1) {
                nextProxy = target[i];
                if (nextProxy) {
                    break;
                };
            };
            if (previousProxy) {
                if (!target.find(function (p, i) {
                    return p === previousProxy && i !== numkey;
                })) {
                    var nodes5 = PROXYNODEMAP.get(previousProxy);
                    removeBetweenDelimiters(nodes5[0], nodes5[1]);
                };
            };
            if (nextProxy) {
                var nextAnchor = PROXYNODEMAP.get(nextProxy)[0];
                parentNode3.insertBefore(node, nextAnchor);
            } else {
                var endNode = PROXYNODEMAP.get(receiver)[1];
                parentNode3.insertBefore(node, endNode);
            };
            __PS_MV_REG = [];
            return Reflect.set(target, key, proxy, receiver);
        } else {
            var otherIndex = target.findIndex(function (p, i) {
                return p === value && i !== numkey;
            });
            var otherProxy = target[key];
            if (otherProxy && value !== otherProxy) {
                var otherNodes = PROXYNODEMAP.get(otherProxy);
                var otherStartNode = otherNodes[0];
                var otherEndNode = otherNodes[1];
                var nodes6 = PROXYNODEMAP.get(value);
                var startNode = nodes6[0];
                var endNode7 = nodes6[1];
                var anchor8 = nodes6[1].nextSibling;
                var parentNode9 = anchor8.parentNode;
                var node10 = startNode;
                while (!node10.isSameNode(endNode7)) {
                    var oldNode = node10;
                    node10 = node10.nextSibling;
                    parentNode9.insertBefore(oldNode, otherStartNode);
                };
                parentNode9.insertBefore(endNode7, otherStartNode);
                var node11 = otherStartNode;
                while (!node11.isSameNode(otherEndNode)) {
                    var oldNode12 = node11;
                    node11 = node11.nextSibling;
                    parentNode9.insertBefore(oldNode12, anchor8);
                };
                parentNode9.insertBefore(otherEndNode, anchor8);
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
        var node12 = descriptor.node;
        var type13 = descriptor.type;
        if (type13 === SYMBOLTEXT && value !== node12.textContent) {
            node12.textContent = value;
        };
        if (type13 === SYMBOLHTML && value !== node12.innerHTML) {
            node12.innerHTML = value;
        };
        if (type13 === SYMBOLEVENT) {
            setEvent(target, value, descriptor, receiver);
        };
        if (type13 === SYMBOLSLOT) {
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
/* (DEFUN REMOVE-BETWEEN-DELIMITERS (START-NODE END-NODE)
     (LET ((NODE START-NODE))
       (LOOP WHILE (NOT (CHAIN NODE (IS-SAME-NODE END-NODE)))
             DO (LET ((OLD-NODE NODE))
                  (SETF NODE (@ NODE NEXT-SIBLING))
                  (CHAIN OLD-NODE (REMOVE))))
       (CHAIN END-NODE (REMOVE)))) */
function removeBetweenDelimiters(startNode, endNode) {
    var node = startNode;
    while (!node.isSameNode(endNode)) {
        var oldNode = node;
        node = node.nextSibling;
        oldNode.remove();
    };
    return endNode.remove();
};
/* (DEFUN SET-SLOT (TARGET KEY VALUE DESCRIPTOR)
     (LET* ((ANCHOR (@ DESCRIPTOR ANCHOR))
            (SLOT (@ DESCRIPTOR SLOT))
            (TEMPLATE (@ DESCRIPTOR TEMPLATE))
            (HASH (CHAIN *TARGET-NODE-MAP* (GET TARGET)))
            (NODES (GETPROP HASH KEY))
            (PARENT-NODE (@ ANCHOR PARENT-NODE))
            (START-NODE (CREATE-ANCHOR 0 KEY))
            (END-NODE (CREATE-ANCHOR 1 KEY))
            (RETURN-VALUE NIL))
       (WHEN NODES
         (REMOVE-BETWEEN-DELIMITERS (@ NODES 0) (@ NODES 1))
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
                 (CHAIN *PROXY-NODE-MAP* (SET PROXY (GETPROP HASH KEY)))
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
    var anchor14 = descriptor.anchor;
    var slot15 = descriptor.slot;
    var template16 = descriptor.template;
    var hash = TARGETNODEMAP.get(target);
    var nodes = hash[key];
    var parentNode17 = anchor14.parentNode;
    var startNode = createAnchor(0, key);
    var endNode = createAnchor(1, key);
    var returnValue = null;
    if (nodes) {
        removeBetweenDelimiters(nodes[0], nodes[1]);
        delete hash[key];
    };
    hash[key] = [startNode, endNode];
    parentNode17.insertBefore(startNode, anchor14);
    if (value) {
        if (Array.isArray(value)) {
            var result = createArray(value, template16);
            var nodes18 = result[0];
            var proxy = result[1];
            parentNode17.insertBefore(startNode, anchor14);
            var _js20 = nodes18.length;
            for (var _js19 = 0; _js19 < _js20; _js19 += 1) {
                var node = nodes18[_js19];
                parentNode17.insertBefore(node, anchor14);
            };
            parentNode17.insertBefore(endNode, anchor14);
            PROXYANCHORMAP.set(proxy, anchor14);
            PROXYNODEMAP.set(proxy, hash[key]);
            returnValue = proxy;
        } else {
            var result21 = createBinding(value, template16);
            var node22 = result21[0];
            var proxy23 = result21[1];
            parentNode17.insertBefore(node22, anchor14);
            returnValue = proxy23;
        };
    } else {
        var _js24 = slot15.childNodes;
        var _js26 = _js24.length;
        for (var _js25 = 0; _js25 < _js26; _js25 += 1) {
            var node27 = _js24[_js25];
            parentNode17.insertBefore(node27.cloneNode(true), anchor14);
        };
    };
    parentNode17.insertBefore(endNode, anchor14);
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
    var node27 = descriptor.node;
    var event28 = descriptor.event;
    var hash = TARGETEVENTMAP.get(target);
    var listener = hash[event28];
    if (listener) {
        node27.removeEventListener(event28, listener, listener.options);
    };
    if (value) {
        var boundListener = value.bind(receiver);
        boundListener.options = value.options;
        node27.addEventListener(event28, boundListener, boundListener.options);
        return hash[event28] = boundListener;
    };
};
/* (DEFUN CREATE-CONTEXT (CLONE)
     (LET ((NODE NIL)
           (ITER (CHAIN DOCUMENT (CREATE-NODE-ITERATOR CLONE 1)))
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
                                         (WHEN (EQ KEY 'TEXT)
                                           (SETF RESULT
                                                   (CREATE NODE NODE TYPE
                                                    *SYMBOL-TEXT*)))
                                         (WHEN (EQ KEY 'UNSAFE-HTML)
                                           (SETF RESULT
                                                   (CREATE NODE NODE TYPE
                                                    *SYMBOL-HTML*)))
                                         (WHEN (CHAIN KEY (STARTS-WITH 'EVENT))
                                           (SETF RESULT
                                                   (CREATE NODE NODE EVENT
                                                    (CHAIN KEY (SLICE 5)
                                                           (TO-LOWER-CASE))
                                                    TYPE *SYMBOL-EVENT*)))
                                         (WHEN RESULT
                                           (DELETE
                                            (GETPROP (@ NODE DATASET) KEY))
                                           (SETF (GETPROP CONTEXT VALUE)
                                                   RESULT)))))
       CONTEXT)) */
function createContext(clone) {
    var node = null;
    var iter = document.createNodeIterator(clone, 1);
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
            if (key === 'text') {
                result = { node : node, type : SYMBOLTEXT };
            };
            if (key === 'unsafeHtml') {
                result = { node : node, type : SYMBOLHTML };
            };
            if (key.startsWith('event')) {
                result = { node : node,
                        event : key.slice(5).toLowerCase(),
                        type : SYMBOLEVENT
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
    var _js30 = array.length;
    for (var _js29 = 0; _js29 < _js30; _js29 += 1) {
        var item = array[_js29];
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
            (NODES (LIST START-NODE END-NODE)))
       (CHAIN CLONE (INSERT-BEFORE START-NODE (@ CLONE FIRST-CHILD)))
       (CHAIN CLONE (APPEND-CHILD END-NODE))
       (CHAIN *PROXY-NODE-MAP* (SET PROXY NODES))
       (CHAIN *TARGET-CONTEXT-MAP* (SET OBJ CONTEXT))
       (CHAIN *TARGET-EVENT-MAP* (SET OBJ (CREATE)))
       (CHAIN *TARGET-NODE-MAP* (SET OBJ (CREATE)))
       (CHAIN *OBJECT (ASSIGN PROXY OBJ))
       (LIST CLONE PROXY))) */
function createBinding(obj, template) {
    var root = template.content || template;
    var clone = root.cloneNode(true);
    var proxy = new Proxy(obj, PROXYOBJECT);
    var context = createContext(clone);
    var startNode = createAnchor(0, 'proxy');
    var endNode = createAnchor(1, 'proxy');
    var nodes = [startNode, endNode];
    clone.insertBefore(startNode, clone.firstChild);
    clone.appendChild(endNode);
    PROXYNODEMAP.set(proxy, nodes);
    TARGETCONTEXTMAP.set(obj, context);
    TARGETEVENTMAP.set(obj, {  });
    TARGETNODEMAP.set(obj, {  });
    Object.assign(proxy, obj);
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
/* (EXPORT DEFAULT MAIN) */
export default main;

