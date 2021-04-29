
/* (DEFVAR *COMMENT-REGEXP* (REGEX /{{!(.+?)}}/gs)) */
if ('undefined' === typeof COMMENTREGEXP) {
    var COMMENTREGEXP = /{{!(.+?)}}/gs;
};
/* (DEFVAR *VAR-REGEXP* (REGEX /^{{([^{}]+?)}}$/)) */
if ('undefined' === typeof VARREGEXP) {
    var VARREGEXP = /^{{([^{}]+?)}}$/;
};
/* (DEFVAR *VAR-REGEXP-GLOBAL* (REGEX /({{(?:[^{}]+?)}})/gm)) */
if ('undefined' === typeof VARREGEXPGLOBAL) {
    var VARREGEXPGLOBAL = /({{(?:[^{}]+?)}})/gm;
};
/* (DEFVAR *UNESCAPED-VAR-REGEXP* (REGEX /^{{[{&]([^{}]+?)}{2,3}$/)) */
if ('undefined' === typeof UNESCAPEDVARREGEXP) {
    var UNESCAPEDVARREGEXP = /^{{[{&]([^{}]+?)}{2,3}$/;
};
/* (DEFVAR *SECTION-OPEN-REGEXP* (REGEX /{{#([^{}]+?)}}/)) */
if ('undefined' === typeof SECTIONOPENREGEXP) {
    var SECTIONOPENREGEXP = /{{#([^{}]+?)}}/;
};
/* (DEFVAR *SECTION-CLOSE-REGEXP* (REGEX /{{\/([^{}]+?)}}/)) */
if ('undefined' === typeof SECTIONCLOSEREGEXP) {
    var SECTIONCLOSEREGEXP = /{{\/([^{}]+?)}}/;
};
/* (DEFVAR *PARTIAL-REGEXP* (REGEX /{{>([^{}]+?)}}/)) */
if ('undefined' === typeof PARTIALREGEXP) {
    var PARTIALREGEXP = /{{>([^{}]+?)}}/;
};
/* (DEFUN PROCESS-ELEMENT (ELEMENT)
     (LET ((ATTRIBUTE-ENTRIES
            (LOOP FOR ATTRIBUTE IN (@ ELEMENT ATTRIBUTES)
                  COLLECT (LIST (@ ATTRIBUTE NAME) (@ ATTRIBUTE VALUE)))))
       (LOOP FOR ATTRIBUTE IN ATTRIBUTE-ENTRIES
             DO (LET* ((NAME (@ ATTRIBUTE 0))
                       (VALUE (@ ATTRIBUTE 1))
                       (RESULT (CHAIN VALUE (MATCH *VAR-REGEXP*)))
                       (TRIMMED NIL)
                       (SPECIAL-CASE 0))
                  (WHEN (NOT RESULT) (CONTINUE))
                  (SETF TRIMMED (CHAIN (@ RESULT 1) (TRIM)))
                  (WHEN (EQ NAME 'CLASS)
                    (SETF (@ ELEMENT DATASET CLASS) TRIMMED
                          SPECIAL-CASE 1))
                  (WHEN (EQ NAME 'VALUE)
                    (SETF (@ ELEMENT DATASET VALUE) TRIMMED
                          SPECIAL-CASE 1))
                  (WHEN (CHAIN NAME (STARTS-WITH on))
                    (CHAIN ELEMENT
                           (SET-ATTRIBUTE
                            (+ data-event- (CHAIN NAME (SLICE 2))) TRIMMED))
                    (SETF SPECIAL-CASE 1))
                  (WHEN (CHAIN NAME (STARTS-WITH data-))
                    (CHAIN ELEMENT (SET-ATTRIBUTE NAME TRIMMED))
                    (SETF SPECIAL-CASE 2))
                  (WHEN (NOT SPECIAL-CASE)
                    (CHAIN ELEMENT
                           (SET-ATTRIBUTE (+ data-attribute- NAME) TRIMMED)))
                  (WHEN (NOT (EQ SPECIAL-CASE 2))
                    (CHAIN ELEMENT (REMOVE-ATTRIBUTE NAME))))))
     (WHEN
         (AND (LENGTH (@ ELEMENT CHILD-NODES))
              (EQ (@ ELEMENT CHILD-NODES 0 NODE-TYPE) (@ *NODE TEXT_NODE)))
       (LET* ((TEXT (CHAIN (@ ELEMENT CHILD-NODES 0 NODE-VALUE) (TRIM)))
              (MATCH-VAR (CHAIN (@ ELEMENT TEXT-CONTENT) (MATCH *VAR-REGEXP*)))
              (MATCH-UNESCAPED-VAR
               (CHAIN (@ ELEMENT TEXT-CONTENT) (MATCH *UNESCAPED-VAR-REGEXP*))))
         (WHEN MATCH-VAR
           (SETF (@ ELEMENT DATASET TEXT) (@ MATCH-VAR 1)
                 (@ ELEMENT TEXT-CONTENT) ))
         (WHEN MATCH-UNESCAPED-VAR
           (SETF (@ ELEMENT DATASET UNSAFE-HTML) (@ MATCH-UNESCAPED-VAR 1)
                 (@ ELEMENT TEXT-CONTENT) ))))
     (LET* ((NODES
             (LOOP FOR NODE IN (@ ELEMENT CHILD-NODES)
                   COLLECT NODE))
            (CONTAINER NIL)
            (LAST-OPENED NIL))
       (LOOP FOR NODE IN NODES
             DO (WHEN (NOT (EQ (@ NODE NODE-TYPE) (@ *NODE TEXT_NODE)))
                  (WHEN CONTAINER (CHAIN CONTAINER (APPEND-CHILD NODE)))
                  (CONTINUE)) (LET* ((TEXT (@ NODE NODE-VALUE))
                                     (TOKENS
                                      (CHAIN TEXT (SPLIT *VAR-REGEXP-GLOBAL*)
                                             (MAP
                                              (LAMBDA (S) (CHAIN S (TRIM))))
                                             (FILTER *BOOLEAN))))
                                (LOOP FOR TOKEN IN TOKENS
                                      DO (LET ((NEW-NODE NIL)
                                               (MATCH-OPEN
                                                (CHAIN TOKEN
                                                       (MATCH
                                                        *SECTION-OPEN-REGEXP*)))
                                               (MATCH-CLOSE
                                                (CHAIN TOKEN
                                                       (MATCH
                                                        *SECTION-CLOSE-REGEXP*)))
                                               (MATCH-PARTIAL
                                                (CHAIN TOKEN
                                                       (MATCH
                                                        *PARTIAL-REGEXP*))))
                                           (WHEN MATCH-OPEN
                                             (SETF LAST-OPENED
                                                     (CHAIN (@ MATCH-OPEN 1)
                                                            (TRIM))
                                                   NEW-NODE
                                                     (CHAIN DOCUMENT
                                                            (CREATE-ELEMENT
                                                             'SLOT))
                                                   CONTAINER NEW-NODE)
                                             (CHAIN NEW-NODE
                                                    (SET-ATTRIBUTE 'NAME
                                                     LAST-OPENED))
                                             (CHAIN NODE PARENT-NODE
                                                    (INSERT-BEFORE NEW-NODE
                                                     NODE))
                                             (CONTINUE))
                                           (WHEN (AND MATCH-PARTIAL CONTAINER)
                                             (SETF (@ CONTAINER DATASET
                                                    TEMPLATE)
                                                     (CHAIN (@ MATCH-PARTIAL 1)
                                                            (TRIM)))
                                             (CONTINUE))
                                           (WHEN
                                               (AND MATCH-CLOSE
                                                    (EQ
                                                     (CHAIN (@ MATCH-CLOSE 1)
                                                            (TRIM))
                                                     LAST-OPENED))
                                             (SETF LAST-OPENED NIL
                                                   CONTAINER NIL)
                                             (CONTINUE))
                                           (SETF NEW-NODE
                                                   (CHAIN DOCUMENT
                                                          (CREATE-TEXT-NODE
                                                           TOKEN)))
                                           (CHAIN NODE PARENT-NODE
                                                  (INSERT-BEFORE NEW-NODE
                                                   NODE))))
                                (CHAIN NODE (REMOVE)))))) */
function processElement(element) {
    var attributeEntries = (function () {
        var _js1 = element.attributes;
        var _js3 = _js1.length;
        var collect4 = [];
        for (var _js2 = 0; _js2 < _js3; _js2 += 1) {
            var attribute = _js1[_js2];
            collect4.push([attribute.name, attribute.value]);
        };
        
        return collect4;
    })();
    var _js6 = attributeEntries.length;
    for (var _js5 = 0; _js5 < _js6; _js5 += 1) {
        var attribute = attributeEntries[_js5];
        var name = attribute[0];
        var value = attribute[1];
        var result = value.match(VARREGEXP);
        var trimmed = null;
        var specialCase = 0;
        if (!result) {
            continue;
        };
        trimmed = result[1].trim();
        if (name === 'class') {
            element.dataset['class'] = trimmed;
            specialCase = 1;
        };
        if (name === 'value') {
            element.dataset.value = trimmed;
            specialCase = 1;
        };
        if (name.startsWith('on')) {
            element.setAttribute('data-event-' + name.slice(2), trimmed);
            specialCase = 1;
        };
        if (name.startsWith('data-')) {
            element.setAttribute(name, trimmed);
            specialCase = 2;
        };
        if (!specialCase) {
            element.setAttribute('data-attribute-' + name, trimmed);
        };
        if (specialCase !== 2) {
            element.removeAttribute(name);
        };
    };
    if (element.childNodes.length && element.childNodes[0].nodeType === Node['TEXT_NODE']) {
        var text = element.childNodes[0].nodeValue.trim();
        var matchVar = element.textContent.match(VARREGEXP);
        var matchUnescapedVar = element.textContent.match(UNESCAPEDVARREGEXP);
        if (matchVar) {
            element.dataset.text = matchVar[1];
            element.textContent = '';
        };
        if (matchUnescapedVar) {
            element.dataset.unsafeHtml = matchUnescapedVar[1];
            element.textContent = '';
        };
    };
    var nodes = (function () {
        var _js7 = element.childNodes;
        var _js9 = _js7.length;
        var collect10 = [];
        for (var _js8 = 0; _js8 < _js9; _js8 += 1) {
            var node = _js7[_js8];
            collect10.push(node);
        };
        
        return collect10;
    })();
    var container = null;
    var lastOpened = null;
    var _js12 = nodes.length;
    for (var _js11 = 0; _js11 < _js12; _js11 += 1) {
        var node = nodes[_js11];
        if (node.nodeType !== Node['TEXT_NODE']) {
            if (container) {
                container.appendChild(node);
            };
            continue;
        };
        var text13 = node.nodeValue;
        var tokens = text13.split(VARREGEXPGLOBAL).map(function (s) {
            return s.trim();
        }).filter(Boolean);
        var _js15 = tokens.length;
        for (var _js14 = 0; _js14 < _js15; _js14 += 1) {
            var token = tokens[_js14];
            var newNode = null;
            var matchOpen = token.match(SECTIONOPENREGEXP);
            var matchClose = token.match(SECTIONCLOSEREGEXP);
            var matchPartial = token.match(PARTIALREGEXP);
            if (matchOpen) {
                lastOpened = matchOpen[1].trim();
                newNode = document.createElement('slot');
                container = newNode;
                newNode.setAttribute('name', lastOpened);
                node.parentNode.insertBefore(newNode, node);
                continue;
            };
            if (matchPartial && container) {
                container.dataset.template = matchPartial[1].trim();
                continue;
            };
            if (matchClose && matchClose[1].trim() === lastOpened) {
                lastOpened = null;
                container = null;
                continue;
            };
            newNode = document.createTextNode(token);
            node.parentNode.insertBefore(newNode, node);
        };
        node.remove();
    };
};
/* (DEFUN PARSE (TEMPLATE)
     (WHEN (EQ (TYPEOF TEMPLATE) 'STRING)
       (LET ((ELEMENT (CHAIN DOCUMENT (CREATE-ELEMENT 'TEMPLATE))))
         (SETF (@ ELEMENT INNER-H-T-M-L) TEMPLATE
               TEMPLATE ELEMENT)))
     (SETF (@ TEMPLATE INNER-H-T-M-L)
             (CHAIN TEMPLATE INNER-H-T-M-L (REPLACE *COMMENT-REGEXP* )))
     (LET* ((CONTENT (@ TEMPLATE CONTENT))
            (TRIMMER
             (CHAIN DOCUMENT
                    (CREATE-NODE-ITERATOR CONTENT (@ *NODE-FILTER SHOW_TEXT))))
            (ITERATOR
             (CHAIN DOCUMENT
                    (CREATE-NODE-ITERATOR CONTENT
                     (@ *NODE-FILTER SHOW_ELEMENT))))
            (CURRENT NIL))
       (LOOP WHILE (SETF CURRENT (CHAIN TRIMMER (NEXT-NODE)))
             DO (LET ((TRIMMED (CHAIN CURRENT NODE-VALUE (TRIM))))
                  (SETF (@ CURRENT NODE-VALUE) TRIMMED)
                  (WHEN (NOT TRIMMED) (CHAIN CURRENT (REMOVE)))))
       (LOOP WHILE (SETF CURRENT (CHAIN ITERATOR (NEXT-NODE)))
             DO (PROCESS-ELEMENT CURRENT)))
     TEMPLATE) */
function parse(template) {
    if (typeof template === 'string') {
        var element = document.createElement('template');
        element.innerHTML = template;
        template = element;
    };
    template.innerHTML = template.innerHTML.replace(COMMENTREGEXP, '');
    var content13 = template.content;
    var trimmer = document.createNodeIterator(content13, NodeFilter['SHOW_TEXT']);
    var iterator = document.createNodeIterator(content13, NodeFilter['SHOW_ELEMENT']);
    var current = null;
    while (current = trimmer.nextNode()) {
        var trimmed = current.nodeValue.trim();
        current.nodeValue = trimmed;
        if (!trimmed) {
            current.remove();
        };
    };
    while (current = iterator.nextNode()) {
        processElement(current);
    };
    
    return template;
};
/* (EXPORT DEFAULT PARSE) */
export default parse;

