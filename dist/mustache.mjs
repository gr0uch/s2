
/* (DEFPARAMETER *TAG-OPEN* {{) */
var TAGOPEN = '{{';
/* (DEFPARAMETER *TAG-CLOSE* }}) */
var TAGCLOSE = '}}';
/* (DEFPARAMETER *COMMENT-REGEXP*
     (NEW (*REG-EXP (+ *TAG-OPEN* !(.+?) *TAG-CLOSE*) gs))) */
var COMMENTREGEXP = new RegExp(TAGOPEN + '!(.+?)' + TAGCLOSE, 'gs');
/* (DEFPARAMETER *VAR-REGEXP*
     (NEW (*REG-EXP (+ ^ *TAG-OPEN* ([^{}]+?) *TAG-CLOSE* $)))) */
var VARREGEXP = new RegExp('^' + TAGOPEN + '([^{}]+?)' + TAGCLOSE + '$');
/* (DEFPARAMETER *VAR-REGEXP-GLOBAL*
     (NEW (*REG-EXP (+ ( *TAG-OPEN* {1,2}(?:.+?) *TAG-CLOSE* {1,2})) gm))) */
var VARREGEXPGLOBAL = new RegExp('(' + TAGOPEN + '{1,2}(?:.+?)' + TAGCLOSE + '{1,2})', 'gm');
/* (DEFPARAMETER *UNESCAPED-VAR-REGEXP*
     (NEW (*REG-EXP (+ ^ *TAG-OPEN* [{&]([^{}]+?)}? *TAG-CLOSE* $)))) */
var UNESCAPEDVARREGEXP = new RegExp('^' + TAGOPEN + '[{&]([^{}]+?)}?' + TAGCLOSE + '$');
/* (DEFPARAMETER *SECTION-OPEN-REGEXP*
     (NEW (*REG-EXP (+ *TAG-OPEN* #([^{}]+?) *TAG-CLOSE*)))) */
var SECTIONOPENREGEXP = new RegExp(TAGOPEN + '#([^{}]+?)' + TAGCLOSE);
/* (DEFPARAMETER *SECTION-CLOSE-REGEXP*
     (NEW (*REG-EXP (+ *TAG-OPEN* /([^{}]+?) *TAG-CLOSE*)))) */
var SECTIONCLOSEREGEXP = new RegExp(TAGOPEN + '/([^{}]+?)' + TAGCLOSE);
/* (DEFPARAMETER *PARTIAL-REGEXP*
     (NEW (*REG-EXP (+ *TAG-OPEN* >([^{}]+?) *TAG-CLOSE*)))) */
var PARTIALREGEXP = new RegExp(TAGOPEN + '>([^{}]+?)' + TAGCLOSE);
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
            (CONTAINER (LIST))
            (LAST-OPENED (LIST)))
       (LOOP FOR NODE IN NODES
             DO (WHEN (NOT (EQ (@ NODE NODE-TYPE) (@ *NODE TEXT_NODE)))
                  (WHEN (LENGTH CONTAINER)
                    (CHAIN (@ CONTAINER 0) (APPEND-CHILD NODE)))
                  (CONTINUE)) (LET* ((TEXT (@ NODE NODE-VALUE))
                                     (TOKENS
                                      (CHAIN TEXT (SPLIT *VAR-REGEXP-GLOBAL*)
                                             (FILTER
                                              (LAMBDA (S) (CHAIN S (TRIM)))))))
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
                                                        *PARTIAL-REGEXP*)))
                                               (MATCH-VAR
                                                (CHAIN TOKEN
                                                       (MATCH *VAR-REGEXP*)))
                                               (MATCH-UNESCAPED-VAR
                                                (CHAIN TOKEN
                                                       (MATCH
                                                        *UNESCAPED-VAR-REGEXP*))))
                                           (WHEN MATCH-OPEN
                                             (SETF NEW-NODE
                                                     (CHAIN DOCUMENT
                                                            (CREATE-ELEMENT
                                                             'SLOT)))
                                             (LET ((NAME
                                                    (CHAIN (@ MATCH-OPEN 1)
                                                           (TRIM))))
                                               (CHAIN LAST-OPENED
                                                      (UNSHIFT NAME))
                                               (CHAIN NEW-NODE
                                                      (SET-ATTRIBUTE 'NAME
                                                       NAME)))
                                             (IF (LENGTH CONTAINER)
                                                 (CHAIN CONTAINER 0
                                                        (APPEND-CHILD
                                                         NEW-NODE))
                                                 (CHAIN NODE PARENT-NODE
                                                        (INSERT-BEFORE NEW-NODE
                                                         NODE)))
                                             (CHAIN CONTAINER
                                                    (UNSHIFT NEW-NODE))
                                             (CONTINUE))
                                           (WHEN
                                               (AND MATCH-PARTIAL
                                                    (LENGTH CONTAINER))
                                             (SETF (@ CONTAINER 0 DATASET
                                                    TEMPLATE)
                                                     (CHAIN (@ MATCH-PARTIAL 1)
                                                            (TRIM)))
                                             (CONTINUE))
                                           (WHEN
                                               (AND MATCH-CLOSE
                                                    (EQ
                                                     (CHAIN (@ MATCH-CLOSE 1)
                                                            (TRIM))
                                                     (@ LAST-OPENED 0)))
                                             (CHAIN LAST-OPENED (SHIFT))
                                             (CHAIN CONTAINER (SHIFT))
                                             (CONTINUE))
                                           (WHEN MATCH-VAR
                                             (SETF NEW-NODE
                                                     (CHAIN DOCUMENT
                                                            (CREATE-ELEMENT
                                                             'SPAN))
                                                   (@ NEW-NODE DATASET TEXT)
                                                     (@ MATCH-VAR 1))
                                             (CHAIN NODE PARENT-NODE
                                                    (INSERT-BEFORE NEW-NODE
                                                     NODE))
                                             (CONTINUE))
                                           (WHEN MATCH-UNESCAPED-VAR
                                             (SETF NEW-NODE
                                                     (CHAIN DOCUMENT
                                                            (CREATE-ELEMENT
                                                             'SPAN))
                                                   (@ NEW-NODE DATASET
                                                    UNSAFE-HTML)
                                                     (@ MATCH-UNESCAPED-VAR 1))
                                             (CHAIN NODE PARENT-NODE
                                                    (INSERT-BEFORE NEW-NODE
                                                     NODE))
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
        var _js5 = element.attributes;
        var _js7 = _js5.length;
        var collect8 = [];
        for (var _js6 = 0; _js6 < _js7; _js6 += 1) {
            var attribute = _js5[_js6];
            collect8.push([attribute.name, attribute.value]);
        };
        
        return collect8;
    })();
    var _js10 = attributeEntries.length;
    for (var _js9 = 0; _js9 < _js10; _js9 += 1) {
        var attribute = attributeEntries[_js9];
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
        var _js11 = element.childNodes;
        var _js13 = _js11.length;
        var collect14 = [];
        for (var _js12 = 0; _js12 < _js13; _js12 += 1) {
            var node = _js11[_js12];
            collect14.push(node);
        };
        
        return collect14;
    })();
    var container = [];
    var lastOpened = [];
    var _js16 = nodes.length;
    for (var _js15 = 0; _js15 < _js16; _js15 += 1) {
        var node = nodes[_js15];
        if (node.nodeType !== Node['TEXT_NODE']) {
            if (container.length) {
                container[0].appendChild(node);
            };
            continue;
        };
        var text17 = node.nodeValue;
        var tokens = text17.split(VARREGEXPGLOBAL).filter(function (s) {
            return s.trim();
        });
        var _js19 = tokens.length;
        for (var _js18 = 0; _js18 < _js19; _js18 += 1) {
            var token = tokens[_js18];
            var newNode = null;
            var matchOpen = token.match(SECTIONOPENREGEXP);
            var matchClose = token.match(SECTIONCLOSEREGEXP);
            var matchPartial = token.match(PARTIALREGEXP);
            var matchVar20 = token.match(VARREGEXP);
            var matchUnescapedVar21 = token.match(UNESCAPEDVARREGEXP);
            if (matchOpen) {
                newNode = document.createElement('slot');
                var name22 = matchOpen[1].trim();
                lastOpened.unshift(name22);
                newNode.setAttribute('name', name22);
                if (container.length) {
                    container[0].appendChild(newNode);
                } else {
                    node.parentNode.insertBefore(newNode, node);
                };
                container.unshift(newNode);
                continue;
            };
            if (matchPartial && container.length) {
                container[0].dataset.template = matchPartial[1].trim();
                continue;
            };
            if (matchClose && matchClose[1].trim() === lastOpened[0]) {
                lastOpened.shift();
                container.shift();
                continue;
            };
            if (matchVar20) {
                newNode = document.createElement('span');
                newNode.dataset.text = matchVar20[1];
                node.parentNode.insertBefore(newNode, node);
                continue;
            };
            if (matchUnescapedVar21) {
                newNode = document.createElement('span');
                newNode.dataset.unsafeHtml = matchUnescapedVar21[1];
                node.parentNode.insertBefore(newNode, node);
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
    var content17 = template.content;
    var trimmer = document.createNodeIterator(content17, NodeFilter['SHOW_TEXT']);
    var iterator = document.createNodeIterator(content17, NodeFilter['SHOW_ELEMENT']);
    var current = null;
    while (current = trimmer.nextNode()) {
        var trimmed = current.nodeValue.trim();
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

