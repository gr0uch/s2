
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
     (NEW (*REG-EXP (+ *TAG-OPEN* (?:>|&gt;)([^{}]+?) *TAG-CLOSE*)))) */
var PARTIALREGEXP = new RegExp(TAGOPEN + '(?:>|&gt;)([^{}]+?)' + TAGCLOSE);
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
         (AND (@ ELEMENT FIRST-CHILD)
              (EQ (@ ELEMENT FIRST-CHILD NODE-TYPE)
                  (@ PARSE WINDOW *NODE TEXT_NODE)))
       (LET* ((TEXT (CHAIN (@ ELEMENT FIRST-CHILD NODE-VALUE) (TRIM)))
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
             (CHAIN *ARRAY PROTOTYPE SLICE (CALL (@ ELEMENT CHILD-NODES))))
            (CONTAINER (LIST))
            (LAST-OPENED (LIST)))
       (LOOP FOR NODE IN NODES
             DO (WHEN
                    (NOT
                     (EQ (@ NODE NODE-TYPE) (@ PARSE WINDOW *NODE TEXT_NODE)))
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
                                                     (CHAIN PARSE WINDOW
                                                            DOCUMENT
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
                                                     (CHAIN PARSE WINDOW
                                                            DOCUMENT
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
                                                     (CHAIN PARSE WINDOW
                                                            DOCUMENT
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
                                                   (CHAIN PARSE WINDOW DOCUMENT
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
    if (element.firstChild && element.firstChild.nodeType === parse.window.Node['TEXT_NODE']) {
        var text = element.firstChild.nodeValue.trim();
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
    var nodes = Array.prototype.slice.call(element.childNodes);
    var container = [];
    var lastOpened = [];
    var _js8 = nodes.length;
    for (var _js7 = 0; _js7 < _js8; _js7 += 1) {
        var node = nodes[_js7];
        if (node.nodeType !== parse.window.Node['TEXT_NODE']) {
            if (container.length) {
                container[0].appendChild(node);
            };
            continue;
        };
        var text9 = node.nodeValue;
        var tokens = text9.split(VARREGEXPGLOBAL).filter(function (s) {
            return s.trim();
        });
        var _js11 = tokens.length;
        for (var _js10 = 0; _js10 < _js11; _js10 += 1) {
            var token = tokens[_js10];
            var newNode = null;
            var matchOpen = token.match(SECTIONOPENREGEXP);
            var matchClose = token.match(SECTIONCLOSEREGEXP);
            var matchPartial = token.match(PARTIALREGEXP);
            var matchVar12 = token.match(VARREGEXP);
            var matchUnescapedVar13 = token.match(UNESCAPEDVARREGEXP);
            if (matchOpen) {
                newNode = parse.window.document.createElement('slot');
                var name14 = matchOpen[1].trim();
                lastOpened.unshift(name14);
                newNode.setAttribute('name', name14);
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
            if (matchVar12) {
                newNode = parse.window.document.createElement('span');
                newNode.dataset.text = matchVar12[1];
                node.parentNode.insertBefore(newNode, node);
                continue;
            };
            if (matchUnescapedVar13) {
                newNode = parse.window.document.createElement('span');
                newNode.dataset.unsafeHtml = matchUnescapedVar13[1];
                node.parentNode.insertBefore(newNode, node);
                continue;
            };
            newNode = parse.window.document.createTextNode(token);
            node.parentNode.insertBefore(newNode, node);
        };
        node.remove();
    };
};
/* (DEFUN PARSE (TEMPLATE)
     (WHEN (EQ (TYPEOF TEMPLATE) 'STRING)
       (LET ((ELEMENT (CHAIN PARSE WINDOW DOCUMENT (CREATE-ELEMENT 'TEMPLATE))))
         (SETF (@ ELEMENT INNER-H-T-M-L) TEMPLATE
               TEMPLATE ELEMENT)))
     (SETF (@ TEMPLATE INNER-H-T-M-L)
             (CHAIN TEMPLATE INNER-H-T-M-L (REPLACE *COMMENT-REGEXP* )))
     (LET* ((CONTENT (@ TEMPLATE CONTENT))
            (TRIMMER
             (CHAIN PARSE WINDOW DOCUMENT (CREATE-NODE-ITERATOR CONTENT 4)))
            (ITERATOR
             (CHAIN PARSE WINDOW DOCUMENT (CREATE-NODE-ITERATOR CONTENT 1)))
            (CURRENT NIL))
       (LOOP WHILE (SETF CURRENT (CHAIN TRIMMER (NEXT-NODE)))
             DO (LET ((TRIMMED (CHAIN CURRENT NODE-VALUE (TRIM))))
                  (WHEN (NOT TRIMMED) (CHAIN CURRENT (REMOVE)))))
       (LOOP WHILE (SETF CURRENT (CHAIN ITERATOR (NEXT-NODE)))
             DO (PROCESS-ELEMENT CURRENT)))
     TEMPLATE) */
function parse(template) {
    if (typeof template === 'string') {
        var element = parse.window.document.createElement('template');
        element.innerHTML = template;
        template = element;
    };
    template.innerHTML = template.innerHTML.replace(COMMENTREGEXP, '');
    var content9 = template.content;
    var trimmer = parse.window.document.createNodeIterator(content9, 4);
    var iterator = parse.window.document.createNodeIterator(content9, 1);
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
/* (SETF (@ PARSE WINDOW) WINDOW) */
parse.window = window;
/* (EXPORT DEFAULT PARSE) */
export default parse;

