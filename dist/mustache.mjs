
/* (DEFPARAMETER *TAG-OPEN* \{\{) */
var TAGOPEN = '\\{\\{';
/* (DEFPARAMETER *TAG-CLOSE* \}\}) */
var TAGCLOSE = '\\}\\}';
/* (DEFPARAMETER *TAG-TEXT-OPEN* \{{2,3}) */
var TAGTEXTOPEN = '\\{{2,3}';
/* (DEFPARAMETER *TAG-TEXT-CLOSE* \}{2,3}) */
var TAGTEXTCLOSE = '\\}{2,3}';
/* (DEFPARAMETER *COMMENT-REGEXP*
     (NEW (*REG-EXP (+ *TAG-OPEN* !(.*?) *TAG-CLOSE*) gsu))) */
var COMMENTREGEXP = new RegExp(TAGOPEN + '!(.*?)' + TAGCLOSE, 'gsu');
/* (DEFPARAMETER *ATTR-REGEXP*
     (NEW
      (*REG-EXP (+ \s([^< ]+?)=['"] *TAG-OPEN* \s*(\S+?)\s* *TAG-CLOSE* ['"])
       gmu))) */
var ATTRREGEXP = new RegExp('\\s([^< ]+?)=[\'\"]' + TAGOPEN + '\\s*(\\S+?)\\s*' + TAGCLOSE + '[\'\"]', 'gmu');
/* (DEFPARAMETER *FREE-TEXT-REGEXP*
     (NEW (*REG-EXP (+ *TAG-TEXT-OPEN* ([^>]*?) *TAG-TEXT-CLOSE*) gmu))) */
var FREETEXTREGEXP = new RegExp(TAGTEXTOPEN + '([^>]*?)' + TAGTEXTCLOSE, 'gmu');
/* (DEFPARAMETER *UNESCAPED-TEXT-REGEXP* (REGEX /\{\{\{([^>]*?)\}\}\}/u)) */
var UNESCAPEDTEXTREGEXP = /\{\{\{([^>]*?)\}\}\}/u;
/* (DEFPARAMETER *ENCLOSED-TEXT-REGEXP*
     (NEW
      (*REG-EXP
       (+ <(.+?)>\s* *TAG-TEXT-OPEN* ([^>]+?) *TAG-TEXT-CLOSE* \s*</(\S+?)>)
       gmu))) */
var ENCLOSEDTEXTREGEXP = new RegExp('<(.+?)>\\s*' + TAGTEXTOPEN + '([^>]+?)' + TAGTEXTCLOSE + '\\s*</(\\S+?)>', 'gmu');
/* (DEFPARAMETER *PARTIAL-REGEXP*
     (NEW
      (*REG-EXP
       (+ *TAG-OPEN* #\s*(\S+?)\s* *TAG-CLOSE* \s* *TAG-OPEN* >\s*(\S+?)\s*
          *TAG-CLOSE* \s* *TAG-OPEN* /\s*(\S+?)\s* *TAG-CLOSE*)
       gmu))) */
var PARTIALREGEXP = new RegExp(TAGOPEN + '#\\s*(\\S+?)\\s*' + TAGCLOSE + '\\s*' + TAGOPEN + '>\\s*(\\S+?)\\s*' + TAGCLOSE + '\\s*' + TAGOPEN + '/\\s*(\\S+?)\\s*' + TAGCLOSE, 'gmu');
/* (DEFPARAMETER *SECTION-OPEN-REGEXP*
     (NEW (*REG-EXP (+ *TAG-OPEN* #\s*(\S+?)\s* *TAG-CLOSE*) gmu))) */
var SECTIONOPENREGEXP = new RegExp(TAGOPEN + '#\\s*(\\S+?)\\s*' + TAGCLOSE, 'gmu');
/* (DEFPARAMETER *SECTION-CLOSE-REGEXP*
     (NEW (*REG-EXP (+ *TAG-OPEN* /\s*(\S+?)\s* *TAG-CLOSE*) gmu))) */
var SECTIONCLOSEREGEXP = new RegExp(TAGOPEN + '/\\s*(\\S+?)\\s*' + TAGCLOSE, 'gmu');
/* (DEFPARAMETER *SELF-CLOSING-REGEXP*
     (REGEX /<(?!\/)(\S+?)\s+([^>]*?)\s*?\/>/gmu)) */
var SELFCLOSINGREGEXP = /<(?!\/)(\S+?)\s+([^>]*?)\s*?\/>/gmu;
/* (DEFPARAMETER *TEMPLATE-HASH-MAP* (NEW (*WEAK-MAP))) */
var TEMPLATEHASHMAP = new WeakMap();
/* (DEFUN REPLACE-ATTR (MATCH ATTR KEY)
     (SETF ATTR (CHAIN ATTR (TRIM))
           KEY (CHAIN KEY (TRIM)))
     (LET ((PREFIX attribute-))
       (WHEN (CHAIN ATTR (STARTS-WITH on))
         (SETF PREFIX event-
               ATTR (CHAIN ATTR (SLICE 2))))
       (WHEN (CHAIN ATTR (STARTS-WITH style:))
         (SETF PREFIX style-
               ATTR (CHAIN ATTR (SLICE 6))))
       (WHEN (CHAIN ATTR (STARTS-WITH class:))
         (SETF PREFIX classlist-
               ATTR (CHAIN ATTR (SLICE 6))))
       (WHEN (CHAIN ATTR (STARTS-WITH data-))
         (SETF PREFIX
               ATTR (CHAIN ATTR (SLICE 5))))
       (WHEN (OR (EQ ATTR class) (EQ ATTR value)) (SETF PREFIX ))
       (+  data- PREFIX ATTR =" KEY "))) */
function replaceAttr(match, attr, key) {
    attr = attr.trim();
    key = key.trim();
    var prefix = 'attribute-';
    if (attr.startsWith('on')) {
        prefix = 'event-';
        attr = attr.slice(2);
    };
    if (attr.startsWith('style:')) {
        prefix = 'style-';
        attr = attr.slice(6);
    };
    if (attr.startsWith('class:')) {
        prefix = 'classlist-';
        attr = attr.slice(6);
    };
    if (attr.startsWith('data-')) {
        prefix = '';
        attr = attr.slice(5);
    };
    if (attr === 'class' || attr === 'value') {
        prefix = '';
    };
    return ' data-' + prefix + attr + '=\"' + key + '\"';
};
/* (DEFUN REPLACE-FREE-TEXT (MATCH KEY)
     (LET ((ATTR text))
       (WHEN (CHAIN MATCH (MATCH *UNESCAPED-TEXT-REGEXP*))
         (SETF ATTR unsafe-html))
       (+ <span data- ATTR =" (CHAIN KEY (TRIM)) "></span>))) */
function replaceFreeText(match, key) {
    var attr = 'text';
    if (match.match(UNESCAPEDTEXTREGEXP)) {
        attr = 'unsafe-html';
    };
    return '<span data-' + attr + '=\"' + key.trim() + '\"></span>';
};
/* (DEFUN REPLACE-ENCLOSED-TEXT (MATCH OPENING-TAG KEY CLOSING-TAG)
     (LET ((ATTR text))
       (WHEN (CHAIN MATCH (MATCH *UNESCAPED-TEXT-REGEXP*))
         (SETF ATTR unsafe-html))
       (+ < OPENING-TAG  data- ATTR =" (CHAIN KEY (TRIM)) "></ CLOSING-TAG >))) */
function replaceEnclosedText(match, openingTag, key, closingTag) {
    var attr = 'text';
    if (match.match(UNESCAPEDTEXTREGEXP)) {
        attr = 'unsafe-html';
    };
    return '<' + openingTag + ' data-' + attr + '=\"' + key.trim() + '\"></' + closingTag + '>';
};
/* (DEFUN HASH-STR (STR)
     (LET ((I -1) (H 2))
       (LOOP WHILE (< I (1- (LENGTH STR)))
             DO (SETF H
                        (ASH (+ (* 3 H) (CHAIN STR (CHAR-CODE-AT (INCF I))))
                             0)))
       (ABS H))) */
function hashStr(str) {
    var i = -1;
    var h = 2;
    while (i < str.length - 1) {
        h = 3 * h + str.charCodeAt(++i) >> 0;
    };

    return Math.abs(h);
};
/* (DEFUN PARSE-MUSTACHE (TEMPLATE)
     (LET ((ELEMENT
            (CHAIN PARSE-MUSTACHE WINDOW DOCUMENT (CREATE-ELEMENT 'TEMPLATE))))
       (SETF TEMPLATE (PROCESS-MUSTACHE TEMPLATE)
             (@ ELEMENT INNER-H-T-M-L) TEMPLATE)
       ELEMENT)) */
function parseMustache(template) {
    var element = parseMustache.window.document.createElement('template');
    template = processMustache(template);
    element.innerHTML = template;

    return element;
};
/* (DEFUN PROCESS-MUSTACHE (TEMPLATE)
     (LET ((RESULT
            (CHAIN TEMPLATE (REPLACE *COMMENT-REGEXP* )
                   (REPLACE-ALL *PARTIAL-REGEXP*
                    <slot name="$1" data-template="$2"></slot>)
                   (REPLACE-ALL *SECTION-OPEN-REGEXP* <slot name="$1">)
                   (REPLACE-ALL *SECTION-CLOSE-REGEXP* </slot>)
                   (REPLACE-ALL *ATTR-REGEXP* REPLACE-ATTR)
                   (REPLACE-ALL *ENCLOSED-TEXT-REGEXP* REPLACE-ENCLOSED-TEXT)
                   (REPLACE-ALL *FREE-TEXT-REGEXP* REPLACE-FREE-TEXT))))
       (WHEN (@ PARSE-MUSTACHE SELF-CLOSING)
         (SETF RESULT
                 (CHAIN RESULT
                        (REPLACE-ALL *SELF-CLOSING-REGEXP* <$1 $2></$1>))))
       RESULT)) */
function processMustache(template) {
    var result = template.replace(COMMENTREGEXP, '').replaceAll(PARTIALREGEXP, '<slot name=\"$1\" data-template=\"$2\"></slot>').replaceAll(SECTIONOPENREGEXP, '<slot name=\"$1\">').replaceAll(SECTIONCLOSEREGEXP, '</slot>').replaceAll(ATTRREGEXP, replaceAttr).replaceAll(ENCLOSEDTEXTREGEXP, replaceEnclosedText).replaceAll(FREETEXTREGEXP, replaceFreeText);
    if (parseMustache.selfClosing) {
        result = result.replaceAll(SELFCLOSINGREGEXP, '<$1 $2></$1>');
    };
    return result;
};
/* (DEFUN CREATE-MUSTACHE-TAG (REGISTER-TEMPLATE)
     (DEFUN TAGGED-MUSTACHE (STRS)
       (LET ((RESULT (LIST (ELT STRS 0)))
             (ARGS
              (CHAIN *ARRAY PROTOTYPE SLICE (CALL ARGUMENTS 1)
                     (MAP
                      (LAMBDA (ELEMENT)
                        (CHAIN *TEMPLATE-HASH-MAP* (GET ELEMENT))))))
             (ELEMENT NIL)
             (HASH NIL))
         (LOOP FOR HASH IN ARGS
               FOR I FROM 1 TO (LENGTH STRS)
               DO (CHAIN RESULT
                         (PUSH
                          (IF HASH
                              (+ {{> HASH }})
                              (ELT ARGUMENTS I))
                          (ELT STRS I))))
         (SETF RESULT (CHAIN RESULT (JOIN ))
               ELEMENT (PARSE-MUSTACHE RESULT)
               HASH (+ template (HASH-STR RESULT)))
         (CHAIN *TEMPLATE-HASH-MAP* (SET ELEMENT HASH))
         (REGISTER-TEMPLATE HASH ELEMENT)
         ELEMENT))
     TAGGED-MUSTACHE) */
function createMustacheTag(registerTemplate) {
    function taggedMustache(strs) {
        var result = [strs[0]];
        var args = Array.prototype.slice.call(arguments, 1).map(function (element) {
            return TEMPLATEHASHMAP.get(element);
        });
        var element1 = null;
        var hash = null;
        var _js3 = args.length;
        var _js4 = strs.length;
        var FIRST5 = true;
        for (var _js2 = 0; _js2 < _js3; _js2 += 1) {
            var hash6 = args[_js2];
            var i = FIRST5 ? 1 : i + 1;
            if (i > _js4) {
                break;
            };
            result.push(hash6 ? '{{>' + hash6 + '}}' : arguments[i], strs[i]);
            FIRST5 = null;
        };
        result = result.join('');
        element1 = parseMustache(result);
        hash = 'template' + hashStr(result);
        TEMPLATEHASHMAP.set(element1, hash);
        registerTemplate(hash, element1);

        return element1;
    };
    return taggedMustache;
};
/* (SETF (@ PARSE-MUSTACHE WINDOW)
           (IF (NOT (EQ (TYPEOF WINDOW) 'UNDEFINED))
               WINDOW
               NIL)
         (@ PARSE-MUSTACHE SELF-CLOSING) FALSE) */
parseMustache.window = typeof window !== 'undefined' ? window : null;
parseMustache.selfClosing = false;
/* (EXPORT DEFAULT PARSE-MUSTACHE NAMES (PROCESS-MUSTACHE CREATE-MUSTACHE-TAG)) */
export { processMustache, createMustacheTag, };
export default parseMustache;
