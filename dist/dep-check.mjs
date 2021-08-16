
/* (DEFPARAMETER *PASSED-CHECK* FALSE) */
var PASSEDCHECK = false;
/* (DEFPARAMETER *DEP-MAP*
     '(((*NODE PROTOTYPE CLONE-NODE) FUNCTION)
       ((*NODE PROTOTYPE APPEND-CHILD) FUNCTION)
       ((*NODE PROTOTYPE INSERT-BEFORE) FUNCTION)
       ((*NODE PROTOTYPE NEXT-SIBLING) PROPERTY)
       ((*ELEMENT PROTOTYPE REMOVE) FUNCTION)
       ((DOCUMENT QUERY-SELECTOR) FUNCTION)
       ((DOCUMENT CREATE-TEXT-NODE) FUNCTION)
       ((DOCUMENT CREATE-COMMENT) FUNCTION)
       ((DOCUMENT CREATE-DOCUMENT-FRAGMENT) FUNCTION)
       ((WINDOW REQUEST-ANIMATION-FRAME) FUNCTION) ((*NODE) FUNCTION)
       ((*OBJECT PROTOTYPE HAS-OWN-PROPERTY) FUNCTION)
       ((*NUMBER PARSE-INT) FUNCTION) ((*NUMBER IS-NA-N) FUNCTION)
       ((*ARRAY IS-ARRAY) FUNCTION) ((*OBJECT ASSIGN) FUNCTION)
       ((*PROMISE) FUNCTION) ((*SYMBOL) FUNCTION) ((*REFLECT) OBJECT)
       ((*WEAK-MAP) FUNCTION) ((*PROXY) FUNCTION))) */
var DEPMAP = [[['Node', 'prototype', 'cloneNode'], 'function'], [['Node', 'prototype', 'appendChild'], 'function'], [['Node', 'prototype', 'insertBefore'], 'function'], [['Node', 'prototype', 'nextSibling'], 'property'], [['Element', 'prototype', 'remove'], 'function'], [['document', 'querySelector'], 'function'], [['document', 'createTextNode'], 'function'], [['document', 'createComment'], 'function'], [['document', 'createDocumentFragment'], 'function'], [['window', 'requestAnimationFrame'], 'function'], [['Node'], 'function'], [['Object', 'prototype', 'hasOwnProperty'], 'function'], [['Number', 'parseInt'], 'function'], [['Number', 'isNaN'], 'function'], [['Array', 'isArray'], 'function'], [['Object', 'assign'], 'function'], [['Promise'], 'function'], [['Symbol'], 'function'], [['Reflect'], 'object'], [['WeakMap'], 'function'], [['Proxy'], 'function']];
/* (DEFUN DEP-CHECK ()
     (WHEN *PASSED-CHECK* (RETURN-FROM DEP-CHECK))
     (LOOP FOR TUPLE IN *DEP-MAP*
           DO (LET ((PATH (@ TUPLE 0)) (TYPE-STR (@ TUPLE 1)) (TARGET WINDOW))
                (WHEN (EQ TYPE-STR 'PROPERTY)
                  (LOOP FOR I FROM 0 TO (- (LENGTH PATH) 2)
                        DO (LET ((KEY (GETPROP PATH I)))
                             (IF (GETPROP TARGET KEY)
                                 (SETF TARGET (GETPROP TARGET KEY))
                                 (PROGN (SETF TARGET UNDEFINED) (BREAK)))))
                  (WHEN (NOT (IN (GETPROP PATH (- (LENGTH PATH) 1)) TARGET))
                    (THROW
                        (NEW
                         (*TYPE-ERROR
                          (+ Expected  (CHAIN PATH (JOIN .))  to exist)))))
                  (CONTINUE))
                (LOOP FOR KEY IN PATH
                      DO (IF (GETPROP TARGET KEY)
                             (SETF TARGET (GETPROP TARGET KEY))
                             (PROGN (SETF TARGET UNDEFINED) (BREAK))))
                (WHEN (NOT (EQ (TYPEOF TARGET) TYPE-STR))
                  (THROW
                      (NEW
                       (*TYPE-ERROR
                        (+ Expected  (CHAIN PATH (JOIN .))  to have type "
                           TYPE-STR ")))))))
     (SETF *PASSED-CHECK* T)) */
function depCheck() {
    if (PASSEDCHECK) {
        return;
    };
    var _js6 = DEPMAP.length;
    for (var _js5 = 0; _js5 < _js6; _js5 += 1) {
        var tuple = DEPMAP[_js5];
        var path = tuple[0];
        var typeStr = tuple[1];
        var target = window;
        if (typeStr === 'property') {
            var _js7 = path.length - 2;
            for (var i = 0; i <= _js7; i += 1) {
                var key = path[i];
                if (target[key]) {
                    target = target[key];
                } else {
                    target = undefined;
                    break;
                };
            };
            if (!(path[path.length - 1] in target)) {
                throw new TypeError('Expected ' + path.join('.') + ' to exist');
            };
            continue;
        };
        var _js9 = path.length;
        for (var _js8 = 0; _js8 < _js9; _js8 += 1) {
            var key10 = path[_js8];
            if (target[key10]) {
                target = target[key10];
            } else {
                target = undefined;
                break;
            };
        };
        if (typeof target !== typeStr) {
            throw new TypeError('Expected ' + path.join('.') + ' to have type \"' + typeStr + '\"');
        };
    };
    
    return PASSEDCHECK = true;
};
/* (EXPORT DEFAULT DEP-CHECK) */
export default depCheck;

