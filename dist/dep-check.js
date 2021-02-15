var __PS_MV_REG;
/* (DEFVAR *PASSED-CHECK* FALSE) */
if ('undefined' === typeof PASSEDCHECK) {
    var PASSEDCHECK = false;
};
/* (DEFVAR *DEP-MAP*
     (LIST (LIST (LIST '*NODE 'PROTOTYPE 'CLONE-NODE) function)
           (LIST (LIST '*NODE 'PROTOTYPE 'APPEND-CHILD) function)
           (LIST (LIST '*NODE 'PROTOTYPE 'INSERT-BEFORE) function)
           (LIST (LIST '*NODE 'PROTOTYPE 'NEXT-SIBLING) property)
           (LIST (LIST '*ELEMENT 'PROTOTYPE 'REMOVE) function)
           (LIST (LIST 'DOCUMENT 'QUERY-SELECTOR) function)
           (LIST (LIST 'DOCUMENT 'CREATE-TEXT-NODE) function)
           (LIST (LIST 'DOCUMENT 'CREATE-COMMENT) function)
           (LIST (LIST 'DOCUMENT 'CREATE-NODE-ITERATOR) function)
           (LIST (LIST '*NODE-FILTER) function)
           (LIST (LIST '*OBJECT 'PROTOTYPE 'HAS-OWN-PROPERTY) function)
           (LIST (LIST '*NUMBER 'PARSE-INT) function)
           (LIST (LIST '*NUMBER 'IS-NA-N) function)
           (LIST (LIST '*ARRAY 'IS-ARRAY) function)
           (LIST (LIST '*OBJECT 'ASSIGN) function)
           (LIST (LIST '*PROMISE) function) (LIST (LIST '*SYMBOL) function)
           (LIST (LIST '*REFLECT) object) (LIST (LIST '*WEAK-MAP) function)
           (LIST (LIST '*PROXY) function))) */
if ('undefined' === typeof DEPMAP) {
    var DEPMAP = [[['Node', 'prototype', 'cloneNode'], 'function'], [['Node', 'prototype', 'appendChild'], 'function'], [['Node', 'prototype', 'insertBefore'], 'function'], [['Node', 'prototype', 'nextSibling'], 'property'], [['Element', 'prototype', 'remove'], 'function'], [['document', 'querySelector'], 'function'], [['document', 'createTextNode'], 'function'], [['document', 'createComment'], 'function'], [['document', 'createNodeIterator'], 'function'], [['NodeFilter'], 'function'], [['Object', 'prototype', 'hasOwnProperty'], 'function'], [['Number', 'parseInt'], 'function'], [['Number', 'isNaN'], 'function'], [['Array', 'isArray'], 'function'], [['Object', 'assign'], 'function'], [['Promise'], 'function'], [['Symbol'], 'function'], [['Reflect'], 'object'], [['WeakMap'], 'function'], [['Proxy'], 'function']];
};
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
    var _js2 = DEPMAP.length;
    for (var _js1 = 0; _js1 < _js2; _js1 += 1) {
        var tuple = DEPMAP[_js1];
        var path = tuple[0];
        var typeStr = tuple[1];
        var target = window;
        if (typeStr === 'property') {
            var _js3 = path.length - 2;
            for (var i = 0; i <= _js3; i += 1) {
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
        var _js5 = path.length;
        for (var _js4 = 0; _js4 < _js5; _js4 += 1) {
            var key6 = path[_js4];
            if (target[key6]) {
                target = target[key6];
            } else {
                target = undefined;
                break;
            };
        };
        if (typeof target !== typeStr) {
            throw new TypeError('Expected ' + path.join('.') + ' to have type \"' + typeStr + '\"');
        };
    };
    __PS_MV_REG = [];
    return PASSEDCHECK = true;
};
/* (EXPORT DEFAULT DEP-CHECK) */
export default depCheck;

