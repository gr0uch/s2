(defvar *passed-check* false)
(defvar *dep-map*
  (list
   ;; Sanity checking the JS/DOM.
   (list (list '*node 'prototype 'insert-before) "function")
   (list (list '*element 'prototype 'remove) "function")
   (list (list 'document 'query-selector) "function")
   (list (list 'document 'create-text-node) "function")
   (list (list 'document 'create-node-iterator) "function")
   (list (list '*symbol) "function")
   (list (list '*reflect) "object")
   (list (list '*weak-map) "function")
   (list (list '*proxy) "function")))

(defun dep-check ()
  (when *passed-check* (return-from dep-check))

  (loop
   for tuple in *dep-map* do
   (let ((path (@ tuple 0))
         (type-str (@ tuple 1))
         (target window))
     (loop
      for key in path do
      (if (getprop target key)
          (setf target (getprop target key))
        (progn (setf target undefined) (break))))

     (when (not (eq (typeof target) type-str))
       (throw
        (new (*type-error
              (+ "Expected " (chain path (join "."))
                 " to have type \"" type-str "\"")))))))

  (setf *passed-check* t))

(export
 :default dep-check)
