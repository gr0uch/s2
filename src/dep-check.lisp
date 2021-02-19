;; OPTIONAL MODULE
;; Use this if you are unsure about compatibility with the browser environment.

(defvar *passed-check* false)
(defvar *dep-map*
  (list
   ;; Sanity checking DOM.
   (list (list '*node 'prototype 'clone-node) "function")
   (list (list '*node 'prototype 'append-child) "function")
   (list (list '*node 'prototype 'insert-before) "function")
   (list (list '*node 'prototype 'next-sibling) "property")
   (list (list '*element 'prototype 'remove) "function")
   (list (list 'document 'query-selector) "function")
   (list (list 'document 'create-text-node) "function")
   (list (list 'document 'create-comment) "function")
   (list (list 'document 'create-document-fragment) "function")
   (list (list '*node) "function")

   ;; Sanity checking JS.
   (list (list '*object 'prototype 'has-own-property) "function")
   (list (list '*number 'parse-int) "function")
   (list (list '*number 'is-na-n) "function")
   (list (list '*array 'is-array) "function")
   (list (list '*object 'assign) "function")
   (list (list '*promise) "function")
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

     (when (eq type-str 'property)
       (loop
        for i from 0 to (- (length path) 2) do
        (let ((key (getprop path i)))
          (if (getprop target key)
              (setf target (getprop target key))
            (progn (setf target undefined) (break)))))
       (when (not (in (getprop path (- (length path) 1)) target))
         (throw
          (new (*type-error
                (+ "Expected " (chain path (join ".")) " to exist")))))
       (continue))

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
