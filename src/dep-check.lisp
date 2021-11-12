;; OPTIONAL MODULE
;; Use this if you are unsure about compatibility with the browser environment.

(defparameter *passed-check* false)
(defparameter *dep-map*
  '(
    ;; Sanity checking DOM.
    ((*node prototype clone-node) function)
    ((*node prototype append-child) function)
    ((*node prototype insert-before) function)
    ((*node prototype next-sibling) property)
    ((*node prototype first-child) property)
    ((*element prototype remove) function)
    ((document query-selector) function)
    ((document create-text-node) function)
    ((document create-comment) function)
    ((document create-document-fragment) function)
    ((window request-animation-frame) function)
    ((*node) function)

    ;; Sanity checking JS.
    ((*object prototype has-own-property) function)
    ((*number parse-int) function)
    ((*number is-na-n) function)
    ((*array is-array) function)
    ((*object assign) function)
    ((*promise) function)
    ((*symbol) function)
    ((*reflect) object)
    ((*weak-map) function)
    ((*proxy) function)))

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
