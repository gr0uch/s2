;; OPTIONAL MODULE
;; Computed properties implementation.

(defparameter *observable-context-map* (new (*weak-map)))
(defparameter *target-observables-map* (new (*weak-map)))
(defparameter *read-stack* (list))
(defparameter *clear-stack-timeout* nil)
(defparameter *stack-delimiter-symbol* (*symbol 'stack-delimiter))

(defparameter *proxy-observable*
  (let ((set-property (make-set-property)))
    (create get get-property
            set set-property
            delete-property set-property)))

(defparameter *proxy-deep-observable*
  (let ((set-property (make-set-property t)))
    (create get get-property
            set set-property
            delete-property set-property)))


(defun clear-stack ()
  (setf *clear-stack-timeout* nil)
  (loop
   while (length *read-stack*) do
   (chain *read-stack* (pop))))

(defun pop-stack ()
  (loop
   for i from (- (length *read-stack*) 1) downto 0 do
   (when (eq (chain *read-stack* (pop)) *stack-delimiter-symbol*) (break))))


;; By coordinating the function calls with the get trap, we can correlate
;; computed properties to their observables.
(defun get-property (target key receiver)
  ;; Qualifying if a key has already been read or not means we don't have to
  ;; re-compute when the same key is read multiple times.
  (when (not (chain *read-stack*
                    (find (lambda (tuple)
                            (and (eq (elt tuple 0) target)
                                 (eq (elt tuple 1) key))))))
    (chain *read-stack* (push (list target key)))
    ;; Prevent possible memory leaks from reading.
    (when (not *clear-stack-timeout*)
      (setf *clear-stack-timeout* (set-timeout clear-stack 0))))
  (chain *reflect (get target key receiver)))

(defun is-object (obj)
  (and obj (eq (typeof obj) 'object)))

(defun make-set-property (is-deep)
  (defun set-property (target key value receiver)
    (let ((old-value (getprop target key)))
      ;; Skip if nothing changed.
      (when (eq old-value value) (return-from set-property t))

      ;; Just overwrite keys on deep observables.
      (when (and is-deep (is-object value))
        (if (is-object old-value)
            (progn
              (deep-replace (getprop receiver key) value)
              (return-from set-property t))
          (setf value (create-source value t)))))

    (if (not (eq value undefined))
        (chain *reflect (set target key value receiver))
      (chain *reflect (delete-property target key)))
    (let ((context (chain *observable-context-map* (get target)))
          (key-bindings nil))
      (when (not context)
        (return-from set-property t))
      (setf key-bindings (or (getprop context key) (list)))
      (loop
       for key-binding in key-bindings do
       (let* ((obj (@ key-binding 0))
              (obj-key (@ key-binding 1))
              (fn (@ key-binding 2)))
         (compute-dependencies obj obj-key fn))))
    t)
  set-property)


(defun deep-replace (proxy obj)
  (loop
   for key of obj do
   (let ((value (getprop obj key))
         (old-value (getprop proxy key)))
     (if (and (is-object value) (is-object old-value))
         (deep-replace old-value value)
       (setf (getprop proxy key) value))))
  (loop
   for key of proxy do
   (let ((old-value (getprop proxy key)))
     (when (not (chain obj (has-own-property key)))
       (delete (getprop proxy key))))))


;; Observable objects are sources of data that control computed properties.
(defun create-source (obj is-deep)
  (when (not obj) (setf obj (create)))
  (let ((proxy (new (*proxy obj
                            (if is-deep *proxy-deep-observable*
                              *proxy-observable*)))))
    (when is-deep
      (loop for key of obj do
            (let ((value (getprop obj key)))
              (when (is-object value)
                (setf (getprop obj key) (create-source value t))))))
    proxy))


;; What this function does is call the functions on the object, which will
;; trigger the get traps on the proxy objects which in turn will give us
;; all the necessary info to track dependencies automatically.
(defun mount-object (obj)
  (loop
   for key of obj do
   (let* ((value (getprop obj key))
          (is-function (eq (typeof value) 'function)))
     (when is-function
       (when (@ value is-event-listener) (continue))
       (compute-dependencies obj key value)))))


;; Each time a computed function is run, it tracks all observable keys it read.
;; If a dependency re-appears it should not be duplicated.
(defun compute-dependencies (obj key fn)
  (chain *read-stack* (push *stack-delimiter-symbol*))
  (let ((return-value (chain fn (call obj))))
    (if (not (eq return-value undefined))
        (setf (getprop obj key) return-value)
      (delete (getprop obj key)))
    (loop
     for i from (- (length *read-stack*) 1) downto 0 do
     (when (eq (typeof (getprop *read-stack* i)) 'symbol) break)
     (let* ((tuple (getprop *read-stack* i))
            (observable (@ tuple 0))
            (observable-key (@ tuple 1))
            (observable-context nil))

       (when (not (chain *target-observables-map* (has obj)))
         (chain *target-observables-map* (set obj (list))))
       (let ((observables (chain *target-observables-map* (get obj))))
         (when (not (chain observables (includes observable)))
           (chain observables (push observable))))

       (when (not (chain *observable-context-map* (has observable)))
         (chain *observable-context-map* (set observable (create))))
       (setf observable-context
             (chain *observable-context-map* (get observable)))

       (when (not (getprop observable-context observable-key))
         (setf (getprop observable-context observable-key) (list)))

       (let ((key-bindings (getprop observable-context observable-key)))
         (when (not (chain key-bindings
                           (find (lambda (entry)
                                   (and (eq (elt entry 0) obj)
                                        (eq (elt entry 1) key))))))
           (chain key-bindings (push (list obj key fn))))))))
  (pop-stack))


;; Careful: this will only remove dependencies if unmount is called! This can
;; cause memory leaks if unmount is not called. Unmount should be called
;; recursively, so it should not be necessary to directly call it in any
;; circumstance.
(defun unmount-object (obj)
  (let ((observables (chain *target-observables-map* (get obj))))
    (when (not observables) (return-from unmount-object))
    (loop
     for observable in observables do
     (let ((context (chain *observable-context-map* (get observable))))
       (loop
        for key of context do
        (let ((key-bindings (getprop context key)))
          (loop
           for i from (- (length key-bindings) 1) downto 0 do
           (let* ((key-binding (getprop key-bindings i))
                  (target (@ key-binding 0)))
             (when (eq target obj)
               (chain key-bindings (splice i 1)))))))))))


;; This is a function that returns the actual function meant to be used.
;; This is because mount and unmount are symbols that are unknowable without
;; being passed by reference, and we only need to pass this in once.
(defun create-computed (mount-symbol unmount-symbol)
  (defun computed (obj)
    (let ((mount (getprop obj mount-symbol))
          (unmount (getprop obj unmount-symbol)))
      (setf
       (getprop obj mount-symbol)
       (lambda ()
         (when mount (chain mount (call this)))
         (mount-object this))
       (getprop obj unmount-symbol)
       (lambda ()
         (when unmount (chain unmount (call this)))
         (unmount-object this))))
    obj)
  computed)


(export :names
        ((create-source observable)
         ;; TODO: the `create-source` should probably be deprecated in favor
         ;; of `observable`.
         create-source create-computed))
