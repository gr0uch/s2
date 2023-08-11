;; OPTIONAL MODULE
;; Computed properties implementation.

(defparameter *observable-context-map* (new (*weak-map)))
(defparameter *target-observables-map* (new (*weak-map)))
(defparameter *read-stack* (list))
(defparameter *will-clear-stack* false)
(defparameter *stack-delimiter-symbol* (*symbol 'stack-delimiter))
(defparameter *ref-symbol* (*symbol 'ref))
(defparameter *has-unmounted-symbol* (*symbol 'has-unmounted))
(defparameter *proxy-target-symbol* (*symbol 'proxy-target))

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
  (loop
   while (length *read-stack*) do
   (chain *read-stack* (pop)))
  (setf *will-clear-stack* false))


(defun pop-stack ()
  (loop
   for i from (- (length *read-stack*) 1) downto 0 do
   (when (eq (chain *read-stack* (pop)) *stack-delimiter-symbol*) (break))))


;; By coordinating the function calls with the get trap, we can correlate
;; computed properties to their observables.
(defun get-property (target key receiver)
  (when (eq key *proxy-target-symbol*) (return-from get-property target))

  ;; Qualifying if a key has already been read or not means we don't have to
  ;; re-compute when the same key is read multiple times.
  (let ((has-read false))
    (loop
     for i from (- (length *read-stack*) 1) downto 0 do
     (let ((tuple (elt *read-stack* i)))
       (when (eq tuple *stack-delimiter-symbol*) (break))
       (when (and (eq (elt tuple 0) target)
                  (eq (elt tuple 1) key))
         (setf has-read t))))
    (when (not has-read)
      (chain *read-stack* (push (list target key)))
      ;; Prevent possible memory leaks from reading.
      (when (not *will-clear-stack*)
        (setf *will-clear-stack* t)
        (queue-microtask clear-stack))))

  (chain *reflect (get target key receiver)))

;; It is not sufficient to check type for object, it must also be not built-in.
;; This will intentionally restrict objects with a custom class from being
;; proxied.
;; https://2ality.com/2016/11/proxying-builtins.html#wrapping-instances-of-built-in-constructors
(defun is-object-proxyable (obj)
  (let ((ctor (and obj (@ obj constructor))))
    (or (eq ctor *object) (eq ctor *array)))
  ; The following is incomplete, it doesn't handle built-in constructors.
  ; (and obj (eq (typeof obj) 'object))
  )


(defun make-set-property (is-deep)
  (defun set-property (target key value receiver)
    (let ((old-value (getprop target key)))
      ;; Skip if nothing changed.
      (when (eq old-value value) (return-from set-property t))

      ;; Just overwrite keys on deep observables.
      (when (and is-deep (is-object-proxyable value)
                 (not (getprop value *ref-symbol*)))
        (if (and (is-object-proxyable old-value)
                 (not (getprop old-value *ref-symbol*)))
            (progn
              (deep-replace old-value value)
              (return-from set-property t))
          (setf value (create-source value t)))))

    (if (not (eq value undefined))
        (chain *reflect (set target key value receiver))
      (chain *reflect (delete-property target key)))

    (let ((context (chain *observable-context-map* (get target)))
          (key-bindings nil))
      (when (not context)
        ;; Missing some special case handling for new indices on arrays!
        (return-from set-property t))
      (setf key-bindings (or (getprop context key) (list)))
      (loop
       for i from (- (length key-bindings) 1) downto 0 do
       (let ((key-binding (getprop key-bindings i)))
         (when (not key-binding) (continue))
         (let* ((obj (@ key-binding 0))
                (obj-key (@ key-binding 1))
                (fn (@ key-binding 2)))
           (when (getprop obj *has-unmounted-symbol*) (continue))
           (compute-dependencies obj obj-key fn)))))
    t)
  set-property)


(defun deep-replace (proxy obj)
  (let ((old-target (getprop proxy *proxy-target-symbol*)))
    (loop
     for key of obj do
     (let ((value (getprop obj key))
           (old-value (getprop old-target key)))
       (if (and (is-object-proxyable value) (is-object-proxyable old-value)
                (not (getprop old-value *ref-symbol*)))
           (deep-replace old-value value)
         (setf (getprop proxy key) value))))
    (loop
     for key of old-target do
     (let ((old-value (getprop old-target key)))
       (when (not (chain *object prototype has-own-property (call obj key)))
         (delete (getprop proxy key)))))))


;; Observable objects are sources of data that control computed properties.
(defun create-source (obj is-deep)
  (when (not obj) (setf obj (create)))
  (let* ((proxy (new (*proxy obj
                             (if is-deep *proxy-deep-observable*
                               *proxy-observable*))))
         (symbols (chain *object (get-own-property-symbols obj)))
         (mount-fn (getprop obj (elt symbols 0))))
    (when is-deep
      (loop for key of obj do
            (let ((value (getprop obj key)))
              (when (and (is-object-proxyable value)
                         (not (getprop value *ref-symbol*)))
                (setf (getprop obj key) (create-source value t))))))

    ;; If a computed object is passed in, automatically mount it.
    (when (eq (typeof mount-fn) 'function) (chain mount-fn (call proxy)))
    proxy))


;; Each time a computed function is run, it tracks all observable keys it read.
;; If a dependency re-appears it should not be duplicated.
(defun compute-dependencies (obj key fn)
  (chain *read-stack* (push *stack-delimiter-symbol*))
  (let ((delimiter-index (- (length *read-stack*) 1))
        (upper-index nil)
        (return-value (chain fn (call obj)))
        (observables (list)))
    (if (not (eq return-value undefined))
        (setf (getprop obj key) return-value)
      (delete (getprop obj key)))

    ;; Always resetting this guarantees that we won't keep references to
    ;; observables which should be garbage collected.
    (chain *target-observables-map* (set obj observables))

    (loop
     for i from (+ delimiter-index 1) to (- (length *read-stack*) 1) do
     (when (eq (getprop *read-stack* i) *stack-delimiter-symbol*) break)
     (setf upper-index i))

    (loop
     for i from upper-index downto (+ delimiter-index 1) do
     (let* ((tuple (getprop *read-stack* i))
            (observable (@ tuple 0))
            (observable-key (@ tuple 1))
            (context nil))

       (when (not (chain observables (includes observable)))
         (chain observables (push observable)))

       (when (not (chain *observable-context-map* (has observable)))
         (chain *observable-context-map* (set observable (create))))
       (setf context (chain *observable-context-map* (get observable)))

       (when (not (chain *object prototype has-own-property (call context observable-key)))
         (setf (getprop context observable-key) (list)))

       (let* ((key-bindings (getprop context observable-key))
              (match-index
               (chain key-bindings
                      (find-index (lambda (entry)
                                    (and (eq (elt entry 0) obj)
                                         (eq (elt entry 1) key)))))))
         (when (not (eq match-index -1))
           (chain key-bindings (splice match-index 1)))
         (chain key-bindings (push (list obj key fn))))))

    (pop-stack)
    return-value))


;; Careful: this will only remove dependencies if unmount is called! This can
;; cause memory leaks if unmount is not called. Unmount should be called
;; recursively, so it should not be necessary to directly call it in most
;; circumstances.
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
               (chain key-bindings (splice i 1))))))))))
  (setf (getprop obj *has-unmounted-symbol*) t))


;; This is a function that returns the actual function meant to be used.
;; This is because mount and unmount are symbols that are unknowable without
;; being passed by reference, and we only need to pass this in once.
(defun create-computed (mount-symbol unmount-symbol)
  (defun computed (obj)
    (let ((mount (getprop obj mount-symbol))
          (unmount (getprop obj unmount-symbol)))
      (defun computed-mount ()
        (when mount (chain mount (call this)))
        (when (not (length arguments))
          (loop for key of obj do
                (let ((fn (getprop obj key)))
                  (when (eq (typeof fn) 'function)
                    (chain fn (call this)))))))
      (defun computed-unmount ()
        (when unmount (chain unmount (call this)))
        (unmount-object this))
      (setf
       ;; The mount symbol is not really used for the main functionality,
       ;; it is only here to be manually called in case emulating a mount
       ;; is needed.
       (getprop obj mount-symbol) computed-mount
       (getprop obj unmount-symbol) computed-unmount))

    ;; Replace functions with tracked functions.
    (chain
     *object (keys obj)
     (for-each
      (lambda (key)
        (let ((fn (getprop obj key)))
          (when (eq (typeof fn) 'function)
            (defun computed-property ()
              (let ((value (getprop this key)))
                (if (and value (@ value is-event-listener))
                    (chain fn (apply this arguments))
                  (compute-dependencies this key fn))))
            (setf
             (getprop obj key) computed-property))))))
    obj)
  computed)


;; Assign the ref symbol on an object, to mark it as not deeply observable.
(defun ref (obj)
  (setf (getprop obj *ref-symbol*) t)
  obj)


(export :names
        ((create-source observable)
         ;; TODO: the `create-source` should probably be deprecated in favor
         ;; of `observable`.
         create-source
         create-computed
         ref))
