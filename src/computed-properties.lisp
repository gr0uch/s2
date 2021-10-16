(defparameter *observable-context-map* (new (*weak-map)))
(defparameter *target-observables-map* (new (*weak-map)))
(defparameter *observable-callback-map* (new (*weak-map)))
(defparameter *read-stack* (list))
(defparameter *clear-stack-timeout* nil)
(defparameter *stack-delimiter-symbol* (*symbol 'stack-delimiter))

(defparameter *proxy-observable*
  (create get get-property
          set set-property
          delete-property set-property))


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
  (chain *read-stack* (push (list target key)))
  ;; Prevent possible memory leaks from reading.
  (when (not *clear-stack-timeout*)
    (setf *clear-stack-timeout* (set-timeout clear-stack 0)))
  (chain *reflect (get target key receiver)))


(defun set-property (target key value receiver)
  ;; Skip if nothing changed.
  (when (eq (getprop target key) value) (return-from set-property t))

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
            (fn (@ key-binding 2))
            (return-value (chain fn (call obj))))
       (setf (getprop obj obj-key) return-value))))
  t)


;; Observable objects are sources of data that control computed properties.
(defun create-source (obj)
  (let ((proxy (new (*proxy (or obj (create)) *proxy-observable*))))
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
       (chain *read-stack* (push *stack-delimiter-symbol*))
       (let ((return-value (chain value (call obj))))
         (when (not (eq return-value undefined))
           (setf (getprop obj key) return-value))
         (loop
          for i from (- (length *read-stack*) 1) downto 0 do
          (when (eq (typeof (getprop *read-stack* i)) 'symbol) break)
          (let* ((tuple (getprop *read-stack* i))
                 (observable (@ tuple 0))
                 (observable-key (@ tuple 1))
                 (observable-context nil))

            (when (not (chain *target-observables-map* (has obj)))
              (chain *target-observables-map* (set obj (list))))
            (chain *target-observables-map* (get obj) (push observable))

            (when (not (chain *observable-context-map* (has observable)))
              (chain *observable-context-map* (set observable (create))))
            (setf observable-context
                  (chain *observable-context-map* (get observable)))

            (when (not (getprop observable-context observable-key))
              (setf (getprop observable-context observable-key) (list)))

            (let ((key-bindings (getprop observable-context observable-key)))
              (chain key-bindings (push (list obj key value)))))))
       (pop-stack)))))


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
         create-source create-computed))
