;; This is the main entry point for s2.
(defparameter *context-slot* 'slot)
(defparameter *context-text* 'text)
(defparameter *context-html* 'html)
(defparameter *context-value* 'value)
(defparameter *context-class* 'class)
(defparameter *context-attribute* 'attribute)
(defparameter *context-data* 'data)
(defparameter *context-event* 'event)

(defparameter *symbol-mount* (*symbol 'mount))
(defparameter *symbol-unmount* (*symbol 'unmount))
(defparameter *symbol-move* (*symbol 'move))
(defparameter *symbol-root* (*symbol 'root))
(defparameter *symbol-target* (*symbol 'target))
(defparameter *tag-slot* '*slot*)

;; Condensed form of console methods.
(defmacro console-log (&body forms) `(chain console (log ,@forms)))
(defmacro console-warn (&body forms) `(chain console (warn ,@forms)))

;; A map of targets to contexts.
(defparameter *target-context-map* (new (*weak-map)))

;; A map of targets to hash maps keyed by event names and valued by listeners.
(defparameter *target-event-map* (new (*weak-map)))

;; A map of targets to hash maps keyed by key names and valued by an array of Nodes.
;; This is used to keep track of delimiters for slots.
(defparameter *target-delimiter-map* (new (*weak-map)))

;; A map of proxies to unmount/move functions.
(defparameter *proxy-unmount-map* (new (*weak-map)))
(defparameter *proxy-move-map* (new (*weak-map)))

;; A map of proxies to arrays of Nodes.
;; This is used to keep track of delimiters for individual proxy objects.
;; Also holds delimiters for arrays.
(defparameter *proxy-delimiter-map* (new (*weak-map)))

;; A map of array proxies to templates.
(defparameter *proxy-template-map* (new (*weak-map)))

;; A map of array proxies to anchors.
(defparameter *proxy-anchor-map* (new (*weak-map)))

;; A map of templates to their processed nodes.
(defparameter *template-processed-map* (new (*weak-map)))

;; A map of templates to the paths of their keys.
(defparameter *template-context-map* (new (*weak-map)))

(defparameter *proxy-object* (create set set-property delete-property set-property))
(defparameter *proxy-array* (create set set-index delete-property set-index))

(defparameter *deferred-queue* (list))

(defparameter *templates-hash* (create))

(defparameter *property-handlers* (create))
(setf
 (getprop *property-handlers* *context-text*)
 (lambda (node key value)
   (when (not (eq value (@ node text-content)))
     (setf (@ node text-content) value)))
 (getprop *property-handlers* *context-html*)
 (lambda (node key value)
   (when (not (eq value (@ node inner-h-t-m-l)))
     (setf (@ node inner-h-t-m-l) (or value ""))))
 (getprop *property-handlers* *context-value*)
 (lambda (node key value)
   (when (not (eq value (@ node value)))
     (if (eq value undefined)
         (progn (chain node (remove-attribute 'value))
                (setf (@ node value) ""))
       (progn (chain node (set-attribute 'value value))
              (setf (@ node value) value)))))
 (getprop *property-handlers* *context-class*) set-class
 (getprop *property-handlers* *context-attribute*) set-attribute
 (getprop *property-handlers* *context-data*) set-data)


;; The logic contained here is the most difficult. The flow is like:
;; - Set length: delete extra values.
;; - Deletion: just delete the proxy at the index.
;; - Setter: depends on whether the value is a new object or a proxy
;;   that already exists in the array.
;;   - Insert/replace: insert if the index doesn't exist yet.
;;   - Swap: swap indexes of proxies only.
;;     - The target location may or may not have a proxy.
;;     - The target location's proxy may or may not be the same proxy.
(defun set-index (target key value receiver is-initializing)
  (when (and (@ main is-deferred) (not is-initializing))
    (enqueue (lambda () (set-index target key value receiver t)))
    (return-from set-index t))

  (when (@ main debug) (console-log 'set-index arguments))
  (let* ((numkey (chain *number (parse-int
                                 (if (eq (typeof key) 'string) key nil) 10)))
         (is-index (not (chain *number (is-na-n numkey))))
         (is-setter (not (eq value undefined))))

    (when (eq key 'length)
      ;; Setting array length should remove extra values.
      (loop
       for i from value to (- (length target) 1) do
       (let* ((proxy (getprop target i))
              (nodes (chain *proxy-delimiter-map* (get proxy)))
              (unmount (chain *proxy-unmount-map* (get proxy))))
         (when nodes (remove-between-delimiters
                      (@ nodes 0) (@ nodes 1) unmount proxy)))
       (delete (getprop target i))))

    ;; Handle deletion.
    (when (and (not is-setter) (getprop target key))
      (let* ((proxy (getprop target key))
             (nodes (chain *proxy-delimiter-map* (get proxy)))
             (unmount (chain *proxy-unmount-map* (get proxy))))
        (remove-between-delimiters (@ nodes 0) (@ nodes 1) unmount proxy))
      (delete (getprop target key)))

    (when is-setter
      (when (not is-index)
        (return-from
         set-index (chain *reflect (set target key value receiver))))

      (if (not (chain target (includes value)))
          ;; Inserting or replacing an object.
          (let* ((anchor (chain *proxy-anchor-map* (get receiver)))
                 (parent-node (@ anchor parent-node))
                 (template (chain *proxy-template-map* (get receiver)))
                 (result (create-binding
                          value template (getprop receiver *symbol-root*)))
                 (proxy (@ result 0))
                 (node (@ result 1))
                 (previous-proxy (getprop target key))
                 (next-proxy nil))

            (loop for i from (+ numkey 1) to (- (length target) 1) do
                  (setf next-proxy (getprop target i))
                  (when next-proxy (break)))

            (when previous-proxy
              ;; If last proxy does not exist elsewhere in the array, this means
              ;; it's a replacement and the old nodes need to be removed.
              (when (not (chain target
                                (find (lambda (p i)
                                        (and (eq p previous-proxy)
                                             (not (eq i numkey)))))))
                (let ((nodes (chain *proxy-delimiter-map* (get previous-proxy)))
                      (unmount (chain *proxy-unmount-map* (get previous-proxy))))
                  (remove-between-delimiters
                   (@ nodes 0) (@ nodes 1) unmount proxy))))

            (if next-proxy
                (let ((next-anchor
                       (@ (chain *proxy-delimiter-map* (get next-proxy)) 0)))
                  (chain parent-node (insert-before node next-anchor)))
              (let ((end-node
                     (@ (chain *proxy-delimiter-map* (get receiver)) 1)))
                (chain parent-node (insert-before node end-node))))

            (return-from
             set-index (chain *reflect (set target key proxy receiver))))
        ;; Swapping a proxy.
        (let* ((other-index
                (chain target (find-index
                               (lambda (p i) (and (eq p value)
                                                  (not (eq i numkey)))))))
               (other-proxy (getprop target key))
               (move (chain *proxy-move-map* (get value)))
               (nodes (chain *proxy-delimiter-map* (get value)))
               (start-node (@ nodes 0))
               (end-node (@ nodes 1)))
          (if other-proxy
              (let* ((other-nodes (chain *proxy-delimiter-map* (get other-proxy)))
                     (other-start-node (@ other-nodes 0))
                     (other-end-node (@ other-nodes 1))
                     (other-move (chain *proxy-move-map* (get other-proxy)))
                     (anchor (@ nodes 1 next-sibling))
                     (parent-node (@ anchor parent-node)))

                (when (not (eq value other-proxy))
                  ;; Call move first.
                  (when move
                    (let ((node (@ start-node next-sibling)))
                      (loop while (not (chain node (is-same-node end-node))) do
                            (let ((old-node node))
                              (setf node (@ node next-sibling))
                              (chain move (call value old-node))))))
                  (when other-move
                    (let ((node (@ other-start-node next-sibling)))
                      (loop while (not (chain node (is-same-node other-end-node))) do
                            (let ((old-node node))
                              (setf node (@ node next-sibling))
                              (chain other-move (call other-proxy old-node))))))

                  ;; Move the current proxy to before the other position.
                  (let ((node start-node))
                    (loop while (not (chain node (is-same-node end-node))) do
                          (let ((old-node node))
                            (setf node (@ node next-sibling))
                            (chain parent-node
                                   (insert-before old-node other-start-node))))
                    (chain parent-node (insert-before end-node other-start-node)))

                  ;; Move other proxy to after the current position.
                  (let ((node other-start-node))
                    (loop while (not (chain node (is-same-node other-end-node))) do
                          (let ((old-node node))
                            (setf node (@ node next-sibling))
                            (chain parent-node (insert-before old-node anchor))))
                    (chain parent-node (insert-before other-end-node anchor))))
                )
            (when move
              (let ((node (@ start-node next-sibling)))
                (loop while (not (chain node (is-same-node end-node))) do
                      (let ((old-node node))
                        (setf node (@ node next-sibling))
                        (chain move (call value old-node)))))))

          ;; After moving, set the other proxy to the other position.
          (when (~ other-index)
            (chain *reflect (set target other-index other-proxy receiver)))
          ))
      (chain *reflect (set target key value receiver))))
  t)


;; When using deferred mode.
(defun enqueue (fn)
  (when (not (length *deferred-queue*))
    (chain
     main window
     (request-animation-frame
      (lambda ()
        (let ((q (length *deferred-queue*)))
          (loop
           while (length *deferred-queue*) do
           (let ((func (chain *deferred-queue* (shift))))
             (func)))
          (when (@ main debug)
            (console-log "queue flushed" q)))))))
  (chain *deferred-queue* (push fn)))


(defun set-property (target key value receiver is-initializing)
  (when (and (@ main is-deferred) (not is-initializing))
    (enqueue (lambda () (set-property target key value receiver t)))
    (return-from set-property t))

  (when (@ main debug) (console-log 'set-property arguments))
  (let* ((context (chain *target-context-map* (get target)))
         (is-setter (not (eq value undefined)))
         (is-changed (not (eq (getprop target key) value)))
         (descriptors (getprop context key))
         (node nil)
         (type nil)
         (return-value nil)
         (has-return-value false))

    (when descriptors
      (loop
       for descriptor in descriptors do
       (setf node (@ descriptor node)
             type (@ descriptor type))
       (when (or is-changed is-initializing)
         (when (and (eq (typeof value) 'function)
                    (not (eq type *context-event*))
                    (not has-return-value))
           (setf has-return-value t
                 return-value (chain value (call receiver))))
         (if (chain *property-handlers* (has-own-property type))
             ((getprop *property-handlers* type)
              node (@ descriptor name)
              (if has-return-value return-value value))
           (progn
             (when (eq type *context-event*)
               (set-event target value descriptor receiver))
             (when (eq type *context-slot*)
               (let ((proxy
                      (set-slot
                       target key
                       (if has-return-value return-value value) receiver
                       descriptor is-initializing)))
                 (when proxy
                   (return-from
                    set-property
                    (chain *reflect (set target key proxy receiver)))))))))

       ;; Handle automatic event binding for value.
       (when (and (eq type *context-value*)
                  (not (@ descriptor is-listening)))
         (chain node (add-event-listener
                      'input
                      (lambda (event)
                        (chain *reflect (set target key
                                             (@ event target value)
                                             receiver)))))
         (setf (@ descriptor is-listening) t))))

    (if is-setter
        (chain *reflect (set target key value receiver))
      (chain *reflect (delete-property target key)))))


(defun set-attribute (node name value)
  (if (not (or (eq value nil) (eq value undefined) (eq value false)))
      (chain node (set-attribute name (if (eq value t) "" value)))
    (chain node (remove-attribute name))))


(defun set-data (node name value)
  (if (not (or (eq value nil) (eq value undefined)))
      (setf (getprop (@ node dataset) name) value)
    (delete (getprop (@ node dataset) name))))


(defun set-class (node name value)
  (if value
      (setf (@ node class-name)
            (if (chain *array (is-array value))
                (chain value (split " ")) value))
    (chain node (remove-attribute 'class))))


(defun remove-between-delimiters (start-node end-node unmount self)
  (let* ((node start-node)
         (first-node node))
    (setf node (@ node next-sibling))
    (chain first-node (remove))
    (loop while (not (chain node (is-same-node end-node))) do
          (let ((old-node node))
            (setf node (@ node next-sibling))
            (if unmount
                (chain *promise (resolve (chain unmount (call self old-node)))
                       (then (lambda () (chain old-node (remove)))))
              (chain old-node (remove)))))
    (chain end-node (remove)))
  (when self (recursive-unmount self false (new (*weak-set))))
  nil)


(defun recursive-unmount (self should-unmount cycle-set)
  (chain cycle-set (add self))
  (loop
   for key of self do
   (let ((value (getprop self key)))
     (when (and (eq (typeof value) 'object)
                (not (eq value nil))
                (not (chain cycle-set (has value))))
       (recursive-unmount value t cycle-set))))
  (when should-unmount
    (let ((unmount (chain *proxy-unmount-map* (get self))))
      (when unmount (chain unmount (call self))))))


;; The way that this works is it tries to present an immediate-mode interface
;; on top of a retained mode underlying layer, the DOM. For example, assigning
;; a new object where there was a previous object before, will try to make the
;; least DOM updates possible.
(defun set-slot (target key value receiver descriptor is-initializing)
  (when (@ main debug) (console-log 'set-slot arguments))

  (when (or
         ;; Skip deletion if already empty.
         (not (or (getprop target key) value is-initializing))
         ;; Skip if value is invalid: not undefined nor null, AND not an object.
         (and (not (eq value undefined)) (not (eq (typeof value) 'object))))
    (return-from set-slot))

  (let* ((anchor (@ descriptor node))
         (slot (@ descriptor slot))
         (template (@ descriptor template))
         (hash (chain *target-delimiter-map* (get target)))
         (nodes (getprop hash key))
         (parent-node (@ anchor parent-node))
         (start-node (create-anchor 0 key))
         (end-node (create-anchor 1 key))
         (previous-value (getprop target key))
         (is-previous-array (chain *array (is-array previous-value)))
         (is-previous-object (and previous-value
                                  (eq (typeof previous-value) 'object)))
         (is-value-array (chain *array (is-array value)))
         (is-type-mismatch (not (eq is-previous-array is-value-array)))
         (return-value nil))

    (when (and nodes (or (not value) (and value (not previous-value))
                         is-type-mismatch))
      (if (chain *array (is-array previous-value))
          (loop
           for proxy in previous-value do
           (let ((unmount (chain *proxy-unmount-map* (get proxy)))
                 (nodes (chain *proxy-delimiter-map* (get proxy))))
             (when nodes
               (remove-between-delimiters
                (@ nodes 0) (@ nodes 1) unmount proxy))))
        (when previous-value
          (let ((unmount (chain *proxy-unmount-map* (get previous-value)))
                (nodes (chain *proxy-delimiter-map* (get previous-value))))
            (remove-between-delimiters
             (@ nodes 0) (@ nodes 1) unmount previous-value))))
      (if (and value (not previous-value))
          ;; This is specifically to remove slot placeholder.
          (remove-between-delimiters (@ nodes 0) (@ nodes 1))
        (progn
          (chain nodes 0 (remove))
          (chain nodes 1 (remove)))))

    (setf (getprop hash key) (list start-node end-node))
    (chain parent-node (insert-before start-node anchor))

    (if value
        (if (or (not previous-value)
                (not is-previous-object)
                is-type-mismatch)
            ;; Create proxies
            (if (chain *array (is-array value))
                (let* ((result (create-array
                                value template (getprop receiver *symbol-root*)))
                       (nodes (@ result 0))
                       (proxy (@ result 1)))
                  (loop for node in nodes do
                        (chain parent-node (insert-before node anchor)))
                  (chain *proxy-anchor-map* (set proxy anchor))
                  (chain *proxy-delimiter-map* (set proxy (getprop hash key)))
                  (setf return-value proxy))
              (let* ((result (create-binding
                              value template (getprop receiver *symbol-root*)))
                     (proxy (@ result 0))
                     (node (@ result 1)))
                (chain parent-node (insert-before node anchor))
                (setf return-value proxy)))
          ;; Assign values on existing proxies
          (let* (
                 ;; Casting to array first makes this a little simpler to do.
                 (previous-values
                  (if is-previous-array previous-value (list previous-value)))
                 (values
                  (if is-value-array value (list value))))
            ;; Bail out on type mismatch. Should not be possible currently.
            ;; (when (not (eq is-previous-array is-value-array))
            ;;   (throw (new (*type-error
            ;;                (+ "Object/array mismatch on key `" key "`.")))))
            (loop
             for i from 0 to (- (length values) 1) do
             (let ((prev (getprop previous-values i))
                   (obj (getprop values i)))
               (if prev
                   (progn
                     ;; Assignment.
                     (loop
                      for key of obj do
                      (setf (getprop prev key) (getprop obj key)))
                     ;; Removal.
                     (loop
                      for key of prev do
                      (when (not (chain obj (has-own-property key)))
                        (delete (getprop prev key)))))
                 (setf (getprop previous-values i) obj))))
            (when (chain *array (is-array previous-value))
              (setf (length previous-values) (length values)))
            (setf return-value previous-value)))
      ;; Use empty state from slot
      (loop
       for node in (@ slot child-nodes) do
       (chain parent-node (insert-before
                           (chain node (clone-node t)) anchor))))

    (chain parent-node (insert-before end-node anchor))
    return-value))


(defun set-event (target value descriptor receiver)
  (let* ((node (@ descriptor node))
         (event (@ descriptor event))
         (hash (chain *target-event-map* (get target)))
         (listener (getprop hash event)))
    (when listener
      (chain node (remove-event-listener
                   event listener (@ listener options))))
    (when value
      (setf (@ value is-event-listener) t)
      (let ((bound-listener (chain value (bind receiver))))
        (setf (@ bound-listener options) (@ value options))
        (chain node (add-event-listener
                     event bound-listener (@ bound-listener options)))
        (setf (getprop hash event) bound-listener)))))


(defun process-template (template)
  (let* ((root (or (@ template content) template))
         (clone (chain root (clone-node t)))
         (context (create)))

    (defun walk (parent-node path)
      (loop
       ;; backwards iteration avoids problems with removing nodes.
       for i from 0 to (- (length (@ parent-node child-nodes)) 1) do
       (let ((node (getprop (@ parent-node child-nodes) i)))
         (when (not (eq (@ node node-type)
                        (@ main window *node "ELEMENT_NODE")))
           (continue))
         (when (or (eq (@ node tag-name) *tag-slot*)
                   (@ node dataset key))
           (let* ((slot-name (or (@ node dataset key)
                                 (chain node (get-attribute 'name))))
                  (anchor (create-anchor 2 slot-name))
                  (template-selector (@ node dataset template))
                  (template-node
                   ;; Special case: when `data-key` exists and template is nested.
                   (if template-selector
                       (or (getprop *templates-hash* template-selector)
                           (chain main window document
                                  (query-selector template-selector)))
                     node)))
             (when (eq slot-name undefined)
               (throw (new (*type-error
                            "Missing `name` or `data-key` for slot."))))

             ;; Hygiene.
             (delete (@ node dataset key))

             ;; Move slot contents into document fragment.
             (when (and (not template-selector)
                        (eq (@ node tag-name) *tag-slot*))
               (setf template-node (chain main window document
                                          (create-document-fragment)))
               (loop
                while (@ node first-child) do
                (let ((child-node (@ node first-child)))
                  (chain template-node (append-child child-node)))))

             (chain parent-node (insert-before anchor node))
             (if (not (chain context (has-own-property slot-name)))
                 (setf (getprop context slot-name) (list))
               (throw
                (new (*error
                      (+ "The key \"" slot-name "\" was used in a template "
                         "more than once, which is not allowed.")))))
             (chain
              (getprop context slot-name)
              (push
               (create
                path (chain path (concat i))
                ;; This is for the placeholder content. If the template is nested,
                ;; then we need to make a dummy placeholder.
                slot (if template-selector node
                       (chain main window document (create-element 'div)))
                template template-node
                type *context-slot*)))
             (chain node (remove)))
           (continue))

         (when (@ node first-child)
           (walk node (chain path (concat i))))

         (loop
          for key of (@ node dataset) do
          (let ((value (getprop (@ node dataset) key))
                (result nil))
            (case
             key
             ("text" (setf result (create type *context-text*)))
             ("value" (setf result (create type *context-value*)))
             ("class" (setf result (create type *context-class*)))
             ("unsafeHtml" (setf result (create type *context-html*))))
            (when (chain key (starts-with 'attribute))
              (setf result
                    (create
                     type *context-attribute*
                     ;; Slice off "attribute", all attributes are lowercase.
                     name (chain key (slice 9) (to-lower-case)))))
            (when (chain key (starts-with 'event))
              (setf result
                    (create
                     type *context-event*
                     ;; Slice off "event", all events are lowercase.
                     event (chain key (slice 5) (to-lower-case)))))

            ;; Handle data attribute reflection.
            (when (not result)
              (setf result (create type *context-data* name key))
              ;; Handle special edge case for self-named attribute.
              (when (not value) (setf value key)))

            (when result
              (delete (getprop (@ node dataset) key))
              (chain node (remove-attribute key))
              (setf (@ result path) (chain path (concat i)))
              (when (not (chain context (has-own-property value)))
                (setf (getprop context value) (list)))
              (chain (getprop context value) (push result))))))))

    (walk clone (list))
    (chain *template-processed-map* (set template clone))
    (chain *template-context-map* (set template context))))


;; Note: this has to be fast, because this runs in between DOM node insertions.
(defun create-context (clone template)
  (let ((context (chain *template-context-map* (get template)))
        (cloned-context (create)))

    (loop
     for key of context do
     (setf (getprop cloned-context key) (list))
     (loop
      for descriptor in (getprop context key) do
      (let* ((path (@ descriptor path))
             (node (get-path clone path)))
        (chain (getprop cloned-context key)
               (push (chain *object (assign (create node node)
                                            descriptor)))))))

    cloned-context))


(defun get-path (node path)
  (let ((result node))
    (loop
     for i from 0 to (- (length path) 1) do
     (let ((j (getprop path i)))
       (setf result (getprop (@ result child-nodes) j))))
    result))


(defun create-array (array template root)
  (let* ((nodes (list))
         (proxies (list))
         (proxy nil))
    (loop
     for item in array do
     (let ((result (create-binding item template root)))
       (chain proxies (push (@ result 0)))
       (chain nodes (push (@ result 1)))))
    (setf proxy (new (*proxy proxies *proxy-array*)))
    (setf (getprop proxy *symbol-root*) root)
    (chain *proxy-template-map* (set proxy template))
    (list nodes proxy)))


(defun create-binding (obj template root)
  (when (not (chain *template-processed-map* (get template)))
    (process-template template))
  (let* ((clone (chain (chain *template-processed-map* (get template))
                       (clone-node t)))
         (target (create))
         (proxy (new (*proxy target *proxy-object*)))
         (context (create-context clone template))
         (start-node (create-anchor 0 'proxy))
         (end-node (create-anchor 1 'proxy))
         (nodes (list start-node end-node))
         (mount (getprop obj *symbol-mount*))
         (unmount (getprop obj *symbol-unmount*))
         (move (getprop obj *symbol-move*))
         (fragment (chain main window document (create-document-fragment))))

    (when unmount (chain *proxy-unmount-map* (set proxy unmount)))
    (when move (chain *proxy-move-map* (set proxy move)))

    (chain *target-context-map* (set target context))
    (chain *target-event-map* (set target (create)))
    (chain *target-delimiter-map* (set target (create)))

    ;; Initialization
    (setf (getprop proxy *symbol-root*) (or root proxy))
    (setf (getprop proxy *symbol-target*) obj)
    (loop
     for key of obj do
     (when (chain context (has-own-property key)) (continue))
     (setf (getprop target key) (getprop obj key)))
    (loop
     for key of context do
     (set-property target key (getprop obj key) proxy t))

    ;; The following is commented out because it does not seem to work in all
    ;; cases, and exhibits bugs such as at the top level mount.
    ;; (when mount
    ;;   (if (eq (@ clone node-type) (@ *node "ELEMENT_NODE"))
    ;;       (chain mount (call proxy clone))
    ;;     ;; Handle document fragment.
    ;;     (loop
    ;;      for node in (@ clone child-nodes) do
    ;;      (when (eq (@ node node-type) (@ *node "ELEMENT_NODE"))
    ;;        (chain mount (call proxy node))))))
    (when mount (chain mount (call proxy clone)))

    ;; Each proxy should contain references to its own delimiters.
    (chain fragment (append-child start-node))
    (chain fragment (append-child clone))
    (chain fragment (append-child end-node))
    (chain *proxy-delimiter-map* (set proxy nodes))

    (list proxy fragment)))


(defun create-anchor (type key)
  (if (@ main debug)
      (let ((comment (+ (if (eq type 0) 'start
                          (if (eq type 1) 'end 'anchor))
                        " " key)))
        (chain main window document (create-comment comment)))
    (chain main window document (create-text-node ""))))


(defun register-template (name template)
  (when (eq (typeof template) 'string)
    (let ((element (chain main window document (create-element 'template))))
      (setf (@ element inner-h-t-m-l) template
            template element)))
  (setf (getprop *templates-hash* name) template))


(defun main (origin template)
  (when (chain *templates-hash* (has-own-property template))
    (setf template (getprop *templates-hash* template)))
  (create-binding origin template))


(setf (@ main debug) (not t)
      (@ main is-deferred) (not t)
      (@ main window) (if (not (eq (typeof window) 'undefined)) window nil))


(export
 :default main
 :names ((*symbol-mount* mount)
         (*symbol-unmount* unmount)
         (*symbol-move* move)
         (*symbol-root* root)
         (*symbol-target* target)
         register-template))
