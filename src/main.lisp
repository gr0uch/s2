(defvar *symbol-slot* (*symbol 'slot))
(defvar *symbol-text* (*symbol 'text))
(defvar *symbol-html* (*symbol 'html))
(defvar *symbol-class* (*symbol 'class))
(defvar *symbol-attribute* (*symbol 'attribute))
(defvar *symbol-event* (*symbol 'event))
(defvar *symbol-mount* (*symbol 'mount))
(defvar *symbol-unmount* (*symbol 'unmount))
(defvar *tag-slot* '*slot*)

;; Condensed form of console.log.
(defmacro console-log (&body forms) `(chain console (log ,@forms)))

;; A map of targets to contexts.
(defvar *target-context-map* (new (*weak-map)))

;; A map of targets to hash maps keyed by event names and valued by listeners.
(defvar *target-event-map* (new (*weak-map)))

;; A map of targets to hash maps keyed by key names and valued by an array of Nodes.
;; This is used to keep track of delimiters for slots.
(defvar *target-delimiter-map* (new (*weak-map)))

;; A map of proxies to unmount functions.
(defvar *proxy-unmount-map* (new (*weak-map)))

;; A map of proxies to arrays of Nodes.
;; This is used to keep track of delimiters for individual proxy objects.
;; Also holds delimiters for arrays.
(defvar *proxy-delimiter-map* (new (*weak-map)))

;; A map of array proxies to templates.
(defvar *proxy-template-map* (new (*weak-map)))

;; A map of array proxies to anchors.
(defvar *proxy-anchor-map* (new (*weak-map)))

;; A map of templates to their processed nodes.
(defvar *template-processed-map* (new (*weak-map)))

;; A map of templates to the paths of their keys.
(defvar *template-context-map* (new (*weak-map)))

(defvar *proxy-object* (create set set-property delete-property set-property))
(defvar *proxy-array* (create set set-index delete-property set-index))


;; The logic contained here is the most difficult. The flow is like:
;; - Set length: delete extra values.
;; - Deletion: just delete the proxy at the index.
;; - Setter: depends on whether the value is a new object or a proxy
;;   that already exists in the array.
;;   - Insert/replace: insert if the index doesn't exist yet.
;;   - Swap: swap indexes of proxies only.
;;     - The target location may or may not have a proxy.
;;     - The target location's proxy may or may not be the same proxy.
(defun set-index (target key value receiver)
  (when (@ main debug) (console-log 'set-index arguments))
  (let* ((numkey (chain *number (parse-int key 10)))
         (is-index (not (chain *number (is-na-n numkey))))
         (is-setter (eq (length arguments) 4))
         (is-delete (eq (length arguments) 2)))

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

    (when (and is-delete (getprop target key))
      (let* ((proxy (getprop target key))
             (nodes (chain *proxy-delimiter-map* (get proxy)))
             (unmount (chain *proxy-unmount-map* (get proxy))))
        (remove-between-delimiters (@ nodes 0) (@ nodes 1) unmount proxy))
      (delete (getprop target key)))

    (when (and is-setter (not is-index))
      (return-from
       set-index (chain *reflect (set target key value receiver))))

    (when is-setter
      (if (not (chain target (includes value)))
          ;; Inserting or replacing an object.
          (let* ((anchor (chain *proxy-anchor-map* (get receiver)))
                 (parent-node (@ anchor parent-node))
                 (template (chain *proxy-template-map* (get receiver)))
                 (result (create-binding value template))
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
               (other-proxy (getprop target key)))
          (when (and other-proxy (not (eq value other-proxy)))
            (let* ((other-nodes (chain *proxy-delimiter-map* (get other-proxy)))
                   (other-start-node (@ other-nodes 0))
                   (other-end-node (@ other-nodes 1))
                   (nodes (chain *proxy-delimiter-map* (get value)))
                   (start-node (@ nodes 0))
                   (end-node (@ nodes 1))
                   (anchor (@ nodes 1 next-sibling))
                   (parent-node (@ anchor parent-node)))

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
                (chain parent-node (insert-before other-end-node anchor)))
              ))

          ;; After moving, set the other proxy to the other position.
          (when (~ other-index)
            (chain *reflect (set target other-index other-proxy receiver)))
          ))
      (chain *reflect (set target key value receiver))))
  t)


(defun set-property (target key value receiver)
  (let* ((context (chain *target-context-map* (get target)))
         ;; Even though there may be 5 arguments here,
         ;; we don't care about setting if initializing.
         (is-setter (eq (length arguments) 4))
         (is-delete (eq (length arguments) 2))
         (is-changed (not (eq (getprop target key) value))))
    (when (and (chain *object prototype has-own-property (call context key))
               is-changed)
      (let* ((descriptor (getprop context key))
             (node (@ descriptor node))
             (type (@ descriptor type)))
        (when (and (eq type *symbol-text*)
                   (not (eq value (@ node text-content))))
          (setf (@ node text-content) value))
        (when (and (eq type *symbol-html*)
                   (not (eq value (@ node inner-h-t-m-l))))
          (setf (@ node inner-h-t-m-l) value))
        (when (eq type *symbol-class*)
          (set-class node value))
        (when (eq type *symbol-attribute*)
          (set-attribute node (@ descriptor name) value))
        (when (eq type *symbol-event*)
          (set-event target value descriptor receiver))
        (when (eq type *symbol-slot*)
          (let ((proxy (set-slot target key value descriptor)))
            (when proxy
              (return-from
               set-property
               (chain *reflect (set target key proxy receiver))))))))

    (when is-delete (delete (getprop target key)))
    (when is-setter (chain *reflect (set target key value receiver))))
  t)


(defun set-attribute (node name value)
  (if (or (eq value nil) (eq value undefined))
      (chain node (set-attribute name value))
    (chain node (remove-attribute name))))


(defun set-class (node value)
  (if value
      (let* ((class-list (@ node class-list))
             (prototype (chain *object (get-prototype-of value)))
             (array
              (if (eq prototype (@ *set prototype)) (chain value (values))
                (if (eq prototype (@ *array prototype)) value
                  (if (eq prototype (@ *string prototype))
                      (chain value (split " "))
                    (list))))))
        (loop for cls in (chain class-list (values)) do
              (when (not (chain array (includes cls)))
                (chain class-list (remove cls))))
        (loop for cls in array do
              (when (not (chain class-list (contains cls)))
                (chain class-list (add cls)))))
    (chain node (remove-attribute 'class)))
  nil)


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
    (chain end-node (remove))))


;; The way that this works is it tries to present an immediate-mode interface
;; on top of a retained mode underlying layer, the DOM. For example, assigning
;; a new object where there was a previous object before, will try to make the
;; least DOM updates possible.
(defun set-slot (target key value descriptor)
  ;; Skip deletion if already empty.
  (when (not (or (getprop target key) value)) (return-from set-slot))

  (let* ((anchor (@ descriptor node))
         (slot (@ descriptor slot))
         (template (@ descriptor template))
         (hash (chain *target-delimiter-map* (get target)))
         (nodes (getprop hash key))
         (parent-node (@ anchor parent-node))
         (start-node (create-anchor 0 key))
         (end-node (create-anchor 1 key))
         (previous-value (getprop target key))
         (return-value nil))

    (when (and nodes (or (not value) (and value (not previous-value))))
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
      (remove-between-delimiters (@ nodes 0) (@ nodes 1))
      (delete (getprop hash key)))

    (setf (getprop hash key) (list start-node end-node))
    (chain parent-node (insert-before start-node anchor))

    (if value
        (if (not previous-value)
            ;; Create proxies
            (if (chain *array (is-array value))
                (let* ((result (create-array value template))
                       (nodes (@ result 0))
                       (proxy (@ result 1)))
                  (loop for node in nodes do
                        (chain parent-node (insert-before node anchor)))
                  (chain *proxy-anchor-map* (set proxy anchor))
                  (chain *proxy-delimiter-map* (set proxy (getprop hash key)))
                  (setf return-value proxy))
              (let* ((result (create-binding value template))
                     (proxy (@ result 0))
                     (node (@ result 1)))
                (chain parent-node (insert-before node anchor))
                (setf return-value proxy)))
          ;; Assign values on existing proxies
          (let* (
                 ;; Casting to array first makes this a little simpler to do.
                 (previous-values
                  (if (chain *array (is-array previous-value))
                      previous-value (list previous-value)))
                 (values
                  (if (chain *array (is-array value))
                      value (list value))))
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
       for i from 0 to (- (length (@ parent-node child-nodes)) 1) do
       (let ((node (getprop (@ parent-node child-nodes) i)))
         (when (not (eq (@ node node-type) (@ *node "ELEMENT_NODE"))) (continue))
         (when (length (@ node children))
           (walk node (chain path (concat i))))

         (when (or (eq (@ node tag-name) *tag-slot*) (@ node dataset template))
           (let* ((slot-name (or (@ node dataset key) (@ node name)))
                  (anchor (create-anchor 2 slot-name))
                  (template-node
                   (chain document (query-selector
                                    (@ node dataset template)))))
             (when (eq slot-name undefined)
               (throw (new (*error "Missing `name` or `data-key` for slot."))))
             (chain parent-node (insert-before anchor node))
             (setf (getprop context slot-name)
                   (create
                    path (chain path (concat i))
                    slot node
                    template template-node
                    type *symbol-slot*))
             (chain node (remove))
             )
           (continue))

         (loop
          for key of (@ node dataset) do
          (let ((value (getprop (@ node dataset) key))
                (result nil))
            (case
             key
             ("text" (setf result (create type *symbol-text*)))
             ("class" (setf result (create type *symbol-class*)))
             ("unsafeHtml" (setf result (create type *symbol-html*))))
            (when (chain key (starts-with 'attribute))
              (setf result
                    (create
                     type *symbol-attribute*
                     ;; Slice off "attribute", all attributes are lowercase.
                     name (chain key (slice 9) (to-lower-case)))))
            (when (chain key (starts-with 'event))
              (setf result
                    (create
                     type *symbol-event*
                     ;; Slice off "event", all events are lowercase.
                     event (chain key (slice 5) (to-lower-case)))))
            (when result
              (delete (getprop (@ node dataset) key))
              (chain node (remove-attribute key))
              (setf
               (@ result path) (chain path (concat i))
               (getprop context value) result)))))))

    (walk clone (list))
    (chain *template-processed-map* (set template clone))
    (chain *template-context-map* (set template context))))


;; Note: this has to be fast, because this runs in between DOM node insertions.
(defun create-context (clone template)
  (let ((context (chain *template-context-map* (get template)))
        (cloned-context (create)))

    (loop
     for key of context do
     (let* ((descriptor (getprop context key))
            (path (@ descriptor path))
            (node (get-path clone path)))
       (setf (getprop cloned-context key)
             (chain *object (assign (create node node) descriptor)))))

    cloned-context))


(defun get-path (node path)
  (let ((result node))
    (loop
     for i from 0 to (- (length path) 1) do
     (let ((j (getprop path i)))
       (setf result (getprop (@ result child-nodes) j))))
    result))


(defun create-array (array template)
  (let* ((nodes (list))
         (proxies (list))
         (proxy nil))
    (loop
     for item in array do
     (let ((result (create-binding item template)))
       (chain proxies (push (@ result 0)))
       (chain nodes (push (@ result 1)))))
    (setf proxy (new (*proxy proxies *proxy-array*)))
    (chain *proxy-template-map* (set proxy template))
    (list nodes proxy)))


(defun create-binding (obj template)
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
         (fragment (chain document (create-document-fragment))))

    (when mount (chain mount (call proxy clone)))
    (when unmount (chain *proxy-unmount-map* (set proxy unmount)))

    ;; Each proxy should contain references to its own delimiters.
    (chain fragment (append-child start-node))
    (chain fragment (append-child clone))
    (chain fragment (append-child end-node))
    (chain *proxy-delimiter-map* (set proxy nodes))

    (chain *target-context-map* (set target context))
    (chain *target-event-map* (set target (create)))
    (chain *target-delimiter-map* (set target (create)))

    ;; Initialization
    (loop
     for key of obj do
     (set-property target key (getprop obj key) proxy))

    (list proxy fragment)))


(defun create-anchor (type key)
  (if (@ main debug)
      (let ((comment (+ (if (eq type 0) 'start
                          (if (eq type 1) 'end 'anchor))
                        " " key)))
        (chain document (create-comment comment)))
    (chain document (create-text-node ""))))


(defun main (origin template)
  (create-binding origin template))


(export
 :default main
 :names
 (*symbol-mount* as mount *symbol-unmount* as unmount))
