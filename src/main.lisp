(defvar *symbol-slot* (*symbol 'slot))
(defvar *symbol-text* (*symbol 'text))
(defvar *symbol-html* (*symbol 'html))
(defvar *symbol-class* (*symbol 'class))
(defvar *symbol-attribute* (*symbol 'attribute))
(defvar *symbol-event* (*symbol 'event))
(defvar *tag-slot* '*slot*)

;; A map of targets to contexts.
(defvar *target-context-map* (new (*weak-map)))

;; A map of targets to hash maps keyed by event names and valued by listeners.
(defvar *target-event-map* (new (*weak-map)))

;; A map of targets to hash maps keyed by key names and valued by an array of Nodes.
;; This is used to keep track of delimiters for slots.
(defvar *target-node-map* (new (*weak-map)))

;; A map of proxies to arrays of Nodes.
;; This is used to keep track of delimiters for individual proxy objects.
;; Also holds delimiters for arrays.
(defvar *proxy-node-map* (new (*weak-map)))

;; A map of array proxies to templates.
(defvar *proxy-template-map* (new (*weak-map)))

;; A map of array proxies to anchors.
(defvar *proxy-anchor-map* (new (*weak-map)))

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
  (when (@ main debug) (chain console (log 'set-index arguments)))
  (let* ((numkey (chain *number (parse-int key 10)))
         (is-index (not (chain *number (is-na-n numkey))))
         (is-setter (eq (length arguments) 4))
         (is-delete (eq (length arguments) 2)))

    (when (eq key 'length)
      ;; Setting array length should remove extra values.
      (loop
       for i from value to (- (length target) 1) do
       (let ((nodes (chain *proxy-node-map* (get (getprop target i)))))
         (when nodes (remove-between-delimiters (@ nodes 0) (@ nodes 1))))
       (delete (getprop target i))))

    (when is-delete
      (let ((nodes (chain *proxy-node-map* (get (getprop target key)))))
        (remove-between-delimiters (@ nodes 0) (@ nodes 1)))
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
                 (node (@ result 0))
                 (proxy (@ result 1))
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
                (let ((nodes (chain *proxy-node-map* (get previous-proxy))))
                  (remove-between-delimiters (@ nodes 0) (@ nodes 1)))))

            (if next-proxy
                (let ((next-anchor
                       (@ (chain *proxy-node-map* (get next-proxy)) 0)))
                  (chain parent-node (insert-before node next-anchor)))
              (let ((end-node (@ (chain *proxy-node-map* (get receiver)) 1)))
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
            (let* ((other-nodes (chain *proxy-node-map* (get other-proxy)))
                   (other-start-node (@ other-nodes 0))
                   (other-end-node (@ other-nodes 1))
                   (nodes (chain *proxy-node-map* (get value)))
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
         (is-setter (eq (length arguments) 4))
         (is-delete (eq (length arguments) 2)))
    (when (chain *object prototype has-own-property (call context key))
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
  (if value
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


(defun remove-between-delimiters (start-node end-node)
  (let ((node start-node))
    (loop while (not (chain node (is-same-node end-node))) do
          (let ((old-node node))
            (setf node (@ node next-sibling))
            (chain old-node (remove))))
    (chain end-node (remove))))


(defun set-slot (target key value descriptor)
  ;; Skip deletion if already empty.
  (when (not (or (getprop target key) value)) (return-from set-slot))

  (let* ((anchor (@ descriptor anchor))
         (slot (@ descriptor slot))
         (template (@ descriptor template))
         (hash (chain *target-node-map* (get target)))
         (nodes (getprop hash key))
         (parent-node (@ anchor parent-node))
         (start-node (create-anchor 0 key))
         (end-node (create-anchor 1 key))
         (return-value nil))

    (when nodes
      (remove-between-delimiters (@ nodes 0) (@ nodes 1))
      (delete (getprop hash key)))

    (setf (getprop hash key) (list start-node end-node))
    (chain parent-node (insert-before start-node anchor))
    ;; Create proxies
    (if value
        (if (chain *array (is-array value))
            (let* ((result (create-array value template))
                   (nodes (@ result 0))
                   (proxy (@ result 1)))
              (chain parent-node (insert-before start-node anchor))
              (loop for node in nodes do
                    (chain parent-node (insert-before node anchor)))
              (chain parent-node (insert-before end-node anchor))
              (chain *proxy-anchor-map* (set proxy anchor))
              (chain *proxy-node-map* (set proxy (getprop hash key)))
              (setf return-value proxy))
          (let* ((result (create-binding value template))
                 (node (@ result 0))
                 (proxy (@ result 1)))
            (chain parent-node (insert-before node anchor))
            (setf return-value proxy)))
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


;; TODO: this can be optimized after the first call.
(defun create-context (clone)
  (let ((node nil)
        (iter (chain document (create-node-iterator clone 1)))
        (context (create)))
    (loop
     while (setf node (chain iter (next-node))) do
     (when (eq (@ node tag-name) *tag-slot*)
       (let* ((slot-name (@ node name))
              (anchor (create-anchor 2 slot-name))
              (template-node
               (chain document (query-selector (@ node dataset template)))))
         (chain node parent-node (insert-before anchor node))
         (setf (getprop context slot-name)
               (create anchor anchor
                       slot node
                       template template-node
                       type *symbol-slot*))
         (chain node (remove)))
       (continue))
     (loop
      for key of (@ node dataset) do
      (let ((value (getprop (@ node dataset) key))
            (result nil))
        (case
         key
         ("text" (setf result (create node node type *symbol-text*)))
         ("class" (setf result (create node node type *symbol-class*)))
         ("unsafeHtml" (setf result (create node node type *symbol-html*))))
        (when (chain key (starts-with 'attribute))
          (setf result
                (create node node type *symbol-attribute*
                        ;; Slice off "attribute", all attributes are lowercase.
                        name (chain key (slice 9) (to-lower-case)))))
        (when (chain key (starts-with 'event))
          (setf result
                (create node node type *symbol-event*
                        ;; Slice off "event", all events are lowercase.
                        event (chain key (slice 5) (to-lower-case)))))
        (when result
          (delete (getprop (@ node dataset) key))
          (setf (getprop context value) result)))))

    context))


(defun create-array (array template)
  (let* ((nodes (list))
         (proxies (list))
         (proxy nil))
    (loop
     for item in array do
     (let ((result (create-binding item template)))
       (chain nodes (push (@ result 0)))
       (chain proxies (push (@ result 1)))))
    (setf proxy (new (*proxy proxies *proxy-array*)))
    (chain *proxy-template-map* (set proxy template))
    (list nodes proxy)))


(defun create-binding (obj template)
  (let* ((root (or (@ template content) template))
         (clone (chain root (clone-node t)))
         (proxy (new (*proxy obj *proxy-object*)))
         (context (create-context clone))
         (start-node (create-anchor 0 'proxy))
         (end-node (create-anchor 1 'proxy))
         (nodes (list start-node end-node)))

    ;; Each proxy should contain references to its own delimiters.
    (chain clone (insert-before start-node (@ clone first-child)))
    (chain clone (append-child end-node))
    (chain *proxy-node-map* (set proxy nodes))

    (chain *target-context-map* (set obj context))
    (chain *target-event-map* (set obj (create)))
    (chain *target-node-map* (set obj (create)))

    ;; Initialization
    (chain *object (assign proxy obj))

    (list clone proxy)))


(defun create-anchor (type key)
  (if (@ main debug)
      (let ((comment (+ (if (eq type 0) 'start
                          (if (eq type 1) 'end 'anchor))
                        " " key)))
        (chain document (create-comment comment)))
    (chain document (create-text-node ""))))


(defun main (origin template)
  (create-binding origin template))


(export :default main)
