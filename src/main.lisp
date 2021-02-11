(import :default dep-check :path "./dep-check.js")

(defvar *symbol-slot* (*symbol "slot"))
(defvar *symbol-text* (*symbol "text"))
(defvar *symbol-html* (*symbol "html"))
(defvar *symbol-event* (*symbol "event"))
(defvar *tag-slot* "SLOT")

;; A map of targets to contexts.
(defvar *target-context-map* (new (*weak-map)))

;; A map of targets to hash maps keyed by event names and valued by listeners.
(defvar *target-event-map* (new (*weak-map)))

;; A map of targets to hash maps keyed by key names and valued by an array of Nodes.
;; This is used to keep track of delimiters for slots.
(defvar *target-node-map* (new (*weak-map)))

;; A map of proxies to arrays of Nodes.
;; This is used to keep track of delimiters for individual proxy objects.
(defvar *proxy-node-map* (new (*weak-map)))

(defvar *proxy-object* (create set set-property delete-property set-property))
(defvar *proxy-array* (create set set-index delete-property set-index))


(defun set-index (target key value receiver)
  (chain console (log "i" arguments))
  (let* ((is-setter (eq (length arguments) 4))
         (is-delete (eq (length arguments) 2)))

    ;; TODO: array logic!

    (when is-delete
      ;; (let ((nodes (chain *proxy-node-map* (get (getprop target key)))))
      ;;   (remove-between-delimiters (@ nodes 0) (@ nodes 1)))
      (delete (getprop target key)))
    (when is-setter (chain *reflect (set target key value receiver))))
  t)


(defun set-property (target key value receiver)
  (let* ((context (chain *target-context-map* (get target)))
         (is-setter (eq (length arguments) 4))
         (is-delete (eq (length arguments) 2)))
    (when (chain *object prototype has-own-property (call context key))
      (let* ((descriptor (getprop context key))
             (node (@ descriptor node))
             (type (@ descriptor type)))
        (when (eq type *symbol-text*) (setf (@ node text-content) value))
        (when (eq type *symbol-html*) (setf (@ node inner-h-t-m-l) value))
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


(defun remove-between-delimiters (start-node end-node)
  (let ((node start-node))
    (loop while (not (chain node (is-same-node end-node))) do
          (let ((old-node node))
            (setf node (@ node next-sibling))
            (chain old-node (remove))))
    (chain end-node (remove))))


(defun set-slot (target key value descriptor)
  (let* ((anchor (@ descriptor anchor))
         (slot (@ descriptor slot))
         (template (@ descriptor template))
         (hash (chain *target-node-map* (get target)))
         (nodes (getprop hash key)))
    ;; TODO: use slot empty state
    (when nodes
      (remove-between-delimiters (@ nodes 0) (@ nodes 1))
      (delete (getprop hash key)))
    (when value
      (let ((parent-node (@ anchor parent-node))
            (start-node (create-anchor 0 key))
            (end-node (create-anchor 1 key)))
        (setf (getprop hash key) (list start-node end-node))
        (if (chain *array (is-array value))
          (let* ((result (create-array value template))
                 (nodes (@ result 0))
                 (proxy (@ result 1)))
            (chain parent-node (insert-before start-node anchor))
            (loop for node in nodes do
                  (chain parent-node (insert-before node anchor)))
            (chain parent-node (insert-before end-node anchor))
            proxy)
        (let* ((result (create-binding value template))
               (node (@ result 0))
               (proxy (@ result 1)))
          (chain parent-node (insert-before start-node anchor))
          (chain parent-node (insert-before node anchor))
          (chain parent-node (insert-before end-node anchor))
          proxy))))))


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
        (when (eq key "text")
          (setf result (create node node type *symbol-text*)))
        (when (eq key "unsafeHtml")
          (setf result (create node node type *symbol-html*)))
        (when (chain key (starts-with "event"))
          (setf result
                (create node node
                        ;; Slice off "event", all events are lowercase.
                        event (chain key (slice 5) (to-lower-case))
                        type *symbol-event*)))
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
    (list nodes proxy)))


(defun create-anchor (type key)
  (let ((comment (+ (if (eq type 0) "start" (if (eq type 1) "end" "anchor"))
                    " " key)))
    (chain document (create-comment comment))))


(defun create-binding (obj template)
  (let* ((root (or (@ template content) template))
         (clone (chain root (clone-node t)))
         (proxy (new (*proxy obj *proxy-object*)))
         (context (create-context clone))
         (start-node (create-anchor 0 "proxy"))
         (end-node (create-anchor 1 "proxy"))
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


(defun main (origin template)
  (dep-check)
  (create-binding origin template))


(export :default main)
