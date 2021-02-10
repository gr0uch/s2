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
(defvar *target-node-map* (new (*weak-map)))

(defvar *proxy-handler*
  (create set setter
          delete-property setter))


(defun setter (target key value receiver)
  (chain console (log "set" arguments))
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
               setter (chain *reflect (set target key proxy receiver))))))))

    (when is-delete (delete (getprop target key)))
    (when is-setter (chain *reflect (set target key value receiver))))
  t)


(defun set-slot (target key value descriptor)
  (let* ((anchor (@ descriptor anchor))
         (slot (@ descriptor slot))
         (template (@ descriptor template))
         (hash (chain *target-node-map* (get target)))
         (nodes (getprop hash key)))
    (when nodes
      (loop
       for item in nodes do
       (if (chain *array (is-array item))
           (loop for node in item do
                 (chain node (remove)))
         (chain item (remove))))
      (delete (getprop hash key)))
    (when value
      (if (chain *array (is-array value))
          (progn
            ;; TODO: unhandled case
            )
        (let* ((result (create-proxy value template))
               (node (@ result 0))
               (proxy (@ result 1)))
          (setf (getprop hash key)
                (chain *array prototype slice (call (@ node child-nodes))))
          (chain (@ anchor parent-node) (insert-before node anchor))
          proxy)))))


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
       (let ((key (@ node name))
             (anchor (chain document (create-text-node "")))
             (template-node
              (chain document (query-selector (@ node dataset template)))))
         (chain node parent-node (insert-before anchor node))
         (setf (getprop context key)
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


(defun create-proxy (obj template)
  (let* ((root (or (@ template content) template))
         (clone (chain root (clone-node t)))
         (iter nil)
         (node nil)
         (proxy (new (*proxy obj *proxy-handler*)))
         (context (create-context clone)))

    (chain *target-context-map* (set obj context))
    (chain *target-event-map* (set obj (create)))
    (chain *target-node-map* (set obj (create)))

    ;; Initialization
    (loop
     for key of obj do
     (setf (getprop proxy key) (getprop obj key))))

  (list clone proxy))


(defun main (origin template)
  (dep-check)
  (create-proxy origin template))


(export :default main)
