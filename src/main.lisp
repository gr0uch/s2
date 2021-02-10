(import :default dep-check :path "./dep-check.js")

(defvar *symbol-slot* (*symbol "slot"))
(defvar *symbol-text* (*symbol "text"))
(defvar *symbol-html* (*symbol "html"))
(defvar *symbol-event* (*symbol "event"))
(defvar *tag-slot* "SLOT")

;; A map of targets to contexts.
(defvar *target-context-map* (new (*weak-map)))

;; A map of templates to contexts.
(defvar *template-context-map* (new (*weak-map)))

;; A map of targets to hash maps keyed by event names and valued by listeners.
(defvar *target-event-map* (new (*weak-map)))

;; A map of targets to hash maps keyed by key names and valued by an array of Nodes.
(defvar *target-node-map* (new (*weak-map)))

(defvar *proxy-handler*
  (create set setter
          delete-property setter))


(defun setter (target key value receiver)
  (let* ((context (chain *target-context-map* (get target)))
         (is-setter (eq (length arguments) 4))
         (is-delete (eq (length arguments) 2)))
    (when (chain *object prototype has-own-property (call context key))
      (let* ((descriptor (getprop context key))
             (node (@ descriptor node))
             (type (@ descriptor type)))
        (when (eq type *symbol-text*) (setf (@ node text-content) value))
        (when (eq type *symbol-html*) (setf (@ node inner-h-t-m-l) value))
        (when (eq type *symbol-event*) (set-event target value descriptor))))

    (when is-delete (delete (getprop target key)))
    (when is-setter (chain *reflect set (apply nil arguments))))
  t)


(defun set-event (target value descriptor)
  (let* ((node (@ descriptor node))
         (event (@ descriptor event))
         (hash (chain *target-event-map* (get target)))
         (listener (getprop hash event)))
    (when listener
      (chain node (remove-event-listener
                   event listener (@ listener options))))
    (when value
      (let ((bound-listener (chain value (bind target))))
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
             (marker (chain document (create-text-node "")))
             (template-node
              (chain document (query-selector (@ node dataset template)))))
         (chain node parent-node (insert-before marker node))
         (setf (getprop context key)
               (create node marker
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
                        event (chain key (slice 5) (to-lower-case))
                        type *symbol-event*)))
        (when result (setf (getprop context value) result)))))
    context))


(defun create-proxy (obj template)
  (let* ((root (or (@ template content) template))
         (clone (chain root (clone-node t)))
         (iter nil)
         (node nil)
         (proxy (new (*proxy obj *proxy-handler*)))
         (context (chain *template-context-map* (get template))))

    ;; Building up context object for the first time takes work to
    ;; iterate through all nodes, better to skip this if possible.
    (when (not context)
      (setf context (create-context clone))
      (chain *template-context-map* (set template context)))

    (chain *target-context-map* (set obj context))
    (chain *target-event-map* (set obj (create)))
    (chain *target-node-map* (set obj (create)))
    (chain console (log "x" obj context))
    ;; Initialization
    (loop
     for key of obj do
     (setf (getprop proxy key) (getprop obj key))))

  (list clone proxy))


(defun main (origin template)
  (dep-check)
  (create-proxy origin template))


(export :default main)
