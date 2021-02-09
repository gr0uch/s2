(import :default dep-check :path "./dep-check.js")

(defvar *symbol-slot* (*symbol "slot"))

;; To avoid attaching context to target objects.
(defvar *context-map* (new (*weak-map)))

(defun setter (target key value receiver)
  (chain console (log (chain *context-map* (get target))))
  (chain *reflect set (apply nil arguments)))

(defvar *proxy-handler* (create set setter))

(defun main (origin template)
  (dep-check)
  (create-proxy origin template))

(defun create-proxy (obj template)
  (let* ((root (or (@ template content) template))
         (clone (chain root (clone-node t)))
         (iter (chain document (create-node-iterator clone 1)))
         (node nil)
         (proxy nil)
         (context (create)))
    (loop
     while (setf node (chain iter (next-node))) do
     (when (eq (@ node tag-name) "SLOT")
       (let ((marker (chain document (create-text-node "")))
             (template-node
              (chain document (query-selector (@ node dataset template)))))
         (chain node parent-node (insert-before marker node))
         (setf (getprop context (@ node name))
               (create node marker
                       template template-node
                       type *symbol-slot*))
         (chain node (remove)))
       (continue))
     (loop for key of (@ node dataset) do
           (chain console (log "x" node key))))
    (setf proxy (new (*proxy obj *proxy-handler*)))
    (chain *context-map* (set obj context)))
  (list clone proxy))

(export :default main)
