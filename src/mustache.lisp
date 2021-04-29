(defvar *comment-regexp* (regex "/{{!(.+?)}}/gs"))
(defvar *var-regexp* (regex "/^{{([^{}]+?)}}$/"))
(defvar *var-regexp-global* (regex "/({{(?:[^{}]+?)}})/gm"))
(defvar *unescaped-var-regexp* (regex "/^{{[{&]([^{}]+?)}{2,3}$/"))
(defvar *section-open-regexp* (regex "/{{#([^{}]+?)}}/"))
(defvar *section-close-regexp* (regex "/{{\\/([^{}]+?)}}/"))
(defvar *partial-regexp* (regex "/{{>([^{}]+?)}}/"))

(defun process-element (element)
  ;; match attributes
  (let ((attribute-entries
         (loop
          for attribute in (@ element attributes) collect
          (list (@ attribute name) (@ attribute value)))))
    (loop
     for attribute in attribute-entries do
     (let* ((name (@ attribute 0))
            (value (@ attribute 1))
            (result (chain value (match *var-regexp*)))
            (trimmed nil)
            (special-case 0))
       (when (not result) (continue))
       (setf trimmed (chain (@ result 1) (trim)))
       (when (eq name 'class)
         (setf (@ element dataset class) trimmed
               special-case 1))
       (when (eq name 'value)
         (setf (@ element dataset value) trimmed
               special-case 1))
       (when (chain name (starts-with "on"))
         (chain element (set-attribute
                         (+ "data-event-" (chain name (slice 2)))
                         trimmed))
         (setf special-case 1))
       (when (chain name (starts-with "data-"))
         (chain element (set-attribute name trimmed))
         (setf special-case 2))
       (when (not special-case)
         (chain element (set-attribute (+ "data-attribute-" name)
                                       trimmed)))
       (when (not (eq special-case 2))
         (chain element (remove-attribute name))))))
  ;; match text
  (when (and (length (@ element child-nodes))
             (eq (@ element child-nodes 0 node-type)
                 (@ *node "TEXT_NODE")))
    (let* ((text (chain (@ element child-nodes 0 node-value) (trim)))
           (match-var (chain (@ element text-content)
                             (match *var-regexp*)))
           (match-unescaped-var (chain (@ element text-content)
                                       (match *unescaped-var-regexp*))))
      (when match-var
        (setf (@ element dataset text) (@ match-var 1)
              (@ element text-content) ""))
      (when match-unescaped-var
        (setf (@ element dataset unsafe-html) (@ match-unescaped-var 1)
              (@ element text-content) ""))))
  ;; match sections
  (let* ((nodes
          (loop for node in (@ element child-nodes) collect node))
         (container nil)
         (last-opened nil))
    ;; first pass: tokenize text
    (loop
     for node in nodes do
     (when (not (eq (@ node node-type) (@ *node "TEXT_NODE")))
       (when container
         (chain container (append-child node)))
       (continue))
     (let* ((text (@ node node-value))
            (tokens (chain text
                           (split *var-regexp-global*)
                           (map (lambda (s) (chain s (trim))))
                           (filter *boolean))))
       (loop
        for token in tokens do
        (let ((new-node nil)
              (match-open (chain token (match *section-open-regexp*)))
              (match-close (chain token (match *section-close-regexp*)))
              (match-partial (chain token (match *partial-regexp*))))
          (when match-open
            (setf last-opened (chain (@ match-open 1) (trim))
                  new-node (chain document (create-element 'slot))
                  container new-node)
            (chain new-node (set-attribute 'name last-opened))
            (chain node parent-node (insert-before new-node node))
            (continue))
          (when (and match-partial container)
            (setf (@ container dataset template)
                  (chain (@ match-partial 1) (trim)))
            (continue))
          (when (and match-close (eq (chain (@ match-close 1) (trim))
                                     last-opened))
            (setf last-opened nil
                  container nil)
            (continue))
          (setf new-node (chain document (create-text-node token)))
          (chain node parent-node (insert-before new-node node))))
       (chain node (remove))))))

(defun parse (template)
  (when (eq (typeof template) 'string)
    (let ((element (chain document (create-element 'template))))
      (setf (@ element inner-h-t-m-l) template
            template element)))

  ;; First, strip comments.
  (setf (@ template inner-h-t-m-l)
        (chain template inner-h-t-m-l (replace *comment-regexp* "")))

  (let* ((content (@ template content))
         (trimmer (chain document (create-node-iterator
                                   content (@ *node-filter "SHOW_TEXT"))))
         (iterator (chain document (create-node-iterator
                                    content (@ *node-filter "SHOW_ELEMENT"))))
         (current nil))
    (loop
     while (setf current (chain trimmer (next-node))) do
     (let ((trimmed (chain current node-value (trim))))
       (setf (@ current node-value) trimmed)
       (when (not trimmed) (chain current (remove)))))
    (loop
     while (setf current (chain iterator (next-node))) do
     (process-element current)))
  template)

(export :default parse)
