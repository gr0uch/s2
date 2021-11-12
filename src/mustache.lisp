;; OPTIONAL MODULE
;; This function converts a subset of Mustache into data attributes, as
;; expected by s2.

(defparameter *tag-open* "{{")
(defparameter *tag-close* "}}")
(defparameter *comment-regexp*
  (new (*reg-exp (+ *tag-open* "!(.+?)" *tag-close*) "gs")))
(defparameter *var-regexp*
  (new (*reg-exp (+ "^" *tag-open* "([^{}]+?)" *tag-close* "$"))))
(defparameter *var-regexp-global*
  (new (*reg-exp (+ "(" *tag-open* "{1,2}(?:.+?)" *tag-close* "{1,2})") "gm")))
(defparameter *unescaped-var-regexp*
  (new (*reg-exp (+ "^" *tag-open* "[{&]([^{}]+?)}?" *tag-close* "$"))))
(defparameter *section-open-regexp*
  (new (*reg-exp (+ *tag-open* "#([^{}]+?)" *tag-close*))))
(defparameter *section-close-regexp*
  (new (*reg-exp (+ *tag-open* "\/([^{}]+?)" *tag-close*))))
(defparameter *partial-regexp*
  (new (*reg-exp (+ *tag-open* "(?:>|\&gt;)([^{}]+?)" *tag-close*))))

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
  (when (and (@ element first-child)
             (eq (@ element first-child node-type)
                 (@ parse window *node "TEXT_NODE")))
    (let* ((text (chain (@ element first-child node-value) (trim)))
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
          (chain *array prototype slice (call (@ element child-nodes))))
         ;; these are stacks
         (container (list))
         (last-opened (list)))
    ;; first pass: tokenize text
    (loop
     for node in nodes do
     (when (not (eq (@ node node-type) (@ parse window *node "TEXT_NODE")))
       (when (length container)
         (chain (@ container 0) (append-child node)))
       (continue))
     (let* ((text (@ node node-value))
            (tokens (chain text
                           (split *var-regexp-global*)
                           ;; may want to keep whitespace
                           ;; (map (lambda (s) (chain s (trim))))
                           (filter (lambda (s) (chain s (trim)))))))
       (loop
        for token in tokens do
        (let ((new-node nil)
              (match-open (chain token (match *section-open-regexp*)))
              (match-close (chain token (match *section-close-regexp*)))
              (match-partial (chain token (match *partial-regexp*)))
              (match-var (chain token (match *var-regexp*)))
              (match-unescaped-var
               (chain token (match *unescaped-var-regexp*))))
          (when match-open
            (setf new-node (chain parse window document (create-element 'slot)))
            (let ((name (chain (@ match-open 1) (trim))))
              (chain last-opened (unshift name))
              (chain new-node (set-attribute 'name name)))
            (if (length container)
                (chain container 0 (append-child new-node))
              (chain node parent-node (insert-before new-node node)))
            (chain container (unshift new-node))
            (continue))
          (when (and match-partial (length container))
            (setf (@ container 0 dataset template)
                  (chain (@ match-partial 1) (trim)))
            (continue))
          (when (and match-close (eq (chain (@ match-close 1) (trim))
                                     (@ last-opened 0)))
            (chain last-opened (shift))
            (chain container (shift))
            (continue))
          ;; interpolate free vars as span elements
          (when match-var
            (setf new-node (chain parse window document (create-element 'span))
                  (@ new-node dataset text) (@ match-var 1))
            (chain node parent-node (insert-before new-node node))
            (continue))
          (when match-unescaped-var
            (setf new-node (chain parse window document (create-element 'span))
                  (@ new-node dataset unsafe-html) (@ match-unescaped-var 1))
            (chain node parent-node (insert-before new-node node))
            (continue))
          (setf new-node (chain parse window document (create-text-node token)))
          (chain node parent-node (insert-before new-node node))))
       (chain node (remove))))))

(defun parse (template)
  (when (eq (typeof template) 'string)
    (let ((element (chain parse window document (create-element 'template))))
      (setf (@ element inner-h-t-m-l) template
            template element)))

  ;; First, strip comments.
  (setf (@ template inner-h-t-m-l)
        (chain template inner-h-t-m-l (replace *comment-regexp* "")))

  (let* ((content (@ template content))
         (trimmer (chain parse window document
                         (create-node-iterator
                          content
                          ;; Hardcoding value here.
                          ;; (@ parse window *node-filter "SHOW_TEXT")
                          4)))
         (iterator (chain parse window document
                          (create-node-iterator
                           content
                           ;; Hardcoding value here.
                           ;; (@ parse window *node-filter "SHOW_ELEMENT")
                           1)))
         (current nil))
    (loop
     while (setf current (chain trimmer (next-node))) do
     (let ((trimmed (chain current node-value (trim))))
       ;; Commented out to preserve whitespace in templates.
       ;; (setf (@ current node-value) trimmed)
       (when (not trimmed) (chain current (remove)))))
    (loop
     while (setf current (chain iterator (next-node))) do
     (process-element current)))
  template)

(setf (@ parse window) window)

(export :default parse)
