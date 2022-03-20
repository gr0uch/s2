;; OPTIONAL MODULE
;; This function converts a subset of Mustache into data attributes, as
;; expected by s2.

(defparameter *tag-open* "\\{\\{")
(defparameter *tag-close* "\\}\\}")
(defparameter *tag-text-open* "\\{{2,3}")
(defparameter *tag-text-close* "\\}{2,3}")
(defparameter *comment-regexp*
  (new (*reg-exp (+ *tag-open* "!(.*?)" *tag-close*) "gsu")))
(defparameter *attr-regexp*
  (new (*reg-exp (+ "\\s([^< ]+?)=['\"]"
                    *tag-open* "\\s*(\\S+?)\\s*" *tag-close* "['\"]") "gmu")))
(defparameter *free-text-regexp*
  (new (*reg-exp (+ *tag-text-open* "([^>]*?)" *tag-text-close*) "gmu")))
(defparameter *unescaped-text-regexp*
  (regex "/\\{\\{\\{([^>]*?)\\}\\}\\}/u"))
(defparameter *enclosed-text-regexp*
  (new (*reg-exp (+ "<(.+?)>\\s*"
                    *tag-text-open* "([^>]+?)" *tag-text-close*
                    "\\s*<\/(\\S+?)>") "gmu")))
(defparameter *partial-regexp*
  (new (*reg-exp (+ *tag-open* "#\\s*(\\S+?)\\s*" *tag-close*
                    "\\s*" *tag-open* ">\\s*(\\S+?)\\s*" *tag-close* "\\s*"
                    *tag-open* "\/\\s*(\\S+?)\\s*" *tag-close*) "gmu")))
(defparameter *section-open-regexp*
  (new (*reg-exp (+ *tag-open* "#\\s*(\\S+?)\\s*" *tag-close*) "gmu")))
(defparameter *section-close-regexp*
  (new (*reg-exp (+ *tag-open* "\/\\s*(\\S+?)\\s*" *tag-close*) "gmu")))
(defparameter *self-closing-regexp*
  (regex "/<(?!\\/)(\\S+?)\\s+([^>]*?)\\s*?\\/>/gmu"))

(defparameter *template-hash-map* (new (*weak-map)))

(defun replace-attr (match attr key)
  (setf attr (chain attr (trim))
        key (chain key (trim)))
  (let ((prefix "attribute-"))
    (when (chain attr (starts-with "on"))
      (setf prefix "event-"
            attr (chain attr (slice 2))))
    (when (chain attr (starts-with "style:"))
      (setf prefix "style-"
            attr (chain attr (slice 6))))
    (when (chain attr (starts-with "class:"))
      (setf prefix "classlist-"
            attr (chain attr (slice 6))))
    (when (chain attr (starts-with "data-"))
      (setf prefix ""
            attr (chain attr (slice 5))))
    (when (or (eq attr "class")
              (eq attr "value"))
      (setf prefix ""))
    (+ " data-" prefix attr "=\"" key "\"")))

(defun replace-free-text (match key)
  (let ((attr "text"))
    (when (chain match (match *unescaped-text-regexp*))
      (setf attr "unsafe-html"))
    (+ "<span data-" attr "=\"" (chain key (trim)) "\"></span>")))

(defun replace-enclosed-text (match opening-tag key closing-tag)
  (let ((attr "text"))
    (when (chain match (match *unescaped-text-regexp*))
      (setf attr "unsafe-html"))
    (+ "<" opening-tag " data-" attr "=\"" (chain key (trim))
       "\"></" closing-tag ">")))

(defun hash-str (str)
  (let ((i -1)
        (h 2))
    (loop while (< i (1- (length str))) do
          (setf h (ash (+ (* 3 h) (chain str (char-code-at (incf i)))) 0)))
    (abs h)))

(defun parse-mustache (template)
  (let ((element (chain parse-mustache window document
                        (create-element 'template))))
    (setf template (process-mustache template)
          (@ element inner-h-t-m-l) template)
    element))

(defun process-mustache (template)
  (let ((result
         (chain
          template
          ;; First, strip comments.
          (replace *comment-regexp* "")
          ;; Process partials.
          (replace-all *partial-regexp*
                       "<slot name=\"$1\" data-template=\"$2\"></slot>")
          ;; Process sections.
          (replace-all *section-open-regexp* "<slot name=\"$1\">")
          (replace-all *section-close-regexp* "</slot>")
          ;; Process attributes.
          (replace-all *attr-regexp* replace-attr)
          ;; Process text.
          (replace-all *enclosed-text-regexp* replace-enclosed-text)
          (replace-all *free-text-regexp* replace-free-text))))

    ;; Optional: fix JSX-style self-closing tags.
    (when (@ parse-mustache self-closing)
      (setf result
            (chain result
                   (replace-all *self-closing-regexp* "<$1 $2></$1>"))))

    result))

(defun create-mustache-tag (register-template)
  (defun tagged-mustache (strs)
    (let ((result (list (elt strs 0)))
          (args (chain *array prototype slice
                       (call arguments 1)
                       (map (lambda (element)
                              (chain *template-hash-map* (get element))))))
          (element nil)
          (hash nil))
      (loop for hash in args
            for i from 1 to (length strs) do
            (chain result
                   (push (if hash (+ "{{>" hash "}}") (elt arguments i))
                         (elt strs i))))
      (setf result (chain result (join ""))
            element (parse-mustache result)
            hash (+ "template" (hash-str result)))
      (chain *template-hash-map* (set element hash))
      (register-template hash element)
      element))
  tagged-mustache)

(setf (@ parse-mustache window)
      (if (not (eq (typeof window) 'undefined)) window nil)
      (@ parse-mustache self-closing) false)

(export :default parse-mustache
        :names (process-mustache
                create-mustache-tag))
