;; OPTIONAL MODULE
;; This function converts a subset of Mustache into data attributes, as
;; expected by s2.

(defparameter *tag-open* "{{")
(defparameter *tag-close* "}}")
(defparameter *tag-text-open* "{{2,3}")
(defparameter *tag-text-close* "}{2,3}")
(defparameter *comment-regexp*
  (new (*reg-exp (+ *tag-open* "!(.*?)" *tag-close*) "gs")))
(defparameter *attr-regexp*
  (new (*reg-exp (+ "\\s([^< ]+?)=['\"]"
                    *tag-open* "(.*?)" *tag-close* "['\"]") "gm")))
(defparameter *free-text-regexp*
  (new (*reg-exp (+ *tag-text-open* "(.*?)" *tag-text-close*) "gm")))
(defparameter *unescaped-text-regexp*
  (new (*reg-exp "{{{(.*?)}}}")))
(defparameter *enclosed-text-regexp*
  (new (*reg-exp (+ "<(.+?)>\\s*"
                    *tag-text-open* "(.*?)" *tag-text-close*
                    "\\s*<\/(.+?)>") "gm")))
(defparameter *partial-regexp*
  (new (*reg-exp (+ *tag-open* "#(.*?)" *tag-close*
                    "\\s*" *tag-open* ">(.*?)" *tag-close* "\\s*"
                    *tag-open* "\/(.*?)" *tag-close*) "gm")))
(defparameter *section-open-regexp*
  (new (*reg-exp (+ *tag-open* "#(.*?)" *tag-close*) "gm")))
(defparameter *section-close-regexp*
  (new (*reg-exp (+ *tag-open* "\/(.*?)" *tag-close*) "gm")))

(defparameter *template-hash-map* (new (*weak-map)))

(defun replace-attr (match attr key)
  (setf attr (chain attr (trim))
        key (chain key (trim)))
  (let ((prefix "attribute-"))
    (when (chain attr (starts-with "on"))
      (setf prefix "event-"
            attr (chain attr (slice 2))))
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

(defun replace-partial (match key partial)
  (+ "<slot name=\"" (chain key (trim))
     "\" data-template=\"" (chain partial (trim)) "\"></slot>"))

(defun replace-section-open (match key)
  (+ "<slot name=\"" (chain key (trim)) "\">"))

(defun replace-section-close (match)
  (progn "</slot>"))

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
  (chain
   template
   ;; First, strip comments.
   (replace *comment-regexp* "")
   ;; Process partials.
   (replace-all *partial-regexp* replace-partial)
   ;; Process sections.
   (replace-all *section-open-regexp* replace-section-open)
   (replace-all *section-close-regexp* replace-section-close)
   ;; Process attributes.
   (replace-all *attr-regexp* replace-attr)
   ;; Process text.
   (replace-all *enclosed-text-regexp* replace-enclosed-text)
   (replace-all *free-text-regexp* replace-free-text)))

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
      (if (not (eq (typeof window) 'undefined)) window nil))

(export :default parse-mustache
        :names (process-mustache
                create-mustache-tag))
