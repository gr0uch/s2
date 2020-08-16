(defvar *passed-check* false)

(defun dep-check ()
  (when *passed-check* (return-from dep-check))
  ;; TODO
  (setf *passed-check* t))

(export
 :default dep-check)
