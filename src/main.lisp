(import
 :default dep-check
 :path "./dep-check.js")

(defun main ()
  (dep-check)
  )

(export
 :default main)
