;;; test-helper --- Test helper for haxe-imports

;;; Commentary:
;; test helper inspired from https://github.com/tonini/overseer.el/blob/master/test/test-helper.el

;;; Code:

(require 'f)

(defvar cpt-path
  (f-parent (f-this-file)))

(defvar haxe-imports-test-path
  (f-dirname (f-this-file)))

(defvar haxe-imports-root-path
  (f-parent haxe-imports-test-path))

(defvar haxe-imports-sandbox-path
  (f-expand "sandbox" haxe-imports-test-path))

(when (f-exists? haxe-imports-sandbox-path)
  (error "Something is already in %s. Check and destroy it yourself" haxe-imports-sandbox-path))

(defmacro within-sandbox (&rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(let ((default-directory haxe-imports-sandbox-path))
     (when (f-exists? haxe-imports-sandbox-path)
       (f-delete default-directory :force))
     (f-mkdir haxe-imports-sandbox-path)
     ,@body
     (f-delete default-directory :force)))

(require 'ert)
(require 'el-mock)
(eval-when-compile
    (require 'cl))
(require 'haxe-imports)

(provide 'test-helper)
;;; test-helper.el ends here
