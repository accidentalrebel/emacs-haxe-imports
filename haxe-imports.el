;;; Code:
(require 'thingatpt)
(require 's)

(defun haxe-imports-add-import (class-name)
  (message (concat "Class name is " class-name))
  )

(defun haxe-imports-add-import-dwim ()
  "Add an import statement for the class at point. If no class is
found, prompt for the class name. If the class's package already
exists in the cache, add it and return, otherwise prompt for the
package and cache it for future statements."
  (interactive)
  (let ((class (or (thing-at-point 'symbol)
                   (read-string "Class name: "))))
    (haxe-imports-add-import class)))

(provide 'haxe-imports)

;;; haxe-imports.el ends here
