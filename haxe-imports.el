;;; Code:
(require 'thingatpt)
(require 's)

(defun haxe-imports-read-package (class-name)
  "Reads a package name for a class, offers default values for
known classes"
  (let ((package-name "sample.package.name"))
    package-name
    )
  )

(defun haxe-imports-go-to-imports-start ()
  "Go to the point where java import statements start or should
start (if there are none)."
  (goto-char (point-min))
  )

(defun haxe-imports-add-import-with-package (class-name package)
  "Add an import for the class for the name and package. Uses no caching."
  (interactive (list (read-string "Class name: " (thing-at-point 'symbol))
                     (read-string "Package name: " (thing-at-paint 'symbol))))
  (save-excursion
    (haxe-imports-go-to-imports-start)
    (insert "import " (concat package "." class-name) ";")
    "FullNameTest"
    )
  )

(defun haxe-imports-add-import (class-name)
  (interactive (list (read-string "Class name: " (thing-at-point 'symbol))))
  (save-excursion
    (let* ((key (intern class-name))
           (package (haxe-imports-read-package class-name))
           (full-name (haxe-imports-add-import-with-package class-name package)))
      (message (concat "Class name is " class-name))
      (message (concat "Package is " package))
      (message (concat "Full name is " full-name))
      )
    )
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
