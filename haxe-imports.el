;; package test.package;

;; import test.import.Test;

;;; Code:
(require 'cl-lib)
(require 'thingatpt)
(require 's)
(require 'pcache)

(defgroup haxe-imports nil
  "Customization for haxe imports package"
  :group 'languages)

(defcustom haxe-imports-cache-name "haxe-imports"
  "Name of the cache to be used for the ClassName to Package
  mapping cache."
  :group 'haxe-imports
  :type 'string)

(defcustom haxe-imports-use-cache t
  "Whether packages for classes should be cached"
  :group 'haxe-imports
  :type 'boolean)

(defcustom haxe-imports-save-buffer-after-import-added t
  "`t' to save the current buffer after inserting an import statement."
  :group 'haxe-imports
  :type 'boolean)

(defcustom haxe-imports-find-block-function 'haxe-imports-find-place-after-last-import
  "A function that should find a proper insertion place within
  the block of import declarations."
  :group 'haxe-imports
  :type 'function)

(defcustom haxe-imports-default-packages
  '(("EnumTools" . "haxe")
    ("Serializer" . "haxe")
    ("Unserializer" . "haxe")
    ("Fast" . "haxe.xml"))
  "An alist mapping class names to probable packages of the
classes."
  :group 'haxe-imports
  :type '(alist :key-type string :value-type string))

(defun haxe-imports-go-to-imports-start ()
  "Go to the point where java import statements start or should
start (if there are none)."
  (goto-char (point-min))
  ;; package declaration is always in the beginning of a file, so no need to
  ;; reset the point after the first search
  (let ((package-decl-point (re-search-forward "package .*;" nil t))
        (import-decl-point (re-search-forward "import .*;" nil t)))
    ;; 1. If there are imports in the file - go to the first one
    ;;
    ;; 2. No imports, and the package declaration is available - go to the end
    ;; of the declaration
    ;;
    ;; 3. Neither package nor import declarations are present - just go to the
    ;; first line
    (cond (import-decl-point (goto-char import-decl-point)
                             (beginning-of-line))
          (import-decl-point (goto-char package-decl-point)
                             (forward-line)
                             (open-line 2)
                             (forward-line))
          (t (goto-char (point-min))
             (open-line 1)))))

(defun haxe-imports-get-import (line)
  "Return the fully-qualified package for the given import line."
  (when line
    (cadr (s-match "import \\\(.*\\\);"
                   (string-trim line)))))

(defun haxe-imports-find-place-aftr-last-import (full-name class-name package)
  "Finds the insertion place by moving past the last import declaration in the file."
  (while (re-search-forward "import[ \t]+.+[ \t]*;" nil t))
  (beginning-of-line)
  (unless (equal (point-at-bol) (point-at-eol))
    (forward-line)
    (open-line 1)))

(defun haxe-imports-read-package (class-name cached-package)
  "Reads a package name for a class, offers default values for
known classes"
  (or (and (not current-prefix-arg)
           cached-package)
      (let* ((default-package (cdr (assoc-string class-name haxe-imports-default-packages)))
             (default-prompt (if default-package
                                 (concat "[" default-package "]") ""))
             (prompt (concat "Package " default-prompt ": ")))
        (read-string prompt nil nil default-package))))

;;;###autoload
(defun haxe-imports-scan-file ()
  "Scans a haxe-mode buffer, adding any import class -> package
mappings to the import cache. If called with a prefix arguments
overwrites any existing cache entries for the file."
  (interactive)
  (when (eq 'haxe-mode major-mode)
    (let* ((cache (pcache-repository haxe-imports-cache-name)))
      (dolist (import (haxe-imports-list-imports))
        (message (concat "import: " import))
        ))
    )
  )

;;;###autoload
(defun haxe-imports-list-imports ()
  "Return a list of all fully-qualified packages in the current
Haxe-mode buffer"
  (interactive)
  (cl-mapcar
   #'haxe-imports-get-import
   (cl-remove-if-not (lambda (str) (s-matches? "import[ \t]+.+[ \t]*;" str))
                     (s-lines (buffer-string)))))

;;;###autoload
(defun haxe-imports-add-import-with-package (class-name package)
  "Add an import for the class for the name and package. Uses no caching."
  (interactive (list (read-string "Class name: " (thing-at-point 'symbol))
                     (read-string "Package name: " (thing-at-paint 'symbol))))
  (save-excursion
    (let ((full-name "FullNameTest"))
      (haxe-imports-go-to-imports-start)
      
      (funcall haxe-imports-find-block-function full-name class-name package)

      (insert "import " (concat package "." class-name) ";")
      full-name)))

;;;###autoload
(defun haxe-imports-add-import (class-name)
  "Import the Java class for the symbol at point. Uses the symbol
at the point for the class name, ask for a confirmation of the
class name before adding it.
Checks the import cache to see if a package entry exists for the
given class. If found, adds an import statement for the class. If
not found, prompts for the package and saves it to the cache.
If called with a prefix argument, overwrites the package for an
already-existing class name."
  (interactive (list (read-string "Class name: " (thing-at-point 'symbol))))
  (save-excursion
    (let* ((key (intern class-name))
           (cache (pcache-repository haxe-imports-cache-name))
           ;; Check if we have seen this class's package before
           (cached-package (and haxe-imports-use-cache
                                (pcache-get cache key)))
           ;; If called with a prefix, overwrite the cached value always
           (add-to-cache? (or current-prefix-arg
                              (eq nil cached-package)))

           (package (haxe-imports-read-package class-name cached-package))
           (full-name (haxe-imports-add-import-with-package class-name package)))

      ;; Optionally save the buffer and cache the full package name
      (when haxe-imports-save-buffer-after-import-added
        (save-buffer))

      (when add-to-cache?
        (message "Adding %s -> %s to haxe imports cache"
                 class-name package)
        (pcache-put cache key package)
        (pcache-save cache))
      full-name)))

;;;###autoload
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
