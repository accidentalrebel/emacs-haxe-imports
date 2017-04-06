(require 'f)

(defvar emacs-haxe-imports-support-path
  (f-dirname load-file-name))

(defvar emacs-haxe-imports-features-path
  (f-parent emacs-haxe-imports-support-path))

(defvar emacs-haxe-imports-root-path
  (f-parent emacs-haxe-imports-features-path))

(add-to-list 'load-path emacs-haxe-imports-root-path)

;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'emacs-haxe-imports)
  (require 'espuds)
  (require 'ert))

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
