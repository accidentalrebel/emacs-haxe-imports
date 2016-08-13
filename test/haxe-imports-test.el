;;; haxe-imports-test.el --- tests for haxe imports

;;; Code:

(require 'ert)
;;(load-file "haxe-imports.el")

(ert-deftest t-go-to-imports-start ()
  ;; both package and imports present? Goto to the first import line beginning
  (with-temp-buffer
    (insert "package mypackage;\n")
    (insert "\n")
    (insert "import haxe.EnumTools;\n")
    (insert "import haxe.xml.Fast;\n")
    (insert "\n\n")
    (haxe-imports-go-to-imports-start)
    (should (equal (line-number-at-pos) 3)))

  ;; no package and imports present? First import line
  (with-temp-buffer
    (insert "\n")
    (insert "\n")
    (insert "\n")
    (insert "import haxe.EnumTools;\n")
    (insert "import haxe.xml.Fast;\n")
    (insert "\n\n")
    (haxe-imports-go-to-imports-start)
    (should (equal (line-number-at-pos) 4)))

  ;; package present, no imports? Add a correct import place, keeping the empty
  ;; lines
  (with-temp-buffer
    (insert "\n")
    (insert "package mypackage;\n")
    (insert "\n")
    (insert "\n")
    (insert "class A {}\n")
    (haxe-imports-go-to-imports-start)
    (should (equal (line-number-at-pos) 4))
    (should (equal (count-lines (point-min) (point-max)) 7)))

  ;; no package, no imports? Stay in the beginning, add lines required
  (with-temp-buffer
    (insert "\n")
    (insert "\n")
    (insert "\n")
    (insert "class A {}\n")
    (haxe-imports-go-to-imports-start)
    (should (equal (line-number-at-pos) 1))
    (should (equal (count-lines (point-min) (point-max)) 5))))

(ert-deftest t-add-imports ()
  (with-temp-buffer
    (setq-local haxe-imports-find-block-function
                #'haxe-imports-find-place-after-last-import)
    (insert "package mypackage;\n\n")
    (insert "import haxe.xml.Fast;\n\n\n")
    (haxe-imports-add-import-with-package "EnumTools" "haxe")
    (should
     (equal
      (buffer-string)
      (concat
       "package mypackage;\n\n"
       "import haxe.xml.Fast;\n"
       "import haxe.EnumTools;\n\n\n")))))

(ert-deftest t-list-imports ()
  (with-temp-buffer
    (insert "package mypackage;\n")
    (insert "\n")
    (insert "import org.Thing;\n")
    (insert "\n")
    (insert "import haxe.xml.Fast;\n")
    (insert "import haxe.EnumTools;\n")
    (insert "\n")
    (insert "public class Foo {}")
    (should
     (equal
      (haxe-imports-list-imports)
      '("org.Thing" "haxe.xml.Fast" "haxe.EnumTools")))))

(ert-deftest t-pkg-and-class-from-import ()
  (should
   (equal (haxe-imports-get-package-and-class "haxe.xml.Fast")
          '("haxe.xml" "Fast")))
  (should
   (equal (haxe-imports-get-package-and-class "org.foo.bar.baz.ThingOne")
          '("org.foo.bar.baz" "ThingOne"))))

;; End:
;;; haxe-imports-test.el ends here
