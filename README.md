# Emacs Haxe Imports

[![MELPA](http://melpa.org/packages/haxe-imports-badge.svg)](http://melpa.org/#/haxe-imports)

Code for dealing with Haxe imports

## Introduction

This package is based on java-imports by Mathew Lee Hinman found here http://www.github.com/dakrone/emacs-java-imports.

This package adds the *import-haxe-class* function, which will prompt for the class at
point, then the package, then add the required import statement for the class
and cache a "class-name -> package" relationship for any future importing of the
class.

## Usage

```emacs
(require 'haxe-imports)

;; whatever you want to bind it to
(define-key haxe-mode-map (kbd "M-I") 'haxe-imports-add-import-dwim)

```

I also recommend having haxe-imports automatically add any seen imports to the
import cache by adding:

```emacs
(add-hook 'haxe-mode-hook 'haxe-imports-scan-file)
```

## Functions

Functions you may want to bind to a key in Haxe-mode:

| Function                       | Use                                                    |
|--------------------------------|--------------------------------------------------------|
| *haxe-imports-add-import-dwim* | Add import for the symbol at point (or ask if none)    |
| *haxe-imports-add-import*      | Add import for symbol at point, confirming class first |
| *haxe-imports-scan-file*       | Scan imports in the file, adding them to the cache     |

Other useful functions for writing your own tools:

| Function                               |
|----------------------------------------|
| *haxe-imports-add-import-with-package* |
| *haxe-imports-list-imports*            |

## Customization

### Saving buffer automatically after adding an import

*haxe-imports* will default to saving the buffer after adding an import, but you
can customize *haxe-imports-save-buffer-after-import-added* to change this.

### Caching

By default packages are cached the first time they're manually entered, if you
want to overwrite what's in the cache you can invoke *haxe-imports-add-import*
with the prefix key (*C-u*).

To disable caching, set *haxe-imports-use-cache* to *nil*.

### Import style

You can customize *haxe-imports-find-block-function*, either setting it to a
custom function. Currently it is set to the one below:

* *haxe-imports-find-place-after-last-import* (default)

  Simply appends the import to the end of the list of imports

### Cache name

By default haxe-imports will use "*haxe-imports*" as the name of the cache of
class->package names, however, if you want to have separate caches per project,
you can customize *haxe-imports-cache-name* to have a separate String name
(perhaps in a *.dir-locals.el* for per-project imports).

## Things to do:
[x] Add tests
