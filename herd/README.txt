** What is herd? **

  Given assembler programs for Power, ARM, or X86 (a litmus test)
  herd executes them on top of weak memory models.
  Models are described in simple text files.
           
  Herd is part of the diy tool suite.

** Home **
http://diy.inria.fr/
diy-devel@inria.fr

** Documentation **
Herd documentation is part of diy documentation
http://diy.inria.fr/doc/herd.html

** Compilation and installation **
See INSTALL.txt


** Contents of this release **
This release contains litmus sources.

** Law **

Litmus authors are Jade Alglave and Luc Maranget.

Copyright 2010-2013: Institut National de Recherche en Informatique et
en Automatique, and the authors.

litmus is released under the terms of the Gnu Lesser General Public License
(LGPL). See file LICENSE.txt

** Syntax highlighting **

If you would like syntax highlighting for .cat files,
and you currently use emacs with Tuareg mode, then
copy the following into your ~/.emacs file.


;; "cat mode" is derived from tuareg-mode
;; which is the normal mode for editing
;; OCaml code
(define-derived-mode cat-mode tuareg-mode
  (setq mode-name "herd model")
) 
;; Define extra keywords. I don't know what
;; the semantic difference is between a
;; governing, a keyword, and a builtin is,
;; so I just picked it so the colours are
;; nice.
(font-lock-add-keywords 'cat-mode '(
  ("unshow"   . tuareg-font-lock-governing-face)
  ("show"   . tuareg-font-lock-governing-face)
  ("undefined_unless" . tuareg-font-lock-governing-face)
  ("rln" . font-lock-keyword-face)
  ("set" . font-lock-keyword-face)
  ("acyclic" . font-lock-builtin-face)
  ("empty" . font-lock-builtin-face)
  ("irreflexive" . font-lock-builtin-face)
))
;; Enter this mode when opening a .cat file
(setq auto-mode-alist 
  (cons '("\\.cat$" . cat-mode) auto-mode-alist)
)
