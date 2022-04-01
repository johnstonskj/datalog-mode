;;; datalog-mode.el --- Major mode for editing Datalog resources. -*- coding: utf-8; lexical-binding: t; -*-

;; Author: simon Johnston <johnstonskj@gmail.com>

;;; License:

;; MIT License
;;
;; Copyright (c) 2022 Simon Johnston
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Very simple Datalog major mode with syntax highlighting.

;; This is a very preliminary attempt, it needs some analysis for flycheck,
;; completion, indenting and more.

;;; Code:

;; --------------------------------------------------------------------------
;; Top-level customization.
;; --------------------------------------------------------------------------

(defgroup datalog nil
  "Settings for the Datalog language."
  :group 'languages)

;; --------------------------------------------------------------------------
;; Initialize Font-lock customization
;; --------------------------------------------------------------------------

(defgroup datalog-font-lock nil
  "Datalog mode font locking patterns."
  :group 'datalog)

;; Keywords

(defcustom datalog-processing-instructions
  '("assert" "from" "infer" "input" "pragma" "output")
  "List of Datalog processing instructions used by font locking."
  :group 'datalog-font-lock
  :type '(list string))

(defcustom datalog-pragmas
  '("base"
    "constraints"
    "disjunction"
    "extended_numerics"
    "functional_dependencies"
    "negation"
    "results"
    "strict")
  "List of Datalog pragmas used by font locking."
  :group 'datalog-font-lock
  :type '(list string))

(defcustom datalog-types
  '("boolean" "decimal" "float" "integer" "string")
  "List of Datalog types used by font locking."
  :group 'datalog-font-lock
  :type '(list string))

(defcustom datalog-operator-terms
  '("and" "or" "not" "matches")
  "List of Datalog types used by font locking."
  :group 'datalog-font-lock
  :type '(list string))

(defcustom datalog-constants
  '("false" "true")
  "List of Datalog constant values used by font locking."
  :group 'datalog-font-lock
  :type '(list string))

;; Faces

(defface datalog-comment-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Font face for line and block comments."
  :group 'datalog-font-lock)

(defface datalog-type-face
  '((t :inherit font-lock-type-face))
  "Font face for builtin types."
  :group 'datalog-font-lock)

(defface datalog-constant-face
  '((t :inherit font-lock-constant-face))
  "Font face for boolean and numeric constant values."
  :group 'datalog-font-lock)

(defface datalog-string-face
  '((t :inherit font-lock-string-face))
  "Font face for string values."
  :group 'datalog-font-lock)

(defface datalog-processing-instruction-face
  '((t :inherit font-lock-keyword-face))
  "Font face for processing instructions."
  :group 'datalog-font-lock)

(defface datalog-pragma-face
  '((t :inherit font-lock-builtin-face))
  "Font face for pragma identifiers."
  :group 'datalog-font-lock)

(defface datalog-predicate-face
  '((t :inherit font-lock-function-name-face))
  "Font face for relation and atom predicates."
  :group 'datalog-font-lock)

(defface datalog-variable-name-face
  '((t :inherit font-lock-variable-name-face :weight bold))
  "Font face for literal variables."
  :group 'datalog-font-lock)

(defun datalog--make-font-lock-defaults ()
  (let ((processing-instructions-regexp
         (concat "\\." (regexp-opt datalog-processing-instructions 'symbols)))
        (datalog-pragmas-regexp
         (regexp-opt datalog-pragmas 'symbols))
        (datalog-types-regexp
         (regexp-opt datalog-types 'symbols))
        (datalog-constants-regexp
         (regexp-opt datalog-constants 'symbols)))
    `(((,datalog-types-regexp . 'datalog-type-face)
       (,datalog-constants-regexp . 'datalog-constant-face)
       (,datalog-pragmas-regexp . 'datalog-pragma-face)
       (,processing-instructions-regexp . 'datalog-processing-instruction-face)
       ("\\_<[[:lower:]][[:lower:][:upper:][:digit:]_]*\\_>" .
        'datalog-predicate-face)
       ("\\_<[[:upper:]][[:lower:][:upper:][:digit:]_]*\\_>" .
        'datalog-variable-name-face)
       ("[+-]?[[:digit:]]+\\([.][[:digit:]]+\\([eE][+-]?[[:digit:]]+\\)?\\)?" .
        'datalog-constant-face)))))

;; --------------------------------------------------------------------------
;; Keymap
;; --------------------------------------------------------------------------

(defvar datalog-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for Datalog major mode")

;; --------------------------------------------------------------------------
;; Manage syntax table(s)
;; --------------------------------------------------------------------------

(defvar datalog-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Words
    (modify-syntax-entry ?_ "_" table)
    
    ;; Punctuation
    (modify-syntax-entry ?\( "." table)
    (modify-syntax-entry ?\) "." table)
    (modify-syntax-entry ?, "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?\; "." table)
    (modify-syntax-entry ?. "." table)
    (modify-syntax-entry ?~ "." table)
    
    ;; Implication
    (modify-syntax-entry ?← "." table)

    ;; Functionl Dependency
    (modify-syntax-entry ?⟶ "." table)
    
    ;; Arithmetic Operators
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?≠ "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?≤ "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?≥ "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?≛ "." table)

    ;; Logical Connectives
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?∧ "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?∨ "." table)
    (modify-syntax-entry ?! "." table)
    (modify-syntax-entry ?¬ "." table)

    ;; Comments
    (modify-syntax-entry ?/ ". 14" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?% "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for Datalog major mode")

;; --------------------------------------------------------------------------
;; Initialize other module variables
;; --------------------------------------------------------------------------

(defvar datalog-mode-hook nil)

(when (boundp 'show-paren-mode)
  (message "Datalog: adding `show-paren-mode' minor mode")
  (add-hook 'datalog-mode-hook 'show-paren-mode))

(defvar datalog-prettify-symbols-alist
  '(("&" . ?∧)
    ("|" . ?∨)
    ("!" . ?¬)
    ("<=" . ?≤)
    (">=" . ?≥)
    ("!=" . ?≠)
    ("/=" . ?≠)
    ("*=" . ?≛)
    ("-->" . ?⟶)
    ("<-" . ?←))
  "Alist of symbol replacements used by `prettify-symbols-alist'.")

(when (boundp 'prettify-symbols-mode)
  (message "Datalog: adding `prettify-symbols-mode' minor mode")
  (add-hook 'datalog-mode-hook 'prettify-symbols-mode))

;; --------------------------------------------------------------------------
;; Mode entry-point function
;; --------------------------------------------------------------------------

;;;###autoload
(define-derived-mode datalog-mode prog-mode "Datalog"
  "Major mode for editing Datalog resources.
Turning on Datalog mode runs the normal hook `datalog-mode-hook'."
  (message "Datalog: set syntax table")
  (set-syntax-table datalog-mode-syntax-table)
  (message "Datalog: set key map")
  (use-local-map datalog-mode-map)
  (message "Datalog: set font lock defaults")
  (setq-local font-lock-defaults (datalog--make-font-lock-defaults))
  (setq-local font-lock-comment-face 'datalog-comment-face)
  (setq-local font-lock-string-face 'datalog-string-face)
  (message "Datalog: set Prettify symbols")
  (setq-local prettify-symbols-alist datalog-prettify-symbols-alist)
  (setq-local text-mode-variant t)
  (setq-local require-final-newline mode-require-final-newline)
  ;; Finally, ...
  (message "Datalog: run mode hook")
  (run-mode-hooks 'datalog-mode-hook))

(defun datalog-mode-reload ()
  (interactive)
  (unload-feature 'datalog-mode)
  (require 'datalog-mode)
  (datalog-mode))

;; --------------------------------------------------------------------------
;; File associations
;; --------------------------------------------------------------------------

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dl\\'" . datalog-mode))

(provide 'datalog-mode)

;;; datalog-mode.el ends here
