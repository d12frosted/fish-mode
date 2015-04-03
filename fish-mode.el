;;; fish-mode.el --- Major mode for fish shell scripts  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Boris Buliga (d12frosted)

;; Author: Boris Buliga <d12frosted@icloud.com>
;; Keywords: Fish, shell
;; Package-Requires: ((emacs "24"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Current features:
;;
;;  - syntax highlighting
;;  - indentation
;;  - comment-dwim support

;;; Code:

;; define several calss of keywords
(setq fish-commands '("alias" "and" "begin" "bg" "bind" "block" "break" "breakpoint" "builtin" "case" "cd" "command" "commandline" "complete" "contains" "continue" "count" "dirh" "dirs" "echo" "else" "emit" "end" "eval" "exec" "exit" "fg" "fish" "fish_config" "fish_indent" "fish_pager" "fish_prompt" "fish_right_prompt" "fish_update_completions" "fishd" "for" "funced" "funcsave" "function" "functions" "help" "history" "if" "isatty" "jobs" "math" "mimedb" "nextd" "not" "open" "or" "popd" "prevd" "psub" "pushd" "pwd" "random" "read" "return" "set" "set_color" "source" "status" "switch" "test" "trap" "type" "ulimit" "umask" "vared" "while"))

;; create the regex string for each class of keywords
(setq fish-commands-regexp (regexp-opt fish-commands 'words))
(setq fish-functions-regexp "function \\([[:alnum:]_-]+\\)")
(setq fish-variables1-regexp "set \\([[:alnum:]_-]+\\)")
(setq fish-variables2-regexp "set -\\w+ \\([[:alnum:]_-]+\\)")
(setq fish-constants-regexp "\\$\\([[:alnum:]_-]+\\)")

;; clear memory
(setq fish-commands nil)

;; create the list for font-lock.
;; each class of keyword is given a particular face
(setq fish-font-lock-keywords
      `((,fish-functions-regexp 1 'font-lock-function-name-face)
        (,fish-commands-regexp 0 font-lock-type-face)
        (,fish-variables1-regexp 1 'font-lock-variable-name-face)
        (,fish-variables2-regexp 1 'font-lock-variable-name-face)
        (,fish-constants-regexp 1 'font-lock-variable-name-face)))

(defun fish-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way. For details, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let ((comment-start "#")
        (comment-end ""))
    (comment-dwim arg)))

;; syntax table
(defvar fish-syntax-table nil "Syntax table for `fish-mode'.")
(setq fish-syntax-table
      (let ((synTable (make-syntax-table)))

        ;; bash style comment: “# …”
        (modify-syntax-entry ?# "< b" synTable)
        (modify-syntax-entry ?\n "> b" synTable)

        (modify-syntax-entry ?\" "\"\"" synTable)
        (modify-syntax-entry ?\' "\"'" synTable)

        synTable))

;; Indentation

(defun what-line-number ()
  "Returns the current line number of point."
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

(defun fish-get-normal-indent ()
  (interactive)
  (let ((not-indented t) cur-indent)
    (while not-indented
      ;; move up
      (forward-line -1)
      (cond
       ;; found block-opening term, so increase indentation level by tab-width
       ((looking-at "[ \t]*\\(if\\|else\\|function\\|while\\|for\\|begin\\|switch\\|case\\)")
        (setq cur-indent (+ (current-indentation) tab-width))
        (setq not-indented nil))

       ;; found empty line, so just skip it
       ((looking-at "[ \t]*$"))

       ;; default case, so return indentation level of current line
       (t
        (setq cur-indent (current-indentation))
        (setq not-indented nil))))
    cur-indent))

(defun fish-get-case-indent ()
  (interactive)
  (let ((not-indented t) cur-indent)
    (while not-indented
      ;; move up
      (forward-line -1)
      (cond
       ;; found 'switch', so increase indentation level by tab-width
       ((looking-at "[ \t]*\\(switch\\)")
        (setq cur-indent (+ (current-indentation) tab-width))
        (setq not-indented nil))

       ;; found another 'case', so return it's indentation level
       ((looking-at "[ \t]*\\(case\\)")
        (setq cur-indent (current-indentation))
        (setq not-indented nil))

       ;; found empty line, so just skip it
       ((looking-at "[ \t]*$"))

       ;; default case, so return indentation level of current line - tab-width
       (t
        (setq cur-indent (- (current-indentation) tab-width))
        (setq not-indented nil))))
    cur-indent))

(defun fish-get-end-indent ()
  (interactive)
  (let (cur-indent (count-of-ends 1))
    (while (not (eq count-of-ends 0))
      ;; move up
      (forward-line -1)
      (cond
       ;; found block-opening term, so check if it matches to our end
       ((looking-at "[ \t]*\\(if\\|function\\|while\\|for\\|begin\\|switch\\)")
        (setq count-of-ends (- count-of-ends 1))
        (if (eq count-of-ends 0)
            ;; block-opening term matches, so return it's indentation level
            (progn (setq cur-indent (current-indentation))
                   (setq pair-not-found nil))
          ;; block-opening term does not match, so seek further
          ))

       ;; found another 'end', so increase count of 'end' terms
       ((looking-at "[ \t]*\\(end\\)")
        (setq count-of-ends (+ count-of-ends 1)))

       ;; nothing interesting found, so seek further
       (t)))
    cur-indent))

(defun fish-indent-line ()
  "Indent current line"
  (interactive)

  (if (bobp)
      (indent-line-to 0)
    (let (cur-indent (rpos (- (point-max) (point))))
      (save-excursion
        (beginning-of-line)
        (cond
         ;; already on line 1, so leave it alone
         ((eq (what-line-number) 1)
          (setq cur-indent (current-indentation)))

         ;; found 'end' - need to move back based on level of matching pair
         ((looking-at "[ \t]*\\(end\\)")
          (setq cur-indent (fish-get-end-indent)))

         ;; found 'case' - need to move forth based on matching switch
         ((looking-at "[ \t]*\\(case\\)")
          (setq cur-indent (fish-get-case-indent)))

         ;; found 'else' - like default condition, but also move left
         ((looking-at "[ \t]*\\(else\\)")
          (setq cur-indent (- (fish-get-normal-indent) tab-width)))

         ;; default case - indent based on previous non-empty line
         (t
          (setq cur-indent (fish-get-normal-indent)))))
      (if (< cur-indent 0) (setq cur-indent 0))
      (indent-line-to cur-indent)
      (if (> (- (point-max) rpos) (point))
          (goto-char (- (point-max) rpos))))))

;; define the mode
(define-derived-mode fish-mode prog-mode
  "Fish"
  "Major mode for editing fish shell scripts"
  :syntax-table fish-syntax-table

  (setq-local indent-line-function 'fish-indent-line)

  ;; code for syntax highlighting
  (setq font-lock-defaults '((fish-font-lock-keywords)))

  (setq-local comment-start "# ")
  (setq-local tab-width 2)

  ;; clear memory
  (setq fish-commands-regexp nil)
  (setq fish-functions-regexp nil)
  (setq fish-variables1-regexp nil)
  (setq fish-variables2-regexp nil)
  (setq fish-constants-regexp nil)

  ;; modify the keymap
  (define-key fish-mode-map [remap comment-dwim] 'fish-comment-dwim)
)

(add-to-list 'auto-mode-alist '("\\.fish\\'" . fish-mode))
(add-to-list 'interpreter-mode-alist '("fish" . fish-mode))

(provide 'fish-mode)

;;; fish-mode.el ends here
