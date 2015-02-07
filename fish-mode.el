;; define several calss of keywords
(setq fish-commands '("alias" "and" "begin" "bg" "bind" "block" "break" "breakpoint" "builtin" "case" "cd" "command" "commandline" "complete" "contains" "continue" "count" "dirh" "dirs" "echo" "else" "emit" "end" "eval" "exec" "exit" "fg" "fish" "fish_config" "fish_indent" "fish_pager" "fish_prompt" "fish_right_prompt" "fish_update_completions" "fishd" "for" "funced" "funcsave" "function" "functions" "help" "history" "if" "isatty" "jobs" "math" "mimedb" "nextd" "not" "open" "or" "popd" "prevd" "psub" "pushd" "pwd" "random" "read" "return" "set" "set_color" "source" "status" "switch" "test" "trap" "type" "ulimit" "umask" "vared" "while"))

;; create the regex string for each class of keywords
(setq fish-commands-regexp (regexp-opt fish-commands 'words))
(setq fish-functions-regexp "function \\([[:alpha:]_][[:alnum:]_]*\\)")
(setq fish-variables1-regexp "set \\([[:alpha:]_][[:alnum:]_]*\\)")
(setq fish-variables2-regexp "set -\\w+ \\([[:alpha:]_][[:alnum:]_]*\\)")
(setq fish-constants-regexp "\\$\\([[:alpha:]_][[:alnum:]_]*\\)")

;; clear memory
(setq fish-commands nil)

;; create the list for font-lock.
;; each class of keyword is given a particular face
(setq fish-font-lock-keywords
  `(
    (,fish-commands-regexp 0 font-lock-type-face)
    (,fish-functions-regexp 1 'font-lock-function-name-face)
    (,fish-variables1-regexp 1 'font-lock-variable-name-face)
    (,fish-variables2-regexp 1 'font-lock-variable-name-face)
    (,fish-constants-regexp 1 'font-lock-variable-name-face)
))

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

(defun fish-swallow-block ()
  "move backward line til begin of the block"
  (let ((not-done t))
    (while not-done
      (forward-line -1)
      (if (looking-at "^[ \t]*end")
          (fish-swallow-block)
        (if (looking-at "^[ \t]*\\(begin\\|for\\|function\\|if\\|switch\\|while\\)")
            (setq not-done nil))))))

(defun fish-get-else-end-indent ()
  (let ((not-indented t) cur-indent)
    (while not-indented
      (forward-line -1)
      (cond
       ((looking-at "^[ \t]*if")
        (setq cur-indent (current-indentation))
        (setq not-indented nil))
       ((looking-at "^[ \t]*\\(begin\\|else\\|for\\|function\\|if\\|switch\\|while\\)")
        (unless (looking-at ".*end$")
          (setq cur-indent (current-indentation))
          (setq not-indented nil)))
       ((looking-at "^[ \t]*case")
        (setq cur-indent (- (current-indentation) tab-width))
        (setq not-indented nil))
       ((looking-at "^[ \t]*end") ; swallow the block
        (fish-swallow-block))
       ((bobp)
        (setq cur-indent 0)
        (setq not-indented nil))))
    (if (< cur-indent 0)
        (setq cur-indent 0)
      cur-indent)))

(defun fish-get-case-indent ()
  (let ((not-indented t) cur-indent)
    (while not-indented
      (forward-line -1)
      (cond
       ((looking-at "^[ \t]*case")
        (setq cur-indent (current-indentation))
        (setq not-indented nil))
       ((looking-at "^[ \t]*switch")
        (message "switch")
        (setq cur-indent (+ (current-indentation) tab-width))
        (setq not-indented nil))
       ((bobp)
        (setq cur-indent 0)
        (setq not-indented nil))))
    (if (< cur-indent 0)
        (setq cur-indent 0)
      cur-indent)))

(defun fish-get-normal-indent ()
  (let ((not-indented t) cur-indent)
    (while not-indented
      (message "normal")
      (forward-line -1)
      (cond
       ((and (looking-at "[ \t]*\\(begin\\|case\\|else\\|for\\|function\\|if\\|switch\\|while\\)\\>")
             (not (looking-at ".*end$")))
        (setq cur-indent (+ (current-indentation) tab-width))
        (setq not-indented nil))
       ((bobp)
        (setq cur-indent 0)
        (setq not-indented nil))
       ((looking-at "[ \t]*$"))
       (t
        (setq cur-indent (current-indentation))
        (setq not-indented nil))))
    (if (< cur-indent 0)
        (setq cur-indent 0)
      cur-indent)))

(defun fish-indent-line ()
  "Indent current line."
  (interactive)

  (if (bobp)
      (indent-line-to 0)
    (let (cur-indent)
      (save-excursion
        (cond
         ((looking-at "[ \t]*\\(end\\|else\\)")
          (setq cur-indent (fish-get-else-end-indent)))
         ((looking-at "[ \t]*case")
          (setq cur-indent (fish-get-case-indent))
          )
         (t
          (setq cur-indent (fish-get-normal-indent)))))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))))

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
