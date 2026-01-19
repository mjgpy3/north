(defvar north-mode-hook nil)

(add-to-list 'auto-mode-alist '("\\.north\\'" . north-mode))

(defconst north-font-lock-keywords-1
  (list
   '("\\<dup\\|swp\\|drop\\|rot\\|nip\\|tuck\\|pick\\|not\\|splat-string\\>" . font-lock-builtin-face)
   '("\\('\\w*'\\)" . font-lock-variable-name-face))
  "Minimal highlighting expressions for North mode")

(defconst north-font-lock-keywords-2
  (append north-font-lock-keywords-1
          (list
           '("\\<cr\\|say\\|\\.\\|read-file\\|trace-stack\\|trace\\|assert\\|done\\|again\\|if\\>" . font-lock-keyword-face)
           '("\\<\\(true\\|false\\)\\>" . font-lock-constant-face)))
  "Additional Keywords to highlight in North mode")

(defconst north-font-lock-keywords-3
  (append north-font-lock-keywords-2
          (list
           '("\\<done\\|again\\|if\\>" . font-lock-constant-face)
           '(":\s+[A-Za-z0-9-_]*" . font-lock-function-name-face)
           '("\\+|\\-\\|\\*" . font-lock-operator-face)
           '(";" . font-lock-function-name-face)
           '("-?[0-9]+\\.?[0-9]*" . font-lock-number-face)
           '("\\[\\|\\]" . font-lock-bracket-face)
           ))
  "Balls-out highlighting in North mode")

(defvar north-font-lock-keywords north-font-lock-keywords-3
  "Default highlighting expressions for North mode")

(defvar north-mode-syntax-table
  (let ((table (make-syntax-table)))

    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?- "w" table)
    (modify-syntax-entry ?\\ "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for north-mode")

(defun north-mode ()
  "Major mode for editing North files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table north-mode-syntax-table)

  (set (make-local-variable 'font-lock-defaults) '(north-font-lock-keywords))
  (setq major-mode 'north-mode)
  (setq mode-name "North")
  (run-hooks 'north-mode-hook))

(provide 'north-mode)
