;;; grace-mode.el --- A major mode for editing Grace code.

;; Author: Timothy Jones
;; Version: 0.1.0
;; Url: https://github.com/zmthy/grace-mode
;; Keywords: languages

;;; Commentary:
;; Adapted from rust-mode.

;;; Code:

;; for GNU Emacs < 24.3
(eval-when-compile
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      "Set variable VAR to value VAL in current buffer."
      (list 'set (list 'make-local-variable (list 'quote var)) val))))

;; Syntax definitions and helpers
(defvar grace-mode-syntax-table
  (let ((table (make-syntax-table)))

    ;; String quotes and escape characters.
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; We cheat a little here, to distinguish operators from punctuation.  The
    ;; Symbol class is supposed to be for characters which combine with word
    ;; characters to form symbols.  We use it here to indicate characters which
    ;; should be interpreted as symbol *characters*, and use them to highlight
    ;; Grace operators.

    ;; Primes are *word* constituents, as we don't have symbols.
    (modify-syntax-entry ?' "w" table)

    ;; Underscores are not parts of words or symbols, they're just punctuation.
    (modify-syntax-entry ?_ "." table)

    ;; These operator characters are not symbols by default, so now they are.
    (dolist (c '(?@ ?# ?. ?:))
      (modify-syntax-entry c "_" table))

    ;; A forward-slash may be part of a end-of-line comment, but can also form
    ;; part of an operator and so is marked as a symbol.
    (modify-syntax-entry ?/   "_ 12" table)
    (dolist (c '(?\n ?\r ?\^m))
      (modify-syntax-entry c ">" table))

    table))

(defgroup grace-mode nil
  "Support for Grace code."
  :link '(url-link "http://www.gracelang.org/")
  :group 'languages)

(defcustom grace-indent-offset 2
  "Indent Grace code by this number of spaces."
  :type 'integer
  :group 'grace-mode)

(defun grace-list-level ()
  "The current level of nested braces and parentheses."
  (nth 0 (syntax-ppss)))

(defun grace-string-or-comment-open ()
  "The opening delimiter of the surrounding string literal or comment, or nil if
no such string or comment exists."
  (nth 8 (syntax-ppss)))

(defun grace-rewind-to-string-or-comment-open ()
  "Rewind to the beginning of the current string literal or comment.  Raises an
error if no such string or comment exists."
  (goto-char (grace-string-or-comment-open)))

(defun grace-rewind-irrelevant ()
  "Rewind to the beginning of any whitespace, ignoring surrounding string
literals and comments."
  (let ((starting (point)))
    (skip-chars-backward "[:space:]\n")
    (if (grace-string-or-comment-open)
        (grace-rewind-to-string-or-comment-open))
    (if (/= starting (point))
        (grace-rewind-irrelevant))))

(defun grace-fast-forward-irrelevant ()
  "Fast-forward to the character after any whitespace under the current point."
  (skip-chars-forward "[:space:]\n"))

(defun grace-current-indent ()
  "The indentation amount of the first statement at the current level."
  (save-excursion
    (if (= (grace-list-level) 0)
        0
      (grace-rewind-irrelevant)
      (backward-up-list)
      (if (looking-at "(")
          (grace-current-indent)
        (forward-char)
        (grace-fast-forward-irrelevant)
        (current-column)))))

(defun grace-rewind-to-beginning-of-current-statement-simple ()
  "Rewind to the beginning of the current statement, ignoring method headers."
  ;; Avoid jumping around if we've just opened a brace.
  (unless (save-excursion
            (grace-rewind-irrelevant)
            (looking-back "{"))
    (let ((current-level (grace-list-level))
          (current-indent (grace-current-indent)))
      (back-to-indentation)
      (while (> (grace-list-level) current-level)
        (backward-up-list)
        (back-to-indentation))
      (while (> (current-column) current-indent)
        (grace-rewind-irrelevant)
        (back-to-indentation)))))

(defun grace-inside-method-header ()
  "Provides the location of the method or class keyword if the current point is
in a method or class header, or nil if no such header exists.

If the cursor is inside a pair of braces inside a method header, the method
header will not be detected."
  (save-excursion
    (grace-rewind-irrelevant)

    (if (looking-back "}")
        (let ((current-level (grace-list-level)))
          (backward-char)
          (backward-up-list)
          (let ((header (grace-inside-method-header)))
            (when header
              (goto-char header)
              (when (/= (grace-list-level) current-level)
                header))))

      (grace-rewind-to-beginning-of-current-statement-simple)

      ;; Either we jump back to the keyword...
      (if (or (looking-at "method ") (looking-at "class "))
          (point)
        (unless (or (looking-back "{") (looking-back "}"))
          (back-to-indentation)

          ;; ... or if the indentation on the previous line is further in than
          ;; expected, we'll check to see if jumping further back finds it.
          (if (> (current-column) (grace-current-indent))
              (grace-inside-method-header)))))))

(defun grace-rewind-to-beginning-of-current-statement ()
  "Rewind to the beginning of the current statement."
  (grace-rewind-to-beginning-of-current-statement-simple)
  (let ((header (grace-inside-method-header)))
    (if header
        (goto-char header))))

(defun grace-rewind-to-beginning-of-outer-statement ()
  "Rewind to the beginning of the statement outside of the current expression."
  (grace-rewind-irrelevant)
  (backward-up-list)
  (grace-rewind-to-beginning-of-current-statement))

(defvar grace-reindent-point nil
  "May hold the point of the last indent.  If set, then if the next indent is at
the same point then it should be interpreted as a reindent rather than an
indent.")

(defun grace-nil-reindent-point ()
  "Set the reindent point to nil."
  (setq grace-reindent-point nil))

(defun grace-mode-indent-line ()
  "Indent the current line as Grace code."
  (interactive)
  (let*
      ((start-point (point))
       (start-column (current-column))
       (start-indentation (current-indentation))
       (start-reindent grace-reindent-point)
       (indent
        (save-excursion
          ;; (grace-nil-reindent-point)
          (back-to-indentation)
          (let* ((level (grace-list-level))
                 (baseline
                  (if (= 0 level)
                      0
                    (save-excursion
                      (grace-rewind-to-beginning-of-outer-statement)
                      (+ (current-column) grace-indent-offset))))
                 (method-decl (grace-inside-method-header)))

            (cond
             ;; If inside a method name declaration, align with the first name,
             ;; unless at the beginning of an annotation list on a class, in
             ;; which case align with the class name.
             (method-decl
              (let ((is (looking-at "is ")))

                ;; Don't indent to the name if we're on the line with the
                ;; initial keyword.
                (if (= method-decl (point))
                    (grace-current-indent)
                  (save-excursion
                    (goto-char method-decl)
                    (when (or (looking-at "method ") (looking-at "class "))
                      (forward-word))
                    (forward-word)
                    (backward-word)
                    (current-column)))))

             ;; A closing brace is 1 level unindented.
             ((looking-at "}") (- baseline grace-indent-offset))

             ;; If we're in any other token-tree / sexp, then:
             (t
              (progn
                (back-to-indentation)

                (if (or
                     ;; If this line begins with "{", stay on the baseline as
                     ;; well (we are continuing an expression, but the "{"
                     ;; should align with the beginning of the expression that
                     ;; it is in.)
                     (looking-at "{")

                     (and
                      ;; Indent if a symbol appears at the start of the line.
                      (or (looking-at "//[^[:punct:]]")
                          (not (looking-at "\\.\\|[[:punct:]]\+ ")))
                      (save-excursion
                        (grace-rewind-irrelevant)
                        (or
                         ;; If we are at the first line, no indentation is
                         ;; needed, so stay at baseline.
                         (= 1 (line-number-at-pos (point)))
                         ;; Or if the previous line ends with any of these:
                         ;;     { ; :alpha: ' ->
                         ;; then we are at the beginning of an expression, so
                         ;; stay on the baseline.
                         (looking-back (concat "[{;[:alpha:]']\\|->"))
                         ;; Or if the previous line ends with any of these:
                         ;;     } ) > " :digit:
                         ;; then we are at the beginning of an expression,
                         ;; except if the next line is a request, in which
                         ;; case it could be a continuation of the previous
                         ;; line.  Note that the appearance of '>' here means
                         ;; that a multiline split on that operator may not
                         ;; be indented.
                         (and
                          (looking-back (concat "[)}\">[:digit:]]"))
                          (setq grace-reindent-point start-point)
                          (if (and start-reindent
                                   (= start-reindent start-point))
                              (> start-indentation baseline)
                            (<= start-indentation baseline)))))))
                    baseline

                  ;; Otherwise, we are continuing the same expression from the
                  ;; previous line, so add an additional indent levels.
                  (+ baseline grace-indent-offset)))))))))

    (when indent
      ;; If we're at the beginning of the line (before or at the current
      ;; indentation), jump with the indentation change.  Otherwise, save the
      ;; excursion so that adding the indentations will leave us at the
      ;; equivalent position within the line to where we were before.
      (if (<= start-column start-indentation)
          (indent-line-to indent)
        (save-excursion (indent-line-to indent)))

      (if grace-reindent-point
          (setq grace-reindent-point (point))))))

;; Font-locking definitions
(defconst grace-mode-keywords
  '("as"
    "class"
    "def" "dialect"
    "import" "inherits" "is"
    "let"
    "method"
    "object" "outer"
    "return"
    "self" "super"
    "type"
    "var"
    "where"))

(defconst grace-mode-symbols
  '("->"
    "="
    ":" ":="
    "."))

(defconst grace-mode-constants
  '("done"
    "false"
    "true"))

(defconst grace-re-ident "[[:alpha:]]\\sw*")
(defconst grace-re-pascal-case "[[:upper:]]\\sw*")
(defun grace-re-word (inner) (concat "\\<" inner "\\>"))
(defun grace-re-symbol (inner) (concat "\\S_" inner "\\S_"))
(defun grace-re-grab (inner) (concat "\\(" inner "\\)"))
(defun grace-re-grabword (inner) (grace-re-grab (grace-re-word inner)))
(defun grace-re-grabsymbol (inner) (grace-re-symbol (grace-re-word inner)))
(defun grace-re-item-def (itype)
  (concat (grace-re-word itype) "\\s-+" (grace-re-grabword grace-re-ident)))

(defvar grace-mode-font-lock
  (append
   `(
     ;; Keywords
     (,(regexp-opt grace-mode-keywords 'words) . font-lock-keyword-face)

     ;; Symbols
     (,(grace-re-symbol (regexp-opt grace-mode-symbols t))
      1 font-lock-keyword-face)

     ;; Constants
     (,(regexp-opt grace-mode-constants 'words) . font-lock-constant-face)

     ;; Types
     (,(grace-re-grabword grace-re-pascal-case) 1 font-lock-type-face)

     ;; Generic delimiters
     (,(concat "\\sw" (grace-re-grab "<")) 1 font-lock-keyword-face)
     (,(concat "\\sw" (grace-re-grab ">+")) 1 font-lock-keyword-face)

     ;; Operators
     (,(grace-re-symbol (grace-re-grab "\\s_+")) 1 font-lock-function-name-face))

   ;; Item definitions
   (mapcar #'(lambda (x)
               (list (grace-re-item-def (car x))
                     1 (cdr x)))
           '(("as" . font-lock-variable-name-face)
             ("def" . font-lock-variable-name-face)
             ("type" . font-lock-type-face)
             ("var" . font-lock-variable-name-face)))))

;; For compatibility with Emacs < 24, derive conditionally
(defalias 'grace-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode grace-mode grace-parent-mode "Grace"
  "Major mode for Grace code."
  :group 'grace-mode
  :syntax-table grace-mode-syntax-table

  ;; Indentation
  (setq-local indent-line-function 'grace-mode-indent-line)

  ;; Electric indent triggers multiple indents on a newline, which isn't
  ;; compatible with the reindenting feature.
  (add-hook 'grace-mode-hook
            (lambda ()
              (set (make-local-variable 'electric-indent-functions)
                   (list (lambda (arg) 'no-indent)))))

  ;; Unset the reindent variable on any buffer change.
  (setq-local after-change-functions
              (cons 'grace-nil-reindent-point after-change-functions))

  ;; Fonts
  (setq-local font-lock-defaults '(grace-mode-font-lock))

  ;; Misc
  (setq-local comment-start "// ")
  (setq-local comment-end   "")
  (setq-local indent-tabs-mode nil))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.grace\\'" . grace-mode))

(defun grace-mode-reload ()
  "Reload the Grace major mode."
  (interactive)
  (unload-feature 'grace-mode)
  (require 'grace-mode)
  (grace-mode))

(provide 'grace-mode)

;;; grace-mode.el ends here
