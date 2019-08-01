;;; Hacks to scheme-mode to better handle Squeam code.
;;; TODO make an actual squeam-mode instead

;; Basing this on http://community.schemewiki.org/?emacs-syntax-hilight

;; TODO keep only one of 'be' and 'match' -- decide which
(put 'be 'scheme-indent-function 1)
(put 'begin 'scheme-indent-function 2)
(put 'case 'scheme-indent-function 0)
(put 'do 'scheme-indent-function 0)
(put 'export 'scheme-indent-function 0)
(put 'for 'scheme-indent-function 2)
(put 'given 'scheme-indent-function 1)
(put 'hide 'scheme-indent-function 0)  ; or nil)
(put 'import 'scheme-indent-function 1)
(put 'let 'scheme-indent-function 1)
(put 'make 'scheme-indent-function 1)
(put 'make-trait 'scheme-indent-function 2)
(put 'match 'scheme-indent-function 1)
(put 'to 'scheme-indent-function 1)
(put 'unless 'scheme-indent-function 1)
(put 'when 'scheme-indent-function 1)

(defconst squeam-font-lock-keywords 
  '() 
  "Custom highlighting in Scheme modes.")

(defun squeam-mode-hook ()
  "Custom Scheme mode hook."
  (interactive)
  (setq squeam-font-lock-keywords
        (append scheme-font-lock-keywords-2
                (eval-when-compile
                  (list
                   (regexp-opt
                    '("be" "export" "for" "given" "hide" "import" "make"
                      "make-trait" "match" "to" "unless" "when")
                    'symbols)
                   ))))
  (setq font-lock-defaults
        `((scheme-font-lock-keywords
           scheme-font-lock-keywords-1
           squeam-font-lock-keywords)
          ,@(cdr font-lock-defaults)))
  (local-set-key "\C-cg" 'squeam-mode-splat)
  )

;; A really crude goto-def and create-def:

(defun squeam-mode-splat ()             ;TODO better name
  "Take the function call at point, and either go to its
definition, or insert a new definition as the next paragraph and
go to that. Save the old point in the mark. (Hacky incomplete
implementation.)"
  (interactive)
  (let* ((call (list-at-point))
         (name (car call))
         (params (squeam-mode-params (cdr call))))
    ;; TODO error check
    ;; TODO if already defined, just go there
    (push-mark nil)
    (let ((defn (squeam-mode-find-function-definition name)))
      (if defn
          (goto-char defn)
        (forward-paragraph)
        (insert "\n")
        (squeam-mode-insert-defun `(,name ,@params))))))

;; Modeled after https://github.com/vkazanov/elpygen/blob/master/elpygen.el
(defun squeam-mode-find-function-definition (name)
  "Return the position of a top-level function definition or nil.
NAME is the symbol to find."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward (squeam-mode-defun-pattern name) nil t)
      (match-end 0))))

(defun squeam-mode-defun-pattern (name)
  "Return a pattern to match a function definition for NAME."
  (concat "(to (" (symbol-name name)))  ;XXX half-assed

(defun squeam-mode-params (args)
  "Turn an argument list into a parameter list. Args that are
symbols pass through unchanged; others get a name made up for
them."
  ;; TODO: handle duplicate names
  ;; TODO: guess smarter new names
  (loop for arg in args
        for n from 0
        collect (if (symbolp arg)
                    arg
                  (make-symbol (char-to-string (+ ?a n))))))

(defun squeam-mode-insert-defun (signature)
  (squeam-mode-insert "(to &\n" signature)
  (insert "  ")
  (let ((here (point)))
    (insert "\n")
    (squeam-mode-insert "  (error \"stub: &\"))\n" (car signature))
    (goto-char here)))

(defun squeam-mode-insert (template sexp)
  (insert (replace-regexp-in-string "&" (format "%s" sexp) template)))

(add-hook 'scheme-mode-hook 'squeam-mode-hook)
