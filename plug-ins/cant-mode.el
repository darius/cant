;; WARNING! This overwrites some scheme-mode settings: font lock
;; keywords and indent functions. Sorry. Someday I'll learn to do
;; this right if there are ever other users.

;; Installation:
;;   (load-library "/path/to/cant/cant-mode.el")
;; You'll probably want to run this too:
;;   (add-to-list 'auto-mode-alist '(".*\\.cant\\'" . cant-mode))
;; (I forget why it has that ' at the of the string.)
;; Yeah, this should just be an Emacs package, however that works.

(defvar cant-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cg" 'cant-mode-splat)
    map))

(define-derived-mode cant-mode
  scheme-mode "Cant"
  "Major mode for Cant."
  (setq cant-font-lock-keywords
        (append scheme-font-lock-keywords-2
                (eval-when-compile
                  (list
                   (regexp-opt
                    '("export" "for" "hide" "import" "make"
                      "make-trait" "to" "unless" "when"
                      ;; more experiments:
;;                      ":" "::" ;;XXX these are keywords too, but emacs seems to dislike them here
                      "be" "given" "hm" "may" "on")
                    'symbols)
                   ))))
  (setq font-lock-defaults
        `((scheme-font-lock-keywords
           scheme-font-lock-keywords-1
           cant-font-lock-keywords)
          ,@(cdr font-lock-defaults)))
  (cant-mode-set-indents))

(defconst cant-font-lock-keywords 
  '() 
  "Custom highlighting in Cont modes.")

(defun cant-mode-set-indents ()
  ;; Based on http://community.schemewiki.org/?emacs-syntax-hilight
  ;; TODO can't we do this without stepping on scheme-mode's toes?
  (put 'begin 'scheme-indent-function 2)
  (put 'do 'scheme-indent-function 0)
  (put 'else 'scheme-indent-function 0)
  (put 'export 'scheme-indent-function 0)
  (put 'for 'scheme-indent-function 2)
  (put 'given 'scheme-indent-function 0)
  (put 'hide 'scheme-indent-function 0)  ; or nil)
  (put 'import 'scheme-indent-function 1)
  (put 'let 'scheme-indent-function 1)
  (put 'make 'scheme-indent-function 1)
  (put 'make-trait 'scheme-indent-function 2)
  (put 'match 'scheme-indent-function 1)
  (put 'to 'scheme-indent-function 1)
  (put 'unless 'scheme-indent-function 1)
  (put 'when 'scheme-indent-function 1)

  ;; TODO revisit these experiments
  (put ': 'scheme-indent-function 0)
  (put ':: 'scheme-indent-function 0)
  (put 'be 'scheme-indent-function 1)
  (put 'hm 'scheme-indent-function 0)
  (put 'may 'scheme-indent-function 1)
  (put 'on 'scheme-indent-function 1)
  )


;; A really crude goto-def and create-def:

(defun cant-mode-splat ()             ;TODO better name
  "Take the function call at point, and either go to its
definition, or insert a new definition as the next paragraph and
go to that. Save the old point in the mark. (Hacky incomplete
implementation.)"
  (interactive)
  (let* ((call (list-at-point))
         (name (car call))
         (params (cant-mode-params (cdr call))))
    ;; TODO error check
    ;; TODO if already defined, just go there
    (push-mark nil)
    (let ((defn (cant-mode-find-function-definition name)))
      (if defn
          (goto-char defn)
        (forward-paragraph)
        (insert "\n")
        (cant-mode-insert-defun `(,name ,@params))))))

;; Modeled after https://github.com/vkazanov/elpygen/blob/master/elpygen.el
(defun cant-mode-find-function-definition (name)
  "Return the position of a top-level function definition or nil.
NAME is the symbol to find."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward (cant-mode-defun-pattern name) nil t)
      (match-end 0))))

(defun cant-mode-defun-pattern (name)
  "Return a pattern to match a function definition for NAME."
  (concat "(to (" (symbol-name name)))  ;XXX half-assed

(defun cant-mode-params (args)
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

(defun cant-mode-insert-defun (signature)
  (cant-mode-insert "(to &\n" signature)
  (insert "  ")
  (let ((here (point)))
    (insert "\n")
    (cant-mode-insert "  (oops \"stub: &\"))\n" (car signature))
    (goto-char here)))

(defun cant-mode-insert (template sexp)
  (insert (replace-regexp-in-string "&" (format "%s" sexp) template)))


(provide 'cant-mode)
