;;; Hacks to scheme-mode to better handle Squeam code.
;;; TODO make an actual squeam-mode instead

;; Basing this on http://community.schemewiki.org/?emacs-syntax-hilight

(put 'begin 'scheme-indent-function 2)
(put 'case 'scheme-indent-function 0)
(put 'do 'scheme-indent-function 0)
(put 'export 'scheme-indent-function 0)
(put 'for 'scheme-indent-function 2)
(put 'given 'scheme-indent-function 1)
(put 'hide 'scheme-indent-function nil)  ; or 0)
(put 'import 'scheme-indent-function 1)
(put 'let 'scheme-indent-function 1)
(put 'make 'scheme-indent-function 1)
(put 'make-trait 'scheme-indent-function 2)
(put 'match 'scheme-indent-function 1)
(put 'unless 'scheme-indent-function 1)
(put 'with 'scheme-indent-function 1)
(put 'when 'scheme-indent-function 1)

(defconst my-scheme-font-lock-keywords 
  '() 
  "Custom highlighting in Scheme modes.")

(defun my-scheme-mode-hook ()
  "Custom Scheme mode hook."
  (interactive)
  (setq my-scheme-font-lock-keywords
        (append scheme-font-lock-keywords-2
                (eval-when-compile
                  (list
                   (regexp-opt '("export" "for" "given" "hide" "import" "make" "make-trait" "match" "unless" "with" "when") 'symbols)
                   ))))
  (setq font-lock-defaults
        `((scheme-font-lock-keywords
           scheme-font-lock-keywords-1
           my-scheme-font-lock-keywords)
          ,@(cdr font-lock-defaults)))
  )

(add-hook 'scheme-mode-hook 'my-scheme-mode-hook)
