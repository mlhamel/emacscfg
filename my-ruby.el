;; Files with the following extensions should open in ruby-mode
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

(add-hook 'ruby-mode-hook 'projectile-on)
(setq projectile-completion-system 'grizzl)

;; rails development environment
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(setq ruby-deep-indent-paren nil)

;; Prevent emacs from adding the encoding line at the top of the file
(setq ruby-insert-encoding-magic-comment nil)
(require 'ruby-hash-syntax)

(global-set-key (kbd "C-c r") nil)
(add-hook 'projectile-mode-hook 'projectile-rails-on)

(add-hook 'ruby-mode-hook
          (lambda ()
            (column-enforce-mode 0))) ;; disable the 80 column rule

;; When folding, take these delimiters into consideration
(add-to-list 'hs-special-modes-alist
             '(ruby-mode
               "\\(class\\|def\\|do\\|if\\)" "\\(end\\)" "#"
               (lambda (arg) (ruby-end-of-block)) nil))

;; (defadvice ruby-indent-line (after line-up-args activate)
;;   (let (indent prev-indent arg-indent)
;;     (save-excursion
;;       (back-to-indentation)
;;       (when (zerop (car (syntax-ppss)))
;;         (setq indent (current-column))
;;         (skip-chars-backward " \t\n")
;;         (when (eq ?, (char-before))
;;           (ruby-backward-sexp)
;;           (back-to-indentation)
;;           (setq prev-indent (current-column))
;;           (skip-syntax-forward "w_.")
;;           (skip-chars-forward " ")
;;           (setq arg-indent (current-column)))))
;;     (when prev-indent
;;       (let ((offset (- (current-column) indent)))
;;         (cond ((< indent prev-indent)
;;                (indent-line-to prev-indent))
;;               ((= indent prev-indent)
;;                (indent-line-to arg-indent)))
;;         ((>  0) (forward-char offset))))))

;; set language environment
(setq ruby-insert-encoding-magic-comment nil)

;; Rspec
(require 'rspec-mode)
;; I want rspec instead of rake spec
;;(setq rspec-use-rake-when-possible nil)
;; Scroll to the first test failure
(setq compilation-scroll-output 'first-error)

;; RVM support
(require 'rvm)
(rvm-use-default)
