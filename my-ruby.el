(use-package ruby-mode
  :config
  ;; Files with the following extensions should open in ruby-mode
  (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

  ;; robe configuration
  (chruby "2.2.3")
  (add-hook 'ruby-mode-hook 'robe-mode)

  ;; projectile configuration
  (add-hook 'ruby-mode-hook 'projectile-on)
  (setq projectile-completion-system 'grizzl)

  ;; rails development environment
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

  ;; set language environment
  (setq ruby-insert-encoding-magic-comment nil)

  ;; Rspec
  ;; I want rspec instead of rake spec
  ;;(setq rspec-use-rake-when-possible nil)
  ;; Scroll to the first test failure
  (setq compilation-scroll-output 'first-error)

  ;; RVM support
  (rvm-use-default)

  ;; ctags-update
  (autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on `ctags-auto-update-mode'." t)
  (add-hook 'ruby-mode-common-hook  'turn-on-ctags-auto-update-mode)
  (global-set-key "\C-cE" 'ctags-update))
