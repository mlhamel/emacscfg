(setq user-full-name "Mathieu Leduc-Hamel")
(setq user-mail-address "mathieu@mtlpy.org")

(require 'package)

(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(when (not package-archive-contents)
  (package-refresh-contents))

;; path configuration
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; Defining emacs configuration folder
(setq dotfiles-dir (file-name-directory (or load-file-name (buffer-file-name))))

;; Selected packages
(setq my-packages (list 'ack
                        'ag
                        'anzu
                        'col-highlight
                        'company
                        'chruby
                        'goto-chg
                        'go-mode
                        'jinja2-mode
                        'flycheck
                        'flymake-ruby
                        'grizzl
                        'inf-ruby
                        'jsx-mode
                        'levenshtein
                        'project
                        'projectile
                        'projectile-rails
                        'rainbow-mode
                        'robe
                        'rspec-mode
                        'ruby-tools
                        'ruby-block
                        'ruby-additional
                        'ruby-hash-syntax
                        'ruby-refactor
                        'rust-mode
                        'rvm
                        'smartscan
                        'solarized-theme
                        'starter-kit
                        'starter-kit-lisp
                        'starter-kit-bindings
                        'toml-mode
                        'use-package
                        'virtualenvwrapper
                        'yaml-mode
                        'zenburn-theme))

;; Install packages from our list
(dolist (package my-packages)
   (when (not (package-installed-p package))
      (package-refresh-contents)
      (package-install package)))

;; Load custom packages
(setq packages-dir (expand-file-name "packages" user-emacs-directory))
(dolist (file (directory-files packages-dir t "\\w+"))
  (when (file-regular-p file)
    (package-install-file file)))

(package-initialize)

(eval-when-compile
  (require 'use-package))

(require 'anzu)
(require 'flymake-ruby)
(require 'git)
(require 'goto-chg)
(require 'inline-string-rectangle)
(require 'mark-more-like-this)
(require 'multiple-cursors)
(require 'nyan-mode)
(require 'rspec-mode)
(require 'rvm)
(require 'yaml-mode)
(require 'company)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" "19352d62ea0395879be564fc36bc0b4780d9768a964d26dfae8aad218062858d" "95a6ac1b01dcaed4175946b581461e16e1b909d354ada79770c0821e491067c6" "4c9ba94db23a0a3dea88ee80f41d9478c151b07cb6640b33bfc38be7c2415cc4" "dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" "16e7c7811fd8f1bc45d17af9677ea3bd8e028fce2dd4f6fa5e6535dea07067b1" "b4018b7d8352dc7f21c0906cd33621ec487e872a97527dcdad590f0fb50cf9e8" "fe6330ecf168de137bb5eddbf9faae1ec123787b5489c14fa5fa627de1d9f82b" default)))
 '(fci-rule-color "#383838")
 '(magit-commit-arguments (quote ("--all")))
 '(nyan-mode t)
 '(package-selected-packages
   (quote
    (helm-projectile helm fish-mode string-inflection ag csv-mode key-leap rubocop ack seq use-package virtualenvwrapper graphviz-dot-mode robe chruby multiple-cursors elixir-mode smart-mode-line toml-mode rust-mode ctags-update grizzl company go-mode fireplace anzu rspec-mode rvm smartscan ruby-refactor ruby-hash-syntaxe ruby-additional ruby-block ruby-tools ruby-hash-syntax solarized-theme rainbow-mode ctags jsx-mode rtags zenburn-theme yaml-mode web-mode starter-kit-lisp starter-kit-bindings projectile-rails nyan-mode neotree markdown-mode mark-multiple levenshtein jinja2-mode git flymake-ruby flycheck column-enforce-mode col-highlight coffee-mode)))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#bc8383")
     (40 . "#cc9393")
     (60 . "#dfaf8f")
     (80 . "#d0bf8f")
     (100 . "#e0cf9f")
     (120 . "#f0dfaf")
     (140 . "#5f7f5f")
     (160 . "#7f9f7f")
     (180 . "#8fb28f")
     (200 . "#9fc59f")
     (220 . "#afd8af")
     (240 . "#bfebbf")
     (260 . "#93e0e3")
     (280 . "#6ca0a3")
     (300 . "#7cb8bb")
     (320 . "#8cd0d3")
     (340 . "#94bff3")
     (360 . "#dc8cc3")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(col-highlight ((t (:background "black" (\, :foreground) "white"))))
 '(highlight ((t (:background "grey" :foreground "white"))))
 '(hl-line ((t (:background "black" (\, :foreground) "white"))))
 '(region ((t (:foreground "white")))))
