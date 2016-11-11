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

(setq dotfiles-dir (file-name-directory (or load-file-name (buffer-file-name))))

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

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))
