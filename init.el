(require 'package)

(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(when (not package-archive-contents)
  (package-refresh-contents))

;; PATH
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; magit setup
(setq magit-last-seen-setup-instructions "1.4.0")
(autoload 'magit-status "magit" nil t)

(setq dotfiles-dir (file-name-directory (or load-file-name (buffer-file-name))))

(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings col-highlight yaml-mode zenburn-theme jinja2-mode levenshtein flycheck project projectile neotree flymake-ruby inf-ruby jsx-mode)
  "A list of packages to ensure are installed at launch.")

(defun insert-quotes ()
  "Inserts quotes (\") around the current region or work."
  (interactive)
  (let (start end bounds)
    (if (and transient-mark-mode mark-active)
        (setq start (region-beginning)
              end (region-end))
      (progn
        (setq bounds (bounds-of-thing-at-point 'symbol))
        (setq start (car bounds)
              end (cdr bounds))))
    (goto-char start)
    (insert "\"")
    (goto-char (+ end 1))
    (insert "\"")))

(global-set-key (kbd "M-'") 'insert-quotes)

;; (dolist (p my-packages)
;;   (when (not (package-installed-p p))
;;     (package-install p)))

;; web-mode installation and configuration
(when (not (package-installed-p 'web-mode))
    (package-install-file (concat dotfiles-dir "/packages/web-mode.el")))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jinja2\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mako\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(setq web-mode-markup-indent-offset 2)

;; neotree configuration
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)


(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(toggle-highlight-column-when-idle 1)
(col-highlight-set-interval 2)
(set-face-background 'col-highlight "grey13")
(hl-line-mode 1)
(global-hl-line-mode 1)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-indent-face ((t (:background "grey13"))))
 '(hl-line ((t (:background "grey13")))))

(turn-off-auto-fill)
(remove-hook 'text-mode-hook #'turn-on-auto-fill)
(put 'ido-exit-minibuffer 'disabled nil)

;; font-size and type adjustment
;;(set-default-font "Inconsolata 18")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (tango-dark)))
 '(custom-safe-themes
   (quote
    ("95a6ac1b01dcaed4175946b581461e16e1b909d354ada79770c0821e491067c6" "4c9ba94db23a0a3dea88ee80f41d9478c151b07cb6640b33bfc38be7c2415cc4" "dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" "16e7c7811fd8f1bc45d17af9677ea3bd8e028fce2dd4f6fa5e6535dea07067b1" "b4018b7d8352dc7f21c0906cd33621ec487e872a97527dcdad590f0fb50cf9e8" "fe6330ecf168de137bb5eddbf9faae1ec123787b5489c14fa5fa627de1d9f82b" default)))
 '(fci-rule-color "#383838")
 '(nyan-mode t)
 '(package-selected-packages
   (quote
    (jsx-mode rtags zenburn-theme yaml-mode web-mode starter-kit-lisp starter-kit-bindings projectile-rails nyan-mode neotree markdown-mode mark-multiple levenshtein jinja2-mode git flymake-ruby flycheck column-enforce-mode col-highlight coffee-mode)))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vc-annotate-background "#2b2b2b")
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
     (360 . "#dc8cc3"))))
 '(vc-annotate-very-old-color "#dc8cc3")
 '(virtualenv-root "~/venv/adintentis2/"))
(global-set-key (kbd "C-c o") 'occur)
(global-set-key "\C-cd" 'kill-whole-line)
(put 'downcase-region 'disabled nil)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-to-list 'auto-mode-alist '("\\.mak$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.mako$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))

(add-hook 'python-mode-hook 'flycheck-mode)

(package-initialize)
;;(elpy-enable)

(global-linum-mode t)
(setq linum-format " %d ")
(set-face-attribute 'linum nil :foreground "#ccc")


;;(require 'project)


(defun fc/isearch-yank-symbol ()
  "Yank the symbol at point into the isearch minibuffer.

C-w does something similar in isearch but it only looks for
the rest of the word. I want to look for the whole string. And
symbol, not word, as I need this for programming the most."
  (interactive)
  (isearch-yank-pop
   (save-excursion
     (when (and (not isearch-forward)
                isearch-other-end)
       (goto-char isearch-other-end))
     (thing-at-point 'symbol))))

(define-key isearch-mode-map (kbd "M-S b") 'fc/isearch-yank-symbol)

;; projectile
(projectile-global-mode)
(add-hook 'python-mode-hook 'projectile-on)
(setq projectile-enable-caching t)

;; rails development environment
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(setq ruby-deep-indent-paren nil)

(global-set-key (kbd "C-c r") nil)
(add-hook 'projectile-mode-hook 'projectile-rails-on)

(add-hook 'ruby-mode-hook
          (lambda ()
            (column-enforce-mode 0))) ;; disable the 80 column rule

;(global-set-key (kbd "C-c r r") 'inf-ruby)

;; mark-multiple
(require 'inline-string-rectangle)
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

(require 'mark-more-like-this)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mark-more-like-this) ; like the other two, but takes an argument (negative is previous)
(global-set-key (kbd "C-*") 'mark-all-like-this)

(add-hook 'sgml-mode-hook
          (lambda ()
            (require 'rename-sgml-tag)
            (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)))


;; nyan-mode
(require 'nyan-mode)


;; git-mode
(require 'git)


(global-auto-revert-mode -1)

;; jsx-mode
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(setq jsx-indent-level 2)
(add-hook 'jsx-mode-hook
          (lambda () (auto-complete-mode 1)))
