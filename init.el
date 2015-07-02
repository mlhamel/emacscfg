(require 'package)

(package-initialize)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(when (not package-archive-contents)
  (package-refresh-contents))

(setq magit-last-seen-setup-instructions "1.4.0")

(setq dotfiles-dir (file-name-directory (or load-file-name (buffer-file-name))))

(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings col-highlight yaml-mode zenburn-theme jinja2-mode levenshtein flycheck project projectile)
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

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(when (not (package-installed-p 'web-mode))
    (package-install-file (concat dotfiles-dir "/packages/web-mode.el")))

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" "16e7c7811fd8f1bc45d17af9677ea3bd8e028fce2dd4f6fa5e6535dea07067b1" "b4018b7d8352dc7f21c0906cd33621ec487e872a97527dcdad590f0fb50cf9e8" "fe6330ecf168de137bb5eddbf9faae1ec123787b5489c14fa5fa627de1d9f82b" default)))
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
(setq linum-format "%d ")
(set-face-attribute 'linum nil :foreground "#333333")


(require 'project)


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
