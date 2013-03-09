(require 'package)

(package-initialize)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings)
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

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(add-hook 'python-mode-hook 'highlight-80+-mode)

(toggle-highlight-column-when-idle 1)
(col-highlight-set-interval 2)
(set-face-background 'col-highlight "color-33")
(hl-line-mode 1)
(global-hl-line-mode 1)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight ((t (:background "color-34" :foreground "brightwhite"))))
 '(hl-line ((t (:background "color-35")))))

(turn-off-auto-fill)
(remove-hook 'text-mode-hook #'turn-on-auto-fill)
(put 'ido-exit-minibuffer 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 (global-set-key (kbd "C-c o") 'occur)
