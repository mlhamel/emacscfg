;;(elpy-enable)

(add-to-list 'auto-mode-alist '("\\.mak$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.mako$" . html-mode))

(add-hook 'python-mode-hook 'projectile-on)

(add-hook 'python-mode-hook 'flycheck-mode)
