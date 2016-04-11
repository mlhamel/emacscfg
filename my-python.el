(use-package python-mode
  :config
  ;;(elpy-enable)
  (add-to-list 'auto-mode-alist '("\\.mak$" . html-mode))
  (add-to-list 'auto-mode-alist '("\\.mako$" . html-mode))

  (add-hook 'python-mode-hook 'projectile-on)

  (add-hook 'python-mode-hook 'flycheck-mode)

  (venv-initialize-interactive-shells) ;; if you want interactive shell support
  (venv-initialize-eshell) ;; if you want eshell support
  (setq venv-location "/Users/mlhamel/.virtualenvs/"))
