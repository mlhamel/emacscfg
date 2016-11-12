(use-package web-mode
  :config
  ;; web-mode
  (add-to-list 'auto-mode-alist '("\\.jinja2\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mako\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))


  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

  ;; css-mode
  (add-to-list 'auto-mode-alist '("\\.less$" . css-mode))

  ;; sgml-mode
  (add-hook 'sgml-mode-hook
            (lambda ()
              (require 'rename-sgml-tag)
              (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)))

  ;; jsx-mode
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
  (setq jsx-indent-level 2)
  (add-hook 'jsx-mode-hook
            (lambda () (auto-complete-mode 1)))

  ;; indentation
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq js-indent-level 2))
