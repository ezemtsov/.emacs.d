(use-package emacs
  :config
  (add-hook 'before-save-hook 'whitespace-cleanup)
  (add-hook 'prog-mode-hook 'hl-line-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'prog-mode-hook 'show-paren-mode)
  (add-hook 'prog-mode-hook 'rainbow-mode)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

  ;; enable jump to definition even when lsp is not enabled
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

  :custom
  ;; Use tree-sitter modes for various languages.
  (major-mode-remap-alist
   '((bash-mode . bash-ts-mode)
     (csharp-mode . csharp-ts-mode)
     (json-mode . json-ts-mode)
     (python-mode . python-ts-mode)
     (rust-mode . rust-ts-mode)
     (toml-mode . toml-ts-mode)
     (yaml-mode . yaml-ts-mode))))

(use-package nix-mode
  :ensure t
  :defer t
  :custom
  (nix-nixfmt-bin "nixpkgs-fmt"))

(use-package web-mode
  :ensure t
  :defer t)

(use-package fsharp-mode
  :defer t
  :ensure t
  :init
  (require 'eglot-fsharp)
  :config
  (add-to-list 'auto-mode-alist '("\\.fsproj\\'" . nxml-mode))
  (add-to-list 'auto-mode-alist '("\\.csproj\\'" . nxml-mode))
  :hook
  (fsharp-mode . eglot-ensure))

(use-package python-mode
  :defer t
  :ensure t
  :config
  (highlight-indent-guides-mode)
  (setenv "PYTHONENCODING" "utf-8")
  (setq python-indent-offset 4)
  :hook
  (python-mode . eglot-ensure))

;; Add support for pushing to gerrit
(use-package magit
  :config
  (defun magit-push-to-gerrit ()
    (interactive)
    (magit-git-command-topdir "git push origin HEAD:refs/for/master"))
  (transient-append-suffix 'magit-push "p"
    '("R" "Push to gerrit" magit-push-to-gerrit)))

(provide 'layers)
