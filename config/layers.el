(use-package emacs
  :config
  (add-hook 'before-save-hook 'whitespace-cleanup) ;; breaks jinja mode for some reason
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
     (json-mode . json-ts-mode)
     (nix-mode . nix-mode)
     (rust-mode . rust-ts-mode)
     (toml-mode . toml-ts-mode)
     (yaml-mode . yaml-ts-mode))))

(use-package eglot
  :custom
  (eglot-autoshutdown t)) ;; shutdown eglot servers after buffer is closed

(use-package nix-mode
  :ensure t
  :defer t
  :custom
  (nix-nixfmt-bin "nixpkgs-fmt"))

;; Add support for pushing to gerrit
(use-package magit
  :config
  (defun magit-push-to-gerrit ()
    (interactive)
    (message (if (magit-commit-at-point) (magit-commit-at-point) "HEAD"))
    (let ((refspec (if (magit-commit-at-point) (magit-commit-at-point) "HEAD")))
      (message refspec)
      (magit-push-refspecs "origin" (format "%s:refs/for/master" refspec) nil)))

  (transient-append-suffix 'magit-push "p"
    '("R" "Push to gerrit" magit-push-to-gerrit)))

(use-package tide
  :defer t
  :ensure t)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

(use-package web-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode)))))

(use-package fsharp-mode
  :defer t
  :ensure t
  :init
  (require 'eglot-fsharp)
  :config
  ;; (highlight-indent-guides-mode t)
  (add-to-list 'auto-mode-alist '("\\.fsproj\\'" . nxml-mode))
  (add-to-list 'auto-mode-alist '("\\.csproj\\'" . nxml-mode))
  ;; :custom
  ;; (eglot-type-hint-face ((t (:inherit nil))))
  :hook
  (fsharp-mode . eglot-ensure))

(use-package python-mode
  :defer t
  :ensure t
  :config
  ;; (highlight-indent-guides-mode)
  (setenv "PYTHONENCODING" "utf-8")
  (setq python-indent-offset 4)
  :hook
  (python-ts-mode . eglot-ensure)
  (python-mode . eglot-ensure))

(use-package typescript-ts-mode
  :defer t
  :ensure t
  :custom
  (typescript-ts-mode-indent-offset 4)
  :hook
  (typescript-ts-mode . setup-tide-mode))

(provide 'layers)
