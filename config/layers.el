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
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package eglot
  :custom
  ;; shutdown eglot servers after buffer is closed
  (eglot-autoshutdown t))

;; (use-package eglot-booster
;;   :vc (:url "https://github.com/jdtsmith/eglot-booster"
;;        :branch "main")
;;   :after eglot
;;   :config (eglot-booster-mode))

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

;; (use-package web-mode
;;   :ensure t
;;   :defer t
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
;;   (add-hook 'web-mode-hook
;;             (lambda ()
;;               (when (string-equal "tsx" (file-name-extension buffer-file-name))
;;                 (setup-tide-mode)))))

(use-package fsharp-mode
  :defer t
  :ensure t
  :init
  (require 'eglot-fsharp)
  :config
  (add-to-list 'auto-mode-alist '("\\.fsproj\\'" . nxml-mode))
  (add-to-list 'auto-mode-alist '("\\.csproj\\'" . nxml-mode))
  ;; :custom
  ;; (eglot-type-hint-face ((t (:inherit nil))))
  :hook
  (fsharp-mode . (lambda ()
                   (highlight-indentation-mode)
                   (eglot-ensure))))

(use-package python-mode
  :defer t
  :ensure t
  :config
  ;; (highlight-indent-guides-mode)
  (setenv "PYTHONENCODING" "utf-8")
  (setq python-indent-offset 4)
  :hook
  (python-mode . eglot-ensure))

(use-package typescript-mode
  :defer t
  :ensure t
  :custom
  (typescript-mode-indent-offset 4)
  :hook
  (typescript-mode . setup-tide-mode))

(use-package rust-mode
  :defer t
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  (add-hook 'before-save-hook eglot-format-buffer)
  :hook
  (rust-mode . eglot-ensure))

(use-package json-mode
  :defer t
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
  :hook
  (json-mode . eglot-ensure))

(use-package sl
  :defer t
  :ensure t
  :hook
  (json-mode . eglot-ensure))

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook
  (go-mode . eglot-ensure))

(use-package jupyter
  :config
  (setq jupyter-repl-echo-eval-p t))

(provide 'layers)
