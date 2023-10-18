;; ;; Use tree-sitter modes for various languages.
;; (setq major-mode-remap-alist
;;       '((bash-mode . bash-ts-mode)
;;         (c++-mode . c++-ts-mode)
;;         (c-mode . c-ts-mode)
;;         (c-or-c++-mode . c-or-c++-ts-mode)
;;         (csharp-mode . csharp-ts-mode)
;;         (json-mode . json-ts-mode)
;;         (python-mode . python-ts-mode)
;;         (rust-mode . rust-ts-mode)
;;         (toml-mode . toml-ts-mode)
;;         (yaml-mode . yaml-ts-mode)
;;         (go-mode . go-ts-mode)
;;         (cmake-mode . cmake-ts-mode)))

;; ;; Add 'modes' folder that contains other settings to load.
;; (add-hook 'before-save-hook
;;           (lambda ()
;;             (whitespace-cleanup)))

;; (add-hook 'prog-mode-hook
;;           (lambda ()
;;             (hl-line-mode)
;;             (flyspell-prog-mode)
;;             (display-line-numbers-mode)
;;             (show-paren-mode)
;;             (rainbow-mode)
;;             (rainbow-delimiters-mode)))

;; (use-package nix-mode
;;   :config
;;   (setq nix-nixfmt-bin "nixpkgs-fmt"))

;; (use-package web-mode
;;   :defer t
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.liquid\\'" . web-mode)))

;; (use-package text-mode
;;   :hook
;;   (flyspell-mode))

;; ;; (use-package dump-mode
;; ;;   :config
;; ;;   (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))


;; ;; (add-to-list 'auto-mode-alist '("\\.tf\\'" . hcl-mode))

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
  :hook
  (python-mode . eglot-ensure))

(provide 'layers)
