;; Global configuration
(use-package emacs
  :init
  (load-theme 'sanityinc-tomorrow-night) ;; dark is nice for eyes
  (telephone-line-mode t) ;; make mode line simpler
  (set-frame-font "JetBrains Mono 10" nil t)
  (recentf-mode t) ;; remember previous files
  (blink-cursor-mode 0) ;; stop blinking
  (windmove-default-keybindings) ;; move with arrows
  (delete-selection-mode t) ;; delete on paste
  (electric-pair-mode t) ;; smart parentesis wrapping
  ;; focus on new windows
  (defun split-window-below-focus () (interactive) (split-window-below) (other-window 1))
  (defun split-window-right-focus () (interactive) (split-window-right) (other-window 1))
  (menu-bar-mode 0) ;; remove menus
  (scroll-bar-mode 0) ;; remove scrollbar
  (tool-bar-mode 0) ;; remove toolbars
  (which-key-mode t) ;; show options
  (setq-default indent-tabs-mode nil) ;; disable tabs
  (setq tab-width 4) ;; 4 spaces always
  (fringe-mode 0) ;; remove borders

  :custom
  (use-short-answers t)	;; y or n
  (inhibit-startup-message t) ;; disable startup screen
  (initial-scratch-message nil) ;; remove startup message
  (dired-listing-switches "-alFh") ;; show human-readable dired
  (auto-save-file-name-transforms '((".*" "/tmp/" t))) ;; keep trash away from source
  (backup-directory-alist '((".*" . "/tmp/"))) ;; keep trash away from source
  (magit-display-buffer-function ;; show magit in the same buffer
   'magit-display-buffer-same-window-except-diff-v1)
  (goto-address-mode t) ;; make HTTP links clickable

  :bind
  ("C-x 2" . split-window-below-focus)
  ("C-x 3" . split-window-right-focus)
  ("C-=" . text-scale-increase)
  ("C--" . text-scale-decrease)
  ("M-TAB" . completion-at-point)
  ("M-/" . completion-at-point)
  ("C-c a" . align-regexp)
  ("C-c w" . whitespace-cleanup))

(use-package vertico
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package projectile
  :config
  (projectile-global-mode)
  :custom
  (projectile-dirconfig-file ".gitignore")
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package consult
  :bind
  (("C-x b" . consult-buffer)
   ;; (("C-x b" . counsel-switch-buffer)
   ("C-s" . consult-line)
   ("C-x p g" . consult-ripgrep)
   ("C-x p f" . consult-find)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic)))

(use-package undo-tree
  :init
  (undo-tree-mode t)
  :custom
  (undo-tree-history-directory-alist
   '(("." . "~/.emacs.d/.undo-tree"))))

;; Enable auto-completion
(use-package corfu
  :init
  (corfu-popupinfo-mode t)
  (global-corfu-mode))

;; File dabbrev & path extensions
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; Use fish for vterm
(use-package vterm
  :custom
  (vterm-shell "fish"))

;; (use-package lsp-bridge
;;   :config
;;   ;; Force lsp-bridge to respect monorepo projects
;;   (setq lsp-bridge-get-project-path-by-filepath
;; 	(lambda (filepath) (project-root (project-current))))
;;   (global-lsp-bridge-mode)

(provide 'core)
