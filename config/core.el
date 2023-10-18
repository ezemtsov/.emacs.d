;; Global configuration
(use-package emacs
  :init
  (load-theme 'modus-operandi)
  (recentf-mode t) ;; remember previous files
  (blink-cursor-mode 0) ;; stop blinking
  (windmove-default-keybindings) ;; move with arrows
  (delete-selection-mode t) ;; delete on paste
  (global-display-line-numbers-mode t) ;; always show line numers
  (electric-pair-mode t) ;; smart parentesis wrapping
  (setq project-vc-extra-root-markers '(".gitignore")) ;; define a project
  (defadvice split-window ;; swtich forcus to the new window
      (after split-window-after activate)
    (other-window 1))
  (menu-bar-mode 0) ;; remove menus
  (scroll-bar-mode 0) ;; remove scrollbar
  (tool-bar-mode 0) ;; remove toolbars
  (envrc-global-mode) ;; support direnv
  
  :custom
  (use-short-answers t)	;; y or n
  (inhibit-startup-message t) ;; disable startup screen
  (initial-scratch-message nil) ;; remove startup message
  (dired-listing-switches "-alFh") ;; show human-readable dired
  (auto-save-file-name-transforms '((".*" "/tmp/" t))) ;; keep trash away from source
  (backup-directory-alist '((".*" . "/tmp/"))) ;; keep trash away from source

  :hook
  (prog-mode . whitespace-cleanup)
  (prog-mode . hl-line-mode)

  :bind
  ("M-TAB" . completion-at-point)
  ("M-/" . completion-at-point)
  ("C-c a" . align-regexp))

(use-package vertico
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package consult
  :bind
  (("C-c M-x" . consult-mode-command)
   ("C-x b" . consult-buffer)
   ("C-s" . consult-line)
   ("C-x s" . consult-project-regexp)
   ("C-x p SPC" . consult-project-buffer)))

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
  (global-corfu-mode)
  :custom
  (corfu-auto t))

;; File dabbrev & path extensions
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; (use-package lsp-bridge
;;   :config
;;   (global-lsp-bridge-mode))

(provide 'core)
