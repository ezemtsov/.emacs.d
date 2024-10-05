;; Global configuration
(use-package emacs
  :init
  (load-theme 'sanityinc-tomorrow-night) ;; dark is nice for eyes
  (doom-modeline-mode nil) ;; make mode line simpler
  (set-frame-font "JetBrains Mono 10" nil t)
  (recentf-mode t) ;; remember previous files
  (blink-cursor-mode 0) ;; stop blinking
  (windmove-default-keybindings) ;; move with arrows
  (delete-selection-mode t) ;; delete on paste
  (electric-pair-mode t) ;; smart parentesis wrapping
  ;; focus on new windows

  (winner-mode t) ;; be able to undo window change
  (auto-save-mode t)

  (defun split-window-below-focus ()
    (interactive)
    (split-window-below)
    (redisplay) ; https://github.com/emacs-exwm/exwm/issues/22
    (windmove-down))

  (defun split-window-right-focus ()
    "Split the window horizontally and focus the new window."
    (interactive)
    (split-window-right)
    (redisplay) ; https://github.com/emacs-exwm/exwm/issues/22
    (windmove-right))

  (menu-bar-mode 0) ;; remove menus
  (scroll-bar-mode 0) ;; remove scrollbar
  (tool-bar-mode 0) ;; remove toolbars
  (which-key-mode t) ;; show options
  (setq-default indent-tabs-mode nil) ;; disable tabs
  (setq tab-width 4) ;; 4 spaces always
  (setq js-indent-level 4)
  (fringe-mode 0) ;; remove borders
  (envrc-global-mode 0) ;; enable direnv support globally
  (setq ring-bell-function 'ignore)
  (global-goto-address-mode) ;; enable URLs as hyperlinks. Navigate with C-c RET

  (defun delete-word (arg)
    "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
    (interactive "p")
    (delete-region (point) (progn (forward-word arg) (point))))

  (defun backward-delete-word (arg)
    "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
    (interactive "p")
    (delete-word (- arg)))

  :custom
  (use-short-answers t)	;; y or n
  (inhibit-startup-message t) ;; disable startup screen
  (initial-scratch-message nil) ;; remove startup message
  (dired-listing-switches "-alFh") ;; show human-readable dired
  (auto-save-file-name-transforms '((".*" "/tmp/" t))) ;; keep trash away from source
  (backup-directory-alist '((".*" . "/tmp/"))) ;; keep trash away from source
  (magit-display-buffer-function ;; show magit in the same buffer
   'magit-display-buffer-same-window-except-diff-v1)

  :bind
  ("C-x 2" . split-window-below-focus)
  ("C-x 3" . split-window-right-focus)
  ("C-=" . text-scale-increase)
  ("C--" . text-scale-decrease)
  ("M-TAB" . completion-at-point)
  ("M-/" . completion-at-point)
  ("C-c a" . align-regexp)
  ("C-c w" . whitespace-cleanup)
  ("M-<backspace>" . backward-delete-word)
  ("C-x k" . kill-this-buffer))

(use-package vertico
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package projectile
  :init
  (projectile-global-mode)
  :custom
  (projectile-globally-ignored-directories '(".git"))
  (projectile-dirconfig-file ".gitignore")
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :bind
  (:map projectile-command-map
        ("SPC" . projectile-find-file)
        ("s" . consult-ripgrep)))

(use-package consult
  :config
  ;; Appropriated from tazjin's little functions fiel
  ;; https://cs.tvl.fyi/depot/-/blob/users/tazjin/emacs/config/functions.el
  (defun executable-list ()
    "Creates a list of all external commands available on $PATH
     while filtering NixOS wrappers."
    (cl-loop
     for dir in (split-string (getenv "PATH") path-separator)
     when (and (file-exists-p dir) (file-accessible-directory-p dir))
     for lsdir = (cl-loop for i in (directory-files dir t)
                          for bn = (file-name-nondirectory i)
                          when (and (not (s-contains? "-wrapped" i))
                                    (not (member bn completions))
                                    (not (file-directory-p i))
                                    (file-executable-p i))
                          collect bn)
     append lsdir into completions
     finally return (sort completions 'string-lessp)))
  :custom
  (consult-project-function
   (lambda (_)
     (if (boundp 'projectile-project-root)
         (projectile-project-root) "/" )))
  (consult-buffer-sources
   '((:name "Current"
      :category buffer
      :items (lambda () (list (buffer-name))))
     (:name "Tabs"
      :category tab
      :items (lambda () (mapcar #'(lambda (tab) (alist-get 'name tab)) (tab-bar-tabs)))
      :action (lambda (cand) (tab-switch cand)))
     consult--source-hidden-buffer
     consult--source-modified-buffer
     consult--source-buffer
     consult--source-recent-file
     (:name "Apps"
      :category app
      :items executable-list
      :action (lambda (cand) (start-process cand nil cand)))))
  :bind
  ("C-s" . consult-line)
  ("C-x b" . consult-buffer))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic)))

;; Enable auto-completion
(use-package corfu
  :config
  (setq corfu-auto t)
  (setq corfu-popupinfo-mode t)
  :init
  (global-corfu-mode))

;; File dabbrev & path extensions
(use-package cape
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package undo-tree
  :custom
  (undo-tree-history-directory-alist
   '(("." . "~/.emacs.d/.undo-tree")))
  :init
  (global-undo-tree-mode))

;; Use fish for vterm
(use-package vterm
  :custom
  (vterm-buffer-name-string "shell %s")
  (vterm-shell "fish"))

;; Pull request viewer for magit
(use-package forge)

(use-package multiple-cursors
  :bind
  ("C->" . 'mc/mark-next-like-this)
  ("C-<" . 'mc/mark-previous-like-this))

(use-package vertico-posframe
  :config
  (setq vertico-posframe-width 200)
  (vertico-posframe-mode t))

(provide 'core)
