(require 'package)

;; Start server
(server-start)

;; Initialize nix packages
(package-initialize)

;; Configure MELPA repositories.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Custom location for standard settings from M-x customize
(setq custom-file (concat user-emacs-directory "config/custom.el"))
(load custom-file)

;; Add config folder that contains other settings to load.
(add-to-list 'load-path (concat user-emacs-directory "config"))

(defun settings-initialise ()
  (interactive)
  (require 'core)
  (require 'desktop)
  (require 'layers))

(add-hook 'after-init-hook 'settings-initialise)

;; (put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
