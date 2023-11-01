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
(add-to-list 'load-path "~/git/lsp-bridge")
(add-to-list 'load-path (concat user-emacs-directory "config"))

(defun installable-packages (pkg-list)
  "Filter out not-yet installed packages from package list."
  (seq-filter (lambda (p) (not (package-installed-p p))) pkg-list))

(defun utils-install-packages (pkg-list)
  (let ((to-install (installable-packages pkg-list)))
    (if (< 0 (length to-install))
        (progn (package-refresh-contents)
               (mapcar #'package-install to-install))
      (message "No new packages to install."))))

;; Import configuration packages
(defun initialize-settings ()
  (interactive)
  (mapc 'require '(desktop
		   core
                   layers)))

(add-hook 'after-init-hook 'initialize-settings)

;; (put 'upcase-region 'disabled nil)
;; (put 'erase-buffer 'disabled nil)
;; (put 'downcase-region 'disabled nil)
