;; Jump to window
(winum-mode)

;; EXWM
(require 'exwm)
(require 'exwm-config)
(require 'exwm-randr)
(require 'exwm-systemtray)
(require 'exwm-xim)
(require 'exwm-layout)

;; (defun screenshot ()
;;   (interactive)
;;   (shell-command "flameshot gui"))

;; (defun suspend ()
;;   (interactive)
;;   (shell-command "systemctl suspend"))

(defun screen-lock ()
  (interactive)
  (start-process "xsecurelock" nil "xsecurelock"))

(defun start-alacritty ()
  (interactive)
  (start-process "alacritty" nil "alacritty"))

(defun monitor-external-disable ()
  (interactive)
  (shell-command "xrandr --output eDP-1 --primary --auto --output DP-1-1-2 --off --output DP-1-1-8 --off")
  (message "Disable external monitor"))

(defun monitor-external-enable ()
  (interactive)
  (shell-command "xrandr --output eDP-1 --auto --output DP-1-1-8 --auto --primary")
  (message "Enable external monitor"))

;; (defun pulseaudio-ctl (cmd)
;;   (shell-command (concat "pulseaudio-ctl " cmd))
;;   (message "Volume command: %s" cmd))

(defun volume-mute ()
  (interactive) (shell-command "pactl set-sink-mute \"alsa_output.pci-0000_00_1f.3.analog-stereo\" toggle")
  (message "Speakers mute toggled"))

(defun volume-up ()
  (interactive) (shell-command "pactl set-sink-volume \"alsa_output.pci-0000_00_1f.3.analog-stereo\" +5%")
  (message "Speakers volume up"))

(defun volume-down ()
  (interactive) (shell-command "pactl set-sink-volume \"alsa_output.pci-0000_00_1f.3.analog-stereo\" -5%")
  (message "Speakers volume down"))

;; (defun brightness-up ()
;;   (interactive)
;;   (shell-command "exec light -A 10")
;;   (message "Brightness increased"))

;; (defun brightness-down ()
;;   (interactive)
;;   (shell-command "exec light -U 10")
;;   (message "Brightness decreased"))

;; ;; Monitor functions
;; (setq monitor-current (alist-get 'name (frame-monitor-attributes)))
;; (setq monitor-list (mapcar
;;                     (lambda (arg) (alist-get 'name arg))
;;                     (display-monitor-attributes-list)))
;; (setq monitor-primary "eDP-1")
;; (setq monitor-external "DP-1-1-8")

;; (defun workspace-move (display)
;;   (plist-put exwm-randr-workspace-output-plist
;;              exwm-workspace-current-index
;;              display)
;;   (exwm-randr--refresh))

;; (defun workspace-move-to-primary ()
;;   (interactive)
;;   (workspace-move monitor-primary))

;; (defun workspace-move-to-external ()
;;   (interactive)
;;   (workspace-move monitor-external))

;; ;;-----------------------------------------------------------
;; ;; EXWM Configuration
;; ;;-----------------------------------------------------------

;; Set 10 workspaces
(setq exwm-workspace-number 10)

;; 's-N': Switch to certain workspace, but switch back to the previous
;; one when tapping twice (emulates i3's `back_and_forth' feature)
(defvar *exwm-workspace-from-to* '(-1 . -1))
(defun exwm-workspace-switch-back-and-forth (target-idx)
  ;; If the current workspace is the one we last jumped to, and we are
  ;; asked to jump to it again, set the target back to the previous
  ;; one.
  (when (and (eq exwm-workspace-current-index (cdr *exwm-workspace-from-to*))
	     (eq target-idx exwm-workspace-current-index))
    (setq target-idx (car *exwm-workspace-from-to*)))

  (setq *exwm-workspace-from-to*
	(cons exwm-workspace-current-index target-idx))

  (exwm-workspace-switch-create target-idx))

;; Provide a binding for jumping to a buffer on a workspace.
(defun exwm-jump-to-buffer ()
  "Jump to a workspace on which the target buffer is displayed."
  (interactive)
  (let ((exwm-layout-show-all-buffers nil)
	(initial exwm-workspace-current-index))
    (call-interactively #'exwm-workspace-switch-to-buffer)
    ;; After jumping, update the back-and-forth list like on a direct
    ;; index jump.
    (when (not (eq initial exwm-workspace-current-index))
      (setq *exwm-workspace-from-to*
	    (cons initial exwm-workspace-current-index)))))

;; Set static name for most of the x classes
(setq exwm-update-class-hook (exwm-workspace-rename-buffer exwm-class-name))

;; Expect for browser, it should be named over it's tab
(setq exwm-update-title-hook
      (lambda ()
	(when (equal exwm-class-name "Chromium-browser")
	  (exwm-workspace-rename-buffer exwm-title))))

;; Show system tray
(exwm-systemtray-enable)

;; Keyboard layout per window
;; (exwm-xim-enable)

(add-hook 'exwm-floating-setup-hook #'exwm-layout-show-mode-line)

(display-battery-mode)
(setq display-time-format "%a %H:%M")
(display-time-mode 1)

(defcustom exwm-workspace-mode-line-format
  `("["
    (:propertize
     (:eval (format "WS-%d" exwm-workspace-current-index))
     face bold
     mouse-face mode-line-highlight)
    "]")
  "EXWM workspace in the mode line."
  :type 'sexp)

(defun toggle-maximize-buffer () "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(use-package emacs
  :config
  (add-to-list 'mode-line-misc-info exwm-workspace-mode-line-format t))

;; Global EXWM keybindings
(setq exwm-input-global-keys
	;; Utilities
      `((,(kbd "s-r")                     . exwm-reset)
	(,(kbd "s-w")                     . exwm-workspace-switch)
	(,(kbd "<print>")                 . screenshot)
	(,(kbd "s-i")                     . exwm-input-toggle-keyboard)
	(,(kbd "s-j")                     . exwm-jump-to-buffer)

	(,(kbd "s-d")                     . counsel-linux-app)
	(,(kbd "s-e")                     . rotate:even-horizontal)
	(,(kbd "s-v")                     . rotate:even-vertical)
	(,(kbd "s-<return>")              . start-alacritty)
	(,(kbd "s-L")                     . screen-lock)
	(,(kbd "s-f")                     . toggle-maximize-buffer)

	;; External monitor
	(,(kbd "s-m <up>")                . monitor-external-enable)
	(,(kbd "s-m <down>")              . monitor-external-disable)
	(,(kbd "s-m S-<up>")              . workspace-move-to-external)
	(,(kbd "s-m S-<down>")            . workspace-move-to-primary)

	;; Switch focus
	(,(kbd "s-<left>")                . windmove-left)
	(,(kbd "s-<right>")               . windmove-right)
	(,(kbd "s-<down>")                . windmove-down)
	(,(kbd "s-<up>")                  . windmove-up)

	;; Move buffers
	(,(kbd "s-S <up>")                . buf-move-up)
	(,(kbd "s-S <down>")              . buf-move-down)
	(,(kbd "s-S <left>")              . buf-move-left)
	(,(kbd "s-S <right>")             . buf-move-right)

	;; Resize buffers
	(,(kbd "C-M-<left>")              . shrink-window-horizontally)
	(,(kbd "C-M-<right>")             . enlarge-window-horizontally)
	(,(kbd "C-M-<up>")                . shrink-window)
	(,(kbd "C-M-<down>")              . enlarge-window)

	;; Media control
	(,(kbd "<XF86AudioMute>")         . volume-mute)
	(,(kbd "<XF86AudioRaiseVolume>")  . volume-up)
	(,(kbd "<XF86AudioLowerVolume>")  . volume-down)
	(,(kbd "<XF86MonBrightnessDown>") . brightness-down)
	(,(kbd "<XF86MonBrightnessUp>")   . brightness-up)

	;; ;; Switch window by s-o N
	,@(mapcar (lambda (i)
		    `(,(kbd (format "M-%d" i)) .
		      (lambda ()
			(interactive)
			(winum-select-window-by-number ,i))))
		  (number-sequence 0 9))

	;; Switch to workspace by s-N (lambda (i)
	,@(mapcar (lambda (i)
		    `(,(kbd (format "s-%d" i)) .
		      (lambda ()
			(interactive)
			(exwm-workspace-switch-back-and-forth ,i))))
		  (number-sequence 1 9))))

;; Line-editing shortcuts
(exwm-input-set-simulation-keys
 '(([?\C-r] . ?\C-r)
   ([?\C-d] . ?\C-d) ;; cancel python
   ([?\C-C] . ?\C-c) ;; cancel process
   ([?\M-w] . ?\C-c) ;; copy
   ([?\C-y] . ?\C-v))) ;; paste

;; Let buffers move seamlessly between workspaces by making them
;; accessible in selectors on all frames.
(setq exwm-workspace-show-all-buffers t)
(setq exwm-layout-show-all-buffers t)

;; Enable EXWM
(exwm-enable)
(exwm-randr-enable)

(provide 'desktop)
