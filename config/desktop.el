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

(defun screen-lock ()
  (interactive)
  (start-process "xsecurelock" nil "xsecurelock"))

;; Start new shell with a name of current folder
(defun start-shell ()
  (interactive)
  (vterm (concat "shell " default-directory)))

(defun xrandr-list ()
  (split-string (shell-command-to-string "xrandr --listmonitors | awk '{print $4}'") "\n" t))

(defun xrandr (&optional monitor)
  (interactive)
  (let ((chosen-monitor (if monitor monitor (completing-read "Choose a monitor: " (xrandr-list)))))
    (shell-command (format "xrandr --output %s --primary --auto" chosen-monitor))))

(defun monitor-external-enable (monitor)
  (interactive "Choose monitor to enable")
  (shell-command (format "xrandr --output %s --primary --auto" monitor))
  (message (format "Trying to enable %s" monitor)))

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

;; ;;-----------------------------------------------------------
;; ;; EXWM Configuration
;; ;;-----------------------------------------------------------

;; Set static name for most of the x classes
(add-hook 'exwm-update-class-hook
	  (lambda () (exwm-workspace-rename-buffer exwm-class-name)))

;; Expect for browser, it should be named over it's tab
(add-hook 'exwm-update-title-hook
      (lambda ()
	(when (member exwm-class-name '("Chromium-browser" "firefox"))
	  (exwm-workspace-rename-buffer exwm-title))))

;; Show system tray
(exwm-systemtray-enable)

;; Keyboard layout per window
;; (exwm-xim-enable)

(add-hook 'exwm-floating-setup-hook #'exwm-layout-show-mode-line)

(display-battery-mode)
(setq display-time-format "%a %H:%M")
(display-time-mode 1)

(defun toggle-maximize-buffer () "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(defun tab-bar-select-or-return ()
  "This function behaves like `tab-bar-select-tab', except it calls
`tab-recent' if asked to jump to the current tab. This simulates
the back&forth behaviour of i3."
  (interactive)
  (let* ((key (event-basic-type last-command-event))
	 (tab (if (and (characterp key) (>= key ?1) (<= key ?9))
		  (- key ?0)
		0))
	 (current (1+ (tab-bar--current-tab-index))))
    (if (eq tab current)
	(tab-recent)
      (tab-bar-select-tab tab))))

(use-package emacs
  :config
  (xrandr "DP-3") ;; Prefer HDMI monitor
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-truncated)
  (setq tab-bar-separator "")
  (setq tab-bar-close-button-show nil) ;; Hide annoying close buttom
  (setq tab-bar-format '(tab-bar-format-tabs tab-bar-format-align-right tab-bar-format-global))
  (tab-bar-mode 1)
  :custom
  (tab-bar-new-tab-choice
   (lambda () (get-buffer-create "*scratch*")))
  (tab-bar-tab-hints 1)
  (tab-bar-show 1))

;; This is a nice macro that allows remap global exwm keys without
;; emacs restart. Found here: https://oremacs.com/2015/01/17/setting-up-ediff
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
		'set-default)
	    ',variable ,value))

;; Note that having shortcuts in global keys is important to be able
;; to reach shortcuts in some of the apps that take over keyboard
;; shortcuts, for example Telegram Desktop
(csetq exwm-input-global-keys
       `(
	 ;; Core actions
	 (, (kbd "s-d") . counsel-linux-app)
	 (, (kbd "s-e") . rotate:even-horizontal)
	 (, (kbd "s-v") . rotate:even-vertical)
	 (, (kbd "s-f") . toggle-maximize-buffer)

	 ;; Start programs
	 (, (kbd "s-L") . screen-lock)
	 (, (kbd "s-<return>") . start-shell)

	 ;; Move focus
	 (, (kbd "s-<left>") . windmove-left)
	 (, (kbd "s-<right>") . windmove-right)
	 (, (kbd "s-<down>") . windmove-down)
	 (, (kbd "s-<up>") . windmove-up)

	 ;; Tab shortcuts
	 (,(kbd "s-w") . tab-close)
	 (,(kbd "s-t") . tab-new)
	 (,(kbd "s-<tab>") . tab-next)
	 (,(kbd "s-<iso-lefttab>") . tab-previous)

	 ;; Switch to tab by s-N
	 ,@(mapcar (lambda (i)
		     `(,(kbd (format "s-%d" i)) .
		       tab-bar-select-or-return))
		   (number-sequence 1 9))

	 ;; Resize buffers
	 (, (kbd "C-M-<left>") . shrink-window-horizontally)
	 (, (kbd "C-M-<right>") . enlarge-window-horizontally)
	 (, (kbd "C-M-<up>") . shrink-window)
	 (, (kbd "C-M-<down>") . enlarge-window)

	 ;; Some linux apps are too hungry for keyboard focus
	 (, (kbd "C-x b") . consult-buffer)))

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
