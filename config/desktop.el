;;; desktop.el - my EXWM functions

(require 'exwm)
(require 'exwm-randr)
(require 'exwm-systemtray)
(require 'exwm-xim)
(require 'exwm-layout)

(require 'transient)
(require 'consult)

(defun screen-lock ()
  (interactive)
  (start-process "xsecurelock" nil "xsecurelock"))

(defun xkb-switch-list ()
  "A list of available keyboard layouts"
  (split-string (shell-command-to-string "xkb-switch --list")))

(defun xkb-switch (layout)
  "A layout switcher based on consult"
  (interactive)
  (shell-command (format "xkb-switch -s %s" layout))
  (message (format "Layout switched to %s" layout)))

(transient-define-prefix keyoard-switcher ()
  "A transient-based keyboard switcher"
  ["Layouts"
   ("1" "English (en)" (lambda () (interactive) (xkb-switch "us")))
   ("2" "Russian (ru)" (lambda () (interactive) (xkb-switch "ru")))
   ("3" "Norwegian (no)" (lambda () (interactive) (xkb-switch "no")))])

(defun xrandr-list ()
  "xrandr query to get a list of monitors"
  (split-string (shell-command-to-string "xrandr --listmonitors | awk '{print $4}'") "\n" t))

(defun xrandr (&optional monitor)
  "Interactive monitor selector"
  (interactive)
  (let ((chosen-monitor (if monitor monitor (completing-read "Choose a monitor: " (xrandr-list)))))
    (shell-command (format "xrandr --output %s --primary --auto" chosen-monitor))))

(defun volume-mute ()
  (interactive) (shell-command "pactl set-sink-mute \"alsa_output.pci-0000_00_1f.3.analog-stereo\" toggle")
  (message "Speakers mute toggled"))

(defun volume-up ()
  (interactive) (shell-command "pactl set-sink-volume \"alsa_output.pci-0000_00_1f.3.analog-stereo\" +5%")
  (message "Speakers volume up"))

(defun volume-down ()
  (interactive) (shell-command "pactl set-sink-volume \"alsa_output.pci-0000_00_1f.3.analog-stereo\" -5%")
  (message "Speakers volume down"))

(defun exwm-workspace-next ()
  (interactive)
  (exwm-workspace-switch-create (1+ exwm-workspace-current-index)))

(defun exwm-workspace-prior ()
  (interactive)
  (exwm-workspace-switch-create (1- exwm-workspace-current-index)))

(defvar fullscreen-buffer--state nil)

(defun fullscreen-buffer--toggle ()
  "Maximize buffer"
  (interactive)
  (if fullscreen-buffer--state
      (let ((val (get-register (tab-bar--current-tab-index))))
        (register-val-jump-to val nil)
        (tab-bar-mode t)
        (setq mode-line-format (default-value 'mode-line-format))
        (setq fullscreen-buffer--state nil))
    (progn
      (window-configuration-to-register (tab-bar--current-tab-index))
      (delete-other-windows)
      (tab-bar-mode -1)
      (setq mode-line-format nil)
      (setq fullscreen-buffer--state t))))

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

;;-----------------------------------------------------------
;; Experimental stuff
;;-----------------------------------------------------------
;; Attempt to ensure correct external monitor handling

(defun my/exwm-ensure-workspaces (num-workspaces)
  "Ensure there are NUM-WORKSPACES created in EXWM"
  (let ((current-num-workspaces (length exwm-workspace--list)))
    (when (< current-num-workspaces num-workspaces)
      (dotimes (_ (- num-workspaces current-num-workspaces))
        (exwm-workspace-add))
      (message "Created %d new workspaces" (- num-workspaces current-num-workspaces)))
    (when (> current-num-workspaces num-workspaces)
      (dotimes (i (- current-num-workspaces num-workspaces))
        (exwm-workspace-delete))
      (message "Deleted %d workspaces" (- current-num-workspaces num-workspaces)))))

(defun my/xrandr-list ()
  "xrandr query to get a list of monitors"
  (split-string (shell-command-to-string "xrandr --listmonitors | awk '{print $4}'") "\n" t))

(defun my/exwm-update-workspaces ()
  "Ensure every connected monitor has a dedicated EXWM workspace"
  (let* ((monitors (my/xrandr-list))
         (num-monitors (length monitors)))
    ;; Ensure enough workspaces exist
    (my/exwm-ensure-workspaces num-monitors)
    ;; Clear current RANDR configuration
    (setq exwm-randr-workspace-monitor-plist nil)
    (dotimes (i num-monitors)
      ;; Assign a workspace to each monitor
      (setq exwm-randr-workspace-monitor-plist
            (append exwm-randr-workspace-monitor-plist
                    (list i (nth i monitors)))))
    (my/exwm-ensure-workspaces num-monitors)
    (message "Updated workspaces for monitors: %s" monitors)))

(add-hook 'exwm-randr-refresh-hook #'my/exwm-update-workspaces)

;;-----------------------------------------------------------
;; EXWM Configuration
;;-----------------------------------------------------------

(use-package tab-bar
  :config
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-all)
  (setq tab-bar-separator "")
  (setq tab-bar-close-button-show nil) ;; Hide annoying close buttom
  (setq tab-bar-format '(tab-bar-format-tabs tab-bar-format-align-right tab-bar-format-global))
  (tab-bar-mode 1)
  (tab-bar-history-mode t)
  :custom
  (tab-bar-new-tab-choice
   (lambda () (get-buffer-create "*scratch*")))
  (tab-bar-tab-hints 1)
  (tab-bar-show t)
  :bind
  ("C-s-<left>" . (lambda () (interactive) (tab-bar-move-tab -1)))
  ("C-s-<right>" . (lambda () (interactive) (tab-bar-move-tab 1))))

(use-package i3bar
  :config
  (i3bar-mode t))

(use-package exwm-modeline
  :config
  (exwm-modeline-mode))

;; Expect for browser, it should be named over it's tab
(setq exwm-update-title-hook nil)
(add-hook 'exwm-update-title-hook
      (lambda ()
        (cond ((member exwm-class-name '("Chromium-browser"))
               (exwm-workspace-rename-buffer (format " %s" exwm-title)))
              ((member exwm-class-name '("Beeper"))
               (exwm-workspace-rename-buffer (format " %s" exwm-title)))
              ((member exwm-class-name '("Slack"))
               (exwm-workspace-rename-buffer (format " %s" exwm-title))))))

;; This is a nice macro that allows remap global exwm keys without
;; emacs restart. Found here: https://oremacs.com/2015/01/17/setting-up-ediff
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))

;; Note that using global keys is important to be able to reach
;; shortcuts for some X-based apps that take over keyboard, for
;; example Telegram Desktop.
(csetq exwm-input-global-keys
       `(
         ;; Core actions
         (, (kbd "s-d") . consult-buffer)
         (, (kbd "s-e") . rotate:even-horizontal)
         (, (kbd "s-v") . rotate:even-vertical)
         (, (kbd "s-f") . fullscreen-buffer--toggle)
         (, (kbd "s-F") . exwm-layout-toggle-fullscreen)
         (, (kbd "s-Q") . exwm-workspace-delete)

         ;; Start programs
         (, (kbd "s-L") . (lambda () (interactive) (start-process "xsecurelock" nil "xsecurelock")))
         (, (kbd "s-<return>") . (lambda () (interactive) (vterm (concat "shell " default-directory))))
         (, (kbd "s-<print>") . (lambda () (interactive) (shell-command "exec flameshot gui")))

         ;; System keys
         (, (kbd "<XF86AudioMute>") . (lambda () (interactive) (shell-command-to-string "amixer set Master toggle")))
         (, (kbd "<XF86AudioLowerVolume>") . (lambda () (interactive) (shell-command-to-string "amixer set Master 10%-")))
         (, (kbd "<XF86AudioRaiseVolume>") . (lambda () (interactive) (shell-command-to-string "amixer set Master 10%+")))
         (, (kbd "<XF86MonBrightnessUp>") . (lambda () (interactive) (shell-command "light -A 10")))
         (, (kbd "<XF86MonBrightnessDown>") . (lambda () (interactive) (shell-command "light -U 10")))

         ;; Move focus
         (, (kbd "s-<left>") . windmove-left)
         (, (kbd "s-<right>") . windmove-right)
         (, (kbd "s-<down>") . windmove-down)
         (, (kbd "s-<up>") . windmove-up)

         ;; Move buffers
         (, (kbd "S-s-<left>") . buf-move-left)
         (, (kbd "S-s-<right>") . buf-move-right)
         (, (kbd "S-s-<up>") . buf-move-up)
         (, (kbd "S-s-<down>") . buf-move-down)

         (, (kbd "s-<SPC>") . keyoard-switcher)

         ;; Tab shortcuts
         (, (kbd "s-w") . tab-close)
         (, (kbd "s-t") . tab-new)
         (, (kbd "s-<tab> <right>") . tab-next)
         (, (kbd "s-<tab> <left>") . tab-previous)
         (, (kbd "s-<tab> <down>") . tab-bar-history-back)
         (, (kbd "s-<tab> <up>") . tab-bar-history-forward)

         (, (kbd "s-<prior>") . exwm-workspace-prior)
         (, (kbd "s-<next>") . exwm-workspace-next)

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

         (, (kbd "s-O") . exwm-workspace-move-window)
         (, (kbd "s-o") . exwm-workspace-switch)
         ))

;; Force Slack to behave
;; https://github.com/ch11ng/exwm/issues/574#issuecomment-490814569
(add-to-list 'exwm-manage-configurations '((equal exwm-class-name "Slack") managed t))

;; Show system tray
(exwm-systemtray-mode t)

(setq exwm-floating-setup-hook nil)
(add-hook 'exwm-floating-setup-hook #'exwm-layout-show-mode-line)

;; Set static name for most of the x classes
(add-hook 'exwm-update-class-hook
          (lambda () (exwm-workspace-rename-buffer exwm-class-name)))

;; Line-editing shortcuts
(exwm-input-set-simulation-key (kbd "C-r") (kbd "C-r")) ;; refresh page
(exwm-input-set-simulation-key (kbd "C-d") (kbd "C-d")) ;; cancel process

(exwm-input-set-simulation-key (kbd "M-w") (kbd "C-c")) ;; copy text
(exwm-input-set-simulation-key (kbd "C-y") (kbd "C-v")) ;; paste text

;; Workspace setup
(setq exwm-workspace-show-all-buffers nil)
(setq exwm-layout-show-all-buffers t)
(add-hook 'exwm-randr-screen-change-hook #'exwm-randr-refresh)

;; Enable EXWM
(exwm-randr-mode)
(exwm-enable)

(provide 'desktop)
