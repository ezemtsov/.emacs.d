;; EXWM
(require 'exwm)
(require 'exwm-config)
(require 'exwm-randr)
(require 'exwm-systemtray)
(require 'exwm-xim)
(require 'exwm-layout)

(require 'transient)
(require 'consult)

;; Appropriated from tazjin's little functions file
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

(defun execute-command ()
  "A super-lightweight replacement for counsel-linux-app"
  (interactive)
  (let ((choice (consult--read (executable-list)
                               :category 'file
                               :prompt "Choose a command: ")))
    (start-process choice nil choice)))


(defun xkb-switch-list ()
  "A list of available keyboard layouts"
  (split-string (shell-command-to-string "xkb-switch --list")))

(defun xkb-switch (layout)
  "A layout switcher based on consult"
  (interactive)
  (shell-command (format "xkb-switch -s %s" layout))
  (message (format "Layout switched to %s" layout)))

(transient-define-prefix keyoard-switcher ()
  "Toggle busy."
  ["Layouts"
   ("1" "English (en)" (xkb-switch "us"))
   ("2" "Russian (ru)" (xkb-switch "ru"))
   ("3" "Norwegian (no)" (xkb-switch "no"))])

(defun start-shell ()
  "Start new shell with a name of current folder"
  (interactive)
  (vterm (concat "shell " default-directory)))

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

;; (defun brightness-up ()
;;   (interactive)
;;   (shell-command "exec light -A 10")
;;   (message "Brightness increased"))

;; (defun brightness-down ()
;;   (interactive)
;;   (shell-command "exec light -U 10")
;;   (message "Brightness decreased"))

(defun exwm-workspace-next ()
  (interactive)
  (exwm-workspace-switch-create (1+ exwm-workspace-current-index)))

(defun exwm-workspace-prior ()
  (interactive)
  (exwm-workspace-switch-create (1- exwm-workspace-current-index)))

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

;; ;;-----------------------------------------------------------
;; ;; EXWM Configuration
;; ;;-----------------------------------------------------------

(use-package tab-bar
  :config
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-current-with-count)
  (setq tab-bar-separator "")
  (setq tab-bar-close-button-show nil) ;; Hide annoying close buttom
  (setq tab-bar-format '(tab-bar-format-tabs tab-bar-format-align-right tab-bar-format-global))
  (tab-bar-mode 1)
  :custom
  (tab-bar-new-tab-choice
   (lambda () (get-buffer-create "*scratch*")))
  (tab-bar-tab-hints 1)
  (tab-bar-show t))

(use-package i3bar
  :config
  (i3bar-mode t))

(use-package exwm-modeline
  :config
  (exwm-modeline-mode))

;; Set static name for most of the x classes
(add-hook 'exwm-update-class-hook
          (lambda () (exwm-workspace-rename-buffer exwm-class-name)))

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

;; Show system tray
(exwm-systemtray-enable)

;; (setq exwm-floating-setup-hook nil)
;; (add-hook 'exwm-floating-setup-hook #'exwm-layout-show-mode-line)

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
         (, (kbd "s-d") . execute-command)
         (, (kbd "s-e") . rotate:even-horizontal)
         (, (kbd "s-v") . rotate:even-vertical)
         (, (kbd "s-f") . toggle-maximize-buffer)

         ;; Start programs
         (, (kbd "s-L") . screen-lock)
         (, (kbd "s-<return>") . start-shell)
         (, (kbd "<print>") . screenshot)

         ;; Move focus
         (, (kbd "s-<left>") . windmove-left)
         (, (kbd "s-<right>") . windmove-right)
         (, (kbd "s-<down>") . windmove-down)
         (, (kbd "s-<up>") . windmove-up)

         (, (kbd "s-<SPC>") . keyoard-switcher)

         ;; Tab shortcuts
         (,(kbd "s-w") . tab-close)
         (,(kbd "s-t") . tab-new)
         (,(kbd "s-<tab>") . tab-next)
         (,(kbd "s-<iso-lefttab>") . tab-previous)

         (,(kbd "s-<prior>") . exwm-workspace-prior)
         (,(kbd "s-<next>") . exwm-workspace-next)

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

         (, (kbd "s-o") . exwm-workspace-switch)

         ;; Some linux apps are too hungry for keyboard focus
         (, (kbd "C-x b") . consult-buffer)))

;; Line-editing shortcuts
(exwm-input-set-simulation-key (kbd "C-r") (kbd "C-r")) ;; refresh page
(exwm-input-set-simulation-key (kbd "C-d") (kbd "C-d")) ;; cancel process
;; (exwm-input-set-simulation-key (kbd "C-c C-c") (kbd "C-c")) ;; cancel process

(exwm-input-set-simulation-key (kbd "M-w") (kbd "C-c")) ;; copy text
(exwm-input-set-simulation-key (kbd "C-y") (kbd "C-v")) ;; copy text

(setq exwm-workspace-show-all-buffers nil)
(setq exwm-layout-show-all-buffers t)

;; Enable EXWM
(exwm-enable)
(exwm-randr-enable)

(provide 'desktop)
