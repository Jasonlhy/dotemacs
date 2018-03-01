;; Package Management
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)


;; ======================== Customize ========================

;; Note: Run package-initialize-packages to install packes on different machines
;; - No annoying backup files
;; - No annoying ring-bell in command error
;; - show matching bracket
;; - Don't bind [tab] to evil-jump-forward
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-by-copying nil)
 '(custom-safe-themes
   (quote
    ("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(evil-want-C-i-jump nil)
 '(make-backup-files nil)
 '(package-selected-packages
   (quote
    (helm spacemacs-theme spaceline markdown-mode evil solarized-theme hungry-delete magit)))
 '(ring-bell-function (quote ignore))
 '(show-paren-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "darkblue"))))
 '(markdown-code-face ((t (:inherit default :height 1.0 :family "Hack")))))

;; =============== Keybinding ========================

;; Purpose: Open a new line just under current line without break of current line
;; 1. move to end of the line.
;; 2. insert newline with index
(defun newline-without-break-of-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(global-set-key (kbd "<C-return>") 'newline-without-break-of-line)

;; Duplicate current line
(defun duplicate-current-line ()
  (interactive)
  (move-beginning-of-line nil)
  (set-mark-command nil)
  (move-end-of-line nil)
  
  (let ((content (buffer-substring (region-beginning) (region-end))))
    (deactivate-mark)
    (message content)
    (newline)
    (insert content)))

(global-set-key (kbd "C-c C-d") 'duplicate-current-line)

;; Purpose: Select current word
(defun my-mark-current-word (&optional arg allow-extend)
  "Put point at beginning of current word, set mark at end."
  (interactive "p\np")
  (setq arg (if arg arg 1))
  (if (and allow-extend
	   (or (and (eq last-command this-command) (mark t))
	       (region-active-p)))
      (set-mark
       (save-excursion
	 (when (< (mark) (point))
	   (setq arg (- arg)))
	 (goto-char (mark))
	 (forward-word arg)
	 (point)))
    (let ((wbounds (bounds-of-thing-at-point 'word)))
      (unless (consp wbounds)
	(error "No word at point"))
      (if (>= arg 0)
	  (goto-char (car wbounds))
	(goto-char (cdr wbounds)))
      (push-mark (save-excursion
		   (forward-word arg)
		   (point)))
      (activate-mark))))

(global-set-key (kbd "C-c C-w") 'my-mark-current-word)

;; Purpose: Hungry Delete
(global-set-key (kbd "<C-backspace>") 'hungry-delete-backward)

;; Purpose: Handy isearch with right hand mouse
;; May not work with Transient Mark mode
(defun isearch-forward-region ()
  (interactive)
  (if (region-active-p)
      (let ((content (buffer-substring (region-beginning) (region-end))))
	(deactivate-mark)
	(isearch-forward nil 1)
	(isearch-yank-string content))
    (error "Please select a region")))

(global-set-key (kbd "C-c C-s") 'isearch-forward-region)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; For Mac only
(when (boundp 'mac-command-modifier) (setq mac-command-modifier 'control))

;; =============== General Setting ========================

;; Show Liline number
(global-linum-mode t)

;; Replace the old buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; auto insert closing bracket
(electric-pair-mode 1)

;; make typing delete/overwrites selected text
(delete-selection-mode 1)

;; turn on highlighting current line
(global-hl-line-mode 1)

;; remember cursor position, for emacs 25.1 or later
(save-place-mode 1)

;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; when a file is updated outside emacs, make it update if it's already opened in emacs
(global-auto-revert-mode 1)

;; Type y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Bigger font
(cond
 ;; Usually Mac has higer DPI
 ((find-font (font-spec :name "Menlo"))
  (set-face-attribute 'default nil :family "Menlo" :height 120))
 ((find-font (font-spec :name "Hack"))
  (set-face-attribute 'default nil :family "Hack" :height 100))
 ((find-font (font-spec :name "Consolas"))
  (set-face-attribute 'default nil :family "Consolas" :height 100)))

;; keep a list of recently opened files
(require 'recentf)
(recentf-mode 1)

;; make indentation commands use space only (never tab character)
;; emacs 23.1, 24.2, default to t
;; if indent-tabs-mode is t, it means it may use tab, resulting mixed space and tab
(progn
  (setq-default indent-tabs-mode nil))

;; Don't show startup screen
(setq inhibit-startup-screen t)

;; Auto reload the file if any outside change
(setq auto-revert-mode t)

;; Make windows to display chinese font smooth
(setq inhibit-compacting-font-caches t)

;; =============== Evil mode ========================
;; C-z doesn't work well in macOS
(setq evil-toggle-key "C-c C-z")

(require 'evil)
(evil-mode t)

;; Remove all keybindings from insert-state keymap,
;; It feels like more natural to use emacs key-binding in edit mode.
(setcdr evil-insert-state-map nil)

;; ESC to switch back normal-state
(define-key evil-insert-state-map [escape] 'evil-normal-state)

;; TAB to indent in normal-state
;; Tab is special in markdown mode and org mode
(define-key evil-normal-state-map (kbd "TAB") 'indent-for-tab-command)

;; I don't use visual block a lot, just keep me scroll like emacs
(define-key evil-normal-state-map (kbd "C-v") 'scroll-up-command)

;; Use j/k to move one visual line insted of gj/gk
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

;; Disable evil in certain mode
(evil-set-initial-state 'help-mode 'emacs)
(evil-set-initial-state 'dired-mode 'emacs)

;; ======= Spacemacs ========
;; Spacemacs Line
(require 'spaceline-config)
(spaceline-spacemacs-theme)

;; Spacemacs theme
(load-theme 'spacemacs-dark)

;; ======= helm =========
(require 'helm-config)

;; Better find command and find files
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

;; helm-mode completes with completion-at-point and implements completion-in-region-function for completing-read-multiple for Emacs 24.4 and later.
(helm-mode 1)

;; ====== ibuffer =======
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("magit" (mode . magit-status-mode))
               ("helm" (mode . helm-major-mode))
               ("erc" (mode . erc-mode))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))
