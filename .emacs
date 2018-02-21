;; Package Management
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; =============== Keybinding ========================

;; Purpose: Open a new line just under current line without break of current line
;; 1. move to end of the line.
;; 2. insert newline with index
(defun newline-without-break-of-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(global-set-key (kbd "<C-return>") 'newline-without-break-of-line)

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
(defun isearch-forward-region ()
  (interactive)
  (let ((content (buffer-substring (region-beginning) (region-end))))
    (deactivate-mark)
    (isearch-forward nil 1)
    (isearch-yank-string content)))

(global-set-key (kbd "C-c C-s") 'isearch-forward-region)

;; For Mac only
(when (boundp 'mac-command-modifier) (setq mac-command-modifier 'control))

;; =============== General Setting ========================

;; Show Liline number
(global-linum-mode t)

;; Bigger font
(cond
 ;; Usually Mac has higer DPI
 ((find-font (font-spec :name "Menlo"))
  (set-face-attribute 'default nil :family "Menlo" :height 120))
 ((find-font (font-spec :name "Hack"))
  (set-face-attribute 'default nil :family "Hack" :height 100))
 ((find-font (font-spec :name "Consolas"))
  (set-face-attribute 'default nil :family "Consolas" :height 100)))

;; ======================== Customize ========================

;; - No annoying backup files
;; - No annoying ring-bell in command error
;; - show matching bracket
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-by-copying nil)
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(make-backup-files nil)
 '(package-selected-packages (quote (solarized-theme hungry-delete magit)))
 '(ring-bell-function (quote ignore))
 '(show-paren-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
