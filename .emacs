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
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

(global-set-key (kbd "<C-return>") 'newline-without-break-of-line)

;; Purpose: Hungry Delete
(global-set-key (kbd "<C-backspace>") 'hungry-delete-backward)

;; =============== General Setting ========================

;; Show Liline number
(global-linum-mode t)

(defun square (x)
  (* x x))

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
 '(make-backup-files nil)
 '(package-selected-packages (quote (hungry-delete magit)))
 '(ring-bell-function (quote ignore))
 '(show-paren-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
