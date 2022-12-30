;;; Package -- Summary
;;; Commentary:
;;; Code:

;; Here is some default parameters

;; No startup  screen
(setq inhibit-startup-screen t)

;; No startup message
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; No message in scratch buffer
(setq initial-scratch-message nil)

;; Initial buffer
(setq initial-buffer-choice nil)
;; For some reason the scratch buffer keep opening so time to load the big artillery to make it go away
(add-hook 'emacs-startup-hook (lambda ()
                                (when (get-buffer "*scratch*")
                                  (delete-windows-on "*scratch*"))))
;; No frame title
(setq frame-title-format nil)

;; No file dialog
(setq use-file-dialog nil)

;; No dialog box
(setq use-dialog-box nil)

;; No popup windows
(setq pop-up-windows nil)

;; No empty line indicators
(setq indicate-empty-lines nil)

;; No cursor in inactive windows
(setq cursor-in-non-selected-windows nil)

;; Text mode is initial mode
(setq initial-major-mode 'text-mode)

;; Text mode is default major mode
(setq default-major-mode 'text-mode)

;; Moderate font lock
(setq font-lock-maximum-decoration nil)

;; No limit on font lock
(setq font-lock-maximum-size nil)

;; No line break space points
(setq auto-fill-mode nil)

;; Fill column at 80
(setq fill-column 80)
(display-fill-column-indicator-mode t)

;; line numbers
(global-display-line-numbers-mode 1)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook
                pdf-view-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; No confirmation for visiting non-existent files
(setq confirm-nonexistent-file-or-buffer nil)

;; No scroll bars
(if (fboundp 'scroll-bar-mode) (set-scroll-bar-mode nil))

;; No toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; No menu bar
(menu-bar-mode -1)

;; y/n for  answering yes/no questions
(fset 'yes-or-no-p 'y-or-n-p)

;; emacs ask for confirmation before comitting suicide
(setq confirm-kill-emacs 'y-or-n-p)

;; don't ask confirmation for killing active process
(setq kill-buffer-query-functions nil)

;; please do not overcrowd the kill ring
(setq kill-ring-max 20)

;; bar cursor
(setq-default cursor-type 'bar)

;; No tabs
(setq-default indent-tabs-mode nil)

;; Tab.space equivalence
(setq-default tab-width 4)

;; hightlight current line
(global-hl-line-mode t)

;; Size of temporary buffers
(temp-buffer-resize-mode)
(setq temp-buffer-max-height 8)

;; Minimum window height
(setq window-min-height 1)

;; delete selection
(delete-selection-mode 1)

;; Buffer encoding
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

;; Unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator " • "
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; Kill term buffer when exiting
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

; Start Emacs in fullscreen
(set-frame-parameter (selected-frame) 'fullscreen 'maximized) 
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; turn on auto revert mode
(global-auto-revert-mode t)

;; move all the backup files to specific cache directory
;; This way you won't have annoying temporary files starting with ~(tilde) in each directory
;; Following setting will move temporary files to specific folders inside cache directory in EMACS_DIR

(setq user-cache-directory (concat EMACS_DIR "cache"))
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-cache-directory)))
      url-history-file (expand-file-name "url/history" user-cache-directory)
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" user-cache-directory)
      projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-cache-directory))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

(provide 'v-default)
