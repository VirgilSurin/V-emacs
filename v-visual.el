;;; Package -- Summary
;;; Commentary:
;;; Code:

;; All-the-icons allows to have sweet looking icons!
;; _IMPORTANT_: you need to run M-x all-the-icons-install-fonts to download the fonts and then install them manually.

(use-package all-the-icons)

;TODO: change the background of the modline if inactive/active
;; (set-face-attribute 'modeline-inactive nil :background  (face-attribute 'hl-line  :background))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(doom-modeline-def-modeline 'main
  '(bar matches buffer-info remote-host buffer-position parrot selection-info)
  '(misc-info minor-modes checker input-method buffer-encoding major-mode process vcs "  "))

;TODO: move the lsp thingy from headerline to modeline
(defvar og-modeline mode-line-format)
(setq-default mode-line-format " ")
(setq-default header-line-format og-modeline)

;; font
(setq nano-font-family-monospaced "JetBrains Mono Bold")
(setq nano-font-size 22)


;; where to display what
(setq display-buffer-alist '(
                             ("\\*eshell*"
                              (display-buffer-reuse-window display-buffer-in-side-window)
                              (side . bottom)
                              (window-height . 10)
                              )
                             ("\\*shell*"
                              (display-buffer-reuse-window display-buffer-in-side-window)
                              (side . bottom)
                              (window-height . 10)
                              )
                             ("\\*help"
                              (display-buffer-reuse-window display-buffer-in-side-window)
                              (side . right)
                              (window-width . 80)
                              )
                             )
      )

;; visual
(use-package atom-one-dark-theme)
(use-package nord-theme)
(use-package doom-themes)
(use-package gruvbox-theme)
(use-package one-themes)
(load-theme 'one-dark t)  

;; Fall back font for glyph missing in Roboto
(defface fallback '((t :family "Fira Code"
                       :inherit 'nano-face-faded)) "Fallback")
(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'fallback))
(set-display-table-slot standard-display-table 'wrap
                         (make-glyph-code ?↩ 'fallback))


(require 'disp-table)

(setq default-frame-alist
      (append (list
	           '(min-height . 1)
               '(height     . 45)
	           '(min-width  . 1)
               '(width      . 81)
               '(vertical-scroll-bars . nil)
               '(internal-border-width . 24)
               '(left-fringe    . 10)
               '(right-fringe   . 10)
               '(tool-bar-lines . 0)
               '(menu-bar-lines . 0))))


(setq window-divider-default-right-width 24)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)

;; color of buffer separator
(set-face-attribute 'internal-border nil :foreground  (face-attribute 'default  :background))
(set-face-attribute 'window-divider nil :foreground  (face-attribute 'default  :background))
(set-face-attribute 'window-divider-first-pixel nil :foreground  (face-attribute 'default  :background))
(set-face-attribute 'window-divider-last-pixel nil :foreground  (face-attribute 'default  :background))


;; dimmer
(use-package dimmer)
(setq dimmer-fraction 0.3)
(dimmer-configure-which-key)
(dimmer-configure-magit)
(dimmer-configure-company-box)
(dimmer-mode t)



;; Dashboard !
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t)
  (setq dashboard-banner-logo-title "Welcome to V-Emacs")
  (setq dashboard-startup-banner "~/.emacs.d/emacs-logo.png")
  (setq dashboard-items '((recents  . 5)
                          (projects . 10)
                          )
        )
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  )
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))


(defun back-to-dashboard ()
    "close all windows and switch back to the dashboard buffer"
    (interactive)
    (switch-to-buffer "*dashboard*")
    (delete-other-windows)
    (dashboard-refresh-buffer)
    )
(global-set-key (kbd "C-x C-b") 'back-to-dashboard)


;; icons for dired
;; (use-package treemacs-icons-dired)
;; (add-hook 'dired-mode-hook 'treemacs-icons-dired-mode)

(provide 'v-visual)
