;;; Package -- Summary
;;; Commentary:
;;; Code:
;; This file will regroup all my little quality of life packages like Ivy, Avy and so

;; Ivy
(use-package smex)

(use-package ivy
  :diminish
  :bind (
         ("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done))
  :config
  (setq ivy-height 10)
  (setq ivy-count-format "")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (ivy-mode 1)
  )

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  )

(use-package counsel
  :bind (
         ("M-x"     . 'counsel-M-x)
         ("C-x C-f" . 'counsel-find-file)
         ("C-c r"   . 'counsel-recentf)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)
         )
)

;; Helpful - better C-h
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key)
  )

;; Wich-key is really helpful. When you start pressing a keybinding like C-c, C-h, C-x and so (prefix kbd) it will display all the possibilities  you have and what they do after one seconds (you can change the idle-delay if you want to) thus allowing you to see what you can do (if you forgot a kbd for example).
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.2))


;; AVY
(use-package use-package-chords
  :ensure t
  :init 
  :config (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.2)
  (setq key-chord-one-key-delay 0.3) ; default 0.2
  )

(use-package avy 
    :ensure t
    :config
    (setq avy-all-windows t)
    (setq avy-background nil)
    (setq avy-orders-alist
          '(
            (avy-goto-char . avy-order-closest)
            (avy-goto-word-1 . avy-order-closest)
            (avy-goto-line . avy-order-closest)
            ))
    (setq avy-keys-alist
          '(
            (avy-goto-char . (?d ?f ?g ?h ?j ?k))
            (avy-goto-word-1 . (?d ?f ?g ?h ?j ?k))
            (avy-goto-line . (?d ?f ?g ?h ?j ?k))
            ))
    (setq avy-style 'at-full)
    :chords
    ("jl" . avy-goto-line)
    ("jk" . avy-goto-char-timer)
    )

;; CRUX - a kot of useful little things
(use-package crux
  :bind (
         ("C-o"           . crux-smart-open-line-above)
         ("C-j"           . crux-smart-open-line)
         ("C-M-k"         . crux-smart-kill-line)
         ("C-c d"         . crux-duplicate-current-line-or-region)
         ("M-<backspace>" . crux-kill-whole-line)
         )
  )

;; I override crux-smart-kill-line because it is not smart the way I want
(defun my-crux-smart-kill-line ()
  "Kill the line and indent on first call. Kill the whole line on second call."
  (interactive)
  (let ((orig-point (point)))
    (mwim-beginning 1)
    (if (= orig-point (point))
        (crux-kill-whole-line)
      (kill-line)
      (indent-according-to-mode))))


(global-set-key (kbd "C-k") 'my-crux-smart-kill-line)
(define-key (current-global-map) [remap comment-region] 'crux-duplicate-current-line-or-region)


;; This package allows to go ignore comment when going to end-of-line
(use-package mwim
  :bind (
         ("C-e"   . mwim-end)
         ("C-a"   . mwim-beginning)
         )
  )


;; dired
(use-package dirvish
  :ensure t
  :init
  ;; Let Dirvish take over Dired globally
  (dirvish-override-dired-mode)
  :config
  (setq dirvish-reuse-session t)
  (setq dirvish-attributes '(vc-state subtree-state all-the-icons collapse git-msg file-size)))


(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-auto-revert-buffer t)
(require 'dired-x)
(define-key dired-mode-map (kbd "C-+") 'dired-create-empty-file)

;; god-mode for modal, easier navigation
(use-package god-mode
  :config
  (god-mode-all))
(global-set-key (kbd "<escape>") #'god-mode-all)

(defun my-god-mode-update-cursor-type ()
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'hollow 'bar)))

(add-to-list 'god-exempt-major-modes 'dired-mode)
(add-hook 'post-command-hook #'my-god-mode-update-cursor-type)

(provide 'v-qol)
;;; v-qol.el ends here
