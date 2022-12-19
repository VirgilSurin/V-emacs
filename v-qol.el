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
  (setq swiper-use-visual-line nil)
  (setq swiper-use-visual-line-p (lambda (a) nil))
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

(defun update-window-divider (theme)
  "Update the window divider background colors according to new theme bg."
  (interactive)
  (set-face-attribute 'internal-border nil :foreground  (face-attribute 'default  :background))
(set-face-attribute 'window-divider nil :foreground  (face-attribute 'default  :background))
(set-face-attribute 'window-divider-first-pixel nil :foreground  (face-attribute 'default  :background))
(set-face-attribute 'window-divider-last-pixel nil :foreground  (face-attribute 'default  :background))
)

(advice-add 'counsel-load-theme :after #'update-window-divider)

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

;; a better zap-to-char
(use-package avy-zap)
(global-set-key (kbd "M-z") 'avy-zap-up-to-char-dwim)

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
            ;; (avy-goto-line . (?d ?f ?g ?h ?j ?k))
            (avy-goto-line . (?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0))
            ))
    (setq avy-style 'at-full)
    :chords
    ("jl" . avy-goto-line)
    ("jk" . avy-goto-char-timer)
    )

;; CRUX - a kot of useful little things
(use-package crux
  :bind (
         ("C-o"      . crux-smart-open-line-above)
         ("C-j"      . crux-smart-open-line)
         ("C-k"    . crux-smart-kill-line)
         ("C-c C-d"  . crux-duplicate-current-line-or-region)
         )
  )

;; I override crux-smart-kill-line because it is not smart the way I want
(defun my-crux-smart-kill-line ()
  "Kill the line backward and indent on first call. Kill the whole line on second call."
  (interactive)
  (let ((orig-point (point)))
    (beginning-of-line-text)
    (if (= orig-point (point))
        (crux-kill-whole-line)
      (goto-char orig-point)
      (kill-line 0)
      (indent-according-to-mode))))

(global-set-key (kbd "M-k") 'join-line)
(global-set-key (kbd "C-M-k") 'my-crux-smart-kill-line)

;; browse kill-ring
(use-package browse-kill-ring)
(browse-kill-ring-default-keybindings)


;; This package allows to go ignore comment when going to end-of-line
(use-package mwim
  :bind (
         ("C-e"   . mwim-end)
         ("C-a"   . mwim-beginning)
         )
  )

;; simply a better comment-dwim
(use-package comment-dwim-2)
(global-set-key (kbd "M-;") 'comment-dwim-2)
(define-key org-mode-map (kbd "M-;") 'org-comment-dwim-2)

;; hungry delete
(use-package hungry-delete
  :config
  (setq hungry-delete-join-reluctantly t)) ; will leave a space
(global-hungry-delete-mode)

(winner-mode 1)
(global-set-key (kbd "C-c <left>")  'winner-undo)
(global-set-key (kbd "C-c <right>") 'winner-redo)


;; dired
(use-package dirvish
  :ensure t
  :init
  ;; Let Dirvish take over Dired globally
  (dirvish-override-dired-mode)
  :config
  (setq dirvish-reuse-session t)
  (setq dirvish-attributes '(vc-state subtree-state all-the-icons collapse git-msg file-size))
   :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
   (("C-x d" . dirvish-dwim)
    :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
    ("a"   . dirvish-quick-access)
    ("f"   . dirvish-file-info-menu)
    ("y"   . dirvish-yank-menu)
    ("N"   . dirvish-narrow)
    ("h"   . dirvish-history-jump) ; remapped `describe-mode'
    ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
    ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
    ("TAB" . dirvish-subtree-toggle)
    ("M-f" . dirvish-history-go-forward)
    ("M-b" . dirvish-history-go-backward)
    ("M-l" . dirvish-ls-switches-menu)
    ("M-m" . dirvish-mark-menu)
    ("M-t" . dirvish-layout-toggle)
    ("M-s" . dirvish-setup-menu)
    ("M-e" . dirvish-emerge-menu)
    ("M-j" . dirvish-fd-jump)
    )
   )

(setq dired-kill-when-opening-new-dired-buffer nil)
(setq dired-auto-revert-buffer t)
(require 'dired-x)
(define-key dired-mode-map (kbd "C-+") 'dired-create-empty-file)

;; god-mode for modal, easier navigation
(use-package god-mode
 :config
 (god-mode-all))
(global-set-key (kbd "<escape>") #'god-local-mode)
(define-key god-local-mode-map (kbd "i") #'god-local-mode)

(defun my-god-mode-update-cursor-type ()
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

(add-to-list 'god-exempt-major-modes 'dired-mode)
(add-to-list 'god-exempt-major-modes 'vterm-mode)
(add-hook 'post-command-hook #'my-god-mode-update-cursor-type)


(use-package vterm
  :ensure t
  :init
  :custom (vterm-kill-buffer-on-exit t)
  )
(define-key vterm-mode-map (kbd "<escape>") 'vterm-copy-mode)
(define-key vterm-mode-map (kbd "C-s") nil)
(define-key vterm-mode-map (kbd "M-<up>") nil)

;; (use-package evil
;;   :init
;;   (setq evil-want-integration t)
;;   (setq evil-want-keybinding nil)
;;   (setq evil-want-C-u-scroll t)
;;   (setq evil-want-C-i-jump nil)
;;   :config
;;   ;; (evil-mode 1)
;;   (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
;;   (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

;;   ;; Use visual line motions even outside of visual-line-mode buffers
;;   (evil-global-set-key 'motion "j" 'evil-next-visual-line)
;;   (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

;;   (evil-set-initial-state 'messages-buffer-mode 'normal)
;;   (evil-set-initial-state 'dashboard-mode 'normal))

;; (use-package evil-collection
;;   :after evil
;;   :config
;;   (evil-collection-init))


(provide 'v-qol)
;;; v-qol.el ends here
