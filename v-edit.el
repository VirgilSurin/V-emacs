;;; Package --- Summary
;;; Commentary:
;;; Code:

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
    )

(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-g k") 'avy-goto-char-timer)

;; CRUX - a kot of useful little things
(use-package crux
  :bind (
         ("C-o"      . crux-smart-open-line-above)
         ("C-j"      . crux-smart-open-line)
         ("C-k"    . crux-smart-kill-line)
         ("M-k"    . crux-kill-whole-line)
         ("C-<backspace>" . crux-kill-line-backwards)
         ("C-c d"  . crux-duplicate-current-line-or-region)
         )
  )

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

;;;;;;;;;;;;;;;;;;;
;; MODAL EDITING ;;
;;;;;;;;;;;;;;;;;;;
(use-package evil
  :init
  (setq evil-want-integration t)
 (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))


;; god-mode for modal, easier navigation
(use-package god-mode)
;; (god-mode-all)
;; (global-set-key (kbd "<escape>") #'god-local-mode)
;; (define-key god-local-mode-map (kbd "i") #'god-local-mode)
;; (defun my-god-mode-update-cursor-type ()
  ;; (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

;; (add-to-list 'god-exempt-major-modes 'dired-mode)
;; (add-to-list 'god-exempt-major-modes 'vterm-mode)
;; (add-hook 'post-command-hook #'my-god-mode-update-cursor-type)


(provide 'v-edit)
;;; v-edit.el ends here
