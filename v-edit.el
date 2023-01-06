;;; Package --- Summary
;;; Commentary:
;;; Code:

;; AVY
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
(use-package crux)

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
  (setq evil-want-C-i-jump t)
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
  :custom (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init)
  (evil-collection-vterm-setup))

;; comment line
(global-set-key (kbd "C-M-;") 'comment-line)


;; kill buffer
(global-set-key (kbd "C-q") 'kill-current-buffer)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-x C-k") 'kill-buffer-and-window)


(define-key (current-global-map) [remap forward-sexp] 'my_forward_sexp)

;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYBINDING SECTION ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer vs/leader-key 
    :states '(normal visual insert emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (vs/leader-key
    "t" '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "tn" '(vs/nordify :which-key "nordify")
    "f" '(:ignore t :which-key "files")
    "ff" '(counsel-find-file :which-key "find file")
    "fr" '(counsel-recentf :which-key "recent files")
    "fh" '((lambda () (interactive) (counsel-find-file "~/.")) :which-key "home")
    "fc" '((lambda () (interactive) (counsel-find-file "~/OneDrive/")) :which-key "onedrive")
    "p" '(:ignore t :which-key "projects")
    "pf" '(projectile-find-file :which-key "find file in project")
    "pp" '(projectile-switch-project :which-key "switch project")
    "ps" '(projectile-save-project-buffers :which-key "save project buffers")
    "pg" '(counsel-projectile-grep :which-key "grep project")
    "pb" '(projectile-switch-to-buffer :which-key "switch to buffer")
    "pk" '(projectile-kill-buffers :which-key "kill project buffers")
    "b" '(:ignore t :which-key "buffers")
    "bb" '(ivy-switch-buffer :which-key "switch buffer")
    "bk" '(kill-current-buffer :which-key "kill buffer")
    "bd" '(kill-buffer-and-window :which-key "kill buffer and window")
    "g" '(magit-status :which-key "magit")
    )
  )

(provide 'v-edit)
;;; v-edit.el ends here
