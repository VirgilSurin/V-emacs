;;; Package -- Summary
;;; Commentary:
;;; Code:
;; This file will regroup all my little quality of life packages like Ivy, Avy and so


;; Get sudo right !
(use-package sudo-edit)

;; Ivy
(use-package smex)

;; (use-package ivy
;;   :diminish
;;   :bind (
;;          ("C-s" . swiper)
;;          :map ivy-minibuffer-map
;;          ("TAB" . ivy-alt-done))
;;   :config
;;   (setq swiper-use-visual-line nil)
;;   (setq swiper-use-visual-line-p (lambda (a) nil))
;;   (setq ivy-height 10)
;;   (setq ivy-count-format "")
;;   (setq ivy-initial-inputs-alist nil)
;;   (setq ivy-use-virtual-buffers t)
;;   (setq enable-recursive-minibuffers t)
;;   (ivy-mode 1)
;;   )

;; (use-package ivy-rich
;;   :init
;;   (ivy-rich-mode 1)
;;   )

(use-package counsel
;;   :bind (
;;          ("M-x"     . 'counsel-M-x)
;;          ("C-x C-f" . 'counsel-find-file)
;;          ("C-c r"   . 'counsel-recentf)
;;          :map minibuffer-local-map
;;          ("C-r" . 'counsel-minibuffer-history)
;;          )
)

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  (setq vertico-count 10)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :bind (
         ("C-s" . consult-line)
         ("C-M-l" . consult-imenu)
         ))

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
  (setq which-key-idle-delay 0.3))

;; browse kill-ring
(use-package browse-kill-ring)
(browse-kill-ring-default-keybindings)


(winner-mode 1)
(define-key evil-normal-state-map (kbd "C-w u") 'winner-undo)


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


(use-package vterm
  :ensure t
  :init
  :custom (vterm-kill-buffer-on-exit t)
  )
(define-key vterm-mode-map (kbd "C-s") nil)


;; Hydra
(use-package hydra
  :ensure t)

(defhydra hydra-zoom ()
  "zoom"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("r" (text-scale-set 0) "reset")
  ("f" nil "finished" :exit t)
  )


(vs/leader-key
  "z" '(hydra-zoom/body :which-key "zoom"))


;; jump between windows 
(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-minibuffer-flag t
        )
  )
(global-set-key (kbd "M-o") 'ace-window)


(provide 'v-qol)

;;; v-qol.el ends here
