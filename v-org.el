;;; Package -- Summary
;;; Commentary:
;;; Code:
;; org mode setup

(defun vs/org-mode-setup ()
  "Doc."
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (setq org-hide-emphasis-markers t)    ; hide the structural markup element like **
  
  )

(defun vs/org-mode-visual-fill ()
  "Doc."
  (setq visual-fill-column-width 150
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1)
  )

(defun vs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

 ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute 'variable-pitch nil :font "Cantarell" :weight 'regular :height 150)

    ;; Ensure that anything that should be fixed-pitch in Org files appears that way
    (set-face-attribute 'org-block nil :font "JetBrains Mono Medium")
    (set-face-attribute 'org-code nil   :font "JetBrains Mono Medium")
    (set-face-attribute 'org-table nil   :font "JetBrains Mono Medium")
    (set-face-attribute 'org-verbatim nil :font "JetBrains Mono Medium")
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))
  )


(use-package org
  :hook (org-mode . vs/org-mode-setup)
  :config
  (setq org-ellipsis " »")
  (setq org-todo-keywords
        '((sequence "TODO" "REVIEW" "COMPLETE" "DONE")))
  (vs/org-font-setup)  
  )

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  )

(use-package visual-fill-column
  :hook (org-mode . vs/org-mode-visual-fill)
  )

(add-hook 'org-mode-hook 'vs/org-mode-visual-fill)
(provide 'v-org)
