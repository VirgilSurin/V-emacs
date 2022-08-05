;;; Package -- Summary
;;; Commentary:
;;; Code:

;; here you will find most of my vanilla bindings

;; kill buffer
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; moving between windows

;; ;; we need to unbind them first because of org mode...
;; (define-key 'org-mode-map (kbd "M-<up>") nil)
;; (define-key 'org-mode-map (kbd "M-<down>") nil)
;; (define-key 'org-mode-map (kbd "M-<left>") nil)
;; (define-key 'org-mode-map (kbd "M-<right>") nil)

;; (define-key ('org-mode-map) [remap 'org-metaup] 'windmove-up)
;; (define-key ('org-mode-map) [remap 'org-metaleft] 'windmove-left)
;; (define-key ('org-mode-map) [remap 'org-metaright] 'windmove-right)
;; (define-key ('org-mode-map) [remap 'org-metadown] 'windmove-down)

(define-key (current-global-map) [remap left-word] 'windmove-left)
(define-key (current-global-map) [remap right-word] 'windmove-right)
(global-set-key (kbd "<M-up>")    'windmove-up)
(global-set-key (kbd "<M-down>")  'windmove-down)

;; comment line
(global-set-key (kbd "C-M-;") 'comment-line)


;; kbd from my custom functions
(require 'v-vanilla)

(global-set-key (kbd "C-x C-i") 'open-init-file)
(global-set-key (kbd "C-c t") 'open-close-shell)

(define-key (current-global-map) [remap forward-sexp] 'my_forward_sexp)

(provide 'v-bindings)