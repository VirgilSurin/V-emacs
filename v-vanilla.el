;;; Package -- Summary
;;; Commentary:
;;; Code:
;; here you will find some vanilla customisation of emacs

(require 'thingatpt) ;Used by some functions

;; Autoindent open-*-lines
(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause them to autoindent.")

;; auto-indent when a newline is created
(defun my-open-next-line (arg)
  "Open a new line below and correctly indent it"
  (interactive "p")
  (newline-and-indent)
  )

;; move with sexp
(defun my_forward_sexp (arg)
  "forward to the next sexp but if reaches the end of one, it will go up to the next sexp"
  (interactive "p")
  (condition-case nil
      (forward-sexp)
    (error (up-list)))
  )

;; change emacs' transparency
(defun change-transparency (n)
  "change transparency to a given value between 0 and 100"
  (interactive "nValue: ")
  (set-frame-parameter nil 'alpha `(,n . ,n))
  (add-to-list 'default-frame-alist `(alpha . (,n . ,n))))
(change-transparency 100)

(defun open-init-file ()
  "Open the init.el file."
  (interactive)
  (find-file EMACS_INIT)
  )

;; shell
(add-to-list 'display-buffer-alist
      '(
        ("*eshell*"
         (display-buffer-reuse-window display-buffer-at-bottom)
         (window-height . 0.25)
         (reusable-frames . nil)
         )
        )
      )

(defun open-close-shell ()
  "Open a shell buffer if none visible, kill it otherwise."
  (interactive)
  (vterm)
  ;; (let ((shell-buffer "*shell*"))
  ;;   (cond ((string= shell-buffer (buffer-name (window-buffer))) ; visible and focused
  ;;          (delete-window))
  ;;         (t                            ; invisible or unfocused
  ;;          (shell)))
  ;;   )
  )

(defun my-kill-buffer ()
  "Kill the current buffer and the associated window."
  (interactive)
  (kill-buffer-and-window)
  )

(provide 'v-vanilla)
;;; v-vanilla.el ends here
