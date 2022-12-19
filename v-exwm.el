;;; Package -- Summary
;;; Commentary:
;;; Code:

(defun vs/exwm-update-class ()
  "Update the window class."
  (exwm-workspace-rename-buffer exwm-class-name))

(use-package exwm
  :config
  (setq exwm-workspace-number 5)
  
  (start-process-shell-command "xmodmap" nil "xmodmap ~/.emacs.d/exwm/Xmodmap")
  (start-process-shell-command "xkbcomp" nil "xkbcomp ~/.emacs.d/exwm/Xkbcomp")

  (exwm-enable)
  )

(provide 'v-exwm)
