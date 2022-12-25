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

  ;; (exwm-enable)
  (exwm-input-set-key (kbd "s-r") 'exwm-reset)
  (exwm-input-set-key (kbd "s-w") 'exwm-workspace-switch)
  (exwm-input-set-key (kbd "s-&") #'(lambda (command)
                                      (interactive (list (read-shell-command "$ ")))
                                      (start-process-shell-command command nil command)))
  )

(provide 'v-exwm)
