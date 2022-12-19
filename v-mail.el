;;; Package -- Summary
;;; Commentary:
;;; Code:

;TODO: do
(use-package mu4e)

;; Set up some common mu4e variables
(setq mu4e-maildir "~/Maildir")
(setq mu4e-sent-folder "/sent")
(setq mu4e-drafts-folder "/drafts")
(setq mu4e-trash-folder "/trash")

;; Set up the default mu4e compose command
(setq mu4e-compose-signature-auto-include t)
(setq mu4e-compose-format-flowed t)
(setq mu4e-compose-signature
      (concat
       "Cordialement,\n"
       "Virgil Surin\n"))

;; Set up some handy shortcuts
(setq mu4e-maildir-shortcuts
      '(("/inbox" . ?i)
        ("/sent" . ?s)
        ("/trash" . ?t)))

;; Set up the the folder-wise poll interval
(setq mu4e-update-interval 300)

;; Bookmarks
(setq mu4e-bookmarks
      `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
        ("date:today..now" "Today's messages" ?t)
        ("date:7d..now" "Last 7 days" ?w)))

;; Start the mu4e mail client
(mu4e)




(provide 'v-mail)
;;; v-mail.el ends here
