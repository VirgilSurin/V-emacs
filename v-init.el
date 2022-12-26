;;; Package -- Summary
;;; Commentary:
;;; Code:
;; Create a variable to indicate where emacs's configuration is installed
(setq EMACS_DIR "~/.emacs.d/")
(setq EMACS_INIT "~/.emacs.d/init.el")
(setq PROJECTS_DIR "~/projects")

;; PACKAGE SOURCES
(defvar bootstrap-version)
(let ((bootstrap-file
     (expand-file-name
      "straight/repos/straight.el/bootstrap.el"
      user-emacs-directory))
    (bootstrap-version 5))
(unless (file-exists-p bootstrap-file)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
       'silent 'inhibit-cookies)
    (goto-char (point-max))
    (eval-print-last-sexp)))
(load bootstrap-file nil 'nomessage))

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(require 'v-default)
(require 'v-visual)
(require 'v-qol)
(require 'v-org)
(require 'v-ide)
(require 'v-bindings)
(require 'v-exwm)
(require 'v-latex)


(provide 'v-init)
