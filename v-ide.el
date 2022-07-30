;;; Package -- Summary
;;; Commentary:
;;; Code:
;; visual guide-lines
(use-package highlight-indent-guides
    :config
    (setq highlight-indent-guides-method 'character))
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

;; Allow for a larger memory usage to read subprocess
(setq gc-cons-threshold 100000000) ;; 100 MB
(setq read-process-output-max (* 1 1024 1024)) ;; 1 MB


;; Parenthesis

;; Color for parenthesis indentation
(use-package rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Show matching brackets and braces
(show-paren-mode 1)

;; ()
(electric-pair-mode 1)


;; Refactoring
(add-to-list 'load-path "~/.emacs.d/elpa/iedit")
(if (eq system-type 'windows-nt)
    ;; For some reason (on windows) I can't install it with use-package
    ;; So I installed it from github directly
    (require 'iedit)
  ;; else it should be ok
  (use-package iedit
    :bind (("C-;" . iedit-mode)))
  )


;; syntax checking
(use-package flycheck)
(add-hook 'prog-mode-hook 'flycheck-mode)

;; code completion
(use-package company)
(add-hook 'prog-mode-hook 'global-company-mode)


;; code snippets
(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (setq yas-indent-line 'fixed)
  :hook (prog-mode . yas-minor-mode)
  )


(use-package yasnippet-snippets
  :after yasnippet)

(use-package java-snippets
  :after yasnippet)

;; better syntex highlight
(use-package tree-sitter)
(use-package tree-sitter-langs)

(require 'tree-sitter)
(require 'tree-sitter-langs)
(global-tree-sitter-mode)
(add-hook 'c-mode-hook #'tree-sitter-hl-mode)
(add-hook 'lsp-mode-hook #'tree-sitter-hl-mode)




;; GIT
(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


;; LSP
(use-package lsp-mode
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ;;(XXX-mode . lsp)
         ;; (java-mode . lsp)
         (java-mode . #'lsp-deferred)
         (python-mode . #'lsp-deferred)
         (c-mode . #'lsp-deferred)
         (ess-r-mode . #'lsp-deferred)
         (c-mode . #'lsp-deferred)
         (c++-mode . #'lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :init (setq
         lsp-keymap-prefix "C-c l"
         lsp-enable-file-watchers nil
         read-process-output-max(* 1024 1024)
         lsp-completion-provider :capf
         lsp-idle-delay 0.500
         lsp-keep-workspace-alive nil
         ;; lsp-log-io nil
         lsp-print-performance t)
  :config 
  (setq lsp-intelephense-multi-root nil) ; don't scan unnecessary projects
  (with-eval-after-load 'lsp-intelephense
    (setf (lsp--client-multi-root (gethash 'iph lsp-clients)) nil))
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(global-set-key (kbd "C-c i") 'lsp-ivy-workspace-symbol)

(global-set-key (kbd "C-c C-v") 'lsp-execute-code-action)

;; LSP UI
(use-package lsp-ui
  :after lsp-mode
  :config
  (lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-ui-sideline-diagnostic-max-line-length 75)
  (setq lsp-ui-sideline-diagnostics-max-lines 5)

  (lsp-ui-peek-enable t)
  (lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-cursor t)

  (lsp-enable-imenu)
  (setq lsp-ui-imenu-window-width 25)
  (define-key lsp-mode-map (kbd "C-c l u") 'lsp-ui-imenu)
  )

;; PYTHON
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred

;; JAVA
(use-package lsp-java
  :after lsp-mode)

;; C
(setq-default c-basic-offset 4)

;; add elisp-mode to prog mode hook
(add-to-list 'auto-mode-alist '("\\.el$" . emacs-lisp-mode))
;; (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)


;; SCHEME
(add-to-list 'auto-mode-alist '("\\.scm$" . scheme-mode))
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme" t)
(setq scheme-program-name "guile")

;; RACKET
;; Provides all the racket support
(use-package racket-mode
  :ensure t)

;; PROLOG
(add-to-list 'auto-mode-alist '("\\.pl$" . prolog-mode))

;; R
;; TODO : not sure that it works well
(use-package ess)
  (add-to-list 'auto-mode-alist '("\\.R$" . ess-r-mode))

;; The code below aims to automatically setup a window layout for R editing
(add-to-list 'display-buffer-alist
      `(("^\\*R Dired"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . right)
         (slot . -1)
         (window-width . 0.33)
         (reusable-frames . nil))
        ("^\\*R"
         (display-buffer-reuse-window display-buffer-at-bottom)
         (window-width . 0.25)
         (reusable-frames . nil))
        ("^\\*Help"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . right)
         (slot . 1)
         (window-width . 0.33)
         (reusable-frames . nil))))

;; DEBUG : TODO

;; LATEX
(use-package tex
  :ensure auctex)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(use-package pdf-tools)

;; Project
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  ;;(when (file-directory-p PROJECTS_DIR)
  ;;  (setq projectile-project-search-path '(PROJECTS_DIR)))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; Copilot
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)
;; you can utilize :map :hook and :config to customize copilot

(add-hook 'prog-mode-hook 'copilot-mode)

                                        ; complete by copilot first, then company-mode
(defun my-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (copilot-complete)))

                                        ; modify company-mode behaviors
(with-eval-after-load 'company
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends)

  (define-key company-mode-map (kbd "C-<tab>") 'my-tab)
  (define-key company-mode-map (kbd "C-TAB") 'my-tab)
  (define-key company-active-map (kbd "C-<tab>") 'my-tab)
  (define-key company-active-map (kbd "C-TAB") 'my-tab))

(provide 'v-ide)

