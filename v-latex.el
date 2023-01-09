

(use-package tex
  :ensure auctex)
(use-package pdf-tools
  :ensure t
  :config
  ;; initialise
  (pdf-tools-install)
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-page)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t)
  ;; use normal isearch
  (define-key pdf-view-mode-map (kbd "C-s") 'pdf-occur)
  ;; keyboard shortcuts
  (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
  (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)
  (define-key pdf-view-mode-map (kbd "c") 'pdf-view-center-in-window)
  ;; wait until map is available
  (with-eval-after-load "pdf-annot"
    (define-key pdf-annot-edit-contents-minor-mode-map (kbd "<return>") 'pdf-annot-edit-contents-commit)
    (define-key pdf-annot-edit-contents-minor-mode-map (kbd "<S-return>") 'newline))
  )
;; PDF

;; LATEX

(defun vs/latex-mode-visual-fill ()
  "Doc."
  (setq visual-fill-column-width 110
        fill-column 110
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1)
  )


(with-eval-after-load 'latex
  
  (add-hook 'LaTeX-mode-hook (lambda () (display-line-numbers-mode 0)))
  (add-hook 'LaTeX-mode-hook 'vs/latex-mode-visual-fill)

  (customize-set-variable 'TeX-auto-save t)
  (customize-set-variable 'TeX-parse-self t)
  (setq-default TeX-master nil)
  
  ;; compile to pdf
  (add-hook 'LaTeX-mode-hook 'tex-pdf-mode)
  
  ;; correlate the source and the output
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  
  
  ;; set a correct indentation in a few additional environments
  (add-to-list 'LaTeX-indent-environment-list '("lstlisting" current-indentation))
  (add-to-list 'LaTeX-indent-environment-list '("tikzcd" LaTeX-indent-tabular))
  (add-to-list 'LaTeX-indent-environment-list '("tikzpicture" current-indentation))
  
  ;; add a few macros and environment as verbatim
  (add-to-list 'LaTeX-verbatim-environments "lstlisting")
  (add-to-list 'LaTeX-verbatim-environments "Verbatim")
  (add-to-list 'LaTeX-verbatim-macros-with-braces "lstinline")
  (add-to-list 'LaTeX-verbatim-macros-with-delims "lstinline")
  
  ;; some pdf config
  ;; (pdf-tools-install)                   ; install it if not already installed
  (customize-set-variable 'TeX-view-program-selection '((output-pdf "PDF Tools")))
  (customize-set-variable 'TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
  (customize-set-variable 'TeX-source-correlate-start-server t)
  
  ;; electric pairs in auctex
  (customize-set-variable 'TeX-electric-sub-and-superscript t)
  (customize-set-variable 'LaTeX-electric-left-right-brace t)
  (customize-set-variable 'TeX-electric-math (cons "$" "$"))
  
  ;; open all buffers with the math mode and auto-fill mode
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
  
  ;; add support for references
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (customize-set-variable 'reftex-plug-into-AUCTeX t)
  
  ;; to have the buffer refresh after compilation
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  
 
  
  ;; sweet looking symbols
  (require 'magic-latex-buffer)
  (setq magic-latex-enable-inline-image nil) ; no image preview
  (add-hook 'LaTeX-mode-hook 'magic-latex-buffer)
)


(provide 'v-latex)
