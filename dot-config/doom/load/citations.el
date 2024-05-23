;;; ../../sources/dotfiles/dot-config/doom/load/citations.el -*- lexical-binding: t; -*-

(setq dr/bibliography-dir "~/org/biblio/"
      dr/bibliography-file (concat dr/bibliography-dir "references.bib")
      )

(use-package! citar
  ;; :init
  ;; Here we define a face to dim non 'active' icons, but preserve alignment.
  ;; Change to your own theme's background(s)
  ;; (defface dr/citar-icon-dim
  ;;   ;; Change these colors to match your theme. Using something like
  ;;   ;; `face-attribute' to get the value of a particular attribute of a face might
  ;;   ;; be more convenient.
  ;;   '((((background dark)) :foreground "#212428")
  ;;     (((background light)) :foreground "#f0f0f0"))
  ;;   "Face for having icons' color be identical to the theme
  ;; background when \"not shown\".")

  :custom
  (citar-bibliography org-cite-global-bibliography)
  ;; (citar-bibliography '("~/org/biblio/references.bib"))
  (citar-library-paths '((concat dr/bibliography-dir "library")))
  (citar-notes-paths '((concat dr/bibliography-dir "notes")))
  (org-cite-global-bibliography '(dr/bibliography-file))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (org-cite-export-processors
   '((md . (csl "chicago-fullnote-bibliography.csl"))   ; Footnote reliant
     (latex biblatex)                                   ; For humanities
     (odt . (csl "chicago-fullnote-bibliography.csl"))  ; Footnote reliant
     (t . (csl "modern-language-association.csl"))))      ; Fallback
  (citar-notes-paths (list org-roam-directory)) ; List of directories for reference nodes
  (citar-open-note-function 'orb-citar-edit-note) ; Open notes in `org-roam'
  (citar-at-point-function 'embark-act)           ; Use `embark'
  (citar-templates
   '((main . "${author editor:30}   ${date year issued:4}    ${title:110}")
     (suffix . "     ${=type=:20}    ${tags keywords keywords:*}")
     (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
     (note . "#+title: Notes on ${author editor}, ${title}") ; For new notes
     ))
  ;; Configuring all-the-icons. From
  ;; https://github.com/bdarcus/citar#rich-ui
  ;; (citar-symbols
  ;;  `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) .
  ;;     ,(all-the-icons-faicon "file-o" :face 'dr/citar-icon-dim :v-adjust -0.1) )
  ;;    (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) .
  ;;          ,(all-the-icons-material "speaker_notes" :face 'dr/citar-icon-dim :v-adjust -0.3))
  ;;    (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) .
  ;;          ,(all-the-icons-octicon "link" :face 'dr/citar-icon-dim :v-adjust 0.01))))
  (citar-symbol-separator "  ")
  (org-cite-csl-styles-dir
   (expand-file-name "~/Zotero/styles/"))

  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)

  :custom-face
  (org-cite ((t (:foreground "DarkSeaGreen4"))))
  (org-cite-key ((t (:foreground "forest green" :slant italic))))

  :general
  (:keymaps 'org-mode-map
   :prefix "C-c b"
   "b" '(citar-insert-citation :wk "Insert citation")
   "r" '(citar-insert-reference :wk "Insert reference")
   "o" '(citar-open-notes :wk "Open note"))

  ;; :bind
  ;; (("C-c w c c" . citar-open)
  ;;  (:map org-mode-map
  ;;   :package org
  ;;   ("C-c w C". #'org-cite-insert)))
  )
