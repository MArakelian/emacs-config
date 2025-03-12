;;Basic Customizations
(electric-pair-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(global-visual-line-mode -1)
(global-display-line-numbers-mode -1)
;; disable scroll bar mode
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(setq mac-command-modifier 'meta)
(setq select-enable-primary nil)
(setq select-enable-clipboard t)
;; Uncommenting makes line numbers relative
;;(setq display-line-numbers-type 'relative)

;;-------ORG CONFIG--------------------------->
(setq org-hide-emphasis-markers t)
(setq org-hide-leading-stars t)
(setq org-startup-indented t)
(setq org-startup-folded t)
(setq org-pretty-entities t)

;; FONTS
(set-frame-font "Monaspace Neon Var" nil t)

;;Set up Use-Package --------------------------> 
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;;ef-themes
(use-package ef-themes
  :ensure t)

;;Flycheck over Flymake
(use-package flycheck
  :ensure t)

;; Enable Vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode)

  ;;Different scroll margin
  (setq vertico-scroll-margin 0)

  ;;Show more candidates
  (setq vertico-count 15)

  ;;Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)

  ;;Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
 )
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure t
  :init
  (savehist-mode))

;; Marginalia provides context sensitive notes in the minibuffer
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

;;Provides for hyphenless and orderless search
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion--category-override '(file (styles basic partial-completion))))

;;AUCTEX
(use-package tex
  :ensure auctex)

(setq org-latex-create-formula-image-program 'dvisvgm)


;; Markdown
(use-package markdown-mode
  :ensure t)

;; Git with Magit!
 (use-package magit
   :ensure t
   :config
   (setq magit-push-always-verify nil)
   (setq git-commit-summary-max-length 50))


;;Git Symbols with Git Gutter		    
(use-package git-gutter			       
  :ensure t					       
  :hook ((prog-mode org-mode) . git-gutter-mode )   
  :config
  (setq git-gutter-interval .5)
  (setq git-gutter:update-interval 2)	
  (setq git-gutter:modified-sign "|")	       
  (setq git-gutter:added-sign "|")		      
  (setq git-gutter:deleted-sign "|")		      
  (set-face-foreground 'git-gutter:added "Green")   
  (set-face-foreground 'git-gutter:modified "Gold") 
  (set-face-foreground 'git-gutter:deleted "Red"))


;; Engraved Faces
(use-package engrave-faces
  :ensure t)

(setq org-latex-src-block-backend 'engraved)
(setq org-latex-engraved-theme 'doom-one-light)  ;; Optional

;;---CITATIONS------------------------------------->

;;Citations for Org Mode
(use-package citar
  :init
  (setq citar-templates
	'((main . "${author :20%sn}   ${date year :4}   ${title:30}")
          (suffix . " ${=key= id:15}    ${=type=:12}")
          (preview . "${author editor:%etal} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
          (note . "Notes on ${author editor:%etal}, ${title}")))
  :no-require
  :custom
  (org-cite-global-bibliography '("~/Desktop/PhilosophyNotes/MyLibrary.json"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  ;; optional: org-cite-insert is also bound to C-c C-x C-@
  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert)))

(use-package citeproc
  :ensure t)

;; Set up Company Mode -------------------------------> 
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0))

(add-hook 'after-init-hook 'global-company-mode)

;; Better Org Mode Bullets ------------------------>
(use-package org-bullets
  :ensure t)

(use-package deft
  :ensure t
  :custom
  (setq deft-extensions '("txt" "md" "org" "tex"))
  (setq deft-recursive t)
  (setq deft-text-mode 'org-mode))
(setq deft-use-filename-as-title t)
(setq deft-directory "~/Desktop/PhilosophyNotes/Notes/")

;; Python formatting with python-black
(use-package python-black
  :ensure t
  :demand t
  :after python)

;; rust mode
(use-package rust-mode
  :ensure t)

(use-package rustic
  :ensure t
  :config(setq rustic-format-on-save nil)
  :custom
  (rustic-cargo-use-last-stored-arguments t)
  :after (rust-mode))

(setq rustic-lsp-client 'eglot)

;; neotree
(use-package neotree
  :ensure t)
(global-set-key [f8] 'neotree-toggle)
(setq neo-window-position :right)

;; ;; Set up Python, C and LISP Literate Notes
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (lisp . t)
   (C . t)))
(setq org-babel-python-command "python3")

;; Treesitter -------------------------------------->
(add-to-list
 'treesit-language-source-alist
     '(rust "https://github.com/tree-sitter/tree-sitter-rust"))

;;(global-tree-sitter-mode t)

;; Hooks, Baby---------------------------------------->
(defun deftyness()
  (display-line-numbers-mode -1))

(defun python-stuff ()
  (show-paren-mode 1)
  (python-black-on-save-mode))

(defun markdown-stuff()
  (display-line-numbers-mode -1)
  (visual-line-mode 1)
  (show-paren-mode 1))

(defun org-stuff()
  "Change org buffers."
  (org-bullets-mode 1)
  (visual-line-mode 1)
  (show-paren-mode 1)
  (display-line-numbers-mode -1))

(defun org-agenda-changes()
  "Change Org Agenda"
  (display-line-numbers-mode -1))

(setq org-agenda-custom-commands
      '(("p" "List Every Task"
         ((agenda "")
          (tags-todo "philosophy")
	  (tags-todo "math")
	  (tags-todo "programming")))))

(setq org-agenda-files '("~/Desktop/PhilosophyNotes/Notes/Agenda.org"))

(defun programming-changes()
  (display-line-numbers-mode)
  ;;(global-tree-sitter-mode)
  (global-hl-line-mode))

;; Enable Line Numbers in Programming modes
(add-hook 'prog-mode-hook 'programming-changes)

;; Markdown mode Hooks
(add-hook 'markdown-mode-hook 'markdown-stuff)

;;Deft mode Hooks
(add-hook 'deft-mode-hook 'deftyness)

;;Org mode hookp
(add-hook 'org-mode-hook 'org-stuff)
(add-hook 'org-agenda-mode-hook 'org-agenda-changes)

;;Python hook
(add-hook 'python-mode-hook 'python-stuff)

;; Rust mode hooks
(add-hook 'rust-mode-hook
	  '(lambda () (setq indent-tabs-mode nil))
	  '(eglot-ensure))
(setq rust-format-on-save t)

;; c and Python offset

(setq-default python-basic-offset 4)
(setq-default c-basic-offset 4)

;; Custom Keybindings ;;
(keymap-global-set "C-c e" 'org-latex-export-to-pdf)
(keymap-global-set "C-c a" 'org-agenda)

;; Eldoc
(setq eldoc-idle-delay 0.2)

;; eglot
(use-package eglot
  :custom
  (eglot-ignored-server-capabilities
   '(:documentHighlightProvider
     :documentRangeFormattingProvider
     :documentOnTypeFormattingProvider
     :coloProvider
     :foldingRangeProvider))
  :bind (:map eglot-mode-map
	      ("C-c C-f" . eglot-format-buffer)))

;; Backup files
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 1
      kept-old-versions 1
      version-control t)


;;org latex fragments size up
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("9013233028d9798f901e5e8efb31841c24c12444d3b6e92580080505d56fd392" default))
 '(org-agenda-files '("~/Desktop/PhilosophyNotes/Notes/Agenda.org"))
 '(org-agenda-loop-over-headlines-in-active-region nil)
 '(package-selected-packages
   '(eglot all-the-icons-nerd-fonts ayu-theme ef-themes yasnippet vertico projectile org-bullets orderless nord-theme markdown-mode marginalia iceberg-theme highlight-indent-guides flycheck engrave-faces doom-themes doom-modeline deft company citar catppuccin-theme auctex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(doom-modeline-mode 1)

;; Ocaml
(add-to-list 'load-path "/Users/markarakelian/.opam/default/share/emacs/site-lisp")
(require 'ocp-indent)

;; Theme
;; For Timu-Rouge theme: (customize-set-variable 'timu-rouge-mode-line-border t)
(load-theme 'ef-frost t)

;;Uncomment for transparent Emacs Gui
;;(dolist (frame (frame-list))
;;  (set-frame-parameter frame 'alpha 90))
