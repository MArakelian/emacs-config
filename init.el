;;Basic Customizations
(electric-pair-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(global-visual-line-mode -1)
(global-display-line-numbers-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(setq mac-command-modifier 'meta)
(setq select-enable-primary nil)
(setq select-enable-clipboard t)

;; Uncommenting makes line numbers relative
;;(setq display-line-numbers-type 'relative)

;; Speed Up On MACOS
;; Disables double buffering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;;------DEFAULT ORG CONFIG--------------------------->
(setq org-hide-emphasis-markers t)
(setq org-hide-leading-stars t)
(setq org-startup-indented t)
(setq org-startup-folded t)
(setq org-pretty-entities t)


;; FONTS
;;(set-frame-font "" nil t)
(set-face-attribute 'fixed-pitch nil :family "Monaspace Neon Var" :height 1.0) ; or whatever font family

;;Set up Use-Package --------------------------> 
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; THEMES
(use-package nano-theme
  :ensure t)

(use-package autothemer
  :ensure t)

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

;; Markdown-mode
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
;; Themes for exported latex code blocks
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

;; Python
;; Python formatting with python-black
(use-package python-black
  :ensure t
  :demand t
  :after python)

;; Rust
;; Rust mode
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

;; Additional Org Config -------------------------------> 

(defun org-stuff()
  "Change org buffers."
  (org-bullets-mode 1)
  (visual-line-mode 1)
  (show-paren-mode 1)
  (display-line-numbers-mode -1))

(defun org-agenda-changes()
  "Change Org Agenda"
  (display-line-numbers-mode -1)
  (setq org-agenda-include-diary t)
  (setq org-agenda-hide-tags-regexp "."))

(setq org-agenda-custom-commands
      '(("p" "List Every Task"
         ((agenda "")
          (tags-todo "inbox|philosophy")))
        ))

(add-hook 'org-agenda-cleanup-fancy-diary-hook
          (lambda ()
            (goto-char (point-min))
            (save-excursion
              (while (re-search-forward "^[a-z]" nil t)
                (goto-char (match-beginning 0))
                (insert "0:00-24:00 ")))
            (while (re-search-forward "^ [a-z]" nil t)
              (goto-char (match-beginning 0))
              (save-excursion
                (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
              (insert (match-string 0)))))

(setq org-directory "~/Desktop/PhilosophyNotes/Notes")
(setq org-agenda-files '("~/Desktop/PhilosophyNotes/Notes/Agenda.org"
			 "~Desktop/PhilosophyNotes/Notes/philosophy.org"))


(setq org-capture-templates
      `(("i" "Inbox" entry  (file "Agenda.org")
        ,(concat "* TODO %?\n"
                 "/Entered on/ %U"))
        ("p" "Philosophy" entry (file "philosophy.org")
        ,(concat "* TODO %?\n"
                 "/Entered on/ %U"))
       ("a" "PhD Applications" entry (file+headline "philosophy.org" "PhD Applications")
        ,(concat "* TODO %? %^G\n"
                 "/Entered on/ %U"))))

;; (setq org-capture-templates
;;       '(("i" "Inbox" entry (file "Agenda.org")
;;          "* TODO %?\n")
;;         ("p" "Philosophy" entry (file "philosophy.org")
;;         "* TODO %?\n")))


;;org latex fragments size up
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

(defun programming-changes()
  (display-line-numbers-mode)
  (hl-line-mode))

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
(define-key global-map (kbd "C-c c") 'org-capture)
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b30351bb744aa2cba9281cdbffe4f05d0a5153442e3b2f866e9d3efcad364238"
     "242b268ffb078e4617d787cb43bffddb5ad3ca568c29188feb130f2081fe1ff2"
     "3c192a9f0bb81ab107504518f7c88ce54ea6ca7af8d5fd031b05956f3403ed29"
     "5cde6fd287788d02948fe6222ceea41abe85f12b4014d17a102f0754f1466f40"
     "413cb743f6fcb29c1a498b7d46ec3b7ea4a1f4430780e0f63d8d48cfc4a77b57"
     "7986c9f254a8fc675fa18b27b70fa0b0f30011c02c47ba9e4eb1e614087a8784"
     "02e54e1d188bea1c9b2f6a14db3c4b37215a614c2f41c7c34859b44cc0844a1e"
     "15563b7bc433d7799d6fddf98f8468f178805468b4ed48f680b3a15dcf3c5e61"
     default))
 '(org-agenda-files
   '("~/Desktop/PhilosophyNotes/Notes/philosophy.org"
     "/Users/markarakelian/Desktop/PhilosophyNotes/Notes/Agenda.org"))
 '(org-agenda-loop-over-headlines-in-active-region nil)
 '(package-selected-packages
   '(auctex autothemer citar company deft doom-modeline doom-themes
	    ef-themes eglot engrave-faces git-gutter lab-themes magit
	    marginalia nano-theme neotree orderless org-bullets
	    python-black rustic vertico)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Ocaml
(add-to-list 'load-path "/Users/markarakelian/.opam/default/share/emacs/site-lisp")
(require 'ocp-indent)

;; Theme
(load-theme 'ocean-charge)

;;Uncomment for transparent Emacs Gui
;;(dolist (frame (frame-list))
;;  (set-frame-parameter frame 'alpha 90))
