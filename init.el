;;Basic Customizations

(setq-default
 inhibit-startup-screen t               ; Disable start-up screen
 inhibit-startup-message t              ; Disable startup message
 inhibit-startup-echo-area-message t    ; Disable initial echo message
 initial-scratch-message ""             ; Empty the initial *scratch* buffer
 initial-buffer-choice t)               ; Open *scratch* buffer at init


(set-default-coding-systems 'utf-8)     ; Default to utf-8 encoding
(prefer-coding-system       'utf-8)     ; Add utf-8 at the front for automatic detection.
(set-terminal-coding-system 'utf-8)     ; Set coding system of terminal output
(set-keyboard-coding-system 'utf-8)     ; Set coding system for keyboard input on TERMINAL
(set-language-environment "English")    ; Set up multilingual environment

(setq-default show-help-function nil    ; No help text
              use-file-dialog nil       ; No file dialog
              use-dialog-box nil        ; No dialog box
              pop-up-windows nil)       ; No popup windows


(setq-default cursor-in-non-selected-windows nil ; Hide the cursor in inactive windows
              ;;cursor-type '(hbar . 2)            ; Underline-shaped cursor
              cursor-intangible-mode t           ; Enforce cursor intangibility
              x-stretch-cursor nil)              ; Don't stretch cursor to the glyph width

(blink-cursor-mode 1)                            ; Blinking cursor

(tooltip-mode -1)                       ; No tooltips
(global-font-lock-mode 1)               ; Font lock mode
(electric-pair-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(global-visual-line-mode -1)
(global-display-line-numbers-mode -1)
(scroll-bar-mode -1)


(setq-default visible-bell nil             ; No visual bell      
              ring-bell-function 'ignore)  ; No bell

(setq mac-command-modifier 'meta) ; Set modifier for MACOS
(setq select-enable-primary nil)
(setq select-enable-clipboard t)

(setq-default scroll-conservatively 101       ; Avoid recentering when scrolling far
              scroll-margin 2                 ; Add a margin when scrolling vertically
              recenter-positions '(5 bottom)) ; Set re-centering positions


(setq-default fill-column 80                          ; Default line width 
              sentence-end-double-space nil           ; Use a single space after dots
              bidi-paragraph-direction 'left-to-right ; Faster
              truncate-string-ellipsis "…")           ; Nicer ellipsis

;;------DEFAULT ORG CONFIG--------------------------->
(setq org-hide-emphasis-markers t)
(setq org-hide-leading-stars t)
(setq org-startup-indented t)
(setq org-startup-folded t)
(setq org-pretty-entities t)

;; FONTS
(set-frame-font "Berkeley Mono" nil t)

;;Set up Use-Package --------------------------> 
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; THEMES
(use-package autothemer
  :ensure t)

(use-package base16-theme
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
(setq org-latex-engraved-theme 'base16-nord-light)  ;; Optional


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
  (org-cite-global-bibliography '("~/Documents/bibliography.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  ;; optional: org-cite-insert is also bound to C-c C-x C-@
  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert)))

(setq citar-library-paths '("~/Documents/Philosophy"))


(use-package citeproc
  :ensure t)

;; Set up Company Mode -------------------------------> 
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0))

(add-hook 'after-init-hook 'global-company-mode)


;; Set up I Menu List Mode
(use-package imenu-list
  :ensure t
  :config
   (setq imenu-list-focus-after-activation t)
   (global-set-key (kbd "C-.") #'imenu-list-minor-mode))

;; Better Org Mode Bullets ------------------------>
(use-package org-bullets
  :ensure t)

;; Notes System -----------------------------------> 
(use-package deft
  :ensure t
  :custom
  (setq deft-extensions '("txt" "md" "org" "tex"))
  (setq deft-recursive t)
  (setq deft-text-mode 'org-mode))
(setq deft-use-filename-as-title t)
(setq deft-directory "~/Documents/Philosophy")

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

;; LSP pyright
(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "pyright") ;; or basedpyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

;; Set up Python, C and LISP Literate Notes
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (lisp . t)
   (C . t)))
(setq org-babel-python-command "python3")

;; Treemacs File Viewer
(use-package treemacs
  :ensure t
  :config
  (setq treemacs-position 'right))
(treemacs-resize-icons 20)

(treemacs-modify-theme "Default"
  :icon-directory "/other/icons/dir"
  :config
  (progn
    (treemacs-create-icon :icon "* " :extensions (fallback))))

(set-face-attribute 'treemacs-root-face nil
                    :height 1.0
                    :weight 'Regular)

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
;; Including Org Agenda
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

(setq org-directory "~/Documents/")
(setq org-agenda-files '("~/Documents/Agenda.org"
			"~/Documents/Philosophy/philosophy.org"))


(setq org-capture-templates
      `(("i" "Inbox" entry  (file "~/Documents/Agenda.org")
        ,(concat "* TODO %?\n"
                 "/Entered on/ %U"))
        ("p" "Philosophy" entry (file "~/Documents/Philosophy/philosophy.org")
        ,(concat "* TODO %?\n"
                 "/Entered on/ %U"))
       ("h" "PhD Applications" entry (file+headline "~/Documents/Philosophy/philosophy.org" "PhD Applications")
        ,(concat "* TODO %? %^G\n"
                 "/Entered on/ %U"))))

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

;; Google style guide requires offset 2
(setq-default cpp-basic-offset 2)

;; Custom Keybindings ;;
(keymap-global-set "C-c e" 'org-latex-export-to-pdf)
(keymap-global-set "C-c a" 'org-agenda)
(keymap-global-set "<f12>" 'treemacs)
(keymap-global-set "<f8>" 'flymake-show-project-diagnostics)
(keymap-global-set "C-c c" 'org-capture)

;; Eldoc
(setq eldoc-idle-delay 0.2)

;; Projectile for projects
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  ;;(setq projectile-completion-system 'ivy)
)

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
;;Uncomment for transparent Emacs Gui
;;(dolist (frame (frame-list))
;;  (set-frame-parameter frame 'alpha 90))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("15471462b38fe558caf976ae73e8ff528cb7245dcc9a529652859ec6cad98a97"
     "0672d7fbe7fa24344bbe98461a8db26cb52a074354f9a4529cea4ba1af456044" default))
 '(package-selected-packages
   '(auctex autothemer base16-theme citar citeproc company deft ef-themes eglot
	    engrave-faces flycheck git-gutter iceberg-theme imenu-list
	    kanagawa-themes lsp-pyright magit marginalia nano-theme orderless
	    org-bullets projectile rustic treemacs vertico)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Theme
(load-theme 'base16-vice)
;;(set-face-attribute 'font-lock-comment-face nil :foreground "#ECEFF4")
