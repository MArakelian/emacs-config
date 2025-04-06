(require 'autothemer)

(autothemer-deftheme
 ocean-charge "Feel the Resch!"

 ((((class color) (min-colors #xFFFFFF))) ;; We're only concerned with graphical Emacs

  ;; Define our color palette
  (oc-black          "#000000")
  (oc-white	     "#ffffff")
  (oc-gray           "gray99")
  (oc-SlateGray	     "SlateGray4")
  (oc-cyan	     "cyan1")
  (oc-aquamarine4    "aquamarine4")
  (oc-aquamarine3    "aquamarine3")
  (oc-DarkSeaGreen2  "Darkseagreen2")
  (oc-SeaGreen	     "SeaGreen3")
  (oc-honeydew       "honeydew2")
  (oc-seashell	     "seashell3")
  (oc-orange	     "orange1")
  (oc-skye4          "DeepSkyBlue4")
  (oc-skye3          "DeepSkyblue3")
  (oc-skye2          "Deepskyblue2")
  )

 ;; Customize faces
 ((default		     (:foreground oc-black :background oc-gray))
  (border                    (:background oc-gray))
  (fringe                    (:background oc-gray))
  (internal-border           (:background oc-gray))
  (cursor		     (:background oc-skye3))
  (region		     (:background oc-honeydew))
  (mode-line		     (:background oc-honeydew))
  (font-lock-keyword-face    (:foreground oc-aquamarine4))
  (font-lock-constant-face   (:foreground oc-SeaGreen))
  (font-lock-string-face     (:foreground oc-orange))
  (font-lock-builtin-face    (:foreground oc-skye3))
  (font-lock-comment-face    (:foreground oc-skye4))
  (vertico-current           (:background oc-honeydew))
  (hl-line                   (:background oc-honeydew))

  ;; Programming Modes
  (font-lock-function-name-face   (:foreground oc-aquamarine3))
  (font-lock-variable-name-face   (:foreground oc-aquamarine3))
  
  ;; Org Agenda
  (org-agenda-date                  (:foreground oc-skye4))
  (org-agenda-structure             (:foreground oc-aquamarine4))
  (org-agenda-structure-filter      (:foreground oc-orange))
  (org-agenda-clocking              (:background oc-orange))
  ;; Org Mode
  (org-todo                  (:foreground oc-orange))
  (org-level-1               (:foreground oc-skye4))
  (org-level-2               (:foreground oc-skye3))
  (org-level-3               (:foreground oc-skye2))))

(setq global-hl-line-mode -1)
(provide-theme 'ocean-charge)
