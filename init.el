;;; Package Management

;; A note on package installation and use:
;;
;; - `straight' is used to install/update packages.
;; - `use-package' is used to precisely control the loading of packages and
;;    configure them.

;; Install the `straight' package if it isn't installed
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/odjhey/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install and load `use-package'
(straight-use-package 'use-package)

;; Tell Straight to use `use-package' config declarations for installation of packages.
(setq straight-use-package-by-default t)

;; Don't load any packages unless explicitly told to do so in the config, see
;; `use-package' documentation for more info.
(setq use-package-always-defer t)


;;; Packages

(use-package general
  :ensure t)


(use-package hydra
  :ensure t)
;; we have pretty-hydra, but try to not use for now
(use-package pretty-hydra
  :ensure t)


(use-package evil
  :ensure t
  :init     ;; tweak evil's configuration before loading it
  (setq evil-default-state 'emacs)
  (evil-mode))
(define-key evil-emacs-state-map [escape] 'evil-normal-state)
;; read more here https://github.com/noctuid/evil-guide#use-some-emacs-keybindings


(use-package evil-surround
  :ensure t
  :init
  (progn
    (global-evil-surround-mode 1)))

(use-package evil-snipe
  :ensure t
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))


(use-package which-key
  :ensure t
  :init
  (which-key-mode t))


(use-package selectrum
  :ensure t
  :init
  (selectrum-mode +1))


(use-package selectrum-prescient
  :ensure t
  :init
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1)
  (setq prescient-filter-method '(literal regexp fuzzy)))


(use-package marginalia
  :ensure t
  :init
  (marginalia-mode 1)
  (setq marginalia-annotators
        '(marginalia-annotators-heavy marginalia-annotators-light)))


(use-package modus-themes
  :ensure t
  :init
  (modus-themes-load-themes)
  :config
  (modus-themes-load-vivendi))


(use-package parinfer-rust-mode
  :init
  (setq parinfer-rust-library "~/.emacs.d/parinfer-rust/parinfer-rust-darwin.so")
  (setq parinfer-rust-auto-download nil)
  :hook emacs-lisp-mode)


(use-package magit
  :init
  (defun magit-status-with-prefix-arg ()
    "Call `magit-status` with a prefix."
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'magit-status)))
  :ensure t
  :config
  (setq magit-repository-directories '(("\~/proj/" . 4) ("\~/proj/work/" . 4))))


(use-package org
  :ensure t
  :config
  (setq org-agenda-files (list "~/org/work.org"
                               "~/org/index.org"))
  (org-archive-location) "org/archive/%s_archive::")


(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(org-babel-do-load-languages 'org-babel-load-languages
                             (append org-babel-load-languages '(
                                                                (shell . t))))

(use-package rainbow-delimiters
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))


(use-package no-littering
  :ensure t
  :demand t
  :config)


;; I want Emacs kill ring and system clipboard to be independent. Simpleclip is the solution to that.
(use-package simpleclip
  :init
  (simpleclip-mode 1))


;; Avy for fast navigation.
(use-package avy
  :defer t
  :config)


(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))


;; Expand-region allows to gradually expand selection inside words, sentences, etc
(use-package expand-region)


(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))


(use-package git-gutter)


(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (js2-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)


;; modeline
(use-package all-the-icons
  :ensure t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-modal-icon nil)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-buffer-state-icon nil)
  (setq doom-modeline-buffer-modification-icon t)
  (setq evil-normal-state-tag   (propertize "[Normal]")
        evil-emacs-state-tag    (propertize "[Emacs]")
        evil-insert-state-tag   (propertize "[Insert]")
        evil-motion-state-tag   (propertize "[Motion]")
        evil-visual-state-tag   (propertize "[Visual]")
        evil-operator-state-tag (propertize "[Operator]")))

(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/org/brain/")
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))


(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/org/"))


(use-package org-journal
  :ensure t
  :defer t
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j ")
  :config
  (setq org-journal-dir "~/org/journal/"
        org-journal-date-format "%A, %d %B %Y"))


(use-package undo-tree
  :ensure t
  :init
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  :config
  (global-undo-tree-mode 1))


(use-package origami
  :ensure t
  :config
  (origami-mode 1))


;; Save point position in files
(use-package saveplace
  :ensure t
  :init (progn
          (setq save-place-file "~/.emacs.d/saveplace")
          (save-place-mode 1)))

(use-package volatile-highlights
  :ensure t
  :init (progn
          (vhl/define-extension
             'evil 'evil-paste-after 'evil-paste-before
             'evil-paste-pop 'evil-move)
          (volatile-highlights-mode 1)
          (vhl/install-extension 'evil)))

;;; for evaluation
;; org-super-agenda


;;; Packages-End


;;; base
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode)
(setq initial-scratch-message ";; *SCRATCH*")


;; Show columns in addition to rows in mode line
(setq column-number-mode t)

;; Never use tabs, use spaces instead.
(setq-default
 indent-tabs-mode nil
 c-basic-indent 2
 c-basic-offset 2
 tab-width 2)

(setq
 inhibit-startup-screen t        ; inhibit startup screen
 inhibit-startup-message t
 cursor-in-non-selected-windows t  ; Hide the cursor in inactive windows
 ring-bell-function 'ignore      ; silent bell when you make a mistake
 default-fill-column 80          ; toggle wrapping text at the 80th character
 initial-scratch-message nil       ; Empty scratch buffer
 sentence-end-double-space nil     ; Sentences should end in one space, come on!
 confirm-kill-emacs 'y-or-n-p      ; y and n instead of yes and no when quitting
 confirm-kill-emacs nil)

(fset 'yes-or-no-p 'y-or-n-p)      ; y and n instead of yes and no everywhere else


;;; editting
(setq show-trailing-whitespace t)


;;; Keyboard modifiers setup.

;; The below is designed for Mac users. You may need to tweak.

;; This makes your command key the 'super' key, which you can bind with "s-a",
;; keep in mind that shift is "S-a".
(setq mac-command-modifier 'super)

;; This makes the left option META and right one OPTION.
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'nil)


(set-face-attribute 'whitespace-space nil :background nil :foreground "gray20")
(set-face-attribute 'whitespace-newline nil :background nil :foreground "gray30")
(setq whitespace-display-mappings
  ;; all numbers are Unicode codepoint in decimal. ⁖ (insert-char 182 1)
  '((space-mark 32 [183] [46]) ; 32 SPACE 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
    ;;(newline-mark 10 [182 10]) ; 10 LINE FEED
    (newline-mark ?\n  [?¬ ?\n]  [?$ ?\n])  ; eol - negation
    (tab-mark 9 [9655 9] [92 9]))) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
(whitespace-mode 1)

;;; Keybindings



;; ;; These are handy display toggles
;; (bind-keys :prefix-map my/global-leader
;;            :prefix "s-'"
;;            ("w" . visual-line-mode)
;;            ("h" . hl-line-mode)
;;            ("l" . display-line-numbers-mode)
;;            ("a" . auto-fill-mode))
;;
;;

;; (bind-keys :prefix-map my/windows-leader
;;            :prefix "s-="
;;            ("s" . split-window-below)
;;            ("v" . split-window-right)
;;            ("k" . delete-window)
;;            ("o" . delete-other-windows)
;;            ("b" . balance-windows))

;; neat! clean this up in the future
(defhydra hydra-projectile
  (:color teal :hint nil)
  "
     PROJECTILE: %(projectile-project-root)

  ^Find File^        ^Search/Tags^        ^Buffers^       ^Cache^                    ^Project^
  ^---------^        ^-----------^        ^-------^       ^-----^                    ^-------^
  _f_: file          _a_: ag              _i_: Ibuffer    _c_: cache clear           _p_: switch proj
  _F_: file dwim     _g_: update gtags    _b_: switch to  _x_: remove known project
  _C-f_: file pwd    _o_: multi-occur   _s-k_: Kill all   _X_: cleanup non-existing
  _r_: recent file   ^ ^                  ^ ^             _z_: cache current
  _d_: dir
"
  ("a"   projectile-ag)
  ("b"   projectile-switch-to-buffer)
  ("c"   projectile-invalidate-cache)
  ("d"   projectile-find-dir)
  ("f"   projectile-find-file)
  ("F"   projectile-find-file-dwim)
  ("C-f" projectile-find-file-in-directory)
  ("g"   ggtags-update-tags)
  ("s-g" ggtags-update-tags)
  ("i"   projectile-ibuffer)
  ("K"   projectile-kill-buffers)
  ("s-k" projectile-kill-buffers)
  ("m"   projectile-multi-occur)
  ("o"   projectile-multi-occur)
  ("p"   projectile-switch-project)
  ("r"   projectile-recentf)
  ("x"   projectile-remove-known-project)
  ("X"   projectile-cleanup-known-projects)
  ("z"   projectile-cache-current-file)
  ("q"   nil "cancel" :color blue))


(defhydra hydra-global-org (:color blue
                                   :hint nil)
  "
Timer^^        ^Clock^         ^Capture^
--------------------------------------------------
s_t_art        _i_ clock in    _c_apture
 _s_top        _o_ clock out   _g_ last capture
_r_eset        _j_ clock goto
_p_rint
"
  ("t" org-timer-start)
  ("s" org-timer-stop)
  ;; Need to be at timer
  ("r" org-timer-set-timer)
  ;; Print timer value to buffer
  ("p" org-timer)
  ;;("i" (org-clock-in '(4)))
  ("i" org-clock-in)
  ("o" org-clock-out)
  ;; Visit the clocked task from any buffer
  ("j" org-clock-goto)
  ("c" org-capture)
  ("g" org-capture-goto-last-stored))


(defun with-faicon (icon str &optional height v-adjust)
  (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(pretty-hydra-define hydra-clock
  (:hint nil :color teal :quit-key "q" :title (with-faicon "clock-o" "Clock" 1 -0.05))
  ("Action"
   (("c" org-clock-cancel "cancel")
    ("d" org-clock-display "display")
    ("e" org-clock-modify-effort-estimate "effort")
    ("i" org-clock-in "in")
    ("j" org-clock-goto "jump")
    ("o" org-clock-out "out")
    ("p" org-pomodoro "pomodoro")
    ("r" org-clock-report "report"))))


(pretty-hydra-define hydra-toggles
  (:color amaranth :quit-key "q" :title (with-faicon "toggle-on" "Toogs"))
  ("Basic"
   (("n" display-line-numbers-mode "line number" :toggle t)
    ("w" whitespace-mode "whitespace" :toggle t)
    ("W" visual-line-mode "wrap" :toggle t)
    ("r" rainbow-delimiters-mode "rainbow parens" :toggle t)
    ("g" git-gutter-mode "git gutter" :toggle t)
    ("|" display-fill-column-indicator-mode "column margin" :toggle t))
   "Highlight"
   (("l" hl-line-mode "line" :toggle t))))


(pretty-hydra-define hydra-control-tower
  (:color red :quit-key "q" :title "Control-Tower")
  ("Zoom"
   (("=" text-scale-increase "zoom-in")
    ("-" text-scale-decrease "zoom-out"))
   "Editor"
   (("r" read-only-mode "read-only c-x c-q"))))

;; Buffer management / navigation
;; (bind-keys
;;  ("s-b" . switch-to-buffer)
;;  ("s-B" . ibuffer)
;;  ("s-k" . kill-this-buffer))
(pretty-hydra-define hydra-buffer
  (:color blue :quit-key "q" :title "buffers")
  ("Buffer"
    (("b" switch-to-buffer "switch-to-buffer"))))

;; add hydra for org-refile/ing

(general-define-key :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 ;; hydras first
 "p" '(hydra-projectile/body :which-key "projectile")
 "i" '(hydra-global-org/body :which-key "timers")
 "/" '(hydra-toggles/body :which-key "toggles")
 "t" '(projectile-find-file :which-key "projectile-find-file")
 "f" '(avy-goto-char-timer :which-key "goto char")
 "g" '(magit-status :which-key "magit")
 "c" '(hydra-control-tower/body :which-key "control tower")
 "b" '(hydra-buffer/body :which-key "buffers"))

(winner-mode +1)

(global-display-fill-column-indicator-mode 1)
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)
(global-visual-line-mode t)
(global-auto-revert-mode t)
(setq-default truncate-lines t)

(setq display-line-numbers-widen t)
;; (setq display-line-numbers-type 'relative)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))


(setq org-default-notes-file "~/org/my.org")


;; Org capture
(load-file
 (concat
  (file-name-directory user-emacs-directory)
  "orgcapture.el"))


(setq frame-resize-pixelwise t)


(put 'narrow-to-region 'disabled nil)
