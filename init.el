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

(use-package evil-surround
  :ensure t
  :init
  (global-evil-surround-mode 1))

(use-package evil
  :ensure t
  :init     ;; tweak evil's configuration before loading it
  (evil-mode))


(use-package which-key
  :ensure t
  :init
  (which-key-mode t))


(use-package bind-key
  :ensure t
  :demand)


;; (use-package whole-line-or-region
;;   :init
;;   (whole-line-or-region-global-mode 1))


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


;;(unless (memq window-system '(mac ns))
(unless (string-equal system-type "darwin")
 (use-package parinfer-rust-mode
   :init
   (setq parinfer-rust-library "~/.emacs.d/parinfer-rust/parinfer-rust-darwin.so")
   (setq parinfer-rust-auto-download nil)
   :hook emacs-lisp-mode))


(use-package magit
   :ensure t
   :config)


(use-package org
  :ensure t)


(use-package rainbow-delimiters
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))


(use-package no-littering
  :ensure t
  :demand t
  :config)


;; modeline
(use-package all-the-icons
  :ensure t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-modal-icon nil))


;;; base
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode)
(setq inhibit-startup-screen t)        ; inhibit startup screen
(setq ring-bell-function 'ignore)      ; silent bell when you make a mistake
(setq default-fill-column 80)          ; toggle wrapping text at the 80th character


;;; editting
(setq-default show-trailing-whitespace t)

;;; Keyboard modifiers setup.

;; The below is designed for Mac users. You may need to tweak.

;; This makes your command key the 'super' key, which you can bind with "s-a",
;; keep in mind that shift is "S-a".
(setq mac-command-modifier 'super)

;; This makes the left option META and right one OPTION.
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'nil)


;;; Settings

;; Discovering the exact behavior of these settings is left as an exercise for
;; the reader. Documentation can be found with "C-h o <symbol>".
;; (delete-selection-mode t)
;; (global-visual-line-mode t)
;; (setq-default truncate-lines t)

;; (global-auto-revert-mode t)


;;; Keybindings

;; Make Esc do the right thing
;; (define-key key-translation-map (kbd "ESC") (kbd "C-g")) ;; does not work on -nw

;; These are to make Emacs more predicable in MacOS
(bind-keys
;;  ("s-n" . make-frame-command)
;;  ("s-m" . iconify-frame)
;;  ("s-s" . save-buffer)
;;  ("s-o" . find-file)
;;  ("s-w" . delete-frame)
;;  ("s-q" . save-buffers-kill-terminal)
;;  ("s-a" . mark-whole-buffer)
;;  ("s-z" . undo-only) ;; Why no redo? Read up on it.
;;  ("s-x" . kill-region)
 ("s-c" . kill-ring-save)
 ("s-v" . yank))
;;  ("s-<up>" . beginning-of-buffer)
;;  ("s-<down>" . end-of-buffer)
;;  ("s-<left>" . beginning-of-visual-line)
;;  ("s-<right>" . end-of-visual-line))



;; Buffer management / navigation
;; (bind-keys
;;  ("s-b" . switch-to-buffer)
;;  ("s-B" . ibuffer)
;;  ("s-[" . previous-buffer)
;;  ("s-]" . next-buffer)
;;  ("s-k" . kill-this-buffer))


;; ;; These are handy display toggles
;; (bind-keys :prefix-map my/global-leader
;;            :prefix "s-'"
;;            ("w" . visual-line-mode)
;;            ("h" . hl-line-mode)
;;            ("l" . display-line-numbers-mode)
;;            ("a" . auto-fill-mode))
;;
;;
;; ;; These let you manage windows (splits)
;; (bind-keys :prefix-map my/windows-leader
;;            :prefix "s-="
;;            ("s" . split-window-below)
;;            ("v" . split-window-right)
;;            ("k" . delete-window)
;;            ("o" . delete-other-windows)
;;            ("b" . balance-windows))


;;; End of Config --- Good luck out there!
