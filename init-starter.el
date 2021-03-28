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

;; organize
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;; Packages

(use-package which-key
  :ensure t
  :init
  (which-key-mode t))

;; (use-package selectrum
;;   :ensure t
;;   :init
;;   (selectrum-mode +1))
;; 
;; 
;; (use-package selectrum-prescient
;;   :ensure t
;;   :init
;;   (selectrum-prescient-mode +1)
;;   (prescient-persist-mode +1)
;;   (setq prescient-filter-method '(literal regexp fuzzy)))
;; 
;; 
;; (use-package marginalia
;;   :ensure t
;;   :init
;;   (marginalia-mode 1)
;;   (setq marginalia-annotators
;;         '(marginalia-annotators-heavy marginalia-annotators-light)))

