;; Org capture templates
(setq org-capture-templates
      '(("t"              ; hotkey
          "Todo list item" ; name
          entry            ; type
          (file+headline org-default-notes-file "comment Unfiled")
          "* TODO %?\n \n%U %i\n %a") ;template


        ("w" ; hotkey
          "(Work) Todo list item" ; name
          entry ; type
          (file+headline "~/org/work.org" "work")
          "* TODO %?\n \n%U %i\n") ;template

        ("j" "Journal Entry"
          entry (file+olp+datetree "~/org/journal.org" "Journal")
          (file "~/.emacs.d/org-templates/journal.orgcaptmpl"))

        ("a" "Attendance"
          entry (file+olp+datetree "~/org/journal.org" "Attendance")
          "***** %U\n%?")
        ("z" ; hotkey
          "capture madness" ; name
          entry ; type
          (file+headline org-default-notes-file "comment Unfiled")
          "* TODO %?\n %U\n %i\n %a %(progn (delete-other-windows) \"\")")

        ("m" "email" entry (file+olp org-default-notes-file "comment Unfiled" "mail")
          "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")))

;; org capture madness!!
(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (when (and (equal "capture" (frame-parameter nil 'name))
         (not (eq this-command 'org-capture-refile)))
    (delete-frame)))

(defadvice org-capture-refile
    (after delete-capture-frame activate)
  "Advise org-refile to close the frame"
  ;;(delete-frame)
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))


(defadvice org-capture
    (after make-full-window-frame activate)
  "Advise capture to be the only window when used as a popup"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-other-windows)))

(defun activate-capture-frame()
  "run org-capture in capture frame"
  (select-frame-by-name "capture")
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (org-capture))
