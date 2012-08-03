;;; org-mode.el --- My custom org-mode settings

;;; Commentary:
;;
;; These are all of my org-mode settings.  Since this configuration started
;; getting big and seemed like it would be worth breaking up into multiple
;; sub-sections, I broke this out into a separate file.
;;
;; I don't define the agenda files files here, since that depends on the
;; specific computer Emacs is running on.

;;; Code:

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(setq org-M-RET-may-split-line nil)
(setq org-refile-targets '((nil . (:maxlevel . 2))))

;; -----------------------------------------------------------------------------
;; TODO
;; -----------------------------------------------------------------------------

(setq org-log-done 'time)
(setq org-enforce-todo-dependencies t)

(setq org-todo-keywords
      '((type "TODO" "WAIT" "PROJ" "|" "DONE")
        (sequence "REVIEW" "AWAITINGPATCH(!)" "REREVIEW(!)" "|" "DONE")))

;; (setq org-tag-alist
;;       '((:startgroup . nil)
;;         ("@office" . ?o)
;;         ("@home" . ?h)
;;         ("@errand" . ?e)
;;         (:endgroup . nil)
;;         ("phone" . ?p)))

;; -----------------------------------------------------------------------------
;; Capture
;; -----------------------------------------------------------------------------

(org-remember-insinuate)
(setq org-remember-templates
      `(("todo" ?t "* TODO %?\n  %i" nil "Tasks")
        ("project" ?p "* PROJ %?" nil "Projects")
        ("inbox" ?i "* %?\n  %i" nil "Inbox")
        ("someday" ?s "* %?\n  %i" nil "Someday/Maybe")
        ("wait" ?w "* WAIT (%U) %?\n  %i" nil "Tasks")))

;; -----------------------------------------------------------------------------
;; Agenda
;; -----------------------------------------------------------------------------

(setq org-agenda-dim-blocked-tasks 'invisible)
;; (setq org-agenda-skip-scheduled-if-done nil)
(setq org-stuck-projects '("+LEVEL=2/+PROJ" ("TODO") nil ""))

(setq org-agenda-custom-commands
      ;; Daily action list that shows my agenda for the day, including all tasks
      ;; that are are scheduled for today, plus all of the tasks that I am
      ;; waiting on.
      '(("d" "Daily Action List"
         ((agenda ""
                  ((org-agenda-span 1)
                   (org-agenda-sorting-strategy '(time-up todo-state-up priority-down tag-up))
                   (org-agenda-skip-scheduled-if-done t)
                   (org-deadline-warning-days 0)))
          (todo "WAIT")))))
