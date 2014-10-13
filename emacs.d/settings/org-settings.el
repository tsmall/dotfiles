;;; org-settings.el --- My custom org-mode settings

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
(setq org-startup-indented t)
(setq org-cycle-separator-lines 0)
(setq org-insert-heading-respect-content nil)
(setq org-blank-before-new-entry '((heading . nil) (plain-list-item . auto)))
(setq org-catch-invisible-edits 'error)

;; -----------------------------------------------------------------------------
;; TODO
;; -----------------------------------------------------------------------------

(setq org-log-done 'time)
(setq org-enforce-todo-dependencies t)
(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(W@/!)" "HOLD(h@/!)"
                        "|"
                        "CANCELLED(c@/!)" "MEETING"))))

;; -----------------------------------------------------------------------------
;; Capture
;; -----------------------------------------------------------------------------

(setq org-agenda-span 'day)
(setq org-default-notes-file "refile.org")

(setq org-capture-templates
      '(("t" "todo" entry (file "")
         "* TODO %?\n%U\n")
        ("m" "meeting" entry (file "")
         "* MEETING %? :MEETING:\n%U"
         :clock-in t :clock-resume t)
        ("n" "note" entry (file "")
         "* %? :NOTE:\n%U")
        ("r" "respond" entry (file "")
         "* TODO Respond to %^{who} on %^{subject}\nSCHEDULED: %t\n%U")))

;; -----------------------------------------------------------------------------
;; Refile
;; -----------------------------------------------------------------------------

(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))

;; -----------------------------------------------------------------------------
;; Agenda
;; -----------------------------------------------------------------------------

(setq org-agenda-dim-blocked-tasks nil)
(setq org-agenda-compact-blocks t)
(setq org-agenda-files '("~/org"))
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-tags-todo-honor-ignore-options t)

(setq org-agenda-custom-commands
      '(("N" "Notes" tags "NOTE"
         ((org-agenda-overriding-header "Notes")
          (org-tags-match-list-sublevels nil)))
        (" " "Agenda"
         ((agenda "" nil)
          (tags "REFILE"
                ((org-agenda-overriding-header "Tasks to Refile")
                 (org-tags-match-list-sublevels nil)))
          (tags-todo "-REFILE-SOMEDAY/!NEXT"
                     ((org-agenda-overriding-header "Project Next Tasks")
                      (org-agenda-todo-ignore-scheduled 'all)))
          (tags-todo "-REFILE-SOMEDAY/!TODO"
                     ((org-agenda-overriding-header "Standalone Tasks")
                      (org-agenda-skip-function 'ts/skip-projects)
                      (org-agenda-todo-ignore-scheduled 'all)))
          (tags-todo "-REFILE/!WAITING|HOLD"
                     ((org-agenda-overriding-header "Waiting and Postponed Tasks")
                      (org-tags-match-list-sublevels nil)
                      (org-agenda-sorting-strategy '(todo-state-up))))
          (tags-todo "-CANCELLED"
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-agenda-skip-function 'ts/skip-non-stuck-projects)
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags-todo "-HOLD-CANCELLED"
                     ((org-agenda-overriding-header "Projects")
                      (org-agenda-skip-function 'ts/skip-non-projects)
                      (org-tags-match-sublevels 'indented)
                      (org-agenda-sorting-strategy '(category-keep))))))))

;; -----------------------------------------------------------------------------
;; Helper Functions
;; -----------------------------------------------------------------------------

(defun ts/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (ts/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next)
                          (< (point) subtree-end)
                          (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil))
        next-headline))))

(defun ts/skip-projects ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (if (ts/is-project-p)
          subtree-end
        nil))))

(defun ts/skip-project-entries ()
  "Skip entries that are the root of a project"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (ts/is-project-p)
          next-headline
        nil))))

(defun ts/skip-non-projects ()
  "Skip trees that are not projects"
  (if (save-excursion (ts/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((ts/is-project-p)
            nil)
           ((and (ts/is-project-subtree-p) (not (ts/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun ts/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun ts/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun ts/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (ts/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun ts/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok)
                                       (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(provide 'org-settings)
;;; org-settings.el ends here
