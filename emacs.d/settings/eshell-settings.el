;;; eshell-settings.el --- Eshell customizations

;;; Commentary:
;; 
;; Most of this code was taken from an elisp script that Jim Menard posted to
;; Github [1]. Thanks go out to him for sharing. I have, of course, made some
;; modifications. Because what is Emacs if not customized to each user's
;; preferences?
;;
;; [1]: https://github.com/jimm/elisp/blob/master/eshell-customize.el

;;; Code:

(defun pwd-replace-home (pwd)
  "Replace home in PWD with tilde (~) character."
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
         (home-len (length home)))
    (if (and
         (>= (length pwd) home-len)
         (equal home (substring pwd 0 home-len)))
        (concat "~" (substring pwd home-len))
      pwd)))

(defun pwd-shorten-dirs (pwd)
  "Shorten all directory names in PWD except the last three."
  (let ((p-lst (split-string pwd "/")))
    (if (> (length p-lst) 3)
        (concat
         (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                                    (substring elm 0 1)))
                    (butlast p-lst 3)
                    "/")
         "/"
         (mapconcat 'identity
                    (last p-lst 3)
                    "/"))
      (mapconcat 'identity
                 p-lst
                 "/"))))

(defun cur-dir-git-branch-string (pwd)
  "Return current git branch as a string.

If PWD is in a git repo and the git command is found, then return the
current git branch as a string.  Otherwise return an empty string."
  (interactive)
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let* ((git-cmd "git branch")
           (grep-cmd "grep '\\*'")
           (sed-cmd "sed -e 's/^\\* //'")
           (git-output (shell-command-to-string (concat git-cmd " | " grep-cmd " | " sed-cmd))))
      (concat "["
              (if (> (length git-output) 0)
                  (substring git-output 0 -1)
                "(no branch)")
              "] "))))

(defun trs-eshell-prompt-fn ()
  "A customized eshell prompt function.

The main features of this prompt are that it:

- shortens directory names if the path gets too long
- replaces the HOME path with the tilde (~) character
- shows the current git branch if you're in a git repo"
  (concat
   (cur-dir-git-branch-string (eshell/pwd))
   (pwd-shorten-dirs (pwd-replace-home (eshell/pwd)))
   "$ "))

(setq eshell-prompt-regexp "^[^#$]*[#$] ")
(setq eshell-prompt-function 'trs-eshell-prompt-fn)

(provide 'eshell-settings)

;;; eshell-settings.el ends here
