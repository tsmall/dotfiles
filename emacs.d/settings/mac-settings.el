;;; mac-settings.el --- Mac OS X Configuration

;;; Commentary:

;; Settings specific to Mac OS X. This file can safely be loaded on all
;; operating systems, however, since it checks the system OS and only
;; applies the Mac-specific settings if it is Mac OS X.

;;; Code:

(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'none)

  (exec-path-from-shell-initialize))

(provide 'mac-settings)

;;; mac-settings.el ends here
