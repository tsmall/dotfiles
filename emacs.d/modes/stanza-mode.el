;;; stanza-mode.el --- Major mode for L.B. Stanza code

;; Authors: Tom Small III <tsmall3@gmail.com>
;; URL: https://github.com/tsmall/dotfiles
;; Keywords: languages stanza lbstanza

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides font-lock and indentation for the L.B. Stanza
;; programming language (http://lbstanza.org).

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Changelog:

;; 1.0.1
;; * Fix first line indentation

;; 1.0.0
;; * First release

;;; Code :

(require 'dash)

(defconst stanza-mode-version "1.0.1"
  "The current version of `stanza-mode'.")


;; Hooks
(defvar stanza-mode-hook nil
  "Hook for customizing stanza-mode.")


;; Buffer-local variables

(defun stanza-mode-variable-setup ()
  "Set up initial buffer-local variables for Stanza mode."
  ;; Stanza forbids tabs.
  (setq-local indent-tabs-mode nil)

  ;; Comments in Stanza follow the same rules as Lisps.
  (setq-local comment-start ";")
  (setq-local comment-start-skip ";+ *")
  (setq-local comment-add 1)
  (setq-local comment-column 40)
  (setq-local comment-use-syntax t))


;; Keymap

(defvar stanza-mode-map
  (let ((map (make-sparse-keymap)))
    ;; TODO: Add default key bindings.
    map))


;; Syntax table
(defvar stanza-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\; "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?- "w" st)
    st))


;; Font-locking

(defconst stanza-function-keywords
  '("defn" "defmulti" "defmethod")
  "Stanza's function definition keywords.")

(defconst stanza-type-keywords
  '("deftype" "defstruct")
  "Stanza's type definition keywords.")

(defconst stanza-package-keywords
  '("defpackage" "import")
  "Stanza's package definition keywords.")

(defconst stanza-visibility-keywords
  '("public" "protected" "private")
  "Stanza's visibility control keywords.")

(defconst stanza-builtin-keywords
  '("val" "var"
    "if" "else"
    "for" "in" "while"
    "let" "where"
    "label"
    "new")
  "Stanza's built-in keywords.")

(defconst stanza-keywords
  (append stanza-function-keywords
          stanza-type-keywords
          stanza-package-keywords
          stanza-visibility-keywords
          stanza-builtin-keywords)
  "Stanza's built-in keywords.")

(defconst stanza-keyword-re
  (->> (regexp-opt stanza-keywords t)
       (format "\\<%s\\>"))
  "Regular expression for Stanza's keywords.")

(defconst stanza-font-lock-keywords
  `((,stanza-keyword-re . font-lock-keyword-face))
  "Keyword highlighting specification for `stanza-mode'.")

(defun stanza-mode-font-lock-setup ()
  "Set up font-lock for editing Stanza code."
  (setq-local font-lock-defaults '(stanza-font-lock-keywords)))


;; Indentation

(defun stanza-indent-line ()
  "Indent the current line of Stanza code."
  (interactive)
  (if (= (line-number-at-pos) 1)
      (indent-line-to 0)
    (let (current-indent)
      (progn
        (save-excursion
          (forward-line -1)
          (if (looking-at ".*[:=][[:space:]]*$")
              (setq current-indent (+ (current-indentation) default-tab-width))
            (setq current-indent (current-indentation))))
        (indent-line-to current-indent)))))

(defun stanza-mode-indentation-setup ()
  "Set up indentation for editing Stanza code."
  (setq-local indent-line-function 'stanza-indent-line))


;; Create stanza-mode

(define-derived-mode stanza-mode prog-mode "Stanza"
  "Major mode for editing L.B. Stanza code.

\\{stanza-mode-map}"
  :syntax-table stanza-mode-syntax-table
  (stanza-mode-variable-setup)
  (stanza-mode-font-lock-setup)
  (stanza-mode-indentation-setup)
  (run-hooks 'stanza-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.stanza\\'" . stanza-mode))

(provide 'stanza-mode)

;;; stanza-mode.el ends here
