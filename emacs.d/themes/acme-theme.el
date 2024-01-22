;;; acme-theme.el --- Acme Editor Theme

;; Copyright (C) 2018 Tom Small III

;; Author: Tom Small III <tsmall3@gmail.com>
;; URL: http://github.com/tsmall/dotfiles
;; Package-Version: 20180117
;; Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This theme has two sources of inspiration:
;; the Acme editor itself (http://acme.cat-v.org/)
;; and the acme-colors Vim theme (https://github.com/plan9-for-vimspace/acme-colors).

;;; Code:

(deftheme acme
  "Theme inspired by the Acme editor.")

(let* (;; Colors
       (black "#000000")
       (black-lighter "#303030")
       (gray-light "#BBBBBB")
       (gray "#999999")
       (gray-dark "#777777")
       (yellow-light "#fcfcce")
       (yellow "#ffffea")
       (yellow-darker "#eceba1")
       (yellow-darkest "#999851")
       (blue-light "#ebffff")
       (blue "#aeeeee")
       (blue-dark "#9a9ed4")
       (blue-darker "#031196")

       ;; Syntax
       (text-color black)
       (cursor-color black)
       (selection-color yellow-darker)
       (background-color yellow)
       (comment-color black-lighter)
       (string-color yellow-darkest)
       (active-status-bar-color blue)
       (inactive-status-bar-color blue-light)
       (highlight-color yellow-light))

  (custom-theme-set-faces
   'acme

   ;; generic stuff
   `(default ((t (:background ,background-color :foreground ,text-color))))
   `(default-italic ((t (:italic t))))
   `(button ((t (:foreground ,text-color :underline t))))
   `(cursor ((t (:background ,text-color :foreground ,background-color))))
   `(region ((t (:background ,selection-color))))
   `(custom-variable-tag ((t nil)))
   `(font-lock-builtin-face ((t nil)))
   `(font-lock-comment-delimiter-face ((t (:foreground ,text-color))))
   `(font-lock-comment-face ((t (:foreground ,comment-color :slant italic))))
   `(font-lock-doc-face ((t (:foreground ,comment-color :slant italic))))
   `(font-lock-constant-face ((t nil)))
   `(font-lock-function-name-face ((t nil)))
   `(font-lock-keyword-face ((t nil)))
   `(font-lock-preprocessor-face ((t nil)))
   `(font-lock-reference-face ((t nil)))
   `(font-lock-string-face ((t (:foreground ,string-color))))
   `(font-lock-type-face ((t nil)))
   `(font-lock-variable-name-face ((t nil)))
   `(font-lock-warning-face ((t (:weight bold))))
   `(fringe ((t (:background ,background-color :foreground ,text-color))))
   `(ido-first-match ((t nil)))
   `(ido-only-match ((t nil)))
   `(ido-subdir ((t nil)))
   `(isearch ((t (:background ,highlight-color))))
   `(highlight ((t (:background ,selection-color))))
   `(lazy-highlight ((t (:background ,selection-color))))
   `(link ((t (:slant italic))))

   ;; mode line and minibuffer
   `(minibuffer-prompt ((t (:weight bold))))
   `(mode-line ((t (:background ,active-status-bar-color :foreground ,text-color :height 0.8))))
   `(mode-line-buffer ((t (:weight bold))))
   `(mode-line-inactive ((t (:background ,inactive-status-bar-color :foreground ,text-color :height 0.8))))
   `(mode-line-minor-mode ((t (:weight ultra-light))))
   `(modeline ((t (:height 0.8))))

   ;; org-mode
   `(org-agenda-date ((t (:height 1.2))))
   `(org-agenda-date-today ((t (:weight bold :height 1.4))))
   `(org-agenda-date-weekend ((t (:weight normal))))
   `(org-agenda-done ((t (:foreground ,gray-light))))
   `(org-agenda-structure ((t (:weight bold))))
   `(org-block ((t nil)))
   `(org-block-begin-line ((t (:foreground ,gray-light))))
   `(org-block-end-line ((t (:foreground ,gray-light))))
   `(org-date ((t (:foreground ,text-color :underline t))))
   `(org-done ((t (:foreground ,gray))))
   `(org-headline-done ((t (:foreground ,gray :strike-through t))))
   `(org-upcoming-deadline ((t (:foreground ,gray-dark))))
   `(org-hide ((t (:foreground ,background-color))))
   `(org-level-1 ((t (:weight bold))))
   `(org-level-2 ((t (:weight bold))))
   `(org-level-3 ((t (:weight bold))))
   `(org-level-4 ((t (:weight bold))))
   `(org-level-5 ((t (:weight bold))))
   `(org-level-6 ((t (:weight bold))))
   `(org-link ((t (:underline t))))
   `(org-quote ((t (:slant italic :inherit org-block))))
   `(org-scheduled ((t nil)))
   `(org-sexp-date ((t nil)))
   `(org-special-keyword ((t nil)))
   `(org-todo ((t nil)))
   `(org-verse ((t (:inherit org-block :slant italic))))
   `(org-column ((t (:inherit org-block :foreground ,text-color))))

   ;; magit
   `(magit-header ((t (:weight bold))))
   `(magit-item-mark ((t (:background ,highlight-color))))
   `(magit-item-highlight ((t (:weight bold))))
   `(magit-section-heading ((t (:weight bold))))
   `(magit-section-highlight ((t (:background ,highlight-color))))
   `(magit-diff-context-highlight ((t (:foreground ,comment-color))))
   `(magit-branch-local ((t (:weight bold))))
   `(magit-branch-remote ((t (:weight bold))))

   ;; compile
   `(compilation-error ((t (:inherit error))))

   ;; flycheck
   `(flycheck-error ((t (:inherit error))))
   `(flycheck-warning ((t (:inherit warning))))

   ;; dired
   `(dired-directory ((t (:weight bold))))

   ;; iedit
   `(iedit-occurrence ((t nil)))

   ;; parentheses
   `(parenthesis ((t (:foreground ,comment-color))))
   `(show-paren-match ((t (:weight bold))))
   `(show-paren-mismatch ((t (:background "red" :foreground ,text-color :weight bold))))

   ;; js2 and rjsx
   `(js2-function-param ((t nil)))
   `(js2-external-variable ((t nil)))
   `(rjsx-attr ((t (:slant italic))))

   ;; markdown
   `(markdown-bold-face ((t (:inherit default :weight bold))))
   `(markdown-italic-face ((t (:inherit default :slant italic))))
   `(markdown-header-face-1 ((t (:weight bold))))
   `(markdown-header-face-2 ((t (:weight bold))))
   `(markdown-header-face-3 ((t (:weight bold))))
   `(markdown-link-face ((t nil)))

   ;; adoc-mode
   `(markup-meta-face ((t (:inherit font-lock-comment-face))))
   `(markup-meta-hide-face ((t nil)))
   `(markup-title-0-face ((t (:weight bold))))
   `(markup-title-1-face ((t (:weight bold))))
   `(markup-title-2-face ((t (:weight bold))))
   `(markup-title-3-face ((t (:weight bold))))
   `(markup-title-4-face ((t (:weight bold))))
   `(markup-title-5-face ((t (:weight bold))))
   `(markup-list-face ((t nil)))
   `(markup-typewriter-face ((t nil)))
   `(markup-verbatim-face ((t nil)))
   `(markup-emphasis-face ((t (:slant italic))))
   `(markup-strong-face ((t (:weight bold))))

   ;; hl-line-mode
   `(hl-line ((t (:background ,yellow-light))))))

;;;###autoload
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'acme)
;;; acme-theme.el ends here
