;;; yast-theme.el --- Yet Another Solarized Theme

;; Copyright (C) 2017 Tom Small III

;; Author: Tom Small III <tsmall3@gmail.com>
;; URL: http://github.com/tsmall/dotfiles
;; Package-Version: 20171213
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

;; My take on Ethan Schoonover's "Solarized" light theme.

;;; Credits:

;; The color scheme is taken directly from Ethan Schoonover's site:
;; http://ethanschoonover.com/solarized

;;; TODO:

;; - Customize magit's diff coloring.
;; - Reorganize the sections below.

;;; Code:

(deftheme yast-light
  "Yet Another Solarized Theme (Light)")

(let ((base00  "#657b83")
      (base01  "#586e75")
      (base02  "#073642")
      (base03  "#002b36")
      (base0   "#839496")
      (base1   "#93a1a1")
      (base2   "#eee8d5")
      (base3   "#fdf6e3")
      (yellow  "#b58900")
      (orange  "#cb4b16")
      (red     "#dc322f")
      (magenta "#d33682")
      (violet  "#6c71c4")
      (blue    "#268bd2")
      (cyan    "#2aa198")
      (green   "#859900"))

  (let ((fg base00)
        (bg base3)
        (comments base1)
        (strings cyan)
        (selection base2)
        (fg-light base1)
        (bg-highlight base2)
        (bg-highlight-2 "LightCyan")
        (bg-highlight-3 "LightGreen"))

    (custom-theme-set-faces
     'yast-light

     ;; generic stuff
     `(default ((t (:background ,bg :foreground ,fg))))
     `(button ((t (:foreground ,fg :underline t))))
     `(cursor ((t (:background ,fg :foreground ,base02))))
     `(region ((t (:background ,selection :foreground ,fg))))
     `(custom-variable-tag ((t (:foreground ,fg :weight bold))))
     `(default-italic ((t (:italic t))))
     `(font-lock-builtin-face ((t (:background ,bg :foreground ,fg :slant italic))))
     `(font-lock-comment-delimiter-face ((t (:foreground ,comments))))
     `(font-lock-comment-face ((t (:foreground ,comments :slant italic))))
     `(font-lock-constant-face ((t (:foreground ,fg))))
     `(font-lock-doc-face ((t (:foreground ,comments :slant italic))))
     `(font-lock-function-name-face ((t (:foreground ,fg))))
     `(font-lock-keyword-face ((t (:foreground ,fg :weight bold))))
     `(font-lock-preprocessor-face ((t (:foreground ,fg))))
     `(font-lock-reference-face ((t (:foreground ,fg))))
     `(font-lock-string-face ((t (:foreground ,strings))))
     `(font-lock-type-face ((t (:foreground ,fg))))
     `(font-lock-variable-name-face ((t (:foreground ,blue :underline nil))))
     `(font-lock-warning-face ((t (:foreground ,fg :weight bold))))
     `(fringe ((t (:background ,bg :foreground ,bg))))
     `(ido-first-match ((t (:foreground ,fg))))
     `(ido-only-match ((t (:foreground ,fg))))
     `(ido-subdir ((t (:foreground ,fg))))
     `(isearch ((t (:foreground ,bg :background ,cyan))))
     `(highlight ((t nil)))
     `(lazy-highlight ((t (:foreground ,bg :background ,base0))))
     `(link ((t (:foreground ,fg))))

     ;; latex
     `(font-latex-bold-face ((t (:foreground ,fg))))
     `(font-latex-italic-face ((t (:foreground ,fg :slant italic))))
     `(font-latex-match-reference-keywords ((t (:foreground ,fg))))
     `(font-latex-match-variable-keywords ((t (:foreground ,fg))))
     `(font-latex-string-face ((t (:foreground "#a9a9a9"))))

     ;; mode line and minibuffer
     `(minibuffer-prompt ((t (:foreground ,fg :weight bold))))
     `(mode-line ((t (:background ,base2 :foreground ,fg :height 0.8))))
     `(mode-line-buffer ((t (:foreground ,fg :weight bold))))
     `(mode-line-inactive ((t (:background ,base2 :foreground ,fg :height 0.8))))
     `(mode-line-minor-mode ((t (:weight ultra-light))))
     `(modeline ((t (:background ,bg :foreground ,fg :height 0.8))))

     ;; org-mode
     `(org-agenda-date ((t (:foreground ,fg :height 1.2))))
     `(org-agenda-date-today ((t (:foreground ,fg :weight bold :height 1.4))))
     `(org-agenda-date-weekend ((t (:foreground ,fg :weight normal))))
     `(org-agenda-structure ((t (:foreground ,fg :weight bold))))
     `(org-block ((t (:foreground ,fg))))
     `(org-block-begin-line ((t (:foreground ,fg-light))))
     `(org-block-end-line ((t (:foreground ,fg-light))))
     `(org-date ((t (:foreground ,fg) :underline)))
     `(org-done ((t (:foreground ,fg-light))))
     `(org-headline-done ((t (:foreground ,fg-light :strike-through t))))
     `(org-hide ((t (:foreground ,bg))))
     `(org-level-1 ((t (:foreground ,fg :weight semi-bold :height 1.3))))
     `(org-level-2 ((t (:foreground ,fg :weight semi-bold :height 1.1))))
     `(org-level-3 ((t (:foreground ,fg :weight semi-bold :height 1.1))))
     `(org-level-4 ((t (:foreground ,fg :weight semi-bold :height 1.1))))
     `(org-level-5 ((t (:foreground ,fg :weight semi-bold :height 1.1))))
     `(org-level-6 ((t (:foreground ,fg :weight semi-bold :height 1.1))))
     `(org-link ((t (:foreground ,fg :underline t))))
     `(org-quote ((t (:foreground ,fg :slant italic :inherit org-block))))
     `(org-scheduled ((t (:foreground ,fg))))
     `(org-sexp-date ((t (:foreground ,fg))))
     `(org-special-keyword ((t (:foreground ,fg))))
     `(org-todo ((t (:foreground ,fg))))
     `(org-verse ((t (:inherit org-block :slant italic))))

     ;; magit
     `(magit-header ((t (:weight bold))))
     `(magit-item-mark ((t (:background ,bg-highlight))))
     `(magit-item-highlight ((t (:weight bold))))
     `(magit-section-heading ((t (:weight bold))))
     `(magit-section-highlight ((t (:background ,base2))))
     `(magit-diff-context-highlight ((t (:foreground ,fg-light))))
     `(magit-branch-local ((t (:weight bold))))
     `(magit-branch-remote ((t (:weight bold))))

     ;; helm
     `(helm-source-header ((t (:foreground ,fg :background "grey90" :weight bold))))
     `(helm-header ((t (:foreground ,fg))))
     `(helm-selection-line ((t (:inherit region :weight bold))))
     `(helm-selection ((t (:background ,bg-highlight))))
     `(helm-ff-directory ((t (:foreground ,fg :weight bold))))
     `(helm-ff-dotted-directory ((t (:foreground ,fg :weight bold))))
     `(helm-ff-symlink ((t (:foreground ,fg :slant italic))))
     `(helm-ff-executable ((t (:foreground ,fg))))

     ;; compile
     `(compilation-error ((t (:inherit error))))

     ;; flycheck
     `(flycheck-error ((t (:inherit error))))
     `(flycheck-warning ((t (:inherit warning))))

     ;; dired
     `(dired-directory ((t (:weight bold))))

     ;; iedit
     `(iedit-occurrence ((t (:background ,bg-highlight-3 :foreground ,fg))))

     ;; parentheses
     '(parenthesis ((t (:foreground "gray70"))))
     `(show-paren-match ((t (:foreground ,base01 :weight bold))))
     `(show-paren-mismatch ((t (:background "red" :foreground "black" :weight bold))))

     ;; js2 and rjsx
     `(js2-function-param ((t (:foreground ,fg))))
     `(js2-external-variable ((t (:foreground ,fg))))
     `(rjsx-attr ((t (:slant italic))))

     ;; perl
     `(cperl-hash-face ((t (:foreground ,fg))))
     `(cperl-array-face ((t (:foreground ,fg))))
     `(cperl-nonoverridable-face ((t (:foreground ,fg))))

     ;; markdown
     `(markdown-bold-face ((t (:inherit default :weight bold))))
     `(markdown-italic-face ((t (:inherit default :slant italic))))
     `(markdown-header-face-1 ((t (:foreground ,blue :weight bold))))
     `(markdown-header-face-2 ((t (:foreground ,cyan :weight bold))))
     `(markdown-header-face-3 ((t (:foreground ,violet :weight bold))))
     `(markdown-link-face ((t (:foreground ,blue))))

     ;; misc
     `(idle-highlight ((t (:background ,bg-highlight))))
     `(yas-field-highlight-face ((t (:background "#eeeee8" :foreground ,fg))))
     `(eshell-prompt ((t (:foreground ,fg :weight bold))))
     `(cider-result-overlay-face ((t (:weight bold))))
     `(slime-repl-inputed-output-face ((t (:foreground ,fg))))
     `(whitespace-line ((t (:background ,bg-highlight-2 :foreground ,fg)))))))

;;;###autoload
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'yast-light)
;;; yast-light-theme.el ends here
