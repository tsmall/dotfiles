;;; alabaster-theme.el --- Minimalistic Syntax Highlighting

;; Copyright (C) 2017 Tom Small III

;; Author: Tom Small III <tsmall3@gmail.com>
;; URL: http://github.com/tsmall/dotfiles
;; Package-Version: 20170601
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

;; A light theme with minimal highlighting.

;;; Credits:

;; This theme is a port of Tonsky's Alabster theme for Visual Studio Code,
;; which you can find here: https://github.com/tonsky/vscode-theme-alabaster

;;; Code:

(deftheme alabaster
  "A light theme with minimal highlighting.")

(let ((fg "#111111")
      (bg "#F7F7F7")
      (line-hl-bg "#F0F0F0")
      (selection-bg "#BFDBFE")
      (selection-hl-bg "#E0E0E0")
      (cursor-fg "#007ACC")
      (find-match-bg "#FFBC5D")
      (find-match-hl-bg "#FFE9A6")
      (comment-fg "#AA3731")
      (string-fg "#448C27")
      (mode-line-bg "#F0F0F0")
      (mode-line-fg "#007ACC")
      (global-entity-fg "#325CC0")
      (literal-fg "#7A3E9D"))

  (custom-theme-set-faces
   'alabaster

   ;; generic stuff
   `(default ((t (:background ,bg :foreground ,fg))))
   `(button ((t (:foreground ,fg :underline t))))
   `(cursor ((t (:background ,bg :foreground ,cursor-fg))))
   `(custom-variable-tag ((t (:foreground ,fg :weight bold))))
   `(default-italic ((t (:italic t))))
   `(font-latex-bold-face ((t (:foreground ,fg :weight bold))))
   `(font-latex-italic-face ((t (:foreground ,fg :slant italic))))
   `(font-latex-match-reference-keywords ((t (:foreground ,fg))))
   `(font-latex-match-variable-keywords ((t (:foreground ,fg))))
   `(font-latex-string-face ((t (:foreground ,string-fg))))
   `(font-lock-builtin-face ((t (:background ,bg :foreground ,fg))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,comment-fg))))
   `(font-lock-comment-face ((t (:foreground ,comment-fg :weight normal :italic t))))
   `(font-lock-constant-face ((t (:foreground ,literal-fg))))
   `(font-lock-doc-face ((t (:foreground ,comment-fg :weight normal :italic t))))
   `(font-lock-function-name-face ((t (:foreground ,global-entity-fg))))
   `(font-lock-keyword-face ((t (:foreground ,fg))))
   `(font-lock-preprocessor-face ((t (:foreground ,fg))))
   `(font-lock-reference-face ((t (:foreground ,fg))))
   `(font-lock-string-face ((t (:foreground ,string-fg))))
   `(font-lock-type-face ((t (:foreground ,fg))))
   `(font-lock-variable-name-face ((t (:foreground ,fg :underline nil))))
   `(font-lock-warning-face ((t (:foreground ,fg :weight bold))))
   `(fringe ((t (:background ,bg :foreground ,bg))))
   `(gnus-header-content ((t (:foreground ,fg))))
   `(gnus-header-from ((t (:foreground ,fg))))
   `(gnus-header-name ((t (:foreground ,fg))))
   `(gnus-header-subject ((t (:foreground ,fg))))
   `(highlight ((t nil)))
   `(ido-first-match ((t (:foreground ,fg))))
   `(ido-only-match ((t (:foreground ,fg))))
   `(ido-subdir ((t (:foreground ,fg))))
   `(isearch ((t (:background ,find-match-bg :foreground ,fg))))
   `(link ((t (:foreground ,fg))))
   `(minibuffer-prompt ((t (:foreground ,fg :weight bold))))
   `(mode-line ((t (:background ,mode-line-bg :foreground ,mode-line-fg :height 0.8))))
   `(mode-line-buffer ((t (:foreground ,mode-line-fg :weight bold))))
   `(mode-line-inactive ((t (:background ,mode-line-bg :foreground ,mode-line-fg :height 0.8))))
   `(mode-line-minor-mode ((t (:weight ultra-light))))
   `(modeline ((t (:background ,bg :foreground ,fg :height 0.8))))
   `(org-agenda-date ((t (:foreground ,fg :height 1.2))))
   `(org-agenda-date-today ((t (:foreground ,fg :weight bold :height 1.4))))
   `(org-agenda-date-weekend ((t (:foreground ,fg :weight normal))))
   `(org-agenda-structure ((t (:foreground ,fg :weight bold))))
   `(org-block ((t (:foreground ,fg))))
   `(org-block-begin-line ((t (:foreground ,fg))))
   `(org-block-end-line ((t (:foreground ,fg))))
   `(org-date ((t (:foreground ,fg) :underline)))
   `(org-done ((t (:foreground ,fg))))
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
   `(region ((t (:background "#eeeee8" :foreground ,fg))))
   `(slime-repl-inputed-output-face ((t (:foreground ,fg))))
   `(whitespace-line ((t (:background ,bg :foreground ,fg))))

   ;; magit
   `(magit-header ((t (:weight bold))))
   `(magit-item-mark ((t (:background ,selection-hl-bg))))
   `(magit-item-highlight ((t (:weight bold))))
   `(magit-section-heading ((t (:weight bold))))
   ;; `(magit-section-highlight ((t (:inherit default))))
   `(magit-diff-context-highlight ((t (:foreground ,fg))))
   `(magit-branch-local ((t (:weight bold))))
   `(magit-branch-remote ((t (:weight bold))))

   ;; compile
   `(compilation-error ((t (:inherit error))))

   ;; flycheck
   `(flycheck-error ((t (:inherit error))))
   `(flycheck-warning ((t (:inherit warning))))

   ;; dired
   `(dired-directory ((t (:weight bold))))

   ;; helm
   `(helm-source-header ((t (:foreground ,fg :background "grey90" :weight bold))))
   `(helm-header ((t (:foreground ,fg))))
   `(helm-selection-line ((t (:inherit region :weight bold))))
   `(helm-selection ((t (:background ,selection-hl-bg))))
   `(helm-ff-directory ((t (:foreground ,fg :weight bold))))
   `(helm-ff-dotted-directory ((t (:foreground ,fg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,fg :slant italic))))
   `(helm-ff-executable ((t (:foreground ,fg))))

   ;; iedit
   `(iedit-occurrence ((t (:background ,selection-bg :foreground ,fg))))

   ;; company
   `(company-echo-common ((t (:foreground ,fg))))
   `(company-tooltip-selection ((t (:background ,selection-hl-bg))))

   ;; parens - parenface
   '(parenface-paren-face ((t (:foreground "gray70"))))
   '(parenface-curly-face ((t (:foreground "gray70"))))
   '(parenface-bracket-face ((t (:foreground "gray70"))))

   ;; parens - paren-face
   '(parenthesis ((t (:foreground "gray70"))))

   ;; parens - other
   `(sp-show-pair-match-face ((t (:foreground "black" :weight bold))))
   `(sp-show-pair-mismatch-face ((t (:background "red" :foreground "black" :weight bold))))
   `(show-paren-match ((t (:foreground "black" :weight bold))))
   `(show-paren-mismatch ((t (:background "red" :foreground "black" :weight bold))))

   ;; js2
   `(js2-function-param ((t (:foreground ,fg))))
   `(js2-external-variable ((t (:foreground ,fg))))

   ;; perl
   `(cperl-hash-face ((t (:foreground ,fg))))
   `(cperl-array-face ((t (:foreground ,fg))))
   `(cperl-nonoverridable-face ((t (:foreground ,fg))))

   ;; rpm-spec-mode
   `(rpm-spec-tag-face ((t (:inherit default))))
   `(rpm-spec-package-face ((t (:inherit default))))
   `(rpm-spec-macro-face ((t (:inherit default))))
   `(rpm-spec-doc-face ((t (:inherit default))))
   `(rpm-spec-var-face ((t (:inherit default))))
   `(rpm-spec-ghost-face ((t (:inherit default))))
   `(rpm-spec-section-face ((t (:inherit default :weight bold))))

   ;; misc
   `(idle-highlight ((t (:background ,selection-hl-bg))))
   `(yas-field-highlight-face ((t (:background "#eeeee8" :foreground ,fg))))
   `(eshell-prompt ((t (:foreground ,fg :weight bold))))
   `(cider-result-overlay-face ((t (:weight bold))))))

;;;###autoload
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'alabaster)
;;; alabaster-theme.el ends here
