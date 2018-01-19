(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(setq js2-indent-switch-body t)

;; Customize js2-mode's lint warnings.
(setq js2-strict-missing-semi-warning nil)
(setq js2-missing-semi-one-line-override nil)
(setq js2-strict-trailing-comma-warning nil)
