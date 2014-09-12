(global-set-key (kbd "C-c t") 'fiplr-find-file)

(setq fiplr-ignored-globs '((directories (".git" ".svn" ".hg" ".bzr" "bower_components" "node_modules"))
                            (files (".#*" "*~" "*.so" "*.jpg" "*.png" "*.gif" "*.gz" "*.pdf" "*.pyc" "*.zip"))))
