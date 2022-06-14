;;; personal/04_neotree.el --- neotree customizations -*- lexical-binding: t; -*-
;;;

(prelude-require-package 'neotree)

(setq
    neo-theme 'ascii
    neo-window-width 42
    neo-smart-open t
    neo-create-file-auto-open nil
    neo-show-updir-line nil
    neo-show-hidden-files t
    neo-auto-indent-point t
    neo-vc-integration nil
    neo-autorefresh nil)

;; When running `projectile-switch-project`, `neotree` will change root automatically.
(setq projectile-switch-project-action 'neotree-projectile-action)

;; Hidden files
(setq neo-hidden-regexp-list
    '(;; vcs folders
         "^\\.\\(?:git\\|hg\\|svn\\)$"
         ;; compiled files
         "\\.\\(?:pyc\\|o\\|elc\\|lock\\|css.map\\|class\\)$"
         ;; generated files, caches or local pkgs
         "^\\(?:node_modules\\|vendor\\|.\\(project\\|cask\\|yardoc\\|sass-cache\\)\\)$"
         ;; org-mode folders
         "^\\.\\(?:sync\\|export\\|attach\\)$"
         ;; temp files
         "~$"
         "^#.*#$"
         ;; Others
         "^\\.\\(cache\\|tox\\|coverage\\)$"
         "^\\.\\(DS_Store\\|python\\-version\\)"
         "^\\(htmlcov\\)$" "\\.elcs$"
         "^\\.coverage\\..*" "\\.ipynb.*$" "\\.py[cod]$"
         "^\\.#.*$" "^__pycache__$"
         "\\.gcda$" "\\.gcov$" "\\.gcno$" "\\.lo$" "\\.o$" "\\.so$"
         "^\\.cproject$" "^\\.project$" "^\\.projectile$"
         "\\.egg\-info$"))
