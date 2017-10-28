(with-eval-after-load 'deft
  (setq deft-directory (concat jv/dropbox-path "/Personal/notes")
    deft-default-extension "org"
    deft-extensions '("org")
    deft-recursive t
    deft-text-mode 'org-mode
    deft-use-filename-as-title t
    deft-use-filter-string-for-filename t
    deft-file-naming-rules '((noslash . "-")
                              (nospace . "-")
                              (case-fn . downcase))
    deft-auto-save-interval 0))
