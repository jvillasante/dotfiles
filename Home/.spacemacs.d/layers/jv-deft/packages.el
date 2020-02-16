(defconst jv-deft-packages
    '(deft
         ))

(defun jv-deft/post-init-deft ()
    (setq
        deft-directory (concat jv/dropbox-path "/Personal/org/notes")
        deft-extensions '("org" "md" "txt")
        deft-default-extension "org"
        deft-recursive t
        deft-text-mode 'org-mode
        deft-use-filename-as-title t
        deft-use-filter-string-for-filename t
        deft-file-naming-rules '((noslash . "-")
                                    (nospace . "-")
                                    (case-fn . downcase))
        deft-auto-save-interval 0)

    ;; The document title is produced from the filename and/or the content according to the following rules:
    ;;
    ;; 1. If the filename is a README.*, the title will be the filename itself.
    ;;    This will be enough, as its directory provides information on the context.
    ;; 2. If the filename has a txt extension, the title will be the filename.
    ;; 3. If the file has a YAML front matter in the beginning denoted by ---,
    ;;    the title will be extracted from it. If the file has a draft status in the
    ;;    YAML front matter, [DRAFT] tag will be prepended to the title.
    ;; 4. Otherwise, the default parsing function (deft-parse-title) will be used. That is,
    ;;    the title will be the first line of the file, but extra characters in Org Mode
    ;;    metadata and Markdown headings are stripped out.
    ;; (advice-add 'deft-parse-title :around #'my-deft/parse-title-with-directory-prepended)
    )
