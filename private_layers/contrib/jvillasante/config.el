;; magit
(with-eval-after-load 'magit
  (setq-default git-magit-status-fullscreen t)
  (setq magit-completing-read-function 'magit-builtin-completing-read
        magit-push-always-verify nil))

;; company
(with-eval-after-load 'company
  (add-hook 'c-mode-hook (lambda () (add-to-list 'company-backends '(company-irony-c-headers company-irony))))
  (add-hook 'c++-mode-hook (lambda () (add-to-list 'company-backends '(company-irony-c-headers company-irony)))))

;; org
(with-eval-after-load 'org
  ;; org problems
  (setq org-planning-line-re "^[    ]*\\(\\(?:CLOSED\\|DEADLINE\\|SCHEDULED\\):\\)")
  (setq org-clock-line-re "^[    ]*CLOCK:")

  (setq org-startup-indented t)
  (setq org-indent-mode t)

  ;; set maximum indentation for description lists
  (setq org-list-description-max-indent 5)

  ;; prevent demoting heading also shifting text inside sections
  (setq org-adapt-indentation nil))

;; Helm at Emacs
(evil-leader/set-key
  "oh" 'hotspots)

(defun hotspots ()
  "helm interface to my hotspots, which includes my locations, org-files and bookmarks"
  (interactive)
  (helm :sources `(((name . "Mail and News and Social")
                    (candidates . (("Mail" . (lambda ()
                                               (if (get-buffer "*mu4e-headers*")
                                                   (progn
                                                     (switch-to-buffer "*mu4e-headers*")
                                                     (delete-other-windows))
                                                 (mu4e))))
                                   ("Calendar" . (lambda () (browse-url "https://www.google.com/calendar/render")))
                                   ("RSS" . elfeed)
                                   ("Notes" . deft)
                                   ("Dotfiles" . spacemacs/find-dotfile)
                                   ("Agenda" . (lambda () (org-agenda "" "w")))))
                    (action . (("Open" . (lambda (x) (funcall x))))))

                   ((name . "My Locations")
                    (candidates . (("elfeed.org" . "~/Dropbox/Personal/elfeed/elfeed.org")
                                   ;; ("master" . "~/Dropbox/org-mode/master.org")
                                   ;; (".emacs.d" . "~/Dropbox/kitchingroup/jmax" )
                                   ;; ("blog" . "~/blogofile-jkitchin.github.com/_blog/blog.org")
                                   ;; ("ese" . "~/Dropbox/books/ese-book/ese.org" )
                                   ;; ("passwords" . "~/Dropbox/org-mode/passwords.org.gpg")
                                   ;; ("Pycse" . "~/Dropbox/books/pycse/pycse.org")
                                   ;; ("references" . "~/Dropbox/bibliography/references.bib")
                                   ;; ("notes" . "~/Dropbox/bibliography/notes.org")
                                   ;; ("journal" . "~/Dropbox/org-mode/journal.org")
                                   ;; ("tasks" . "~/Dropbox/org-mode/tasks.org")
                                   ))
                    (action . (("Open" . (lambda (x) (find-file x))))))

                   ;; ((name . "My org files")
                   ;;  (candidates . ,(f-entries "~/Dropbox/org-mode"))
                   ;;  (action . (("Open" . (lambda (x) (find-file x))))))
                   helm-source-recentf
                   helm-source-bookmarks
                   helm-source-bookmark-set)))

;; javascript stuff, not used for now
;; (setenv "NODE_NO_READLINE" "1")

;; (eval-after-load 'js2-mode
;;   `(progn
;;      ;; BUG: self is not a browser extern, just a convention that needs checking
;;      (setq js2-browser-externs (delete "self" js2-browser-externs))

;;      ;; Consider the chai 'expect()' statement to have side-effects, so we don't warn about it
;;      (defun js2-add-strict-warning (msg-id &optional msg-arg beg end)
;;        (if (and js2-compiler-strict-mode
;;                 (not (and (string= msg-id "msg.no.side.effects")
;;                           (string= (buffer-substring-no-properties beg (+ beg 7)) "expect("))))
;;            (js2-report-warning msg-id msg-arg beg
;;                                (and beg end (- end beg)))))))
;;   (setq-default js2-basic-offset 2)
;;   (setq-default js-indent-level 2)
;;   (add-hook 'js2-mode-hook (lambda () (electric-indent-mode -1)))

;; (eval-after-load 'compile
;;   `(progn
;;      ;; Highlight node.js stacktraces in *compile* buffers
;;      (defvar my-nodejs-compilation-regexp
;;        '("^[ \t]+at +\\(?:.+(\\)?\\([^()\n]+\\):\\([0-9]+\\):\\([0-9]+\\))?$" 1 2 3))

;;      (add-to-list 'compilation-error-regexp-alist-alist
;;                   (cons 'nodejs my-nodejs-compilation-regexp))
;;      (add-to-list 'compilation-error-regexp-alist 'nodejs)))

;; ;; Open files that start with "#!/usr/bin/env node" in js2-mode
;; (add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; ;; jsx syntax highlighting with web-mode
;; (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
;; (defadvice web-mode-highlight-part (around tweak-jsx activate)
;;   (if (equal web-mode-content-type "jsx")
;;       (let ((web-mode-enable-part-face nil))
;;         ad-do-it)
;;     ad-do-it))

;; ;; jsx syntax checking for web-mode
;; (eval-after-load 'flycheck
;;   `(progn
;;      (require 'flycheck)
;;      (setq flycheck-check-syntax-automatically '(save mode-enabled idle-change))
;;      (setq flycheck-idle-change-delay 5)
;;      (flycheck-define-checker jsxhint-checker
;;                               "A JSX syntax and style checker based on JSXHint."
;;                               :command ("jsxhint" (config-file "--config=" jshint-configuration-path) source)
;;                               :error-patterns ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
;;                               :modes (web-mode))

;;      (defun my-find-jshintrc ()
;;        (expand-file-name ".jshintrc"
;;                          (locate-dominating-file
;;                           (or (buffer-file-name) default-directory) ".jshintrc")))

;;      (add-hook 'web-mode-hook
;;                (lambda ()
;;                  (when (equal web-mode-content-type "jsx")
;;                    (tern-mode t)
;;                    (setq-local jshint-configuration-path (my-find-jshintrc))
;;                    (flycheck-select-checker 'jsxhint-checker)
;;                    (flycheck-mode))))))
