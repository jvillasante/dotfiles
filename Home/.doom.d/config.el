;;; ~/Hacking/workspace/dotfiles/Home/.doom.d/config.el -*- lexical-binding: t; -*-

;; check OS type
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (progn
    (message "Microsoft Windows Not Supported!!!!")))
 ((string-equal system-type "darwin") ; Mac OS X
  (progn
    (setq
     jv/dropbox-path "~/Dropbox"
     jv/zsh-path "/usr/local/bin/zsh"
     jv/clang-path "/usr/local/opt/llvm/bin/clang")

    (setq
     browse-url-browser-function 'browse-url-generic
     engine/browser-function 'browse-url-generic
     browse-url-generic-program "open")))
 ((string-equal system-type "gnu/linux") ; linux
  (progn
    (setq
     jv/dropbox-path "~/Dropbox"
     jv/zsh-path "/usr/bin/zsh"
     jv/clang-path "/usr/bin/clang")

     (executable-find "google-chrome"))))

;; Some setq-defaults
(setq-default
 user-full-name "Julio C. Villasante"
 user-mail-address "jvillasantegomez@gmail.com"

 major-mode 'text-mode
 use-dialog-box nil
 vc-follow-symlinks t
 load-prefer-newer t
 fill-column 110                    ; Maximum line width
 truncate-lines t                   ; Don't fold lines
 truncate-partial-width-windows nil ; for vertically-split windows
 split-width-threshold 160          ; Split verticaly by default
 evil-cross-lines t                 ; Make horizontal movement cross lines

 ;; scroll
 scroll-margin 3

 ;; my coding style, bsd with 4 spaces indentation (and no tab characters)
 evil-shift-width 4
 tab-width 4
 indent-tabs-mode nil

 ;; Whitespace settings
 whitespace-action '(auto-cleanup)
 whitespace-style '(indentation::space
                    space-after-tab
                    space-before-tab
                    trailing
                    lines-tail
                    tab-mark
                    face
                    tabs)

 ;; tramp mode
 tramp-default-method "ssh")

;; LaTeX
;; font-latex-fontify-script nil
;; TeX-newline-function 'reindent-then-newline-and-indent)

;; Some more defaults
(setq
 ;; No highligh persisten on evil search
 evil-ex-search-persistent-highlight nil

 flycheck-check-syntax-automatically '(mode-enabled save)

 ;; recentf exclude folders/files
 recentf-exclude '("~/Hacking/workspace/dotfiles/.emacs.d")

 ispell-program-name "aspell"
 auto-window-vscroll nil
 sp-escape-quotes-after-insert nil

 ;; I do not know what this is :)
 max-specpdl-size 5000
 url-queue-timeout 30)

;; Load snippets
(after! yasnippet
  (push (expand-file-name "snippets/" doom-private-dir) yas-snippet-dirs))

;; (def-package! prettier-js
;;   :commands (prettier-js-mode)
;;   :init
;;   (defun setup-prettier-js ()
;;     "Sets up arguments and the mode."
;;     (interactive)
;;     (setq prettier-js-args '("--single-quote"))

;;     (prettier-js-mode))
;;   (add-hook! (typescript-mode
;;               js2-mode)
;;     #'setup-prettier-js)
;;   (add-hook! web-mode (enable-minor-mode '("\\.tsx\\'" . setup-prettier-js))))

;; (after! typescript-mode
;;   (add-hook 'typescript-mode-hook #'flycheck-mode)
;;   (setq typescript-indent-level 2))

;; (after! js2-mode
;;   ;; use eslintd-fix so when i save it fixes dumb shit
;;   (add-hook 'js2-mode-hook #'eslintd-fix-mode)

;;   ;; Indent shit
;;   (setq js2-basic-offset 2))

(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename matches the regexp.
  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
          (funcall (cdr my-pair)))))

(after! web-mode
  (add-hook 'web-mode-hook #'flycheck-mode)

  (setq web-mode-markup-indent-offset 4 ;; Indentation
        web-mode-code-indent-offset 4
        web-mode-enable-auto-quoting nil ;; disbale adding "" after an =
        web-mode-auto-close-style 4))

;; (after! elm
;;   (setq elm-tags-on-save t
;;         elm-sort-imports-on-save t))

;; (map! :leader
;;       :prefix "f"
;;       "n" (lambda! (find-file "~/dotfiles/nixos/configuration.nix"))
;;       "N" (lambda! (find-file "~/dotfiles/nixos/home.nix")))

;; (setq lsp-haskell-process-wrapper-function
;;       (lambda (argv)
;;         (append
;;          (append (list "nix-shell" "-I" "." "--command")
;;                  (list (mapconcat 'identity argv " ")))

;;          (list (concat (lsp-haskell--get-root) "/shell.nix")))))

;; (after! lsp-haskell
;;   ;; These take up a lot of space on my big font size
;;   (setq lsp-ui-sideline-show-code-actions nil))

;; Set twitter edit buffer to be 15 lines high so I can actually see what im
;; editing. FIXME this will be fixed upstream, remove me when it is
;; (after! twittering-mode
;;   (set-popup-rule! "^\\*twittering-edit"
;;     '((size . 15))
;;     '((transient) (quit) (select . t))))

;; Modules
;; (load! "+ruby")    ;; Custom ruby mode. Make sure you disable ruby in init.el
(load! "+ui")      ;; My ui mods. Also contains ligature stuff.
;; (load! "+ranger")  ;; File manager stuff
;; (load! "+reason")  ;; ReasonML stuff
;; (load! "+mail")    ;; Mail stuff
;; (load! "+org")     ;; Org mode stuff like todos and rebindings
;; (load! "+irc") ;; Irc config
