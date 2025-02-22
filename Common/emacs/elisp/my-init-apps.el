;;; my-init-apps.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; password-store for emacs
(use-package password-store
    :preface
    (defun my/password-store-git-push ()
        (interactive)
        (with-editor-async-shell-command "pass git push"))
    (defun my/password-store-reset-gpg-pcscd ()
        (interactive)
        (with-editor-async-shell-command "sudo systemctl restart pcscd"))
    :bind (("C-c P c" . password-store-copy)
           ("C-c P C" . password-store-copy-field)
           ("C-c P g" . password-store-generate)
           ("C-c P G" . password-store-generate-no-symbols)
           ("C-c P e" . password-store-edit)
           ("C-c P r" . password-store-rename)
           ("C-c P R" . password-store-remove)
           ("C-c P i" . password-store-insert)
           ("C-c P P" . my/password-store-git-push)
           ("C-c P X" . my/password-store-reset-gpg-pcscd))
    :custom ((password-store-password-length 25)))

;; gnus
(use-package gnus
    :ensure nil ;; emacs built-in
    :defer t
    :config
    (require 'gnus-topic)
    (setq gnus-select-method '(nnnil))
    ;; (add-to-list 'gnus-secondary-select-methods
    ;;              '(nntp "news.eternal-september.org"
    ;;                     (nntp-open-connection-function nntp-open-ssl-stream)
    ;;                     (nntp-port-number 563)))
    (add-to-list 'gnus-secondary-select-methods
                 '(nntp "news.newshosting.com"
                        (nntp-open-connection-function nntp-open-ssl-stream)
                        (nntp-port-number 563)))

    ;; init file
    (setq gnus-startup-file (expand-file-name "Apps/gnus/newsrc" my/dropbox-path)
          gnus-save-newsrc-file nil
          gnus-read-newsrc-file nil)

    ;; other settings
    (setq gnus-asynchronous t ;; async
          gnus-sum-thread-tree-indent "  "
          gnus-sum-thread-tree-root ""
          gnus-sum-thread-tree-false-root ""
          gnus-sum-thread-tree-single-indent ""
          gnus-sum-thread-tree-vertical        "│"
          gnus-sum-thread-tree-leaf-with-other "├─► "
          gnus-sum-thread-tree-single-leaf     "╰─► "
          gnus-summary-line-format
          (concat
           "%0{%U%R%z%}"
           "%3{│%}" "%1{%d%}" "%3{│%}" ;; date
           "  "
           "%4{%-20,20f%}"             ;; name
           "  "
           "%3{│%}"
           " "
           "%1{%B%}"
           "%s\n")
          gnus-summary-display-arrow t)
    (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
    (add-hook 'gnus-group-mode-hook #'gnus-topic-mode)
    (add-hook 'gnus-select-group-hook #'gnus-group-set-timestamp)
    (dolist (mode '(gnus-group-mode-hook gnus-summary-mode-hook gnus-browse-mode-hook))
        (add-hook mode #'hl-line-mode)))

(use-package eww
    :ensure nil ; emacs built-in
    :defer t
    :config
    (setq
     shr-use-fonts  nil                                ; No special fonts
     shr-use-colors nil                                ; No colors
     shr-indentation 2                                 ; Left-side margin
     shr-width 80                                      ; Fold text to 80 columns
     eww-auto-rename-buffer 'url                       ; open url in new buffer
     eww-retrieve-command nil                          ; the default, use `url-retrieve' to download the data
     eww-download-directory
     (expand-file-name "~/Documents/eww-downloads")    ; keeps eww downloads separate
     eww-use-external-browser-for-content-type
     "\\`\\(video/\\|audio\\)"                         ; On GNU/Linux check your mimeapps.list
     eww-search-prefix "https://duckduckgo.com/?q="))  ; Use another engine for searching

;; calc
(use-package calc-mode
    :ensure nil ;; emacs built-in
    :config (setq calc-window-height 20))

;; nov : Major mode for reading EPUBs in Emacs
(use-package nov
    :defer t
    :mode ("\\.epub\\'" . nov-mode))

;; pdf
(use-package pdf-tools
    :defer t
    :mode ("\\.pdf\\'" . pdf-view-mode)
    :config
    (setq pdf-view-continuous t))

(use-package pdf-view-restore
    :after pdf-tools
    :hook (pdf-view-mode . pdf-view-restore-mode)
    :config (setq pdf-view-restore-filename
                  (expand-file-name "pdf-view-restore" my/var-dir)))

(use-package atomic-chrome
    :disabled t
    :init (atomic-chrome-start-server)
    :config
    (setq atomic-chrome-buffer-open-style 'frame)
    (setq atomic-chrome-default-major-mode 'markdown-mode
          atomic-chrome-url-major-mode-alist `(("github\\.com" . gfm-mode)
                                               ("gitlab\\.com" . gfm-mode)
                                               ("reddit\\.com" . markdown-mode))))

;; mastodon.el : Emacs client for the AcitivityPub social networks that implement the Mastodon API.
(use-package mastodon
    :disabled t
    :defer t
    :init (setq mastodon-instance-url "https://hachyderm.io"
                mastodon-active-user "jvillasante"))

;; circe : A client for IRC in Emacs
(use-package circe
    :disabled t
    :defer t
    :preface
    (defun my/circe-prompt ()
        (lui-set-prompt
         (concat (propertize (concat (buffer-name) ">")
                             'face 'circe-prompt-face)
                 " ")))
    (defun my/irc.libera.chat-password(&rest ignored)
        (string-trim (nth 0 (process-lines "pass" "show" "Logins/irc.libera.chat"))))
    :init
    (require 'lui-autopaste)
    (add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)
    (add-hook 'circe-chat-mode-hook 'my/circe-prompt)
    :config
    (setq circe-use-cycle-completion t)
    (setq circe-reduce-lurker-spam t)
    (setq circe-format-server-topic "*** Topic change by {userhost}: {topic-diff}")
    (setq circe-format-say "{nick:-16s} {body}")
    (setq circe-network-options
          '(("Libera Chat"
             :tls t
             :nick "jvillasante"
             :sasl-username "jvillasante"
             :sasl-password my/irc.libera.chat-password
             :channels ("#emacs" "#emacs-circe" "#pass" "#opensuse")))))

;; speed-type : Practice speed typing
(use-package speed-type
    :disabled t
    :defer t)

;; ledger-mode : Emacs interface for `ledger-cli'
(use-package ledger-mode
    :vc (:url "git@github.com:ledger/ledger-mode.git"
              :rev :newest)
    :hook (ledger-mode . (lambda ()
                             (setq-local tab-always-indent 'complete)
                             (setq-local completion-cycle-threshold t)
                             (setq-local ledger-complete-in-steps t))))

;; elfeed
(use-package elfeed
    :defer t
    :preface
    (defun my/elfeed-delete-window-after-kill-buffer (&rest args)
        (delete-window (selected-window)))
    :init
    ;; `elfeed-kill-buffer' only kills the buffer, but won't delete
    ;; the window. This is not an ideal behavior since you typically
    ;; what to hit `q' to delete the window displaying the news after
    ;; you have finished reading.
    (advice-add #'elfeed-kill-buffer :after #'my/elfeed-delete-window-after-kill-buffer)
    :config
    (setq elfeed-use-curl t)
    (setq elfeed-enclosure-default-dir (expand-file-name "closures" elfeed-db-directory))
    (setq elfeed-search-title-min-width 60)
    (setq elfeed-search-title-max-width 100)
    (setq elfeed-search-trailing-width 0)
    (setq elfeed-search-filter "@6-months-ago +unread")
    (setq elfeed-db-directory (expand-file-name "Apps/elfeed/elfeed_db" my/dropbox-path))
    (setq elfeed-show-entry-switch #'pop-to-buffer)
    (setq shr-max-image-proportion 0.7)
    (add-to-list 'display-buffer-alist
                 '("\\*elfeed-entry"
                   (display-buffer-below-selected)
                   (window-height . 0.85)))
    ;; feeds
    (setq elfeed-feeds
          '(;; News
            ;; ("http://feeds.bbci.co.uk/news/rss.xml" news bbc)
            ("https://news.ycombinator.com/rss" news ycombinator)
            ("https://accu.org/index.xml" news accu)
            ;; VPN
            ("https://mullvad.net/blog/feed/atom" vpn mullvad)
            ;; EMACS
            ("https://planet.emacslife.com/atom.xml" emacs)
            ("https://protesilaos.com/master.xml" emacs)
            ("https://www.masteringemacs.org/feed" emacs)
            ("https://emacs.tv/videos.rss" emacs videos)
            ;; Lisp
            ("https://dthompson.us/feed.xml" lisp guile guix)
            ;; C++
            ("https://isocpp.org/blog/rss" C++ ISO)
            ("http://arne-mertz.de/feed/" C++)
            ("http://herbsutter.com/feed/" C++)
            ("http://scottmeyers.blogspot.com/feeds/posts/default" C++)
            "http://bartoszmilewski.com/feed/"
            "https://akrzemi1.wordpress.com/feed/"
            "https://www.fayewilliams.com/feed/"
            "http://feeds.woboq.com/woboq"
            "https://oopscenities.net/feed/"
            "http://articles.emptycrate.com/node/feed.xml"
            "https://tartanllama.github.io/feed.xml"
            "https://marcoarena.wordpress.com/feed/"
            "http://www.nirfriedman.com/atom.xml"
            "http://tristanbrindle.com/feed.xml"
            "http://templated-thoughts.blogspot.com/feeds/posts/default"
            "http://www.fluentcpp.com/feed/"
            "https://dvmirchevcpp.blogspot.com/feeds/posts/default"
            "https://blog.galowicz.de//feed.xml"
            "https://baptiste-wicht.com/rss.xml"
            "http://feeds.feedburner.com/abseilio"
            "https://mariusbancila.ro/blog/feed/"
            "http://www.nuonsoft.com/blog/feed/"
            "https://levelofindirection.com/main.rss"
            "https://wgml.pl/feed.xml"
            "https://panky-codes.github.io/feed.xml"
            "https://philippegroarke.com//posts/index.xml"
            "https://codingnest.com/rss/"
            "https://cor3ntin.github.io/index.xml"
            "https://bitbashing.io/feed.xml"
            "https://oleksandrkvl.github.io/feed.xml"
            "https://www.sandordargo.com/feed.xml"
            "https://quuxplusone.github.io/blog/feed.xml"
            "https://brevzin.github.io/feed.xml"
            "https://learnmoderncpp.com/feed/"
            "http://bajamircea.github.io/feed.xml"
            "https://orodu.net/feed.xml"
            ;; Golang
            ("https://blog.golang.org/feed.atom" golang)
            ;; Rust
            ("https://blog.rust-lang.org/feed.xml" rust)
            ("https://readrust.net/all/feed.rss" rust)
            "http://www.integer32.com/feed.xml"
            "https://ehsanmkermani.com/feed/"
            "https://www.jameselford.com/rss.xml"
            "https://blog.adamchalmers.com/atom.xml"
            "https://itsallaboutthebit.com/atom.xml"
            ;; Linux
            "http://dominique.leuenberger.net/blog/feed/rss/"
            ;; Misc
            "https://lambdaland.org/index.xml"
            "https://cpp-rendering.io/feed/"
            "https://michaelneuper.com/index.xml"
            "https://world.hey.com/dhh/feed.atom"
            "https://susam.net/feed.xml"
            "https://neil.computer/rss"
            "https://chandlerc.blog/index.xml"
            "https://sqrtminusone.xyz/posts/index.xml"
            "https://blog.orhun.dev/rss.xml"
            "https://ibob.bg/feed.xml"
            "https://rigtorp.se/index.xml"
            "http://eli.thegreenplace.net/feeds/all.atom.xml"
            "https://www.murrayc.com/feed/"
            "https://gendignoux.com/blog/feed.xml"
            "https://drewdevault.com/blog/index.xml"
            "https://incolumitas.com/feeds/all.atom.xml"
            "http://www.mycpu.org/feed.xml"
            "https://blog.codinghorror.com/rss/"
            "https://www.micahcantor.com/atom.xml"
            "https://kerkour.com/feed.xml"
            "https://cliffle.com/rss.xml"
            "https://nigeltao.github.io/feed.xml"
            "https://www.thecodedmessage.com/index.xml"
            "https://unixsheikh.com/feed.rss"
            "https://michal.sapka.me/index.xml"
            "https://borretti.me/feed.xml"
            "http://somethingfast.net/feed.rss.xml"
            "https://www.tedinski.com/feed.xml"
            "https://unixism.net/feed/"
            "https://thelinuxcode.com/feed/"
            "https://selfboot.cn/en/atom.xml"
            ;; More
            ("https://mazzo.li/rss.xml" c low-level unix)
            ("https://simblob.blogspot.com/feeds/posts/default" gamedev math algorithms)
            ("https://box2d.org/posts/index.xml" gamedev math algorithms)
            ("https://davidgomes.com/rss/")
            ("https://fabiensanglard.net/rss.xml" retrogaming)
            ("https://ferd.ca/feed.rss" distsys)
            ("https://blog.singleton.io/index.xml")
            ("https://johnnysswlab.com/feed/" cpp performance)
            ("https://jvns.ca/atom.xml" webdev)
            ("https://matklad.github.io/feed.xml" low-level programming)
            ("https://jonathan-frere.com/index.xml" programming)
            ("https://notes.eatonphil.com/rss.xml" distsys programming)
            ("https://samwho.dev/blog" programming visualization)
            ("https://wingolog.org/feed/atom" compilers guile scheme)
            ("https://jakelazaroff.com/rss.xml" webdev)
            ("https://www.localfirstnews.com/rss/" local-first)
            ("https://www.internalpointers.com/rss" networking concurrency)
            ("https://hazelweakly.me/rss.xml" observability)
            ("https://norvig.com/rss-feed.xml" software)
            ("https://pythonspeed.com/atom.xml" python))))

(provide 'my-init-apps)
;;; my-init-apps.el ends here
