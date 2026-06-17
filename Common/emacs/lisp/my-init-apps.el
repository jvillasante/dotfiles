;;; my-init-apps.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; calc
(use-package calc
    :ensure nil ; emacs built-in
    :defer t
    :init
    (setq calc-window-height 15))

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
    :custom (password-store-password-length 25))

;; gnus
(use-package gnus
    :disabled t
    :ensure nil ;; emacs built-in
    :hook ((dired-mode       . turn-on-gnus-dired-mode)
              (gnus-group-mode   . gnus-topic-mode)
              (gnus-select-group . gnus-group-set-timestamp)
              (gnus-group-mode   . hl-line-mode)
              (gnus-summary-mode . hl-line-mode)
              (gnus-browse-mode  . hl-line-mode))
    :custom
    (gnus-select-method '(nnnil))
    (gnus-secondary-select-methods
        '((nntp "news.eternal-september.org"
              (nntp-open-connection-function nntp-open-ssl-stream)
              (nntp-port-number 563))
             ;; Gwene: RSS/Atom feeds exposed as NNTP groups (plain NNTP, port 119).
             (nntp "news.gwene.org")))
    ;; Use only `.newsrc.eldt'; ignore the legacy `.newsrc' format.
    (gnus-startup-file (expand-file-name "Apps/gnus/newsrc" my/dropbox-path))
    (gnus-save-newsrc-file nil)
    (gnus-read-newsrc-file nil)
    (gnus-asynchronous t)
    (gnus-summary-display-arrow t)
    ;; Thread tree glyphs
    (gnus-sum-thread-tree-indent          "  ")
    (gnus-sum-thread-tree-root            "")
    (gnus-sum-thread-tree-false-root      "")
    (gnus-sum-thread-tree-single-indent   "")
    (gnus-sum-thread-tree-vertical        "│")
    (gnus-sum-thread-tree-leaf-with-other "├─► ")
    (gnus-sum-thread-tree-single-leaf     "╰─► ")
    (gnus-summary-line-format
        (concat "%0{%U%R%z%}"
            "%3{│%}%1{%d%}%3{│%}"   ;; date
            "  "
            "%4{%-20,20f%}"         ;; author
            "  %3{│%} "
            "%1{%B%}%s\n")))

(use-package newsticker
    :disabled t
    :ensure nil ;; emacs built-in
    :commands (newsticker-treeview newsticker-show-news newsticker-start)
    :preface
    (defun my/newsticker-cleanup-buffers (&rest _)
        "Kill leftover newsticker buffers after quitting the tree view."
        (dolist (buf '("*Newsticker List*" "*Newsticker Item*" "*Newsticker Tree*"))
            (when (get-buffer buf)
                (kill-buffer buf))))
    :bind ("C-c n n" . newsticker-treeview)
    :custom
    (newsticker-dir (expand-file-name "Apps/newsticker" my/dropbox-path))
    (newsticker-retrieval-method 'intern)            ;; use built-in url.el; no wget/curl needed
    (newsticker-retrieval-interval 0)                ;; no auto-refresh; press G to fetch (avoids Dropbox races)
    (newsticker-html-renderer #'shr-render-region)   ;; readable article view
    (newsticker-automatically-mark-visited-items-as-old t)
    (newsticker-url-list-defaults nil)               ;; drop bundled sample feeds
    (newsticker-url-list
        '(("Planet Emacslife" "https://planet.emacslife.com/atom.xml")
             ("Hacker News"      "https://news.ycombinator.com/rss")))
    :config
    (advice-add #'newsticker-treeview-quit :after #'my/newsticker-cleanup-buffers))

;;;; SHR
(use-package shr
    :ensure nil ; emacs built-in
    :config
    ;; t is bad for accessibility and generally awkward for HTML email
    ;; (especially with dark themes).
    (setq shr-use-colors nil)
    ;; This option should not exist, given `variable-pitch-mode'.
    ;; Furthermore, its default value runs counter to almost everything
    ;; else in Emacs which just uses the `default' face.
    (setq shr-use-fonts nil)
    (setq shr-indentation 2)             ; Left-side margin
    (setq shr-width 80)                  ; Fold text to 80 columns
    (setq shr-max-image-proportion 0.6)) ; Images use at most 60% of window height

;;;; EWW
(use-package eww
    :ensure nil ; emacs built-in
    :defer t
    :preface
    (defun my/eww-font-setup ()
        "Setup fonts for eww-mode."
        (face-remap-add-relative 'default 'fixed-pitch-large))
    :hook (eww-mode . my/eww-font-setup)
    :config
    (setq eww-auto-rename-buffer 'url)                     ; open url in new buffer
    (setq eww-download-directory
        (expand-file-name "~/Downloads/eww-downloads"))  ; keeps eww downloads separate
    (setq eww-use-external-browser-for-content-type
        "\\`\\(video/\\|audio\\)")                       ; On GNU/Linux check your mimeapps.list
    (setq eww-retrieve-command nil)                      ; default: use `url-retrieve'
    ;;       '("wget" "--quiet" "--output-document=-"))       ; seems buggy at times?
    ;;       '("chromium-browser" "--headless" "--dump-dom")) ; slow!
    ;;       '("curl" "--silent" "-A Mozilla/5.0"))           ; seems buggy at times?
    (setq eww-search-prefix
        "https://duckduckgo.com/?q=")) ; Use another engine for searching

;; nov.el : Major mode for reading EPUBs in Emacs
(use-package nov
    :defer t
    :preface
    (defun my/nov-font-setup ()
        "Setup fonts for nov-mode."
        (face-remap-add-relative 'default 'fixed-pitch-large))
    (defun my/nov-dos2unix-wrapper ()
        "Wrapper to allow my/dos2unix to run in read-only nov buffers."
        (let ((inhibit-read-only t))
            (my/dos2unix)))
    :hook ((nov-mode . my/nov-font-setup)
              (nov-post-html-render . my/nov-dos2unix-wrapper))
    :mode ("\\.epub\\'" . nov-mode)
    :bind (:map nov-mode-map
              ("n" . next-line)
              ("p" . previous-line))
    :custom ((nov-text-width 80)
                (nov-variable-pitch nil) ; breaks rendering!
                (nov-save-place-file (expand-file-name
                                         "nov-places" my/var-dir))))

;; pdf-tools: replacement of DocView for PDF files
(use-package pdf-tools
    :defer t
    :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
    :hook (pdf-view-mode . pdf-tools-enable-minor-modes)
    :bind (:map pdf-view-mode-map
              ("C-v" . pdf-view-next-page)
              ("M-v" . pdf-view-previous-page)
              ("n"   . pdf-view-next-line-or-next-page)
              ("p"   . pdf-view-previous-line-or-previous-page)))

;; pdf-view-restore: open last known pdf position in pdf-view-mode provided by pdf-tools.
(use-package pdf-view-restore
    :hook (pdf-view-mode . pdf-view-restore-mode)
    :config (setq pdf-view-restore-filename
                (expand-file-name "pdf-view-restore" my/var-dir)))

;; doc-view: built-in pdf viewer
(use-package doc-view
    :disabled t
    :ensure nil ;; emacs built-in
    :defer t
    :mode ("\\.[pP][dD][fF]\\'" . doc-view-mode))

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
    (add-hook 'circe-channel-mode-hook #'enable-lui-autopaste)
    (add-hook 'circe-chat-mode-hook #'my/circe-prompt)
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
    :defer t
    :mode ("\\.ledger\\'" . ledger-mode)
    :hook (ledger-mode . (lambda ()
                             (turn-off-auto-fill)
                             (setq-local tab-always-indent 'complete)
                             (setq-local completion-cycle-threshold t)
                             (setq-local ledger-complete-in-steps t)))
    :init
    (setq-default ledger-master-file
        (expand-file-name "Apps/ledger/main.ledger" my/dropbox-path))
    (setq-default ledger-accounts-file
       (expand-file-name "Apps/ledger/accounts.ledger" my/dropbox-path))
    :config
    (setq ledger-post-amount-alignment-column 60)
    (setq ledger-reports
        '(;; All account balances, full tree
             ("balance" "%(binary) -f %(ledger-file) bal")
             ;; Every posting in the file, oldest to newest
             ("register" "%(binary) -f %(ledger-file) reg")
             ;; All postings for the payee under point
             ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
             ;; All postings for the account under point
             ("account" "%(binary) -f %(ledger-file) reg %(account)")
             ;; Net worth: assets minus liabilities, top level only
             ;; (--market values any non-$ holdings at current price)
             ("net worth"
                 "%(binary) -f %(ledger-file) bal --depth 1 --market ^Assets: ^Liabilities:")
             ;; Spending by category for the current month
             ("expenses this month"
                 "%(binary) -f %(ledger-file) bal --period \"this month\" ^Expenses:")
             ;; Actual vs budgeted this month (positive = over budget)
             ("budget this month"
                 "%(binary) -f %(ledger-file) --budget -p \"this month\" bal ^Expenses")
             ;; Actual vs. budget year-to-date (budget covers only months with data)
             ("budget YTD" "%(binary) -f %(ledger-file) --budget -p \"this year\" bal ^Expenses")
             ;; Income and expenses this month; grand total = net saved/overspent
             ("income statement"
                 "%(binary) -f %(ledger-file) -p \"this month\" bal ^Income ^Expenses")
             ;; Current balance owed on each credit card
             ("credit cards"
                 "%(binary) -f %(ledger-file) bal ^Liabilities:Credit")
             ;; Spending by category since Jan 1
             ("YTD expenses"
                 "%(binary) -f %(ledger-file) -p \"this year\" bal ^Expenses")
             ;; Income vs expenses for the year; grand total = net saved
             ("YTD income statement"
                 "%(binary) -f %(ledger-file) -p \"this year\" bal ^Income ^Expenses")
             ;; Postings not yet reconciled (! or unmarked); empty when all cleared
             ("uncleared" "%(binary) -f %(ledger-file) reg --uncleared")
             ;; Everything entered this month, transaction by transaction
             ("this month" "%(binary) -f %(ledger-file) reg -p \"this month\"")
             ;; Total spending per month, to see the trend
             ("monthly expenses" "%(binary) -f %(ledger-file) reg ^Expenses -M --collapse")
             ;; Individual expense postings, largest first
             ("biggest expenses" "%(binary) -f %(ledger-file) reg ^Expenses --sort \"(-amount)\"")
             ;; Net worth at each month-end (running total column)
             ("net worth over time" "%(binary) -f %(ledger-file) reg ^Assets ^Liabilities -M --collapse")
             ;; All debt: credit cards + mortgage + car loan
             ("debt" "%(binary) -f %(ledger-file) bal ^Liabilities")
             ;; Position snapshot: assets vs liabilities only (no income/expense dump)
             ("balance sheet" "%(binary) -f %(ledger-file) bal ^Assets ^Liabilities")
             ;; Loan payoff progress, month by month
             ("loan payoff" "%(binary) -f %(ledger-file) reg ^Liabilities:Loans -M --collapse"))))

;; elfeed
(use-package elfeed
    :defer t
    :preface
    (defun my/elfeed-strip-old-content ()
        "Database Management, recommended to run manually from time to time."
        (interactive)
        (let ((limit (elfeed-float-time "60 days ago")))
            (elfeed-db-visit (entry feed)
                (cond
                    ((< (elfeed-entry-date entry) limit)
                        (elfeed-db-return))
                    ((equal "https://example.com/feed/" (elfeed-feed-url feed))
                        (setf (elfeed-entry-content entry) nil))))))
    :hook (elfeed-show-mode . (lambda ()
                                  (face-remap-add-relative 'default 'fixed-pitch-large)))
    :config
    (setq elfeed-curl-program-name (executable-find "curl"))
    (setq elfeed-use-curl t)
    (setq elfeed-db-directory (expand-file-name "Apps/elfeed/elfeed_db" my/dropbox-path))
    (setq elfeed-enclosure-default-dir (expand-file-name "closures" elfeed-db-directory))
    (setq elfeed-search-title-min-width 60)
    (setq elfeed-search-title-max-width 100)
    (setq elfeed-search-trailing-width 0)
    (setq elfeed-search-filter "@6-months-ago +unread")
    (setq elfeed-show-entry-switch #'pop-to-buffer)
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
             ("https://www.lib.uchicago.edu/keith/emacs/feed.xml" emacs book)
             ("https://www.matem.unam.mx/~omar/apropos-emacs.xml" emacs)
             ("https://planet.emacslife.com/atom.xml" emacs)
             ("https://protesilaos.com/master.xml" emacs)
             ("https://www.masteringemacs.org/feed" emacs)
             ;; ("https://emacs.tv/videos.rss" emacs videos)
             ;; Lisp
             ("https://dthompson.us/feed.xml" lisp guile guix)
             ("https://www.alexvear.com/atom.xml" lisp)
             ;; C++
             ("https://isocpp.org/blog/rss" C++ ISO)
             ("http://arne-mertz.de/feed/" C++)
             ("http://herbsutter.com/feed/" C++)
             ("http://scottmeyers.blogspot.com/feeds/posts/default" C++)
             ("http://bartoszmilewski.com/feed/")
             ("https://akrzemi1.wordpress.com/feed/")
             ("https://www.fayewilliams.com/feed/")
             ("http://feeds.woboq.com/woboq")
             ("https://oopscenities.net/feed/")
             ("http://articles.emptycrate.com/node/feed.xml")
             ("https://tartanllama.github.io/feed.xml")
             ("https://marcoarena.wordpress.com/feed/")
             ("http://www.nirfriedman.com/atom.xml")
             ("http://tristanbrindle.com/feed.xml")
             ("http://templated-thoughts.blogspot.com/feeds/posts/default")
             ("http://www.fluentcpp.com/feed/")
             ("https://dvmirchevcpp.blogspot.com/feeds/posts/default")
             ("https://blog.galowicz.de//feed.xml")
             ("https://baptiste-wicht.com/rss.xml")
             ("http://feeds.feedburner.com/abseilio")
             ("https://mariusbancila.ro/blog/feed/")
             ("http://www.nuonsoft.com/blog/feed/")
             ("https://levelofindirection.com/main.rss")
             ("https://wgml.pl/feed.xml")
             ("https://panky-codes.github.io/feed.xml")
             ("https://philippegroarke.com//posts/index.xml")
             ("https://codingnest.com/rss/")
             ("https://cor3ntin.github.io/index.xml")
             ("https://bitbashing.io/feed.xml")
             ("https://oleksandrkvl.github.io/feed.xml")
             ("https://www.sandordargo.com/feed.xml")
             ("https://quuxplusone.github.io/blog/feed.xml")
             ("https://brevzin.github.io/feed.xml")
             ("https://learnmoderncpp.com/feed/")
             ("http://bajamircea.github.io/feed.xml")
             ("https://orodu.net/feed.xml")
             ;; Golang
             ("https://blog.golang.org/feed.atom" golang)
             ;; Rust
             ("https://blog.rust-lang.org/feed.xml" rust)
             ("https://readrust.net/all/feed.rss" rust)
             ("http://www.integer32.com/feed.xml")
             ("https://ehsanmkermani.com/feed/")
             ("https://www.jameselford.com/rss.xml")
             ("https://blog.adamchalmers.com/atom.xml")
             ("https://itsallaboutthebit.com/atom.xml")
             ;; Linux
             ("http://dominique.leuenberger.net/blog/feed/rss/")
             ;; Math, Security
             ("https://0xkrt26.github.io/math_behind_security/feed.xml")
             ;; Misc
             ("https://david.alvarezrosa.com/index.xml")
             ("https://blog.codingconfessions.com/feed")
             ("https://scheatkode.com/rss.xml")
             ("https://www.birkey.co/rss.xml")
             ("https://lambdaland.org/index.xml")
             ("https://cpp-rendering.io/feed/")
             ("https://michaelneuper.com/index.xml")
             ("https://world.hey.com/dhh/feed.atom")
             ("https://susam.net/feed.xml")
             ("https://neil.computer/rss")
             ("https://chandlerc.blog/index.xml")
             ("https://sqrtminusone.xyz/posts/index.xml")
             ("https://blog.orhun.dev/rss.xml")
             ("https://ibob.bg/feed.xml")
             ("https://rigtorp.se/index.xml")
             ("http://eli.thegreenplace.net/feeds/all.atom.xml")
             ("https://www.murrayc.com/feed/")
             ("https://gendignoux.com/blog/feed.xml")
             ("https://drewdevault.com/blog/index.xml")
             ("https://incolumitas.com/feeds/all.atom.xml")
             ("http://www.mycpu.org/feed.xml")
             ("https://blog.codinghorror.com/rss/")
             ("https://www.micahcantor.com/atom.xml")
             ("https://kerkour.com/feed.xml")
             ("https://cliffle.com/rss.xml")
             ("https://nigeltao.github.io/feed.xml")
             ("https://www.thecodedmessage.com/index.xml")
             ("https://unixsheikh.com/feed.rss")
             ("https://michal.sapka.me/index.xml")
             ("https://borretti.me/feed.xml")
             ("http://somethingfast.net/feed.rss.xml")
             ("https://www.tedinski.com/feed.xml")
             ("https://unixism.net/feed/")
             ("https://thelinuxcode.com/feed/")
             ("https://selfboot.cn/en/atom.xml")
             ("https://morwenn.github.io/feed.xml")
             ;; More
             ("https://jointhefreeworld.org/rss.xml" lisp scheme dev)
             ("https://veitner.bearblog.dev/feed/" c c++ misc)
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
