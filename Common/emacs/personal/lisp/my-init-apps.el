;;; my-init-apps.el -*- lexical-binding: t; -*-

(use-package eww
    :ensure nil ;; emacs built-in
    :init
    (setq eww-search-prefix "https://search.brave.com/search?q="))

(use-package elfeed
    :init
    (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory))
    (setq elfeed-enclosure-default-dir (expand-file-name "closures" elfeed-db-directory))
    :config
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

    ;; `elfeed-kill-buffer' only kills the buffer, but won't delete
    ;; the window. This is not an ideal behavior since you typically
    ;; what to hit `q' to delete the window displaying the news after
    ;; you have finished reading.
    (advice-add #'elfeed-kill-buffer :after #'my/elfeed-delete-window-after-kill-buffer)

    ;; feeds
    (setq! elfeed-feeds
        '(;; General
             ;; "http://feeds.bbci.co.uk/news/rss.xml" ; BBC News
             ;; ycombinator
             "https://news.ycombinator.com/rss"
             ;; VPN
             "https://mullvad.net/blog/feed/atom"
             ;; Emacs
             "https://planet.emacslife.com/atom.xml"
             "http://www.terminally-incoherent.com/blog/feed"
             "http://nullprogram.com/feed"
             "http://fasciism.com/feed.xml"
             "https://protesilaos.com/master.xml"
             "https://jeffbowman.writeas.com/feed/"
             "https://www.masteringemacs.org/feed"
             "https://irreal.org/blog/?feed=rss2"
             "https://olddeuteronomy.github.io/index.xml"
             "https://bzoltan1.github.io/index.xml"
             ;; Embedded
             "http://www.embeddedrelated.com/blogs_rss.php"
             ;; C++
             "https://isocpp.org/blog/rss"
             "http://arne-mertz.de/feed/"
             "http://herbsutter.com/feed/"
             "http://feeds.feedburner.com/CppSoup"
             "http://feeds.feedburner.com/CppTruths"
             "http://www.drdobbs.com/articles/cpp/rss"
             "http://scottmeyers.blogspot.com/feeds/posts/default"
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
             "https://pniedzielski.net/feed.xml"
             "https://dvmirchevcpp.blogspot.com/feeds/posts/default"
             "http://szelei.me/atom.xml"
             "https://blog.galowicz.de//feed.xml"
             "https://baptiste-wicht.com/rss.xml"
             "http://feeds.feedburner.com/abseilio"
             "https://mariusbancila.ro/blog/feed/"
             "https://www.computist.xyz/feeds/posts/default?alt=rss"
             "http://www.nuonsoft.com/blog/feed/"
             "http://blog.vorbrodt.me/?feed=rss2"
             "https://levelofindirection.com/main.rss"
             "https://wgml.pl/feed.xml"
             "https://panky-codes.github.io/feed.xml"
             "https://philippegroarke.com//posts/index.xml"
             "https://codingnest.com/rss/"
             "https://cor3ntin.github.io/index.xml"
             "https://bitbashing.io/feed.xml"
             "https://oleksandrkvl.github.io/feed.xml"
             "https://www.sandordargo.com/feed.xml"
             "https://muit.tech/posts/index.xml"
             "https://quuxplusone.github.io/blog/feed.xml"
             "https://brevzin.github.io/feed.xml"
             "https://learnmoderncpp.com/feed/"
             "http://bajamircea.github.io/feed.xml"
             ;; Golang
             "https://blog.golang.org/feed.atom"
             ;; Rust
             "https://blog.rust-lang.org/feed.xml"
             "https://readrust.net/all/feed.rss"
             "http://www.integer32.com/feed.xml"
             "https://odetorust.com/feed.xml"
             "https://ehsanmkermani.com/feed/"
             "https://www.jameselford.com/rss.xml"
             "https://blog.adamchalmers.com/atom.xml"
             "https://itsallaboutthebit.com/atom.xml"
             ;; Misc
             "https://sqrtminusone.xyz/posts/index.xml"
             "https://blog.orhun.dev/rss.xml"
             "https://ibob.bg/feed.xml"
             "https://rigtorp.se/index.xml"
             "http://www.norvig.com/rss-feed.xml"
             "http://eli.thegreenplace.net/feeds/all.atom.xml"
             "https://pniedzielski.net/feed.xml"
             "https://eklitzke.org/index.rss"
             "https://www.murrayc.com/feed/"
             "https://gendignoux.com/blog/feed.xml"
             "https://drewdevault.com/blog/index.xml"
             "https://incolumitas.com/feeds/all.atom.xml"
             "http://www.mycpu.org/feed.xml"
             "https://muit.tech/index.xml"
             "https://blog.codinghorror.com/rss/"
             "https://www.micahcantor.com/atom.xml"
             "https://h2x.sh/atom.xml"
             "https://kerkour.com/feed.xml"
             "https://cliffle.com/rss.xml"
             "https://nigeltao.github.io/feed.xml"
             "https://www.thecodedmessage.com/index.xml")))

(use-package pdf-tools
    :mode ("\\.pdf\\'" . pdf-view-mode)
    :init
    (setq pdf-view-display-size 'fit-page
        ;; Enable hiDPI support, but at the cost of memory! See politza/pdf-tools#51
        pdf-view-use-scaling t
        pdf-view-use-imagemagick nil
        pdf-view-continuous nil)
    :config
    (add-to-list 'display-buffer-alist
        `("\\*[oO]utline.*pdf\\*"
             (display-buffer-in-side-window display-buffer-reuse-window)
             (side . ,(alist-get 'pdf-outline my/side-window-sides))
             (window-width . 0.3)))

    (add-hook 'pdf-outline-buffer-mode-hook
        (lambda ()
            (my/font-set-small-variable-font)
            (display-line-numbers-mode 0))))

(provide 'my-init-apps)
;;; my-init-apps.el ends here
