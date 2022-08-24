;;; lisp/elfeed.el -*- lexical-binding: t; -*-

(after! elfeed
    (setq elfeed-feeds
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
             "https://cliffle.com/rss.xml"))

    (setq elfeed-search-title-min-width 60)
    (setq elfeed-search-title-max-width 100)
    (setq elfeed-search-trailing-width 0)
    (setq elfeed-search-filter "@6-months-ago +unread")
    (setq elfeed-db-directory (expand-file-name "Apps/elfeed/elfeed_db" +my/dropbox-path)))

(after! elfeed-org
    (setq rmh-elfeed-org-files (list (expand-file-name "Apps/elfeed/elfeed.org" +my/dropbox-path))))

(after! elfeed-search
    (add-hook! 'elfeed-search-mode-hook 'elfeed-update)
    (when (modulep! :editor evil)
        (set-evil-initial-state! 'elfeed-search-mode 'normal)))

(after! elfeed-show-mode
    (when (modulep! :editor evil)
        (set-evil-initial-state! 'elfeed-show-mode 'normal)))
