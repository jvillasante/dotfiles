;;; my-init-apps.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; password-store for emacs
(use-package password-store
    :preface (defun my--password-store-git-push ()
                 (interactive)
                 (with-editor-async-shell-command "pass git push"))
    :bind (("C-c p c" . password-store-copy)
           ("C-c p C" . password-store-copy-field)
           ("C-c p g" . password-store-generate)
           ("C-c p G" . password-store-generate-no-symbols)
           ("C-c p e" . password-store-edit)
           ("C-c p r" . password-store-rename)
           ("C-c p R" . password-store-remove)
           ("C-c p i" . password-store-insert)
           ("C-c p P" . my--password-store-git-push))
    :custom ((password-store-password-length 25)))

;; eww
(use-package eww
    :ensure nil ;; emacs built-in
    :init (setq eww-search-prefix "https://duckduckgo.com/?q="))

;; pdf
(use-package pdf-tools
    :mode ("\\.pdf\\'" . pdf-view-mode)
    :init (setq pdf-view-display-size 'fit-page
                pdf-view-use-scaling t
                pdf-view-use-imagemagick nil
                pdf-view-continuous t)
    :config (add-hook 'pdf-outline-buffer-mode-hook
                      (lambda ()
                          (my--font-set-small-variable-font))))

;; nov.el : Major mode for reading EPUBs in Emacs
(use-package nov
    :disabled t
    :mode ("\\.epub\\'" . nov-mode)
    :custom (nov-text-width t))

;; mastodon.el : Emacs client for the AcitivityPub social networks that implement the Mastodon API.
(use-package mastodon
    :disabled t
    :init (setq mastodon-instance-url "https://hachyderm.io"
                mastodon-active-user "jvillasante"))

;; circe : A client for IRC in Emacs
(use-package circe
    :disabled t
    :preface
    (defun my--circe-prompt ()
        (lui-set-prompt
         (concat (propertize (concat (buffer-name) ">")
                             'face 'circe-prompt-face)
                 " ")))
    (defun my--irc.libera.chat-password(&rest ignored)
        (string-trim (nth 0 (process-lines "pass" "show" "Logins/irc.libera.chat"))))
    :init
    (require 'lui-autopaste)
    (add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)
    (add-hook 'circe-chat-mode-hook 'my--circe-prompt)
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
             :sasl-password my--irc.libera.chat-password
             :channels ("#emacs" "#emacs-circe" "#pass" "#opensuse")))))

;; speed-type : Practice speed typing
(use-package speed-type
    :disabled t)

;; docker : Emacs integration for Docker!
(use-package docker
    :bind ("C-c d" . docker)
    :config
    ;; always run with `vterm' if available
    (if (fboundp 'vterm)
            (setq docker-run-async-with-buffer-function #'docker-run-async-with-buffer-vterm))

    ;; When docker run is called on an image whose repository name matches the regular expression "^postgres",
    ;; the option "-e POSTGRES_PASSWORD=postgres" will appear as set along with the defaults specified by `docker-image-run-default-args'.
    (add-to-list 'docker-image-run-custom-args
                 `("^postgres" ("-e POSTGRES_PASSWORD=postgres" . ,docker-image-run-default-args)))

    ;; docker run --rm --interactive --tty --volume /home/jvillasante/Workspace/Work/Projects/dmxs:/tmp/sm -w /tmp/sm --name dmxs sm:latest /bin/bash
    ;; docker run --rm --interactive --tty --volume /home/jvillasante/Workspace/Work/Projects/dmxs:/tmp/sm -w /tmp/sm --name dmxs registry.gitlab.com/nielsen-media/eng/meters/dmxs/dmxs/sm:dmx2-dmx4-dmx5 /bin/bash
    (add-to-list 'docker-image-run-custom-args
                 `("sm\\:*" ("-v \"$HOME\"/Workspace/Work/Projects/dmxs:/tmp/sm"
                             "-w /tmp/sm"
                             "--name dmxs" . ,docker-image-run-default-args))))

;; elfeed
(use-package elfeed
    :preface (defun my--elfeed-delete-window-after-kill-buffer (&rest args)
                 (delete-window (selected-window)))
    :init
    ;; `elfeed-kill-buffer' only kills the buffer, but won't delete
    ;; the window. This is not an ideal behavior since you typically
    ;; what to hit `q' to delete the window displaying the news after
    ;; you have finished reading.
    (advice-add #'elfeed-kill-buffer :after #'my--elfeed-delete-window-after-kill-buffer)
    :config
    (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory))
    (setq elfeed-enclosure-default-dir (expand-file-name "closures" elfeed-db-directory))
    (setq elfeed-search-title-min-width 60)
    (setq elfeed-search-title-max-width 100)
    (setq elfeed-search-trailing-width 0)
    (setq elfeed-search-filter "@6-months-ago +unread")
    (setq elfeed-db-directory (expand-file-name "Apps/elfeed/elfeed_db" my--dropbox-path))
    (setq elfeed-show-entry-switch #'pop-to-buffer)
    (setq shr-max-image-proportion 0.7)
    (add-to-list 'display-buffer-alist
                 '("\\*elfeed-entry"
                   (display-buffer-below-selected)
                   (window-height . 0.85)))
    ;; feeds
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
            "https://olddeuteronomy.github.io/index.xml"
            "https://bzoltan1.github.io/index.xml"
            "https://systemcrafters.net/rss/news.xml"
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
            "https://orodu.net/feed.xml"
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
            ;; Linux
            "http://dominique.leuenberger.net/blog/feed/rss/"
            ;; Misc
            "https://mazzo.li/rss.xml"
            "https://neil.computer/rss"
            "https://chandlerc.blog/index.xml"
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
            "https://www.thecodedmessage.com/index.xml"
            "https://unixsheikh.com/feed.rss"
            "https://michal.sapka.me/index.xml"
            "https://borretti.me/feed.xml"
            "https://fabiensanglard.net/rss.xml"
            "http://somethingfast.net/feed.rss.xml"
            "https://www.tedinski.com/feed.xml"
            "https://matklad.github.io/feed.xml")))

(provide 'my-init-apps)
;;; my-init-apps.el ends here
