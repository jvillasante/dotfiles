;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; disabled packages
(disable-packages!
    doom-themes
    solaire-mode
    elfeed-goodies
    irony
    ggtags
    ccls)

;; modus-themes : modus-operandi & modus-vivendi themes
(package! modus-themes)

;; whole-line-or-region : In Emacs, operate on the current line if no region is active
(package! whole-line-or-region)

;; crux : A Collection of Ridiculously Useful eXtensions for Emacs
(package! crux)

;; Run M-x dwim-shell-command to execute DWIM shell commands
(package! dwim-shell-command)

;; dired
(package! dired-sidebar)
(package! dired-quick-sort)
(package! dired-single)
(package! dired-open)
(package! dired-hide-dotfiles)

;; others
(package! ibuffer-vc)
(package! tldr)

;; file-info with posframe
(package! posframe)
(package! file-info)
