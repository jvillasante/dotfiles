;;; custom.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ibuffer-formats
      '((mark modified read-only locked " " (name 30 30 :left :elide) " "
            (size 9 -1 :right) " " (mode 16 16 :left :elide) " "
            project-file-relative)))
 '(package-selected-packages
      '(adoc-mode agent-shell anzu bind-key cape cl-generic cl-lib cmake-mode compat
           consult-dir consult-notes corfu crux csv-mode dape devdocs-browser
           diff-hl dired-sidebar diredfl dockerfile-mode editorconfig eglot
           eldoc eldoc-diffstat elfeed elisp-demos embark-consult engine-mode
           erc expreg external-completion faceup flash flymake geiser-guile
           gnuplot go-mode helpful hl-todo ibuffer-project idlwave jinx js2-mode
           json-mode jsonrpc ledger-mode let-alist lua-mode magit map marginalia
           markdown-mode minions modus-themes monkeytype multiple-cursors
           nadvice nov ntlm orderless org org-superstar otpp
           package-lint-flymake password-store pdf-view-restore peg
           persistent-scratch php-mode project python rmsbolt rust-mode seq
           shell-maker so-long soap-client surround svg track-changes tramp-hlo
           trashed ultra-scroll use-package verb verilog-mode vertico
           vim-tab-bar vterm vundo web-mode wgrep which-key window-tool-bar xref
           yaml-mode yasnippet-snippets zig-mode))
 '(package-vc-selected-packages
      '((agent-shell :url "https://github.com/xenodium/agent-shell.git")
           (shell-maker :url "https://github.com/xenodium/shell-maker.git")
           (deadgrep :url "git@github.com:Wilfred/deadgrep.git")
           (flash :url "git@github.com:Prgebish/flash.git")
           (ledger-mode :url "git@github.com:ledger/ledger-mode.git"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family fixed-font :height 147))))
 '(fixed-pitch ((t (:family fixed-font :height 147))))
 '(font-lock-comment-face ((t (:family fixed-font :slant italic))))
 '(variable-pitch ((t (:family variable-font :height 1.0)))))

(provide 'custom)
;;; custom.el ends here
