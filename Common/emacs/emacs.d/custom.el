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
      '(ace-window adoc-mode anzu apheleia bind-key cape cl-generic cl-lib cmake-mode
           compat consult-dir consult-notes corfu crux csv-mode dape deadgrep
           devdocs-browser diff-hl dired-sidebar diredfl docker dockerfile-mode
           dwim-shell-command easy-kill editorconfig eglot eldoc eldoc-diffstat
           elfeed elisp-demos embark-consult engine-mode erc
           exec-path-from-shell expreg external-completion faceup
           fancy-compilation flymake gcmh geiser-guile go-mode gptel helpful
           hl-todo ibuffer-project idlwave jinx js2-mode json-mode jsonrpc
           ledger-mode let-alist lsp-ui lua-mode magit map marginalia minions
           modus-themes monkeytype multiple-cursors nadvice nov ntlm orderless
           org org-superstar otpp password-store pdf-view-restore peg
           persistent-scratch php-mode project python rainbow-delimiters request
           rmsbolt rust-mode seq shrink-path so-long soap-client surround svg
           track-changes tramp trashed use-package verb verilog-mode vertico
           vim-tab-bar vterm vundo web-mode wgrep-deadgrep which-key
           window-tool-bar xref yaml-mode yasnippet-snippets zig-mode))
 '(package-vc-selected-packages
      '((gptel :url "git@github.com:karthink/gptel.git")
           (deadgrep :url "git@github.com:Wilfred/deadgrep.git")
           (ledger-mode :url "git@github.com:ledger/ledger-mode.git"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))

(provide 'custom)
;;; custom.el ends here
