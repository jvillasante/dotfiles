;;;  -*- lexical-binding: t; -*-

(doom! :feature
  ;; debugger
  eval
  (evil +everywhere)
  file-templates
  (lookup +devdocs +docsets)
  snippets
  spellcheck
  syntax-checker ;; +childframe)
  workspaces

  :emacs
  dired
  ediff
  electric
  ;; eshell
  imenu
  term
  vc

  :completion
  (company
    +auto)

  ;; the ultimate code completion backend
  ;; (helm +fuzzy +childframe)
  ;; ido
  (ivy
   ;; +childframe
   ;; +fuzzy
   )

  :ui
  doom
  doom-dashboard
  modeline
  doom-quit
  ;; evil-goggles
  ;; hl-todo
  ;; nav-flash
  ;; tabbar
  vc-gutter

  vi-tilde-fringe
  window-select
  (popup
    +all
    +defaults)
  neotree
  ;; treemacs
  ;; (pretty-code +fira)

  :tools
  gist
  macos
  make
  magit
  password-store
  pdf
  ;; prodigy
  ;; rgb
  ;; tmux
  ;; upload
  editorconfig
  ;; wakatime

  :lang
  assembly
  cc
  ;; crystal
  ;; clojure
  ;; csharp
  data
  ;; elixir
  ;; elm
  emacs-lisp
  ;; ess
  go
  ;; (haskell +intero)
  ;; hy
  ;; (java +meghanada)
  javascript
  ;; julia
  latex
  ;; ledger
  ;; lua
  markdown
  ;; nix
  ;; ocaml
  (org
    +attach
    +babel
    +capture
    +export
    +present
    +publish)
  ;; perl
  ;; php
  ;; plantuml
  ;; purescript
  python
  rest
  ;;ruby
  rust
  ;; scala
  sh
  ;; swift
  web

  ;; Applications are complex and opinionated modules that transform Emacs
  ;; toward a specific purpose. They may have additional dependencies and
  ;; should be loaded late.
  :app
  ;;(email +gmail)
  ;; irc
  (rss +org)
  ;; twitter
  ;; (write
  ;;   +wordnut
  ;;   +langtool)

  :editor
  ;; lispyville
  ;; parinfer
  ;; rotate-text

  :completion
  (lsp
    ;; +javascript
    +go
    ;; +css
    +rust
    ;; +cpp
    ;; +ocaml
    ;; +java
    ;; +python
    ;; +sh
    )

  :config
  ;; The default module set reasonable defaults for Emacs. It also provides
  ;; a Spacemacs-inspired keybinding scheme, a custom yasnippet library,
  ;; and additional ex commands for evil-mode. Use it as a reference for
  ;; your own modules.
  (default +bindings +snippets +evil-commands))
