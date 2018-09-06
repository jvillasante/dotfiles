(cond
  ((spacemacs/system-is-mac)
    (setq
      jv/dropbox-path "~/Dropbox"
      jv/zsh-path "/usr/local/bin/zsh"
      jv/clang-path "/usr/local/opt/llvm/bin/clang"))
  ((spacemacs/system-is-linux)
    (setq
      jv/dropbox-path "~/Dropbox"
      jv/zsh-path "/usr/bin/zsh"
      jv/clang-path "/usr/bin/clang")))

(setq
  flycheck-check-syntax-automatically '(mode-enabled save))

;; https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
(setq display-time-world-list '(("UTC" "UTC")
                                 ("US/Eastern" "Miami")
                                 ("America/Havana" "Habana")
                                 ("America/New_York" "New York")
                                 ("Europe/Amsterdam" "Amsterdam")
                                 ("Europe/Copenhagen" "Denmark")
                                 ("Asia/Shanghai" "China")
                                 ("Asia/Calcutta" "India")))

(setq max-specpdl-size 5000)
(setf url-queue-timeout 30)
