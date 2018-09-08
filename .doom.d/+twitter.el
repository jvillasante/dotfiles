;;;  -*- lexical-binding: t; -*-

(after! twittering-mode
  (require 'ace-link)
  (setq twittering-initial-timeline-spec-string '(":home" ":mentions")
    twittering-timer-interval 300)) ;; 5 seems like a reasonable refresh
;;considering you can hit "r" to refresh
