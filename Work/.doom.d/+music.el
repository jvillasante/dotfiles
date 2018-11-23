;;;  -*- lexical-binding: t; -*-

;; FIXME when run with the SPC command for the first time
;; and it prompts for the code, doesnt give input to the
;; minibuffer and just restores it to the main buffer
;; So the command has to be run manually with M-x
;; for the first time so you can enter the command
(def-package! google-play-music
  :commands
  google-play-music-forward
  google-play-music-play-pause
  google-play-music-start
  google-play-music-unwind)

(map!
  (:leader
    (:desc "applications" :prefix "a"
      (:desc "music" :prefix "m"
        :desc "Play/Pause Music" :n "m" #'google-play-music-play-pause
        :desc "Rewind song" :n "u" #'google-play-music-unwind
        :desc "Skip song" :n "f" #'google-play-music-forward
        :desc "Connect to music player" :n "c" #'google-play-music-start))))
