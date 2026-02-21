;;; .dir-locals --- -*- no-byte-compile: t; lexical-binding: t; -*-
((nil .
      ((eval . (progn
                 ;; At work, we don't auto-format code :(
                 (apheleia-mode -1)

                 ;; Point compile-command at the build script using an absolute
                 ;; path derived from the .dir-locals.el location so it works
                 ;; regardless of which subdirectory the visited file is in.
                 (let ((root (locate-dominating-file default-directory ".dir-locals.el")))
                   (setq-local compile-command
                               (concat "bash "
                                       (expand-file-name ".scripts/build.sh" root)))))))))
