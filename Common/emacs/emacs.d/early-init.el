;;; early-init.el --- Early Init -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Variables

(defvar my/user-directory user-emacs-directory
    "The default value of the `user-emacs-directory' variable.")

;; Paths used throughout
(defconst my/home-path         (expand-file-name "~/"))
(defconst my/dotfiles-path     (expand-file-name "Workspace/Public/dotfiles/" my/home-path))
(defconst my/work-path         (expand-file-name "Workspace/Work/Omicron/"    my/home-path))
(defconst my/software-path     (expand-file-name "Workspace/Software/"        my/home-path))
(defconst my/dropbox-path      (expand-file-name "Dropbox/"                   my/home-path))

;;; Env - needs to be in `early-init.el'

;; Set environment variables - `exec-path-from-shell' is too slow!
(setq my/exec-path-list
    (list (expand-file-name ".go/bin" my/home-path)
        (expand-file-name ".cargo/bin" my/home-path)
        (expand-file-name ".local/bin" my/home-path)
        "/usr/local/bin"
        "/usr/local/sbin"
        "/usr/bin"
        "/usr/sbin"
        "/bin"
        "/sbin"))

;; Set the `$PATH' environment variable
(setenv "PATH" (mapconcat #'identity my/exec-path-list path-separator))

;; Emacs built with GTK lags in its response to keyboard input
(when (or (featurep 'gtk)
          (featurep 'pgtk))
    (setenv "GTK_IM_MODULE" "none"))

;; Set the Emacs-internal `exec-path'
(setq exec-path (append (parse-colon-path (getenv "PATH")) (list exec-directory)))

;;; Startup and garbage collection

(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
    (lambda ()
        (setq gc-cons-threshold (* 64 1024 1024))
        (message "Emacs loaded in %s with %d garbage collections."
            (format "%.2f seconds"
                (float-time
                    (time-subtract after-init-time before-init-time)))
            gcs-done)))

;;; Custom

;; Without the `custom-file', Emacs writes directly to the "init.el",
;; which can be confusing.
(setq custom-file (locate-user-emacs-file "custom.el"))
(add-hook 'after-init-hook
    (lambda () (load custom-file :no-error-if-file-is-missing)))

;; Reducing clutter in ~/.emacs.d by redirecting files to ~/emacs.d/var/
(setq user-lisp-directory (expand-file-name "../user-lisp/" my/user-directory))
(setq my/lisp-dir (expand-file-name "../lisp/"              my/user-directory))
(setq my/var-dir  (expand-file-name "var/"                  my/user-directory))
(setq my/etc-dir  (expand-file-name "etc/"                  my/user-directory))
(setq user-emacs-directory                                  my/var-dir)

;;; Native compilation and Byte compilation
;; Display the architecture using:
;;   gcc -march=native -Q --help=target | grep march
;;
;; The above command asks the compiler to resolve native for your current CPU
;; and display the resulting target. For example, if the output shows
;; -march=skylake, you know that skylake is the identifier you should pass to
;; -mtune and -march.
(defvar my-cpu-architecture "tigerlake")

;; `native-comp-compiler-options' specifies flags passed directly to the C
;; compiler (for example, GCC) when compiling the Lisp-to-C output
;; produced by the native compilation process. These flags affect code
;; generation, optimization, and debugging information.
(setq native-comp-compiler-options '("-O2"
                                     "-g0"
                                     "-fomit-frame-pointer"
                                     "-fno-finite-math-only"))

;; `native-comp-driver-options' specifies additional flags passed to the native
;; compilation driver process, which may invoke the compiler and linker with
;; certain parameters.
(setq native-comp-driver-options `(,(format "-mtune=%s" my-cpu-architecture)
                                   ,(format "-march=%s" my-cpu-architecture)))

(if (and (featurep 'native-compile)
        (fboundp 'native-comp-available-p)
        (native-comp-available-p))
    ;; Activate `native-compile'
    (setq native-comp-jit-compilation t
        package-native-compile t)
    ;; Deactivate the `native-compile' feature if it is not available
    (setq features (delq 'native-compile features)))

;;; UI elements

;; Disable startup screens and messages
(setq inhibit-startup-screen t
    inhibit-startup-buffer-menu t)

;; some default-frame-alist
(when (and (string-match-p "CAIRO" system-configuration-features)
          (string-match-p "HARFBUZZ" system-configuration-features))
    (push '(font-backend . "ftcrhb") default-frame-alist))
(push '(width . 160) default-frame-alist)
(push '(height . 46) default-frame-alist)
(push '(bottom-divider-width . 0) default-frame-alist)
(push '(right-divider-width . 1) default-frame-alist)

;; menu-bar
(push '(menu-bar-lines . 0) default-frame-alist)
(unless (memq window-system '(mac ns))
    (setq menu-bar-mode nil))

;; tool-bar
(push '(tool-bar-lines . 0) default-frame-alist)
(setq tool-bar-mode nil)

;; scroll bars
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(setq scroll-bar-mode nil)
(when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))

;; tooltip
(if (fboundp 'tooltip-mode) (tooltip-mode -1))

;; fringe - explicitly set default fringe width (8px on each side)
(if (fboundp 'fringe-mode) (fringe-mode nil))

;; Disable GUIs because they are inconsistent across systems, desktop
;; environments, and themes, and they don't match the look of Emacs.
(setq use-file-dialog nil)
(setq use-dialog-box nil)

;; Only show error-level warnings (suppress warnings and debug messages)
(setq warning-minimum-level :error)

;; Suppress warnings about packages lacking a `lexical-binding' declaration
(setq warning-suppress-types '((lexical-binding)))

;;; package.el
(setq load-prefer-newer t)
(setq package-enable-at-startup nil
    package-install-upgrade-built-in t
    package-vc-allow-build-commands t
    package-user-dir (expand-file-name "elpa" my/var-dir)
    package-archives
    '(("melpa" . "https://melpa.org/packages/")
         ("gnu" . "https://elpa.gnu.org/packages/")
         ("nongnu" . "https://elpa.nongnu.org/nongnu/"))
    package-archive-priorities
    '(("melpa" . 3)
         ("gnu" . 2)
         ("nongnu" . 1)))

(provide 'early-init)
;;; early-init.el ends here
