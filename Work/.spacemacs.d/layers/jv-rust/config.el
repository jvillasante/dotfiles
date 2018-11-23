;; (defun jv/my-project-try-cargo-toml (dir)
;;   "Try to locate a Rust project."
;;   (when (locate-dominating-file dir "Cargo.toml")
;;     `(transient . ,dir)))

(defun jv/my-project-try-cargo-toml (dir)
  "Try to locate a Rust project above DIR."
  (let ((found (locate-dominating-file dir "Cargo.toml")))
    (if (stringp found) `(transient . ,found) nil)))

;; Try rust projects before version-control (vc) projects
(add-hook 'project-find-functions 'jv/my-project-try-cargo-toml nil nil)
