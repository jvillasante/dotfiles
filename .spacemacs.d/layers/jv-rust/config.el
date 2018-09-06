(defun jv/my-project-try-cargo-toml (dir)
  "Try to locate a Rust project."
  (when (locate-dominating-file dir "Cargo.toml")
    `(transient . ,dir)))

;; Try rust projects before version-control (vc) projects
(add-hook 'project-find-functions 'jv/my-project-try-cargo-toml nil nil)
