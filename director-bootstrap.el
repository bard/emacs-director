(defun director-bootstrap (&rest config)
  "Setup the environment for a simulated user session."

  (require 'package)
  
  (setq byte-compile-warnings nil)
  (when (boundp 'comp-async-report-warnings-errors)
    (setq comp-async-report-warnings-errors nil))

  (let ((user-dir (plist-get config :user-dir))
        (packages (plist-get config :packages))
        (additional-load-paths (plist-get config :load-path)))

    (when user-dir
      (setq user-emacs-directory user-dir)
      (setq package-user-dir (expand-file-name "elpa" user-emacs-directory)))

    (when additional-load-paths
      (setq load-path (append load-path additional-load-paths)))

    ;; attempt requiring director here; if error, add director to list of required
    ;; packages, and retry after initializing packages
    
    (package-initialize)
    (when packages
      (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
      (dolist (package packages)
        (unless (package-installed-p package)
          (package-install package))))

    (require 'director)))

