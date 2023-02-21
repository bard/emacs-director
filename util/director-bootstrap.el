;; Scripts might be stored in a projects's source tree but are
;; supposed to run in a clean environment, thus, disable reading
;; `.dir-locals.el' so that Emacs doesn't try to load it from the
;; project's source tree. (This cannot come as part of the
;; `director-bootstrap' function because, by the time that's called by
;; the script Emacs will already have tried loading the corresponding
;; `.dir-locals.el' file.)
(setq enable-dir-local-variables nil)

(defun director-bootstrap (&rest config)
  "Setup the environment for a simulated user session."
  
  (setq byte-compile-warnings nil)
  (when (boundp 'comp-async-report-warnings-errors)
    (setq comp-async-report-warnings-errors nil))

  (require 'package)

  (let ((user-dir (plist-get config :user-dir))
        (packages `(director ,@(plist-get config :packages)))
        (additional-load-paths (plist-get config :load-path)))

    (when user-dir
      (setq user-emacs-directory user-dir)
      (setq package-user-dir (expand-file-name "elpa" user-emacs-directory)))

    (when additional-load-paths
      (setq load-path (append load-path additional-load-paths)))

    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
    (package-initialize)
    (dolist (package packages)
      (unless (package-installed-p package)
        (package-install package)))

    (require 'director)))

