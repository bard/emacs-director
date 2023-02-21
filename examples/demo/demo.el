;; Run with:
;;
;;   emacs -Q -nw -l ../../util/director-bootstrap.el -l demo.el

(director-bootstrap
 :user-dir "/tmp/director-demo" )

(director-run
 :version 1
 :before-start (lambda ()
                 (switch-to-buffer (get-buffer-create "*example*"))
                 (menu-bar-mode -1)
                 (setq python-indent-guess-indent-offset nil)
                 (python-mode))
 :steps '((:call run-python)
          (:type "\C-xo")
          (:type "def greet():\r")
          (:type "print(\"hello, world!\")")
          (:type "\M-x")
          (:type "python-shell-send-defun")
          (:type [return])
          (:type "\C-xo")
          (:type "greet()\r"))
 :typing-style 'human
 :delay-between-steps 1
 :after-end (lambda () (kill-emacs 0))
 ;; give useful feedback during development
 :on-error (lambda (err) (message "Error while executing director script: %S" err)))
