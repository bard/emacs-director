(load-file "setup.el")

(setup-env :user-dir "/tmp/director-demo"
           :packages '(python)
           :load-path '("~/projects/emacs-director"))

(require 'director)

(director-run
 :before-start (lambda ()
                 (setq python-indent-guess-indent-offset nil)
                 (switch-to-buffer (get-buffer-create "/tmp/example.py"))
                 (python-mode))
 :steps '((:call run-python)
          (:type "def greet():\r")
          (:type "print(\"hello, world!\")")
          (:type "\M-x")
          (:type "python-shell-send-defun")
          (:type [return])
          (:type "\C-xo")
          (:type "greet()\r"))
 :typing-style 'human
 :log-target '(file . "director.log")
 :delay-between-steps 1
 :after-end (lambda () (kill-emacs 0))
 :on-error (lambda () (kill-emacs 1))
 :on-failure (lambda () (kill-emacs 1)))

