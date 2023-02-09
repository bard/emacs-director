# Quickstart

1. Clone this repository.

2. Review [examples/demo/demo.el](https://github.com/bard/emacs-director/tree/master/examples/demo/demo.el):

```lisp
(director-bootstrap
 :user-dir "/tmp/director-demo"
 :packages '()
 :load-path '("../.."))

(director-run
 :version 1
 :before-start (lambda ()
                 (switch-to-buffer (get-buffer-create "*example*"))
                 (menu-bar-mode -1)
                 (setq python-indent-guess-indent-offset nil)
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
 :delay-between-steps 1
 :after-end (lambda () (kill-emacs 0))
 ;; give useful feedback during development
 :on-error (lambda (err) (message "Error while executing director script: %S" err)))
```

3. Launch it:

```sh
$ cd examples/demo
$ emacs -Q -nw -l ../../util/director-bootstrap.el -l demo.el
```

4. Emacs starts and plays:

![](/demo.gif)
