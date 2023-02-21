# Quickstart

1. Download [director-bootstrap.el](https://github.com/bard/emacs-director/tree/master/util/director-bootstrap.el).

2. Download [demo.el](https://github.com/bard/emacs-director/tree/master/examples/demo/demo.el):

```lisp
(director-bootstrap
 :user-dir "/tmp/director-demo")

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

3. Launch the demo:

```sh
$ emacs -Q -nw -l director-bootstrap.el -l demo.el
```

4. Emacs starts and plays:

![](/demo.gif)

5. To write your own scripts, you can use `demo.el` as starting point. Unless you're hacking on Director itself, `director-bootstrap.el` is best left unmodified.

6. See the [API reference](/api/reference) for things you can do in a script. See the guides about [automating screencasts](/automating-screencasts) and [end-to-end testing](/end-to-end-testing) for specific use cases.
