# Running in a controlled environment

You can create and refine simulate sessions in your everyday interactive Emacs,
but often you'll want to run them in a minimal, reproducible environment, either
for correctness (tests) or clarity (demos).

Preparing such environment may consist of:

- setting a `user-directory` other than your `.emacs.d`
- initializing the package system and downloading dependencies
- adding local paths to the `load-path`
- loading Director itself

As a convenience, you can copy
[util/director-bootstrap.el](https://github.com/bard/emacs-director/blob/master/util/director-bootstrap.el) to your project, and
invoke `director-bootstrap` before `director-run`:

```lisp
(director-bootstrap
 :user-dir "/tmp/my-package-test"
 :packages '(some-package-we-depend-on some-other-package)
 :load-path '("/path/to/director" ;; will move to :packages once director is on MELPA
              "/path/to/my/package")))
```

Load it before everything else with:

```sh
$ emacs -Q -nw -l director-bootstrap.el -l my-session-script.el
```

It would be nice if Director itself were able to provide bootstrapping, but
since making Director available is part of the bootstrapping, there's an obvious
chicken-and-egg problem.
