# Director: Simulate Emacs user sessions

Director drives an Emacs session from the point of view of the user. It can be
used for end-to-end testing, hands-free screencast recording, probably more.

Director is similar in spirit to web tools such as Selenium Webdriver. It is
_not_ a general purpose solution for Emacs automation; use Lisp for that.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

**Table of Contents**

- [Status](#status)
- [Quickstart](#quickstart)
- [Usage](#usage)
  - [Recording screencasts](#recording-screencasts)
  - [End-to-end testing](#end-to-end-testing)
  - [Running in a controlled environment](#running-in-a-controlled-environment)
- [Debugging](#debugging)
- [API reference](#api-reference)
  - [Command: `director-resume`](#command-director-resume)
  - [Function: `director-run`](#function-director-run)
- [Limitations and further development](#limitations-and-further-development)

<!-- markdown-toc end -->

## Status

Developer preview, low-level API only. See
[limitations](#limitations-and-further-development). Currently used in
[run-command](https://github.com/bard/emacs-run-command).

## Quickstart

1. Clone this repository.

2. Review [examples/demo/demo.el](examples/demo/demo.el):

```emacs-lisp
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

![](examples/demo/demo.gif)

## Usage

### Recording screencasts

1. [Install asciicast](https://asciinema.org/docs/installation) and an asciicast-to-gif converter such as [agg](https://github.com/asciinema/agg)
2. Write a session script (see [demo.el](examples/demo/demo.el) in this repository for a minimal example, or [run-command's demo.el](https://github.com/bard/emacs-run-command/tree/master/demo/demo.el) for a real-life example) and save it as e.g. `my-session-script.el`
3. Launch with:

```sh
$ asciinema rec demo.cast -c 'emacs -nw -Q -l director-bootstrap.el -l my-session-script.el'
```

4. Review the recording with:

```sh
asciinema play demo.cast
```

5. Convert to a gif with:

```sh
agg demo.cast demo.gif
```

See [below](#running-in-a-controlled-environment) for information about `director-bootstrap.el`.

### End-to-end testing

See [run-command's test scenarios](https://github.com/bard/emacs-run-command/tree/master/test) for an example.

- `run.sh` launches a headless (via `screen`) Emacs instance for each `scenario-*.el` file
  - `scenario-*.el` loads `setup.el` to perform common setup and load fixtures
    - `setup.el` uses `director-bootstrap` to create a controlled testing environment in `/tmp` and install dependencies
  - `scenario-*.el` invokes `director-run` with instructions to run the scenario

See [below](#running-in-a-controlled-environment) for information about `director-bootstrap.el`.

### Running in a controlled environment

You can create and refine simulate sessions in your everyday interactive Emacs,
but often you'll want to run them in a minimal, reproducible environment, either
for correctness (tests) or clarity (demos).

Preparing such environment may consist of:

- setting a `user-directory` other than your `.emacs.d`
- initializing the package system and downloading dependencies
- adding local paths to the `load-path`
- loading Director itself

As a convenience, you can copy
[util/director-bootstrap.el](util/director-bootstrap.el) to your project, and
invoke `director-bootstrap` before `director-run`:

```emacs-lisp
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

## Debugging

Debugging strategies are rudimentary for now:

- assign a value of `(lambda (err) (message "Error while executing director script: %S" err))` to the `:on-error` property
- if you're running a headless session under `screen -D -m`, run a visible one instead
- increase `:delay-between-steps` to see what's going on
- set a `:log-target` file and `tail -f` it
- sprinkle the script with `(:log FORM)` steps
- add a `(:suspend)` step, inspect the session interactively, resume with `M-x director-resume`

## API reference

<!-- elisp-docgen-start (:symbols (director-run director-resume)) -->

#### Function: `(director-run &REST CONFIG)`

Simulate a user session as defined by CONFIG.

CONFIG is a property list containing the following properties and
their values:

- `:version`: required number indicating the config format
  version; must be `1`
- `:steps`: required list of steps (see below for the step
  format)
- `:before-start` : optional function to run before the first
  step
- `:after-end` optional function to run after the last step
- `:after-step` optional function to run after every step
- `:on-failure`: optional function to run when an `:assert` step
  fails
- `:on-error`: optional function to run when a step triggers an
  error; receives the error as argument
- `:log-target`: optional cons cell of the format `(file . "filename")` specifying a file to save the log to
- `:typing-style`: optional symbol changing the way that `:type`
  steps type characters; set to `human` to simulate a human
  typing
- `:delay-between-steps`: optional number specifying how many
  seconds to wait after a step; defaults to `1`; set lower for
  automated tests

A step can be one of:

- `:type`: simulate typing text; can be a string or a vector of
  key events; if a string, it will be converted to key events
  using `listify-key-sequence` and can contain special
  characters, e.g. `(:type "\M-xsetenv\r")`
- `:call`: shortcut to invoke an interactive command, e.g. `(:call setenv)`
- `:eval`: Lisp form; it will be evaluated
- `:log`: Lisp form; it will be evaluated and its result will be
  written to log; e.g. `(:log (buffer-file-name (current-buffer)))`
- `:wait`: number; seconds to wait before next step; overrides
  config-wide `:delay-between-steps`
- `:assert`: Lisp form; if it evaluates to nil, execution is
  interrupted and function configured through `:on-failure` is
  called
- `:suspend`: suspend execution; useful for debugging; resume
  using the `director-resume` command

#### Function: `(director-resume)`

Resume from a `(:suspend)` step.

<!-- elisp-docgen-end -->
