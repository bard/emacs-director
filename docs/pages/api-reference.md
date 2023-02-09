# API

<!-- elisp-docgen-start (:symbols (director-run director-resume)) -->

## Function: `(director-run &REST CONFIG)`

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

## Function: `(director-resume)`

Resume from a `(:suspend)` step.

<!-- elisp-docgen-end -->
