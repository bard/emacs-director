# Debugging

Debugging strategies are rudimentary for now:

- assign a value of `(lambda (err) (message "Error while executing director script: %S" err))` to the `:on-error` property
- if you're running a headless session under `screen -D -m`, run a visible one instead
- increase `:delay-between-steps` to see what's going on
- set a `:log-target` file and `tail -f` it
- sprinkle the script with `(:log FORM)` steps
- add a `(:suspend)` step, inspect the session interactively, resume with `M-x director-resume`
