# End-to-end testing

See [run-command's test scenarios](https://github.com/bard/emacs-run-command/tree/master/test) for an example.

- `run.sh` launches a headless (via `screen`) Emacs instance for each `scenario-*.el` file
  - `scenario-*.el` loads `setup.el` to perform common setup and load fixtures
    - `setup.el` uses `director-bootstrap` to create a controlled testing environment in `/tmp` and install dependencies
  - `scenario-*.el` invokes `director-run` with instructions to run the scenario

See [Running in a controlled environment](./running-in-a-controlled-environment) for information about `director-bootstrap.el`.
