# Automating screencasts

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

See [Running in a controlled environment](./running-in-a-controlled-environment) for information about `director-bootstrap.el`.
