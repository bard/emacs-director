;;; director.el --- Simulate user sessions -*- lexical-binding: t -*-

;; Copyright (C) 2021 Massimiliano Mirra

;; Author: Massimiliano Mirra <hyperstruct@gmail.com>
;; URL: https://github.com/bard/emacs-director
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords:

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Simulate user sessions.

;;; Code:

(defvar director--delay 1)
(defvar director--steps nil)
(defvar director--start-time nil)
(defvar director--counter 0)
(defvar director--error nil)
(defvar director--failure nil)
(defvar director--before-start-function nil)
(defvar director--after-end-function nil)
(defvar director--before-step-function nil)
(defvar director--after-step-function nil)
(defvar director--on-error-function nil)
(defvar director--on-failure-function nil)
(defvar director--log-target nil)
(defvar director--typing-style nil)

(defun director-run (&rest config)
  "Simulate a user session as defined by CONFIG.

CONFIG is a property list containing the following properties and
their values:

- `:version': required number indicating the config format
  version; must be `1'
- `:steps': required list of steps (see below for the step
  format)
- `:before-start' : optional function to run before the first
  step
- `:after-end' optional function to run after the last step 
- `:after-step' optional function to run after every step
- `:on-failure': optional function to run when an `:assert' step
  fails
- `:on-error': optional function to run when a step triggers an
  error
- `:log-target': optional cons cell of the format `(file
  . \"filename\")' specifying a file to save the log to
- `:typing-style': optional symbol changing the way that `:type'
  steps type characters; set to `human' to simulate a human
  typing
- `:delay-between-steps': optional number specifying how many
  seconds to wait after a step; defaults to `1'; set lower for
  automated tests

A step can be one of:

- `:type': simulate typing text; can be a string or a vector of
  key events; if a string, it will be converted to key events
  using `listify-key-sequence' and can contain special
  characters, e.g. `(:type \"\\M-xsetenv\\r\")'
- `:call': shortcut to invoke an interactive command, e.g. `(:call setenv)`
  It's value may be a command symbol or a list of the form:
    (:call (COMMAND ARGS))
  The ARGS must be a string specifying the prefix argument for COMMAND.
  e.g. (:call (org-capture \"C-u C-u\"))
- `:log': Lisp form; it will be evaluated and its result will be
  written to log; e.g. `(:log (buffer-file-name (current-buffer)))'
- `:wait': number; seconds to wait before next step; overrides
  config-wide `:delay-between-steps'
- `:assert': Lisp form; if it evaluates to `nil', execution is
  interrupted and function configured through `:on-failure' is
  called 
- `:suspend': suspend execution; useful for debugging; resume
  using the `director-resume' command"
  (director--read-config config)
  (setq director--start-time (float-time))
  (director--before-start)
  (director--schedule-next))

(defun director--read-config (config)
  (or (map-elt config :version)
      (error "Director: configuration entry `:version' missing"))
  (or (map-elt config :steps)
      (error "Director: configuration entry `:steps' missing"))
  (mapc (lambda (config-entry)
          (pcase config-entry
            (`(:version ,version)
             (or (equal version 1)
                 (error "Invalid :version")))
            (`(:steps ,steps)
             (setq director--steps steps))
            (`(:delay-between-steps ,delay)
             (setq director--delay delay))
            (`(:before-step ,function)
             (setq director--before-step-function function))
            (`(:before-start ,function)
             (setq director--before-start-function function))
            (`(:after-end ,function)
             (setq director--after-end-function function))
            (`(:after-step ,function)
             (setq director--after-step-function function))
            (`(:on-error ,function)
             (setq director--on-error-function function))
            (`(:on-failure ,function)
             (setq director--on-failure-function function))
            (`(:log-target ,target)
             (setq director--log-target target))
            (`(:typing-style ,style)
             (setq director--typing-style style))
            (entry
             (error "Director: invalid configuration entry: `%s'" entry))))
        (seq-partition config 2)))

(defun director--log (message)
  (when director--log-target
    (let ((log-line (format "%06d %03d %s\n"
                            (round (- (* 1000 (float-time))
                                      (* 1000 director--start-time)))
                            director--counter
                            message))
          (target-type (car director--log-target))
          (target-name (cdr director--log-target)))
      (pcase target-type
        ('buffer
         (with-current-buffer (get-buffer-create target-name)
           (goto-char (point-max))
           (insert log-line)))
        ('file
         (let ((save-silently t))
           (append-to-file log-line nil target-name)))
        (_
         (error "Unrecognized log target type: %S" target-type))))))

(defun director--schedule-next (&optional delay-override)
  (cond
   (director--error
    (director--log (format "ERROR %S" director--error))
    (run-with-timer director--delay nil 'director--end))

   (director--failure
    (director--log (format "FAILURE: %S" director--failure))
    (run-with-timer director--delay nil 'director--end))

   ((length= director--steps 0)
    ;; Run after-step callback for last step
    (director--after-step)
    (run-with-timer director--delay nil 'director--end))

   (t
    (unless (eq director--counter 0)
      (director--after-step))
    (let* ((next-step (car director--steps))
           (delay (cond (delay-override delay-override)
                        ((and (listp next-step)
                              (member (car next-step) '(:call :type)))
                         director--delay)
                        (t 0.05))))
      (run-with-timer delay
                      nil
                      (lambda ()
                        (director--before-step)
                        (director--exec-step-then-next)))))))

(defun director--command-prefix-arg (args)
  "Return `current-prefix-arg' value from ARGS string."
  (let (value list negative)
    (mapc (lambda (token)
            (pcase token
              ((or "C-u" "c-u")
               (unless value (setq value 1))
               (setq value (* value 4)
                     list t))
              ("-" (setq negative t
                         list nil
                         value '-))
              ;;assumed numeric arg
              (_ (let ((n (string-to-number token)))
                   (setq value (if negative (- n) n)
                         list nil)))))
          (split-string args " " t " "))
    (if list (list value) value)))

(defun director--exec-step-then-next ()
  (let ((step (car director--steps)))
    (setq director--counter (1+ director--counter)
          director--steps (cdr director--steps))
    (director--log (format "STEP %S" step))
    (condition-case err
        (pcase step
          (`(:call ,command)
           ;; Next step must be scheduled before executing the command, because
           ;; the command might block (e.g. when requesting input) in which case
           ;; we'd never get to schedule the step.
           (director--schedule-next)
           (let (arg)
             (when (listp command)
               (setq arg (director--command-prefix-arg (cadr command))
                     command (car command)))
             (let ((current-prefix-arg arg))
               (call-interactively command))))

          (`(:log ,form)
           (director--schedule-next)
           (director--log (format "LOG %S" (eval form))))

          (`(:type ,key-sequence)
           (if (eq director--typing-style 'human)
               (director--simulate-human-typing
                (listify-key-sequence key-sequence)
                'director--schedule-next)
             (director--schedule-next)
             (setq unread-command-events
                   (listify-key-sequence key-sequence))))

          (`(:wait ,delay)
           (director--schedule-next delay))

          (`(:suspend)
           nil)
          
          (`(:assert ,condition)
           (director--schedule-next)
           (or (eval condition)
               (setq director--failure condition)))

          (step
           (director--schedule-next)
           (error "Unrecognized step: %S" step)))

      ;; Save error so that already scheduled step can handle it
      (error (setq director--error err)))))

(defun director--simulate-human-typing (command-events callback)
  (if command-events
      (let* ((base-delay-ms 50)
             (random-variation-ms (- (random 50) 25))
             (delay-s (/ (+ base-delay-ms random-variation-ms) 1000.0)))
        (setq unread-command-events (list (car command-events)))
        (run-with-timer delay-s nil 'director--simulate-human-typing (cdr command-events) callback))
    (funcall callback)))

;;; Hooks

(defun director--before-step ()
  (when director--before-step-function
    (funcall director--before-step-function)))

(defun director--after-step ()
  (when director--after-step-function
    (funcall director--after-step-function)))

(defun director--before-start ()
  (when director--before-start-function
    (funcall director--before-start-function)))

(defun director--end ()
  (director--log "END")
  (setq director--counter 0)
  (setq director--start-time nil)
  (cond
   ((and director--error director--on-error-function)
    ;; Give time to the current event loop iteration to finish
    ;; in case the on-error hook is a `kill-emacs'
    (setq director--error nil)
    (run-with-timer 0.05 nil director--on-error-function))
   ((and director--failure director--on-failure-function)
    (setq director--failure nil)
    (run-with-timer 0.05 nil director--on-failure-function))
   (director--after-end-function
    (run-with-timer 0.05 nil director--after-end-function))))

;;; Utilities

;; Use to capture a "screenshot" when running under screen:
;;
;;   :after-step (lambda ()
;;                 (director-capture-screen "snapshots/scenario-1/snapshot.%d"))

(defun director-capture-screen (file-name-pattern)
  (let ((capture-directory (file-name-directory file-name-pattern))
        (file-name-pattern (or file-name-pattern
                               (concat temporary-file-directory
                                       "director-capture.%d"))))
    (make-directory capture-directory t)
    (call-process "screen"
                  nil nil nil
                  "-X" "hardcopy" (format file-name-pattern
                                          director--counter))))

(defun director-resume ()
  "Resume from a `(:suspend)' step."
  (interactive)
  (director--schedule-next))

;;; Meta

(provide 'director)

;;; director.el ends here
