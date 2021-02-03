;; -*- lexical-binding: t -*-

(defvar director--delay 0.1)
(defvar director--steps nil)
(defvar director--start-time nil)
(defvar director--counter 0)
(defvar director--error nil)
(defvar director--log-buffer-name "*director-log*")
(defvar director--before-step-function nil)
(defvar director--on-error nil)

(defun director-start (&rest config)
  (or (setq director--steps (plist-get config :steps))
      (error "Must provide steps"))
  (when (plist-member config :delay-between-steps)
    (setq director--delay (plist-get config :delay-between-steps)))
  (when (plist-member config :before-step-function)
    (setq director--before-step-function (plist-get config :before-step-function)))
  (when (plist-member config :on-error)
    (setq director--on-error (plist-get config :on-error)))
  (with-current-buffer (get-buffer-create director--log-buffer-name)
    (erase-buffer))
  (setq director--start-time (float-time))
  (run-with-timer director--delay nil 'director--exec-step-then-next))

(defun director-capture-screen (&optional file-name-pattern)
  (lambda ()
    (let ((capture-directory (file-name-directory file-name-pattern))
          (file-name-pattern (or file-name-pattern
                                 (concat temporary-file-directory
                                         "director-capture.%d"))))
      (make-directory capture-directory t)
      (call-process "screen"
                    nil nil nil
                    "-X" "hardcopy" (format file-name-pattern
                                            director--counter)))))

(defun director--after-last-step ()
  (director--log "END")
  (setq director--counter 0
        director--start-time nil
        director--error nil))

(defun director--log (message)
  (with-current-buffer (get-buffer-create director--log-buffer-name)
    (goto-char (point-max))
    (insert
     (format "%06d %03d %s\n"
             (round (- (* 1000 (float-time))
                       (* 1000 director--start-time)))
             director--counter
             message))))

(defun director--exec-step-then-next ()
  (cond
   (director--error
    (director--log (format "ERROR %S" director--error))
    (director--after-last-step)
    (when director--on-error
      (funcall director--on-error)))
   ((length= director--steps 0)
    (director--after-last-step))
   (t
    (let ((step (car director--steps))
          (remaining-steps (cdr director--steps)))
      (when director--before-step-function
        (funcall director--before-step-function))
      (setq director--counter (1+ director--counter)
            director--steps remaining-steps)
      (director--log (format "STEP %S" step))
      (condition-case err
          (cond
           ((and (listp step) (plist-member step :call))
            (run-with-timer director--delay nil 'director--exec-step-then-next)
            (call-interactively (plist-get step :call)))
           ((and (listp step) (plist-member step :debug))
            (run-with-timer director--delay nil 'director--exec-step-then-next)
            (director--log (format "DEBUG %S" (eval (plist-get step :debug)))))
           ((and (listp step) (plist-member step :type))
            (run-with-timer director--delay nil 'director--exec-step-then-next)
            (setq unread-command-events
                  (listify-key-sequence (plist-get step :type))))
           ((and (listp step) (plist-member step :wait))
            (run-with-timer (plist-get step :wait) nil 'director--exec-step-then-next))
           ((and (listp step) (plist-member step :assert))
            (let ((assertion (plist-get step :assert)))
              (run-with-timer director--delay nil 'director--exec-step-then-next)
              (or (eval assertion)
                  (error "Expectation failed: `%S'" assertion))))
           (t
            (run-with-timer director--delay nil 'director--exec-step-then-next)
            (error "Unrecognized step: `%S'" step)))
        ;; Save error so that already scheduled step can handle it
        (error (setq director--error err)))))))

(provide 'director)
