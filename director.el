;; -*- lexical-binding: t -*-

(defvar director--delay 0.1)
(defvar director--steps nil)
(defvar director--start-time nil)
(defvar director--counter 0)
(defvar director--error nil)
(defvar director--log-buffer-name "*director-log*")
(defvar director--before-step-function nil)

(defun director-start (&rest config)
  (or (setq director--steps (plist-get config :steps))
      (error "Must provide steps"))
  (when (plist-member config :delay-between-steps)
    (setq director--delay (plist-get config :delay-between-steps)))
  (when (plist-member config :before-step-function)
    (setq director--before-step-function (plist-get config :before-step-function)))  
  (with-current-buffer (get-buffer-create director--log-buffer-name)
    (erase-buffer))
  (setq director--start-time (float-time))  
  (run-with-timer director--delay nil 'director--exec-step-then-next))

(defun director-capture-screen (&optional file-name-pattern)
  (lambda ()
    (let ((file-name-pattern (or file-name-pattern
                                 (concat temporary-file-directory
                                         "director-capture.%d"))))
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
     (format "%06d | %02d: %s\n"
             (round (- (* 1000 (float-time))
                       (* 1000 director--start-time)))
             director--counter
             message))))

(defun director--exec-step-then-next ()
  (cond
   (director--error
    (director--log (format "ERR: %S" director--error))
    (director--after-last-step))
   ((length= director--steps 0)
    (director--after-last-step))
   (t
    (let ((step (car director--steps))
          (remaining-steps (cdr director--steps)))
      (when director--before-step-function
        (funcall director--before-step-function))
      (setq director--counter (1+ director--counter)
            director--steps remaining-steps)
      (director--log (format "STEP: %S" step))
      ;; Schedule to run again after `cond'
      (run-with-timer director--delay nil 'director--exec-step-then-next)
      (condition-case err
          (cond
           ((and (listp step) (plist-member step :call))
            (call-interactively (plist-get step :call)))
           ((and (listp step) (plist-member step :type))
            (setq unread-command-events
                  (listify-key-sequence (plist-get step :type))))
           ((stringp step)
            (setq unread-command-events
                  (listify-key-sequence step)))
           ((vectorp step)
            (setq unread-command-events
                  (append step nil)))
           (t (error "Unrecognized step format: `%S'" step)))
        ;; Save error so that already scheduled step can handle it
        (error (setq director--error err)))))))

(provide 'director)
