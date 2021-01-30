;; -*- lexical-binding: t -*-

(defvar director--delay 0.1)
(defvar director--steps nil)
(defvar director--start-time nil)
(defvar director--counter 0)
(defvar director--log-buffer-name "*director-log*")
(defvar director--before-step-function nil)

(defun director-capture-screen (&optional file-name-pattern)
  (lambda ()
    (let ((file-name-pattern (or file-name-pattern
                                 (concat temporary-file-directory
                                         "director-capture.%d"))))
      (call-process "screen"
                    nil nil nil
                    "-X" "hardcopy" (format file-name-pattern
                                            director--counter)))))

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
  (run-with-timer director--delay nil 'director--next))

(defun director--log (message)
  (with-current-buffer (get-buffer-create director--log-buffer-name)
    (goto-char (point-max))
    (insert
     (format "%06d | %02d: %s\n"
             (round (* 1000 (- (float-time)
                               director--start-time)))
             director--counter
             message))))

(defun director--next ()
  (let ((current-step (car director--steps))
        (remaining-steps (cdr director--steps)))
    (when director--before-step-function
      (funcall director--before-step-function))
    (when current-step
      (setq director--counter (1+ director--counter)
            director--steps remaining-steps)
      (director--log (format "%S" current-step))
      (run-with-timer director--delay nil 'director--next)
      (if (symbolp current-step)
          (call-interactively current-step)
        (setq unread-command-events (cond
                                     ((stringp current-step)
                                      (listify-key-sequence current-step))
                                     ((vectorp current-step)
                                      (append current-step nil))
                                     (t (error "Unrecognized step format"))))))))

(provide 'director)
