;;; task-runner.el --- General task runner -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:
(require 'task-runner-models)
(require 'task-runner-status)

;;;###autoload
(defun task-runner-run-task (name)
  "Run the task having the specified NAME."
  (interactive
   (list (completing-read "Command:" (task-runner-get-task-names) nil t)))
  (let ((task (task-runner-find-task (intern name)))
        command
        instance)
    (unless task
      (error "Couldn't find task named %s" name))

    (setq command (task-runner-task-command task)
          instance (task-runner-get-instance-for task))

    (funcall command instance)))

(provide 'task-runner)

;;; task-runner.el ends here
