;;; rocktl.el --- General task runner -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:
(require 'rocktl-models)
(require 'rocktl-status)
(require 'rocktl-shell)

;;;###autoload
(defun rocktl-run-task (name)
  "Run the task having the specified NAME.

If the task is already started, go to its buffer."
  (interactive
   (list (completing-read "Command:" (rocktl-get-task-names (rocktl-shell-tasks)) nil t)))
  (let ((task (rocktl-find-task (rocktl-shell-tasks) (intern name)))
        command
        instance)
    (unless task
      (error "Couldn't find task named %s" name))

    (setq command (rocktl-task-command task)
          instance (rocktl-get-instance-for task))

    (if (rocktl-is-running? instance)
        (rocktl-switch-to-instance instance)
      (funcall command instance))))


;; Initial tasks loading
(rocktl-refresh-shell-tasks)

(provide 'rocktl)

;;; rocktl.el ends here
