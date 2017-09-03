;;; rocktl.el --- General task runner -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:
(require 'rocktl-models)
(require 'rocktl-status)
(require 'rocktl-shell)

;;;###autoload
(defun rocktl-run-task (name)
  "Run the task having the specified NAME."
  (interactive
   (list (completing-read "Command:" (rocktl-get-task-names) nil t)))
  (let ((task (rocktl-find-task (intern name)))
        command
        instance)
    (unless task
      (error "Couldn't find task named %s" name))

    (setq command (rocktl-task-command task)
          instance (rocktl-get-instance-for task))

    (funcall command instance)))

(provide 'rocktl)

;;; rocktl.el ends here
