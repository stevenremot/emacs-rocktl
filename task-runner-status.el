;;; task-runner-status.el --- Task runner status buffer -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:
(require 'seq)

(require 'task-runner-models)

(defun task-runner--status-mode-create-entry (instance)
  "Create a list entry from a task INSTANCE."
  (let ((name (task-runner-task-instance-name instance))
        (status (task-runner-task-instance-status instance)))
    (list name (vector (symbol-name name) (symbol-name status)))))

(defun task-runner--status-mode-refresh ()
  "Refresh the status mode content."
  (setq-local tabulated-list-entries
              (seq-map #'task-runner--status-mode-create-entry (task-runner-get-instances)))
  (tabulated-list-print))

(define-derived-mode task-runner-status-mode tabulated-list-mode "TASKS"
  "Major mode for managing running tasks."
  (setq-local tabulated-list-format
              '[("Task" 50 t)
                ("Status" 0 t :right-align t)])
  (setq-local tabulated-list-revert-hook #'task-runner--status-mode-refresh)
  (task-runner--status-mode-refresh)
  (tabulated-list-init-header))

;;;###autoload
(defun task-runner-status ()
  "Show the task runner status buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*task-runner:instances*")
    (task-runner-status-mode)
    (switch-to-buffer (current-buffer))))

(provide 'task-runner-status)
;;; task-runner-status.el ends here

;; Local Variables:
;; nameless-current-name: "task-runner"
;; End:
