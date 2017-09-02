;;; rocktl-status.el --- Task runner status buffer -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:
(require 'seq)

(require 'rocktl-models)

(defun rocktl--status-mode-create-entry (instance)
  "Create a list entry from a task INSTANCE."
  (let ((name (rocktl-task-instance-name instance))
        (status (rocktl-task-instance-status instance)))
    (list name (vector (symbol-name name) (symbol-name status)))))

(defun rocktl--status-mode-refresh ()
  "Refresh the status mode content."
  (setq-local tabulated-list-entries
              (seq-map #'rocktl--status-mode-create-entry (rocktl-get-instances)))
  (tabulated-list-print))

(define-derived-mode rocktl-status-mode tabulated-list-mode "TASKS"
  "Major mode for managing running tasks."
  (setq-local tabulated-list-format
              '[("Task" 50 t)
                ("Status" 0 t :right-align t)])
  (setq-local tabulated-list-revert-hook #'rocktl--status-mode-refresh)
  (rocktl--status-mode-refresh)
  (tabulated-list-init-header))

;;;###autoload
(defun rocktl-status ()
  "Show the task runner status buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*rocktl:instances*")
    (rocktl-status-mode)
    (switch-to-buffer (current-buffer))))

(provide 'rocktl-status)
;;; rocktl-status.el ends here

;; Local Variables:
;; nameless-current-name: "rocktl"
;; End:
