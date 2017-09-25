;;; rocktl-status.el --- Task runner status buffer -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:
(require 'seq)

(require 'rocktl-models)

(defun rocktl--status-mode-create-entry (instance)
  "Create a list entry from a task INSTANCE."
  (let ((name (rocktl-task-instance-name instance))
        (status (rocktl-task-instance-status instance))
        (directory (rocktl-task-instance-directory instance)))
    (list name (vector (symbol-name name) directory (symbol-name status)))))

(defun rocktl--status-mode-refresh ()
  "Refresh the status mode content."
  (setq-local tabulated-list-entries
              (seq-map #'rocktl--status-mode-create-entry (rocktl-get-instances)))
  (tabulated-list-print))

(defun rocktl--status-visit-entry ()
  "Visit the entry's buffer at POINT."
  (interactive)
  (let* ((entry (tabulated-list-get-entry))
         (instance (rocktl-find-instance-by-name-and-dir (elt entry 0) (elt entry 1)))
         (buffer (rocktl-task-instance-buffer instance)))
    (when buffer
      (switch-to-buffer-other-window buffer))))

(defvar rocktl-status-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent rocktl-status-mode-map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'rocktl-status-mode-map)
    map))

(define-derived-mode rocktl-status-mode tabulated-list-mode "TASKS"
  "Major mode for managing running tasks."
  (setq-local tabulated-list-format
              '[("Task" 20 t)
                ("Directory" 50 t)
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
