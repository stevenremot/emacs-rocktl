;;; rocktl-shell.el --- Easily create shell tasks for rocktl -*- lexical-binding: t -*-

;;; Commentary:
;; 
;;; Code:
(require 'cl-lib)
(require 'rocktl-models)

(cl-defun rocktl-define-shell-task (&key name command)
  "Define a task that will execute a shell command."
  (rocktl-define-task :name name
                :command #'(lambda (instance)
                             (async-shell-command command)
                             (rocktl-set-status instance :running))))

(provide 'rocktl-shell)

;;; rocktl-shell.el ends here


;; Local Variables:
;; nameless-current-name: "rocktl"
;; End:
