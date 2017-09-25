;;; rocktl-shell.el --- Easily create shell tasks for rocktl -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:
(require 'cl-lib)
(require 'shell)
(require 'rocktl-models)

;; -----------------------------------------------------------------------------
;; Task helper

(defun rocktl--make-process-sentinel (instance process event)
  "Create a process sentinel for a task INSTANCE.

PROCESS is the running process.
EVENT is the sent event."
  (cond
   ((string= event "finished\n") (rocktl-set-status instance :finished))
   ((or (string-prefix-p "exited abnormally" event) (string-prefix-p "failed" event))
    (rocktl-set-status instance :failed))
   (t (rocktl-set-status instance :unknown))))

(defun rocktl--make-shell-command (shell-command instance)
  "Create the command for a shell task.

SHELL-COMMAND is a string containing the shell command to run.

INSTANCE is the rocktl instance."
  (let* ((default-directory (rocktl-task-instance-directory instance))
         (name (format "*task:%S*" (rocktl-task-instance-name instance)))
         (buffer (get-buffer-create name)))

    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))

    (make-process
     :name name
     :buffer buffer
     :command (list shell-file-name shell-command-switch shell-command)
     :filter #'comint-output-filter
     :sentinel (apply-partially #'rocktl--make-process-sentinel instance))

    (with-current-buffer buffer
      (shell-mode))
    (switch-to-buffer-other-window buffer)
    (rocktl-set-status instance :running)))

(cl-defun rocktl-make-shell-task (&key name command directory)
  "Define a task that will execute a shell command."
  (make-rocktl-task :name name
                    :command (apply-partially #'rocktl--make-shell-command command)
                    :directory directory))

;; -----------------------------------------------------------------------------
;; Project-local shell task definition

(defgroup rocktl ()
  "Rocktl configuration"
  :group 'applications)

(defcustom rocktl-config-dir (concat (file-name-as-directory user-emacs-directory) "rocktl/")
  "Directory in which Rocktl configuration is registered."
  :group 'rocktl)

(defvar rocktl--shell-tasks '()
  "Shell tasks by directory.")

(defun rocktl--shell-tasks-config-file ()
  "Return the name of the file containing shell tasks."
  (concat rocktl-config-dir "shell-tasks"))

(defconst rocktl--shell-config-template
  "(
 ;; Configuration file for associating shell tasks to a directory.
 ;; The entries are conses in the form (DIRECTORY . TASKS)
 ;;
 ;; TASKS is a list of task definitions.
 ;;
 ;; Example:
 ;;
 ;; (\"/home/my-user\" .
 ;;  ((:name say-hello :command \"echo Hello\")
 ;;   (:name say-goodbye :command \"echo Goodbye\")))
 ;;
 ;; When you modify this file, don't forget to run
 ;; \\[rocktl-refresh-shell-tasks] after saving
 ;;
 (\"/my/directory\" .
  ((:name my-command :command \"echo My first command\")))
 )"
  "Template in case the config file is empty.")

;; -----------------------------------------------------------------------------
;; Read config

(defun rocktl--all-shell-tasks ()
  "Return all the defined shell tasks."
  (let ((config-file (rocktl--shell-tasks-config-file)))
    (if (not (file-exists-p config-file))
        '()
      (with-temp-buffer
        (insert-file-contents config-file)
        (goto-char (point-min))
        (read (current-buffer))))))

;;;###autoload
(defun rocktl-refresh-shell-tasks ()
  "Refresh the shell tasks from configuration."
  (interactive)
  (cl-flet* ((create-task (task-config directory)
                          (rocktl-make-shell-task
                           :name (plist-get task-config :name)
                           :command (plist-get task-config :command)
                           :directory directory))

             (create-task-list (task-config-list directory)
                               (seq-map #'(lambda (task-config) (create-task task-config directory))
                                        task-config-list))

             (create-task-pair (config-pair)
                               (let ((directory (expand-file-name (car config-pair)))
                                     (task-config-list (cdr config-pair)))
                                 (cons directory (create-task-list task-config-list directory))))

             (create-shell-tasks (config)
                                 (seq-map #'create-task-pair config)))
    (setq rocktl--shell-tasks (create-shell-tasks (rocktl--all-shell-tasks)))))


;; -----------------------------------------------------------------------------
;; API

(defun rocktl-shell-tasks-for-path (path)
  "Return the shell tasks defined for a specific PATH."
  (let ((matching-pair (car (seq-filter
                             #'(lambda (pair) (string-prefix-p (car pair) path))
                             rocktl--shell-tasks))))
    (cdr matching-pair)))

(defun rocktl-shell-tasks ()
  "Get shell tasks contextually."
  (rocktl-shell-tasks-for-path (expand-file-name (file-name-as-directory default-directory))))

;;;###autoload
(defun rocktl-config-shell-tasks ()
  "Open the file containing shell tasks configuration."
  (interactive)
  (find-file (rocktl--shell-tasks-config-file))

  (when (= (buffer-size) 0)
    (insert rocktl--shell-config-template))

  (emacs-lisp-mode))

(provide 'rocktl-shell)

;;; rocktl-shell.el ends here


;; Local Variables:
;; nameless-current-name: "rocktl"
;; End:
