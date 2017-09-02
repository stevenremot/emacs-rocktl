;;; task-runner-models.el --- Models of task-runner package -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:
(require 'cl-lib)
(require 'seq)

;; -----------------------------------------------------------------------------
;; Private vars

(defvar task-runner--task-instances '()
  "A list of all tasks currently instanciated.")

(defvar task-runner--tasks '()
  "The list of the defined tasks.")

;; -----------------------------------------------------------------------------
;; Models



(cl-defstruct task-runner-task name command)

(cl-defun task-runner-define-task (&key name command)
  "Create a new runnable task."
  (add-to-list 'task-runner--tasks (make-task-runner-task :name name :command command)))

(cl-defstruct task-runner-task-instance task status)

(defun task-runner-set-status (instance new-status)
  "Update INSTANCE's status to NEW-STATUS."
  (setf (task-runner-task-instance-status instance) new-status))

(defun task-runner-task-instance-name (instance)
  "Return the name of the task associated to an INSTANCE."
  (task-runner-task-name (task-runner-task-instance-task instance)))


;; -----------------------------------------------------------------------------
;; API

(defun task-runner-get-instances ()
  "Return all the current task instances."
  task-runner--task-instances)

(defun task-runner-find-task (name)
  "Return the task with the specified NAME."
  (let ((predicate (lambda (task) (eql name (task-runner-task-name task)))))
    (car (seq-filter predicate task-runner--tasks))))

(defun task-runner-get-task-names ()
  "Return all the available tasks' names."
  (seq-map #'task-runner-task-name task-runner--tasks))

(defun task-runner-find-instance (task)
  "Return an instance associated to TASK."
  (let ((predicate (lambda (instance) (eql task (task-runner-task-instance-task instance)))))
    (car (seq-filter predicate task-runner--task-instances))))

(defun task-runner-get-instance-for (task)
  "Return a properly initialized instance for TASK."
  (let ((instance (task-runner-find-instance task)))
    (unless instance
      (setq instance (make-task-runner-task-instance :task task))
      (add-to-list 'task-runner--task-instances instance))
    instance))

(provide 'task-runner-models)

;;; task-runner-models.el ends here

;; Local Variables:
;; nameless-current-name: "task-runner"
;; End:
