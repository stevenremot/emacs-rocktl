;;; rocktl-models.el --- Models of rocktl package -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:
(require 'cl-lib)
(require 'seq)

;; -----------------------------------------------------------------------------
;; Private vars

(defvar rocktl--task-instances '()
  "A list of all tasks currently instanciated.")

(defvar rocktl--tasks '()
  "The list of the defined tasks.")

;; -----------------------------------------------------------------------------
;; Models



(cl-defstruct rocktl-task name command)

(cl-defun rocktl-define-task (&key name command)
  "Create a new runnable task."
  (add-to-list 'rocktl--tasks (make-rocktl-task :name name :command command)))

(cl-defstruct rocktl-task-instance task status)

(defun rocktl-set-status (instance new-status)
  "Update INSTANCE's status to NEW-STATUS."
  (setf (rocktl-task-instance-status instance) new-status))

(defun rocktl-task-instance-name (instance)
  "Return the name of the task associated to an INSTANCE."
  (rocktl-task-name (rocktl-task-instance-task instance)))


;; -----------------------------------------------------------------------------
;; API

(defun rocktl-get-instances ()
  "Return all the current task instances."
  rocktl--task-instances)

(defun rocktl-find-task (name)
  "Return the task with the specified NAME."
  (let ((predicate (lambda (task) (eql name (rocktl-task-name task)))))
    (car (seq-filter predicate rocktl--tasks))))

(defun rocktl-get-task-names ()
  "Return all the available tasks' names."
  (seq-map #'rocktl-task-name rocktl--tasks))

(defun rocktl-find-instance (task)
  "Return an instance associated to TASK."
  (let ((predicate (lambda (instance) (eql task (rocktl-task-instance-task instance)))))
    (car (seq-filter predicate rocktl--task-instances))))

(defun rocktl-get-instance-for (task)
  "Return a properly initialized instance for TASK."
  (let ((instance (rocktl-find-instance task)))
    (unless instance
      (setq instance (make-rocktl-task-instance :task task))
      (add-to-list 'rocktl--task-instances instance))
    instance))

(provide 'rocktl-models)

;;; rocktl-models.el ends here

;; Local Variables:
;; nameless-current-name: "rocktl"
;; End:
