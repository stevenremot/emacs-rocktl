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

;; -----------------------------------------------------------------------------
;; Models

(cl-defstruct rocktl-task name command directory)

(cl-defstruct rocktl-task-instance task status buffer)

(defun rocktl-set-status (instance new-status)
  "Update INSTANCE's status to NEW-STATUS."
  (setf (rocktl-task-instance-status instance) new-status))

(defun rocktl-set-buffer (instance new-buffer)
  "Update INSTANCE's buffer to NEW-BUFFER."
  (setf (rocktl-task-instance-buffer instance) new-buffer))

(defun rocktl-task-instance-name (instance)
  "Return the name of the task associated to an INSTANCE."
  (rocktl-task-name (rocktl-task-instance-task instance)))

(defun rocktl-task-instance-directory (instance)
  "Return the directory of the task associated to an INSTANCE."
  (rocktl-task-directory (rocktl-task-instance-task instance)))

;; -----------------------------------------------------------------------------
;; API

(defun rocktl-task= (task1 task2)
  "Return t when TASK1 and TASK2 are equal."
  (and (string= (rocktl-task-name task1) (rocktl-task-name task2))
       (string= (rocktl-task-directory task1) (rocktl-task-directory task2))))

(defun rocktl-get-instances ()
  "Return all the current task instances."
  rocktl--task-instances)

(defun rocktl-find-task (task-list name)
  "Return the task in TASK-LIST with the specified NAME."
  (let ((predicate (lambda (task) (eql name (rocktl-task-name task)))))
    (car (seq-filter predicate task-list))))

(defun rocktl-get-task-names (task-list)
  "Return all the available tasks' names in TASK-LIST."
  (seq-map #'rocktl-task-name task-list))

(defun rocktl-find-instance (task)
  "Return an instance associated to TASK."
  (let ((predicate (lambda (instance) (rocktl-task= task (rocktl-task-instance-task instance)))))
    (car (seq-filter predicate rocktl--task-instances))))

(defun rocktl-get-instance-for (task)
  "Return a properly initialized instance for TASK."
  (let ((instance (rocktl-find-instance task)))
    (unless instance
      (setq instance (make-rocktl-task-instance :task task))
      (add-to-list 'rocktl--task-instances instance))
    instance))

(defun rocktl-find-instance-by-name-and-dir (name directory)
  "Return the instance associated to NAME and DIRECTORY's task."
  (let ((predicate (lambda (instance)
                     (and (string= name (rocktl-task-instance-name instance))
                          (string= directory (rocktl-task-instance-directory instance))))))
    (car (seq-filter predicate rocktl--task-instances))))

(provide 'rocktl-models)

;;; rocktl-models.el ends here

;; Local Variables:
;; nameless-current-name: "rocktl"
;; End:
