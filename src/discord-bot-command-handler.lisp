
(in-package #:discord-bot-command-handler)



;;;; command-handler ---------------------------------------

(defun command-handler ()
  (let ((queue (make-queue)))
    (lambda (command &optional value)
      (case command
	(:enqueue
	 (push-queue value queue))
	(:dequeue
	 (pop-queue queue))
	(:clear
	 (setf queue (make-queue)))))))



(defparameter *command-handler* (command-handler))


(defun make-command-list (lst)
  (cons 'list (mapcar #'(lambda (arg)
			  (if (listp arg)
			      (make-command-list arg)
			      (identity arg)))
		      lst)))

(defmacro add-command (value)
  (let ((value-list (make-command-list value)))
    `(funcall *command-handler* :enqueue ,value-list)))


(defun get-command ()
  (funcall *command-handler* :dequeue))

(defun clear-command ()
  (funcall *command-handler* :clear))

;;;; ---------------------------------------------------------
