
(in-package #:discord-bot-define-generic)


;;;; --------------------------------------------------------------
;;;; generic関数の定義 --------------------------------------------

(defgeneric bot-command (key arg))

;;;; --------------------------------------------------------------
;;;; main-command -------------------------------------------------


(defmacro defcommand (bot-type command arg-let &body body)
  (let ((type (gensym))
	(key (gensym)))
    `(defmethod bot-command ((,type (eql ,bot-type)) (,key (eql ,command)) ,arg-let)
       ,@body)))


(defun make-command-list (lst)
  (cons 'list (mapcar #'(lambda (arg)
			  (if (listp arg)
			      (make-command-list arg)
			      (identity arg)))
		      lst)))

(defmacro run-command (list)
  (let ((command (car list))
	(arg  (make-command-list (cdr list))))
    `(bt:make-thread
      #'(lambda () (bot-command ,command ,arg)))
    ))



(defcommand :sleep arg
  (progn (format t "sleep ~a~%" (car arg))
	 (sleep (car arg))))

(defcommand :command-list arg
  (dolist (i arg)
    (let ((command (car i))
	  (arg (cdr i)))
      (bot-command command arg))))


(defcommand :command-dotimes arg
  (let ((dotime (car arg))
	(command-list (cdr arg)))
    (dotimes (i dotime)
      (bot-command :command-list command-list))))




(defun make-loop-list (list)
  (setf (cdr (last list)) list))

(defcommand :loop-command arg
  (let ((looped-command-list (make-loop-list arg)))
    (bot-command :command-list looped-command-list)))



