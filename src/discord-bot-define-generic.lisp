
(in-package #:discord-bot-define-generic)


;;;; --------------------------------------------------------------
;;;; generic関数の定義
;;;; --------------------------------------------------------------

(defgeneric bot-command (bot-type command arg))

;;;; --------------------------------------------------------------
;;;; main-command
;;;; --------------------------------------------------------------

(defmacro defcommand (bot-type command arg-let &body body)
  (let ((type (gensym))
	(key (gensym)))
    `(defmethod bot-command ((,type (eql ,bot-type)) (,key (eql ,command)) ,arg-let)
       ,@body)))


(defun make-command-list (lst)
  (cons 'list
	(mapcar #'(lambda (arg)
		    (if (listp arg)
			(if (listp (cdr arg))
			    (make-command-list arg)
			    `(cons ,(car arg) ,(cdr arg)))
			(identity arg)))
		lst)))



(defmacro run-command (list)
  (let ((bot-type (first list))
	(command (second list))
	(arg (make-command-list (cddr list))))
    `(bt:make-thread
      #'(lambda () (bot-command ,bot-type ,command ,arg)))))



(defcommand :main :sleep arg
  (progn (format t "sleep ~a~%" (car arg))
	 (sleep (car arg))))

(defcommand :main :command-list arg
  (dolist (i arg)
    (let ((bot-type (first i))
	  (command (second i))
	  (arg (cddr i)))
      (bot-command bot-type command arg))))


(defcommand :main :command-dotimes arg
  (let ((dotime (car arg))
	(command-list (cdr arg)))
    (dotimes (i dotime)
      (bot-command :main :command-list command-list))))


(defun make-loop-list (list)
  (setf (cdr (last list)) list))

(defcommand :main :loop-command arg
  (let ((looped-command-list (make-loop-list arg)))
    (bot-command :main :command-list looped-command-list)))



