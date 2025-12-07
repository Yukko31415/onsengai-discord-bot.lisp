
(in-package #:discord-bot-define-generic)


;;;; --------------------------------------------------------------
;;;; generic関数の定義
;;;; --------------------------------------------------------------

(defgeneric bot-command (bot-type command &rest arg))

;;;; --------------------------------------------------------------
;;;; main-command
;;;; --------------------------------------------------------------


(defun transform-command-syntax (x)
  (if (consp x)
      (typecase (car x)
	(keyword `(list ,@(mapcar #'transform-command-syntax x)))
	(symbol x)
	(t `',x))
      x))


(defmacro run-command (command)
  (destructuring-bind (bot-type command &rest arg) command
    (let ((arg-list (mapcar #'transform-command-syntax arg)))
      `(bot-command ,bot-type ,command ,@arg-list))))


(defmacro defcommand (bot-type command arg-let &body body)
  (let ((type (gensym))
	(key (gensym)))
    `(defmethod bot-command
	 ((,type (eql ,bot-type)) (,key (eql ,command)) &rest ,arg-let)
       ,@body)))


(defcommand :main :sleep arg
  (sleep (car arg)))

(defcommand :main :command-list arg
  (dolist (i arg)
    (destructuring-bind (bot-type command &rest arg) i 
      (apply #'bot-command bot-type command arg))))

(defcommand :main :with-thread arg
  (destructuring-bind (bot-type command &rest arg) (car arg)
    (bt:make-thread
     #'(lambda ()
       (apply #'bot-command bot-type command arg)))))

(defcommand :main :command-dotimes arg
  (destructuring-bind (dotime command-list) arg
      (dotimes (i dotime)
	(bot-command :main :command-list command-list))))

(defun make-loop-list (list)
  (setf (cdr (last list)) list))


(defcommand :main :loop-command arg
  (let ((looped-command-list (make-loop-list arg)))
    (bot-command :main :command-list looped-command-list)))


(defcommand :main :test num
  (format t "this is test. num is ~a~%" (car num)))


