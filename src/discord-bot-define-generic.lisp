
(in-package #:discord-bot-define-generic)




;;;; --------------------------------------------------------------
;;;; generic関数の定義 --------------------------------------------

(defgeneric bot-command (key arg))

;;;; --------------------------------------------------------------
;;;; main-command -------------------------------------------------



(defmethod bot-command ((key (eql :sleep)) arg)
  (progn (format t "sleep ~a~%" (car arg))
	 (sleep (car arg))))

(defmethod bot-command ((key (eql :command-list)) arg)
  (dolist (i arg)
    (let ((command (car i))
	  (arg (cdr i)))
      (bot-command command arg))))


(defmethod bot-command ((key (eql :command-dotimes)) arg)
  (let ((dotime (car arg))
	(command-list (cdr arg)))
    (dotimes (i dotime)
      (bot-command :command-list command-list))))




(defun make-loop-list (list)
  (setf (cdr (last list)) list))

(defmethod bot-command ((key (eql :loop-command)) arg)
  (let ((looped-command-list (make-loop-list arg)))
    (bot-command :command-list looped-command-list)))



