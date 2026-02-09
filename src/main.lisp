;;; main.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Your Name

(in-package #:discord-bot)


;;
;; main

(defparameter *rss-bot* (discord-bot-rss:make-rss-bot))

(defun main ()
  (slynk:create-server :port 4006 :dont-close t)
  (discord-bot-token:set-bot-token)
  (discord-bot-rss:run *rss-bot*))
