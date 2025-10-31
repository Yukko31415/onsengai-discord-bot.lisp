
(in-package #:discord-bot-define-class)


;;;; --------------------------------------------------------------
;;;; discord-bot-rss  ---------------------------------------------



(defclass article ()
  ((link
    :initarg :link :accessor link)
   (color
    :initarg :color :accessor color)
   (icon
    :initarg :icon :accessor icon)))


(defclass sandbox (article)
  ((color
    :initform 49408)
   (icon
    :initform "<:SB:1088712219656728657>")))


(defclass wikidot-jp (article)
  ((color
    :initform 15007744)
   (icon
    :initform "<:Pos:1088712480865394700>")))
