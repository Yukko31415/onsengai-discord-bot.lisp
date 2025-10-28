
(in-package #:discord-bot-define-class)


;;;; --------------------------------------------------------------
;;;; discord-bot-rss  ---------------------------------------------



(defclass sandbox ()
  ((link
    :initarg :link :accessor sandbox-link)
   (color
    :initarg :color :accessor sandbox-color
    :initform 49408)
   (icon
    :initarg :icon :accessor sandbox-icon
    :initform "<:SB:1088712219656728657>")))


(defclass wikidot-jp ()
  ((link
    :initarg :link :accessor wikidot-jp-link)
   (color
    :initarg :color :accessor wikidot-jp-color
    :initform 15007744)
   (icon
    :initarg :icon :accessor wikidot-jp-icon
    :initform "<:Pos:1088712480865394700>")))
