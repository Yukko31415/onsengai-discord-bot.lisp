
(defsystem "discord-bot"
  :description "this is a Discord bot for 温泉街"
  :version "0.0.1"
  :author "yukko31415@gmail.com"
  :licence "Public Domain"
  :depends-on ("dexador"
	       "cl-json"
	       "xmls"
	       "babel"
	       "chipz"
	       "puri"
	       "lquery"
	       "local-time"
	       "str"
	       "alexandria"
	       "bordeaux-threads"
	       "lparallel"
	       "slynk")
  :build-operation "program-op"
  :build-pathname "discord-bot"
  :pathname #p"src/"
  :serial t
  :components ((:file "discord-bot-packages")
	       (:file "discord-bot-define-generic")
	       (:file "discord-bot-token")
	       (:file "discord-bot-post")
	       (:file "discord-bot-rss")
	       (:file "discord-bot-main"))
  :entry-point "discord-bot-main:start")



