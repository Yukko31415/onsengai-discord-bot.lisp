
(asdf:defsystem "discord-bot"
  :description "this is a Discord bot for 温泉街"
  :version "0.1.1"
  :author "yukko31415@gmail.com"
  :licence "Public Domain"
  :depends-on ("drakma"
	       "jonathan"
	       "local-time"
	       "str"
	       "alexandria"
	       "bordeaux-threads"
	       "slynk"
	       "log4cl"
	       "cl-rss"
	       "lparallel")
  :build-operation "program-op"
  :build-pathname "discord-bot"
  :pathname #p"src/"
  :serial t
  :components ((:file "package")
	       (:file "discord-bot-token")
	       (:file "discord-bot-post")
	       (:file "discord-bot-rss")
	       (:file "main"))
  :entry-point "discord-bot:main")



