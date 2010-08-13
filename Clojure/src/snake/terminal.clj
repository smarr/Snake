(ns snake.terminal)

(defn clean []
	(print "[2J"))

(defn set-cursor [x y]
	(printf "[%d;%dH" y x))

(defn put [str]
	(print str))