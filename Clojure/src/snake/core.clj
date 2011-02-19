(ns snake.core
  (:use snake.terminal)
  (:use snake.terminal-reader)
  (:use snake.board-view)
	(:gen-class))

(defn -main [& args]
	(snake.terminal-reader/start nil)
	(println "Hello World!")
	(clean)
	(set-cursor 10 3)
	(put "foo1")
	(set-cursor 3 10)
	(put "bar2")
	(println "Hello World!2")
	(. Thread sleep 5000))
