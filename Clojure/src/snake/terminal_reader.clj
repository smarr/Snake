(ns snake.terminal-reader)

(defn read-input [input-receiver]
  (loop [data (read)]
    (case data
      "\u001B[A" ( ;(send input-receiver move 'up)
              (print "UP")
              ; (recur [data (read)])
              )
      "\u001B[B" (print "DOWN")
      
      (print (str "DEFAULT" data)))
    (recur (str data (read)))))

(defn start [input-receiver]
  (send (agent nil) read-input input-receiver))
