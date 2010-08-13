(ns snake.terminal-reader)

(defn read-input [input-receiver]
	(loop [data (read)]
    (if (= data "[A")
        ((print "UP")
         (recur (read)))
   ((if (= data "[B")
        ((print "DOWN")
         (recur (read)))
   ( ;default
    (recur (str data (read)))))))))
;		(case data
;			"[A" ( ;(send input-receiver move 'up)
;				       (print "UP")
;				       ; (recur [data (read)])
;             )
;			"[B" (print "DOWN")
;
;			((print (str "DEFAULT" data))
;			 (recur (str data (read)))))))

(defn start [input-receiver]
	(send (agent nil) read-input input-receiver))
