(ns snake.terminal-reader)

(defn get-input
  "Reads a string from the given input-reader if there is anything to be read"
  [input-reader]
  
  (def buffer (char-array 1024))
  (if (.ready input-reader)
    (do
      (def num-chars-read (.read input-reader buffer))
      (if (= num-chars-read -1)
          ""
          (String/valueOf buffer 0 num-chars-read)))
    ""))


(defn read-input
  "Read from standard input and decode the input into symbols
   to be understoud be the input-receiver."
  [input-receiver input-reader]
  (loop [data (get-input input-reader)]
    (case data
      "\u001B[A" ;(send input-receiver move 'up)
              (do
                (print "UP")
                (flush))
              ; (recur [data (read)])
              
      "\u001B[B" (print "DOWN")
      "\u001B[D" (print "LEFT")
      "\u001B[C" (print "RIGHT")
      :nothing)
    (recur (get-input input-reader))))

(defn start
  "Starts a new thread that is reading from standard in and will
   notify an agent about all read input."
  [input-receiver]
  (def input-reader (new java.io.InputStreamReader System/in))
  (.start (Thread. (fn [] (read-input input-receiver input-reader)))))
