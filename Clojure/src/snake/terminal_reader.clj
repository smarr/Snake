(ns snake.terminal-reader
  (:use snake.input-receiver))

(defn get-input
  "Reads a string from the given input-reader if there is anything to be read"
  [input-reader]
  
  (let [buffer (char-array 1024)]
    (if (.ready input-reader)
      (let [num-chars-read (.read input-reader buffer)]
        (if (= num-chars-read -1)
          ""
          (String/valueOf buffer 0 num-chars-read)))
      "")))


(defn read-input
  "Read from standard input and decode the input into symbols
   to be understoud be the input-receiver."
  [input-receiver input-reader]
  (loop [data (get-input input-reader)]
    (case data
      "\u001B[A" (send input-receiver input-direction :up   )
      "\u001B[B" (send input-receiver input-direction :down )
      "\u001B[D" (send input-receiver input-direction :left )
      "\u001B[C" (send input-receiver input-direction :right)
      :nothing)
    (. Thread sleep 80)  ;; necessary since get-input is non-blocking
    (recur (get-input input-reader))))

(defn start-terminal-reader
  "Starts a new thread that is reading from standard in and will
   notify an agent about all read input."
  [input-receiver]
  (let [input-reader (new java.io.InputStreamReader System/in)]
    (.start (Thread. (fn [] (read-input input-receiver input-reader))))))
