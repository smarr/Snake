(ns snake.input-receiver)

(defn input-direction
  "The agent or map representing the input receiver
   needs to hold a function without arguments for
   all symbols it needs to understand.
   This function will be selected and called."
  [current-agent dir-symbol]
  ((get current-agent dir-symbol)))