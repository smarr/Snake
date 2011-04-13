(ns snake.agents-helpers)

(defn
  ^{:doc "Start an agent with the given state and set a self reference"
    :test (fn []
            (def test-state {:a 1 :b 2 :d 7})
            (def result (start-agent-and-initialize test-state))
            (assert (= 1 (get @result :a)))
            (println (get @result :a))
            (println (get @result :b))
            (println (get @result :d))
            ;(assert (not (nil? (get @result :self))))
            )}

  start-agent-and-initialize

  [initial-state]
  
  (def  new-agent (agent initial-state))

  (send  new-agent (fn [agent-state] (assoc agent-state :self new-agent)))
  (await new-agent)  ; make sure the initialization is completed
  
  new-agent)
  
 ; (test #'start-agent-and-initialize)