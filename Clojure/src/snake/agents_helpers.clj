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
  (def new-agent
    (agent initial-state))
  ; for this to work, we rely the semantics that when the following
  ; send gets enqueued, every other send will be enqueued after it
  ; on the same queue (this is really not too safe, they could go fancy and screw
  ; with this assumption, I suppose...)
  (send new-agent (fn [agent-state] (assoc agent-state :self new-agent))))
  
 ; (test #'start-agent-and-initialize)