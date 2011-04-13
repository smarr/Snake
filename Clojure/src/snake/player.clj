(ns snake.player
  (:use snake.agents-helpers))

(defn create-human-player
  "Starts an agent that represents a human player"
  []
  (start-agent-and-initialize
         {:type  :player-human
          :up    nil    ; TODO: provide the handlers for input
          :left  nil
          :down  nil
          :right nil
          :self  nil    ; self represents the agent reference to itself, is set by initialization
          }))


(defn create-ai-player
  "Start an agent that represents a simple AI player"
  []
  (start-agent-and-initialize {:type :player-ai
                               :self nil}))