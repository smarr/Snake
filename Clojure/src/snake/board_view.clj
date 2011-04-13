(ns snake.board-view
  (:use snake.terminal)
  (:use snake.agents-helpers)
  (:require [clojure.contrib.string :as string]))

(defn create-view
  "Create a new board view"
  [{width :width height :height} & {:keys [x y]}]
  (start-agent-and-initialize {:width  width
                               :height height
                               :x x
                               :y y}))

(defn initialize-view
  "Will just clean the terminal for now"
  [agent-state]
  
  (clean-terminal)
  agent-state)

(defn draw-borders
  "Draws the surrounding border of the board."
  [board-view]
  (let [{width  :width 
         height :height
         x      :x
         y      :y} board-view]

    (set-cursor x y)
    (put "/")
    (put (string/repeat width "-"))
    (put "\\")
  
    (dotimes [i height]
      (set-cursor x             (+ y i 1))
      (put "|")
      (set-cursor (+ x width 1) (+ y i 1))
      (put "|"))
  
    (set-cursor x  (+ y height 1))
    (put "\\")
    (put (string/repeat width "-"))
    (put "/")))

(defmulti add-element :type)
  (defmethod add-element :apple
    [board-view {x :x y :y}]
    
    (set-cursor (+ x 1) (+ y 1))
    (put "o"))
  (defmethod add-element :snake
    [board-view {x :x y :y}]
    
    (set-cursor (+ x 1) (+ y 1))
    (put "#"))

(defn remove-element
  [board-view {x :x y :y}]
  
  (set-cursor (+ x 1) (+ y 1))
  (put " "))

(defn show-board
  [{width  :width
    height :height
    board  :board}]
  
  (doseq [[row] board]
    (doseq [[elem] row]
      (add-element elem))))

; REM: add the add function which will call add-apple or add-snake