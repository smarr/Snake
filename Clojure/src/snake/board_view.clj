(ns snake.board-view
  (:use snake.terminal)
  (:use snake.agents-helpers)
  (:require [clojure.contrib.string :as string]))

(defn create-view
  "Create a new board view"
  [{width :width height :height}]
  (start-agent-and-initialize {:width  width
                               :height height}))

(defn draw-borders
  "Draws the surrounding border of the board."
  [board-view]
  (let [{width  :width 
         height :height} board-view]
    (println width)
    (println height)
    ; (println board-view)
    (set-cursor 0 0)
    (put "/")
    (put (string/repeat width "-"))
    (put "\\")
  
    (dotimes [i height]
      (set-cursor 0           (+ i 1))
      (put "|")
      (set-cursor (+ width 1) (+ i 1))
      (put "|"))
  
    (put "\n")
  
    (put "\\")
    (put (string/repeat width "-"))
    (put "/"))
  (flush)
  (println "down with borders"))

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