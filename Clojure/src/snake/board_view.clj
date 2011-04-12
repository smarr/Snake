(ns snake.board-view
  (:use snake.terminal)
  (:require [clojure.contrib.string :as string]))

(defn create-view
  "Create a new board view"
  [board]
  :not-yet-implemented)

(defn draw-borders
  "Draws the surrounding border of the board."
  [{width :width height :height}]
  
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

(defmulti add-element :type)
  (defmethod add-element :apple
    [{x :x y :y}]
    
    (set-cursor (+ x 1) (+ y 1))
    (put "o"))
  (defmethod add-element :snake
    [{x :x y :y}]
    
    (set-cursor (+ x 1) (+ y 1))
    (put "#"))

(defn remove-element
  [{x :x y :y}]
  
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