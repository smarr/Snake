(ns snake.board-view
  (:use snake.terminal)
  (:require [clojure.contrib.string :as string]))

(defn draw-boarder
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


(defn add-apple
  [{x :x y :y}]
  
  (set-cursor (+ x 1) (+ y 1))
  (put "o"))

(defn add-snake
  [{x :x y :y}]
  
  (set-cursor (+ x 1) (+ y 1))
  (put "#"))

(defn remove
  [{x :x y :y}]
  
  (set-cursor (+ x 1) (+ y 1))
  (put " "))

(defn show-board
  [{width  :width
    height :height
    board  :board}]
  
  (doseq [[row] board]
    (doseq [[elem] row]
      (add elem))))

; REM: add the add function which will call add-apple or add-snake