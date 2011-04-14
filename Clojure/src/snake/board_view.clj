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
         x-zero :x
         y-zero :y} board-view
         x (+ 1 x-zero)
         y (+ 1 y-zero)]

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
    (put "/"))
  board-view)

; REM: cannot use standard lookup function based
;      just on naming the key, since I want to dispatch
;      on the second argument
(defmulti add-element (fn [_ {type :type}] type))
  (defmethod add-element :apple
    [board-view {x-apple :x y-apple :y}]
    
    (let [{x-board :x y-board :y} board-view]
      ; add the coordinate of the board + 1 for the 0-based system
      ; + another 1 for the border for board + the actual apple coordinate
      (set-cursor (+ x-board 1 1 x-apple) (+ y-board 1 1 y-apple))
      (put "o"))
    board-view)
  (defmethod add-element :snake
    [board-view {x-snake :x y-snake :y}]
    
    (let [{x-board :x y-board :y} board-view]
    
      (set-cursor (+ x-board 1 1 x-snake) (+ y-board 1 1 y-snake))
      (put "#"))
    board-view)

(defn remove-element
  [board-view {x :x y :y}]
  
  (let [{x-board :x y-board :y} board-view]
    (set-cursor (+ x-board x 1 1) (+ y-board y 1 1))
    (put " "))
    
  board-view)

(defn show-board
  [{width  :width
    height :height
    board  :board}]
  
  (doseq [[row] board]
    (doseq [[elem] row]
      (add-element elem))))

; REM: add the add function which will call add-apple or add-snake