(ns snake.game-elements)


(defn create-apple
  [x y]
  {:x x
   :y y
   :type :apple})

(defn create-snake
  [] ; [x y]
  
  {:x 1 ; x
   :y 2 ; y
   :type :snake})
