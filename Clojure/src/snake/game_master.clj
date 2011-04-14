(ns snake.game-master)

(import '(java.util TimerTask Timer))

(def turn-time-millis 250)       ; number of milliseconds of a turn

(defn create-heartbeat
  [period]
  (let [task (proxy [TimerTask] []
                (run [] (println ".")))]
    (def timer (new Timer))
    (.schedule timer task (long period) (long period)))
    timer)

(defn initialize-board-with-apples
  [board num-apples board-view]

  (defn add-apples
    [{width  :width
      height :height
      fields :fields
      rnd    :rndGenerator} apples]
      
      (def x (rand-int width))
      (def y (rand-int height))
      
      (if (> 0 (- apples 1))
        (add-apples new-board (- apples 1))
          new-board))
  (add-apples board num-apples))


(defn exec-game-master
  [board-view board & {:keys [num-apples players]}]
  
  ; setup game
  (def initialized-board (initialize-board-with-apples board num-apples board-view))
  
  ; map payers to snakes
  (def player-snake-map {:player :snake})
  
  ; be able to receive directions input from players
  ;   and map the input to the right one
  
  ; make the game move forward based on some heartbeat
  
  ; initalize heartbeat and gameover promise
  (def heartbeat (create-heartbeat turn-time-millis))
  (def game-over-promise (promise))
  
  (def game {:board board
             :board-view board-view
             :num-apples num-apples
             :players player-snake-map})
  
  
  
  game-over-promise)
  
