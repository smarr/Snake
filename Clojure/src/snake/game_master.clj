(ns snake.game-master
  (:use snake.board)
  (:use snake.game-elements)
  (:use snake.board-view))

(import '(java.util TimerTask Timer))

(def turn-time-millis 250)       ; number of milliseconds of a turn

(defn create-heartbeat
  [period]
  (let [task (proxy [TimerTask] []
                (run [] (println ".")))
        timer (new Timer)]
    (.schedule timer task (long period) (long period))
    timer))

(defn initialize-board-with-apples
  [board num-apples board-view]
  
  (letfn [(add-apples [board apples]
            (let [{width  :width
                   height :height} board
                   x      (rand-int width)
                   y      (rand-int height)]
              
              (if (nil? (get-field board x y))
                (let
                  [apple (create-apple x y)
                   new-board (change-field board x y apple)]
                  
                  (send board-view add-element apple)
                  (if (< 0 (- apples 1))
                    (add-apples new-board (- apples 1))
                    new-board))
                (add-apples board apples))))]
    (add-apples board num-apples)))
          




(defn exec-game-master
  [board-view board & {:keys [num-apples players]}]
  
  (let [; setup game
        initialized-board (initialize-board-with-apples board num-apples board-view)
        
        ; map payers to snakes
        player-snake-map  {:player :snake}
        
        ; be able to receive directions input from players
        ;   and map the input to the right one
        
        ; make the game move forward based on some heartbeat
        
        ; initalize heartbeat and gameover promise
        heartbeat         (create-heartbeat turn-time-millis)
        game-over-promise (promise)
        
        game {:board board
              :board-view board-view
              :num-apples num-apples
              :players player-snake-map}]
    game-over-promise))
