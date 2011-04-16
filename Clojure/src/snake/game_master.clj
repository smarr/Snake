; Copyright (C) 2008 - 2011 Stefan Marr <mail@stefan-marr.de>
; 
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to
; deal in the Software without restriction, including without limitation the
; rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
; sell copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
; 
; The above copyright notice and this permission notice shall be included in
; all copies or substantial portions of the Software.
; 
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
; IN THE SOFTWARE.

(ns snake.game-master
  (:use snake.agents-helpers)
  (:use snake.board)
  (:use snake.game-elements)
  (:use snake.board-view))

(import '(java.util TimerTask Timer))

(load "player_decl")

(def turn-time-millis 250)       ; number of milliseconds of a turn


(defn- new-position
  [board snake direction]
  (let [[{x :x y :y}] snake]
    ; *sigh* current (case) does not support nil, already fixed in JIRA
    ; but will work around it for the moment
    (println direction)
    (case (if (nil? direction) :undefined direction)
      :up    (overflow board x       (- y 1))
      :left  (overflow board (- x 1) y)
      :down  (overflow board x       (+ y 1))
      :right (overflow board (+ x 1) y)
      :undefined    [x y]))) ; :undefined could be nil if that would be supported


(defn- do-step-for-player
  [board board-view player {snake :snake direction :direction}]
  (let [new-head-pos  (new-position board snake direction)
        field-content (get-field board new-head-pos)]
    (cond 
      (nil? field-content) ; is a free field, just move normally
        (let [head (create-snake new-head-pos)
              tail (last snake)
              {x-tail :x y-tail :y} tail
              {x-head :x y-head :y} head
              board-tail-removed (change-field board x-tail y-tail nil)
              board-with-new-head (change-field board x-head y-head head)
              new-snake (into [head] (pop snake))]
          (send board-view remove-element tail)
          (send board-view add-element head)
          [board-with-new-head {:snake new-snake :direction direction}])
      (= :apple (get field-content :type)) :eat-apple
      (= :snake (get field-content :type))
        (do
          (send player snake.player/game-over)
          [board {:snake snake :direction direction :state :game-over}]))))


(defn- do-game-turn
  [game-agent]
  (let [{board        :board
         board-view   :board-view
         game-over-promise :game-over-promise
         players      :players} game-agent
         players-seq  (seq players)]
    ; hardcode, just a single player
    (let [[[player-agent player-snake]] players-seq ; get the only player
          [new-board new-snake]         (do-step-for-player board board-view player-agent player-snake)]
      
      ; if no active player is left, the game ends
      (if (= :game-over (get new-snake :state))
        (deliver game-over-promise :quit-game)
        ; else: assemble new game state
        (assoc game-agent
          :board new-board
          :players (assoc players player-agent new-snake))))))

    ;(loop []
    ;  (recur expr*))


(defn- create-heartbeat
  [period game-agent]
  (let [task (proxy [TimerTask] []
                (run [] (send game-agent do-game-turn)))
        timer (new Timer)]
    (.schedule timer task (long period) (long period))
    timer))


(defn- initialize-board-with-apples
  [board num-apples board-view]
  
  (let [{width  :width
         height :height} board
         x      (rand-int width)
         y      (rand-int height)]

    (if (nil? (get-field board x y))
      (let
        [apple      (create-apple x y)
         new-board  (change-field board x y apple)]

        (send board-view add-element apple)
        (if (< 0 (- num-apples 1))
          (initialize-board-with-apples new-board (- num-apples 1) board-view)
          new-board))
      (initialize-board-with-apples board num-apples board-view))))

(defn create-snakes
  [players board]
  ; currently hard-coded for one player
  (let [{width  :width
         height :height}  board
        human-player      (nth players 0)
        x                 (int (/ width  2))
        y                 (int (/ height 2))
        human-snake       [(create-snake x y)]]
    [{human-player {:snake human-snake    :direction nil}}
     (change-field board x y human-snake)]))

(defn exec-game-master
  [board-view board & {:keys [num-apples players]}]
  
  (let [; setup game
        ; map payers to snakes to be able to receive directions input
        ; from players and map the input to the right snake
        [player-snake-map
         board-with-snakes] (create-snakes players board)
        
        board-with-apples   (initialize-board-with-apples board-with-snakes num-apples board-view)
        
        ; hardcoded - show snakes
        [human-player] players
        {[snake-head] :snake} (get player-snake-map human-player)
        _ (send board-view add-element snake-head)
        
        ; initialize game over promise
        game-over-promise (promise)
                
        game {:board      board-with-apples
              :board-view board-view
              :num-apples num-apples
              :players    player-snake-map
              :game-over-promise game-over-promise}
              
        game-agent (start-agent-and-initialize game)
        
        ; make the game move forward based on a heartbeat
        heartbeat (create-heartbeat turn-time-millis game-agent)]
    
    ; the players need to know the game-agent to send their moves
    (doall (map #(send % snake.player/set-game-master game-agent) players))
    game-over-promise))


(defn move
  [game-agent player direction]
  (let [{players :players} game-agent
         {snake :snake} (get players player)
         new-players (assoc players player {:snake snake :direction direction})]
    (println ".")
    (assoc game-agent :players new-players)))
