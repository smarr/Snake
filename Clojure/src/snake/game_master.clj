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
  (:use snake.board-view)
  (:use snake.player))

(import '(java.util TimerTask Timer))

(def turn-time-millis 250)       ; number of milliseconds of a turn


(defn do-game-turn
  [game-agent]
  game-agent)


(defn create-heartbeat
  [period game-agent]
  (let [task (proxy [TimerTask] []
                (run [] (send game-agent do-game-turn)))
        timer (new Timer)]
    (.schedule timer task (long period) (long period))
    timer))


(defn initialize-board-with-apples
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
        human-snake       (create-snake x y)]
    [{human-player human-snake} (change-field board x y human-snake)]))

(defn exec-game-master
  [board-view board & {:keys [num-apples players]}]
  
  (let [; setup game
    
        ; map payers to snakes
        ; to be able to receive directions input from players
        ; and map the input to the right snake
        [player-snake-map
         board-with-snakes] (create-snakes players board)
        
        board-with-apples   (initialize-board-with-apples board-with-snakes num-apples board-view)
        
        ; hardcoded - show snakes
        [human-player] players
        _ (send board-view add-element (get player-snake-map human-player))
                
        game {:board      board-with-apples
              :board-view board-view
              :num-apples num-apples
              :players    player-snake-map}
              
        game-agent (start-agent-and-initialize game)
        
        ; make the game move forward based on some heartbeat
        ; initalize heartbeat and gameover promise
        heartbeat         (create-heartbeat turn-time-millis game-agent)
        game-over-promise (promise)]
    
    ; the players need to know the game-agent to send their moves
    
    (doall (map (fn [player] (send player set-game-master game-agent)) players))
    game-over-promise))


