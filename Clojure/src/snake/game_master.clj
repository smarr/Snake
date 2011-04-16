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
