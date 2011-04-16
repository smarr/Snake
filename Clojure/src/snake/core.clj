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

(ns snake.core
  (:use snake.input-receiver)
  (:use snake.terminal-reader)
  (:use snake.board)
  (:use snake.player)
  (:use snake.board-view)
  (:use snake.game-elements)
  (:use snake.game-master))


(let [human-player       (create-human-player)
      board              (create-board :width  10
                                       :height 10)
      board-view         (create-view  board :x 4 :y 4)]

      (start-terminal-reader  human-player)

      ; initialize the view
      (send  board-view  initialize-view)
      (send  board-view  draw-borders)
      
      ; now start the actual game
      (let [game-over-promise  (exec-game-master
                                          board-view
                                          board
                                          :num-apples 5
                                          :players   [human-player])]
        ;; will block until the games is completed
        (deref game-over-promise)
        (println "GAME OVER")))
