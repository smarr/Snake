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

(ns snake.player
  (:use snake.agents-helpers))

(defn create-human-player
  "Starts an agent that represents a human player"
  []
  (start-agent-and-initialize
         {:type       :player-human
          :up         nil    ; TODO: provide the handlers for input
          :left       nil
          :down       nil
          :right      nil
          :self       nil    ; self represents the agent reference to itself, is set by initialization
          :game-agent nil
          }))


(defn create-ai-player
  "Start an agent that represents a simple AI player"
  []
  (start-agent-and-initialize {:type       :player-ai
                               :self       nil
                               :game-agent nil}))


(defn set-game-master
  [agent-state game-master]
  (assoc agent-state :game-agent game-master))
