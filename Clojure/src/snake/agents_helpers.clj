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

(ns snake.agents-helpers)

(defn
  ^{:doc "Start an agent with the given state and set a self reference"
    :test (fn []
            (let [test-state {:a 1 :b 2 :d 7}
                  result (start-agent-and-initialize test-state)]
              (assert (= 1 (get @result :a)))
              (println (get @result :a))
              (println (get @result :b))
              (println (get @result :d)))
            ;(assert (not (nil? (get @result :self))))
            )}

  start-agent-and-initialize

  [initial-state]
  
  (let [new-agent (agent initial-state)]
    (send  new-agent (fn [agent-state] (assoc agent-state :self new-agent)))
    (await new-agent)  ; make sure the initialization is completed
    
    new-agent))
  
 ; (test #'start-agent-and-initialize)