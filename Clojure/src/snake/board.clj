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

(ns snake.board)

(defn create-board
  [& {:keys [width, height]}]
  
  (let [row    (vec (replicate width  nil))
        fields (vec (replicate height row))]  
    {:width        width
     :height       height
     :fields       fields
     
     ;; STEFAN: this is really strange, not directly supported in Clojure...
     :rndGenerator :no-available-in-clojure 
     }))

(defn change-field
  [board x y new-value]
  
  (let [fields (get board :fields)
        row    (get fields y     )
        new-row    (assoc row    x new-value)
        new-fields (assoc fields y new-row  )]
    (assoc board :fields new-fields)))

(defn get-field
  ([board x y]
  
  (let [fields (get board :fields)
        row    (nth fields y     )]
    (nth row     x)))

  ([board [x y]] ; for convenience
   (get-field board x y)))

(defn overflow
  ([val max]
    (mod val max))
  ([{width :width height :height} x y]
   [(overflow x width) (overflow y height)]))
