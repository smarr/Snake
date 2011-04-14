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
  [board x y]
  
  (let [fields (get board :fields)
        row    (nth fields y     )]
    (nth row     x)))
