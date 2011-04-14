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
