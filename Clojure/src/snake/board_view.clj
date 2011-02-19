(ns snake.board-view
  (:use snake.terminal)
  (:require [clojure.contrib.string :as string]))

(defn draw-boarder
  "Draws the surrounding border of the board."
  [{width :width height :height}]
  
  (set-cursor 0 0)
  (put "/")
  (put (string/repeat width "-"))
  (put "\\")
  
  (dotimes [i height]
    (set-cursor 0           (+ i 1))
    (put "|")
    (set-cursor (+ width 1) (+ i 1))
    (put "|"))
  
  (put "\n")
  
  (put "\\")
  (put (string/repeat width "-"))
  (put "/"))


;public void add(Apple apple) {
;	term.setCursor(apple.getX() + 1, apple.getY() + 1);
;	term.put("o");
;}
;
;public void add(SnakeElement elem) {
;	term.setCursor(elem.getX() + 1, elem.getY() + 1);
;	term.put("#");
;}
;
;public void remove(GameElement elem) {
;	term.setCursor(elem.getX() + 1, elem.getY() + 1);
;	term.put(" ");
;}
;
;public void setBoardData(Object[][] board) {
;	this.board = board;
;}
;
;public void showContent() {
;	for (Object[] row : board) {
;		for (Object elem : row) {
;			if (elem instanceof Apple) {
;				add((Apple)elem);
;			}
;			else if (elem instanceof SnakeElement) {
;				add((SnakeElement)elem);
;			}
;		}
;	}
;}
;