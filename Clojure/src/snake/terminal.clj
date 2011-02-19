(ns snake.terminal)

(defn clean
  "Clear the terminal, i.e., remove all output."
  []
	(print "\u001B[2J"))

(defn set-cursor
  "Set the cursor for the next output to the given coordinates."
  [x y]
	(printf "\u001B[%d;%dH" y x))

(defn put
  "Output the string at the current cursor coordinates."
  [str]
	(print str))