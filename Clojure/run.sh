#!/bin/sh
stty -echo -echok -icanon min 1 time 0
java -cp snake.jar:/opt/local/share/java/clojure/lib/clojure.jar snake.core
stty echo echok icanon
