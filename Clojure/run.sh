#!/bin/sh
stty -echo -echok -icanon min 1 time 0
#java -cp clojure.jar:clojure-contrib.jar:src/ clojure.main src/snake/core.clj
java -cp /opt/local/share/java/clojure/lib/clojure.jar:/opt/local/share/java/clojure/lib/clojure-contrib.jar:src/ clojure.main src/snake/core.clj
#java -cp snake.jar:/opt/local/share/java/clojure/lib/clojure.jar snake.core
#lein run
stty echo echok icanon
