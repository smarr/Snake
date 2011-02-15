#!/bin/sh
stty -echo -echok -icanon min 1 time 0
java -cp bin snake.Main
stty echo echok icanon
