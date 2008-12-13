#!/bin/sh
stty -echo -echok -icanon min 1 time 0
erl -pa ./ -run main start -run init stop -noshell
stty echo echok icanon
