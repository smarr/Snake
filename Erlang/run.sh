#!/bin/sh
stty -icanon min 1 time 0; erl -pa ./ -run main start -run init stop -noshell
