#!/bin/sh
erlc game_master.erl
erl -noshell -s game_master test -s init stop
