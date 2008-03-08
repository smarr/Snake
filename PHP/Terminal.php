<?php
class Terminal {
	const KEY_UP    = NCURSES_KEY_UP;
	const KEY_DOWN  = NCURSES_KEY_DOWN;
	const KEY_LEFT  = NCURSES_KEY_LEFT;
	const KEY_RIGHT = NCURSES_KEY_RIGHT;
	
	public static function init() {
		ncurses_init();
		$full = ncurses_newwin (0,0,0,0);
		ncurses_wborder($full,0,0,0,0,0,0,0,0);
		ncurses_wrefresh($full);
		ncurses_curs_set(0);
	}
	
	public static function cursorTo($x, $y) {
		ncurses_move($y, $x);
	}
	
	public static function clear() {
		ncurses_clear();
	}
	
	public static function put($str) {
		ncurses_addstr($str);
	}
	
	public static function get($timeOut = -1) {
		if ($timeOut != -1) ncurses_timeout($timeOut);
		return ncurses_getch();
	}
	
	public static function end() {
		ncurses_curs_set(1);
		ncurses_end();
	}
}
?>