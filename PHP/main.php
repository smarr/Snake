<?php
error_reporting(E_ALL | E_STRICT);

include_once 'Terminal.php';
include_once 'Elements.php';
include_once 'Board.php';
include_once 'BoardView.php';
include_once 'Snake.php';

Terminal::init();
Terminal::clear();

$board = new Board(10, 10, 5);
$view = new BoardView($board);
$view->drawBoarder();

$snake = new Snake(5, 5, $board);

$dir = 0;
$result = true;
while (true) {
	$i = 4;
	while ($i > 0) {
		$key = Terminal::get(0);
		if (in_array($key, array(Terminal::KEY_UP, Terminal::KEY_DOWN, Terminal::KEY_LEFT, Terminal::KEY_RIGHT))) {
			$dir = $key;
		}
		usleep(100000);
		$i--;
	}
		
	switch ($dir) {
		case Terminal::KEY_UP:
			$result = $snake->moveUp();
			break;
		case Terminal::KEY_DOWN:
			$result = $snake->moveDown();
			break;
		case Terminal::KEY_LEFT:
			$result = $snake->moveLeft();
			break;
		case Terminal::KEY_RIGHT:
			$result = $snake->moveRight();
			break;
	}
	if (!$result) {
		Terminal::cursorTo(5, 15);
		Terminal::put('GAME OVER');
		sleep(5);
		return;
	}
}

Terminal::clear();
?>