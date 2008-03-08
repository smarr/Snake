<?php
include_once 'Board.php';

class BoardView {
	private $board;
	private $width;
	private $height;
	
	public function __construct(Board $board) {
		$board->setView($this);
		$this->width = $board->getWidth();
		$this->height = $board->getHeight();
	}
	
	public function setBoardData($board) {
		$this->board = $board;
	}
	
	public function updateCompletely() {
		foreach ($this->board as $y) {
			foreach ($y as $apple) {
				$this->addApple($apple);
			}
		}
	}
	
	public function remove(SnakeElement $element) {
		Terminal::cursorTo($element->x + 1, $element->y + 1);
		Terminal::put(' ');
	}
	
	public function add(SnakeElement $element) {
		Terminal::cursorTo($element->x + 1, $element->y + 1);
		Terminal::put('#');
	}
	
	public function addApple(Apple $apple) {
		Terminal::cursorTo($apple->x + 1, $apple->y + 1);
		Terminal::put('o');
	}
	
	public function drawBoarder() {
		Terminal::cursorTo(0, 0);
		Terminal::put('/'.str_repeat('-', $this->width).'\\');
		
		
		for ($i = 1; $i < $this->height + 1; $i++) {
			Terminal::cursorTo(0, $i);
			Terminal::put('|');
			Terminal::cursorTo($this->width + 1, $i);
			Terminal::put('|');
		}
		Terminal::put("\n");
		
		Terminal::put('\\'.str_repeat('-', $this->width).'/');
	}
}
?>