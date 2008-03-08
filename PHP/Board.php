<?php
/**
* The board class
*/
class Board {

	private $view;
	private $width;
	private $height;
	
	private $board;
		
	public function __construct($width, $height, $numberOfApples) {
		$this->board = array();
		$this->width = $width;
		$this->height = $height;
		
		while ($numberOfApples >= 0) {
			$this->addApple();
			$numberOfApples--;
		}
	}
	
	public function addApple() {
		$added = false;
		while (!$added) {
			$x = rand(0, $this->width - 1);
			$y = rand(0, $this->height - 1);
			if (!isset($this->board[$x][$y]) ||	$this->board[$x][$y] == null) {
				$this->board[$x][$y] = new Apple($x, $y);
				$added = true;
				if (isset($this->view))
					$this->view->addApple($this->board[$x][$y]);
			}
		}
	}
	
	public function setView($view) {
		$this->view = $view;
		$view->setBoardData($this->board);
		$view->updateCompletely();
	}
	
	public function getWidth() {
		return $this->width;
	}
	
	public function getHeight() {
		return $this->height;
	}
	
	public function isApple($x, $y) {
		$x %= $this->width;
		$y %= $this->height;
		if (isset($this->board[$x]) && isset($this->board[$x][$y])) {
			return ($this->board[$x][$y] instanceof Apple);
		}
		return false;
	}
	
	public function isSnake($x, $y) {
		$x %= $this->width;
		$y %= $this->height;
		if (isset($this->board[$x]) && isset($this->board[$x][$y])) {
			return ($this->board[$x][$y] instanceof SnakeElement);
		}
		return false;
	}
	
	public function remove(SnakeElement $element) {
		unset($this->board[$element->x][$element->y]);
		$this->view->remove($element);
	}
	
	public function add(SnakeElement $element) {
		$this->board[$element->x][$element->y] = $element;
		$this->view->add($element);
	}
}
?>