<?php
class Snake {
	
	private $head;
	private $tail;
	private $board;
	
	public function __construct($x, $y, Board $board) {
		$head = new SnakeElement($x, $y);
		$this->body[] = $head;
		$this->head = $head;
		$this->tail = $head;
		$this->board = $board;
		$this->board->add($head);
	}
	
	public function moveLeft() {
		$newPos = clone $this->head;
		$newPos->x = $this->overflow($newPos->x - 1, $this->board->getWidth());
		return $this->move($newPos);
	}
	
	public function moveRight() {
		$newPos = clone $this->head;
		$newPos->x = $this->overflow($newPos->x + 1, $this->board->getWidth());
		return $this->move($newPos); 
	}
	
	public function moveUp() {
		$newPos = clone $this->head;
		$newPos->y = $this->overflow($newPos->y - 1, $this->board->getHeight());
		return $this->move($newPos); 
	}
	
	public function moveDown() {
		$newPos = clone $this->head;
		$newPos->y = $this->overflow($newPos->y + 1, $this->board->getHeight());
		return $this->move($newPos); 
	}
	
	protected function move(SnakeElement $newPos) {
		$newPos->next = $this->head;
		$this->head->prev = $newPos;
		$this->head = $newPos;
			
		if ($this->board->isApple($newPos->x, $newPos->y)) {
			$this->board->addApple();
		} else {
			if ($this->board->isSnake($newPos->x, $newPos->y)) {
				return false;
			}
			$this->board->remove($this->tail);
			$this->tail = $this->tail->prev;
			$this->tail->next = null;
		}
		
		$this->board->add($newPos);
		return true;
	}
	
	protected function overflow($val, $max) {
		if ($val < 0) {
			$val = $max + $val;
		}
		$val %= $max;
		return $val;
	}
}

?>