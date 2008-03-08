<?php

class Element {
	public $x;
	public $y;
	
	public function __construct($x, $y) {
		$this->x = $x;
		$this->y = $y;
	}
}

class Apple extends Element {
	
}

class SnakeElement extends Element {
	public $next = null;
	public $prev = null;
}

?>