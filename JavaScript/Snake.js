

function Snake(x, y, board) {
	var head = new SnakeElement(x, y);;
	var tail = head;
	board.add(head);
	
	this.moveLeft = function() {
		var newPos = new SnakeElement();
		newPos.y = head.y;
		newPos.x = this.overflow(head.x - 1, board.getWidth());
		return this.move(newPos);
	}
	
	this.moveRight = function() {
		var newPos = new SnakeElement();
		newPos.y = head.y;
		newPos.x = this.overflow(head.x + 1, board.getWidth());
		return this.move(newPos); 
	}
	
	this.moveUp = function() {
		var newPos = new SnakeElement();
		newPos.x = head.x;
		newPos.y = this.overflow(head.y - 1, board.getHeight());
		return this.move(newPos); 
	}
	
	this.moveDown = function() {
		var newPos = new SnakeElement();
		newPos.x = head.x;
		newPos.y = this.overflow(head.y + 1, board.getHeight());
		return this.move(newPos); 
	}
	
	this.move = function(newPos) {
		newPos.next = head;
		head.prev = newPos;
		head = newPos;
			
		if (board.isApple(newPos.x, newPos.y)) {
			board.addApple();
		} else {
			if (board.isSnake(newPos.x, newPos.y)) {
				return false;
			}
			board.remove(tail);
			tail = tail.prev;
			tail.next = null;
		}
		
		board.add(newPos);
		return true;
	}
	
	this.overflow = function(val, max) {
		if (val < 0) {
			val = max + val;
		}
		val %= max;
		return val;
	}
}