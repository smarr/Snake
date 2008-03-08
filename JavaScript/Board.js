

function Board(width, height, numberOfApples) {
	this.width = width;
	this.height = height;
	this.board = new Array();
	
	this.addApple = function() {
		var added = false;
		while (!added) {
			var x = Math.round(Math.random() * (this.width - 1));
			var y = Math.round(Math.random() * (this.height - 1));
			if (this.board[x] == null
				|| this.board[x][y] == null) {
				if (this.board[x] == null) {
					this.board[x] = new Array();
				}
				this.board[x][y] = new Apple(x, y);
				added = true;
				if (this.view != null)
					this.view.addApple(this.board[x][y]);
			}
		}
	}
	
	this.setView = function(view) {
		this.view = view;
		view.setBoardData(this.board);
		view.updateCompletely();
	}
	
	this.getWidth = function() {
		return this.width;
	}
	
	this.getHeight = function() {
		return this.height;
	}
	
	this.isApple = function(x, y) {
		x %= this.width;
		y %= this.height;
		if (this.board[x] && this.board[x][y]) {
			return (this.board[x][y] instanceof Apple);
		}
		return false;
	}
	
	this.isSnake = function(x, y) {
		x %= this.width;
		y %= this.height;
		if (this.board[x] && this.board[x][y]) {
			return (this.board[x][y] instanceof SnakeElement);
		}
		return false;
	}
	
	this.remove = function(element) {
		this.board[element.x][element.y] = null;
		this.view.remove(element);
	}
	
	this.add = function(element) {
		if (this.board[element.x] == null) {
			this.board[element.x] = new Array();
		}
		this.board[element.x][element.y] = element;
		this.view.add(element);
	}
	
	while (numberOfApples >= 0) {
		this.addApple();
		numberOfApples--;
	}
}