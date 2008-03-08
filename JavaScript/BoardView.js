

function BoardView(board) {
	var width  = board.getWidth();
	var height = board.getHeight();
		
	this.setBoardData = function(board) {
		this.board = board;
	}
	
	this.updateCompletely = function() {
		for (var x in this.board) {
			for (var y in this.board[x]) {
				var apple = this.board[x][y];
				Terminal.cursorTo(apple.x + 1, apple.y + 1);
				Terminal.put('o');
			}
		}
	}
	
	this.remove = function(element) {
		Terminal.cursorTo(element.x + 1, element.y + 1);
		Terminal.put(' ');
	}
	
	this.add = function(element) {
		Terminal.cursorTo(element.x + 1, element.y + 1);
		Terminal.put('#');
	}
	
	this.addApple = function(apple) {
		Terminal.cursorTo(apple.x + 1, apple.y + 1);
		Terminal.put('o');
	}
	
	function strRepeat(str, times) {
		var result = new String();
		while (times > 0) {
			times--;
			result += str;
		}
		return result;
	}
	
	this.drawBoarder = function() {
		Terminal.cursorTo(0, 0);
		Terminal.put('/' + strRepeat('-', width) + '\\\n\r');
		
		
		for (i = 1; i < height + 1; i++) {
			//Terminal.cursorTo(0, i);
			//Terminal.put('|');
			//Terminal.cursorTo(this.width + 1, i);
			Terminal.put('|' + strRepeat(' ', width) + '|\n\r');
		}
		//Terminal.put("\n");
		Terminal.put('\\' + strRepeat('-', width) + '/');
	}
	
	this.drawBoarder();
	board.setView(this);
}