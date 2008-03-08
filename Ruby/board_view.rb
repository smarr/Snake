# 
# To change this template, choose Tools | Templates
# and open the template in the editor.
 

class BoardView
  def initialize(board)
    board.view = this
    @width = board.width
    @heigh = board.height
  end
  
  def boardData=(board)
    @board = board
  end
  
  def updateCompletely
    @board.each { |y|  
      y.each { |apple|
        addApple apple
      }
    }
  end
  
  def remove(element)
    Terminal.cursorTo(element.x + 1, element.y + 1)
    Terminal.put ' '
  end
  
  def add(element)
    Terminal.cursorTo(element.x + 1, element.y + 1)
    Terminal.put '#'
  end
  
  def addApple(apple)
    Terminal.cursorTo(apple.x + 1, apple.y + 1)
    Terminal.put 'o'
  end
  
  def drawBoarder
    Terminal.cursorTo 0, 0
    Terminal.put '/'
    @width.times 
  end
end

/*
	
	
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
*/