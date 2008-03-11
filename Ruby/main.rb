
require 'terminal.rb' 
require 'element.rb';
require 'apple.rb';
require 'snake_element.rb';
require 'board.rb';
require 'board_view.rb';
require 'snake.rb';


Terminal.init
Terminal.clear

board = Board.new 10, 10, 5
view = BoardView.new board
view.drawBoarder

snake = Snake.new 5, 5, board

dir = nil
gameOver = false

until gameOver
  for i in 1..4 do
    key = Terminal.get
    if [Terminal::KEY_UP, Terminal::KEY_DOWN, Terminal::KEY_LEFT, Terminal::KEY_RIGHT].include? key
      dir = key
    end
    sleep 0.1
  end
  
  
  
  gameOver = case dir
    when Terminal::KEY_UP: snake.moveUp
    when Terminal::KEY_DOWN: snake.moveDown
    when Terminal::KEY_LEFT: snake.moveLeft
    when Terminal::KEY_RIGHT: snake.moveRight
    else false
  end
  
  if gameOver
    Terminal.cursorTo(15, 15)
    Terminal.put('GAME OVER')
    sleep 5
  end
end

Terminal.clear
Terminal.uninit