# 
# To change this template, choose Tools | Templates
# and open the template in the editor.
 

class BoardView
  def initialize board
    board.view = self
    @width = board.width
    @height = board.height
  end
  
  def boardData= board
    @board = board
  end
  
  def updateCompletely
    @board.each { |y|
      if y
        y.each { |apple|
          if apple
            addApple apple
          end
        }
      end
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
    @width.times { Terminal.put '-' }
    Terminal.put '\\'
    
    for i in 1..@height do
      Terminal.cursorTo 0, i
      Terminal.put '|'
      Terminal.cursorTo @width + 1, i
      Terminal.put '|'
      Terminal.put "\n"
    end
    
    Terminal.put '\\'
    @width.times { Terminal.put '-' }
    Terminal.put '/'
  end
end