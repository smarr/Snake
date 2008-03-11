# 
# To change this template, choose Tools | Templates
# and open the template in the editor.
 

class Snake
  def initialize x, y, board
    head = SnakeElement.new x, y
    @head = head
    @tail = head
    @board = board
    @board.add head
  end
  
  def moveLeft
    newPos = @head.clone
    newPos.x = overflow newPos.x - 1, @board.width
    move newPos
  end
  
  def moveRight
    newPos = @head.clone
    newPos.x = overflow newPos.x + 1, @board.width
    move newPos
  end
  
  def moveUp
    newPos = @head.clone
    newPos.y = overflow newPos.y - 1, @board.width
    move newPos
  end
  
  def moveDown
    newPos = @head.clone
    newPos.y = overflow newPos.y + 1, @board.width
    move newPos
  end
  
  def move newPos
    newPos.next = @head
    @head.prev = newPos
    @head = newPos
    
    if @board.isApple newPos.x, newPos.y
      @board.addApple
    else
      if @board.isSnake newPos.x, newPos.y
        return true
      end
      @board.remove @tail
      @tail = @tail.prev
      @tail.next = nil
    end
    @board.add newPos
    false
  end
  
  def overflow val, max
    if val < 0
      val = max + val
    end
    val %= max
    #val maybe is not necessary 
  end


end
