class Board
  def initialize(width, height, numberOfApples)
    # @board
    @width = width
    @height = height
     
    while numberOfApples >= 0 
      addApple
      numberOfApples -= 1
    end
  end
  
  def addApple
    added = false
    until added
      x = rand @width
      y = rand @height
      if !@board[x] || !@board[x][y]
        unless @board[x]
          @board[x] = Array.new @width
        end
        @board[x][y] = Apple.new(x, y)
        added = true

        if @view
          @view.addApple(@board[x][y])
        end
      end
    end
  end
  
  	
  def view=(view)
    @view = view
    @view.setBoardData @board
    @view.updateCompletely
  end
  
  def width
    @width
  end
  
  def height
    @heigh
  end
  
  def isApple(x, y)
    x %= @width
    y %= @heigh
    
    if @board[x] && board[x][y]
      @board[x][y].is_a Apple
    else
      false
    end
  end
  
  def isSnake(x, y)
    x %= @width
    y %= @heigh

    if @board[x] && board[x][y]
      @board[x][y].is_a SnakeElement
    else
      false
    end
  end
  
  def remove(element)
    @board[x][y] = nil
    @view.remove element
  end

  def add(element)
    @board[x][y] = element
    @view.add element
  end

end
