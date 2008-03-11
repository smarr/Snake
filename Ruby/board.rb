class Board
  def initialize(width, height, numberOfApples)
    @board = Array.new width
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
    @view.boardData = @board
    @view.updateCompletely
  end
  
  def width
    @width
  end
  
  def height
    @height
  end
  
  def isApple(x, y)
    x %= @width
    y %= @height
    
    if @board[x] && @board[x][y]
      @board[x][y].is_a? Apple
    else
      false
    end
  end
  
  def isSnake(x, y)
    x %= @width
    y %= @height

    if @board[x] && @board[x][y]
      @board[x][y].is_a? SnakeElement
    else
      false
    end
  end
  
  def remove(element)
    @board[element.x][element.y] = nil
    @view.remove element
  end

  def add(element)
    if !@board[element.x]
      @board[element.x] = Array.new @height
    end
    @board[element.x][element.y] = element
    @view.add element
  end

end
