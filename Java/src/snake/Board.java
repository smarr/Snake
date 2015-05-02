/**
 * Copyright (C) 2008 - 2011 Stefan Marr <mail@stefan-marr.de>
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package snake;

import java.util.Random;

/**
 * Implements the logic of the game board.
 * 
 * @author Stefan Marr
 */
public class Board {
	
	private final int		 	  width;
	private final int 		 	  height;
	private final GameElement[][] board;
	private final Random     generator;
	
	private BoardView        view;

	public Board(int width, int height, int numberOfApples) {
		this.width  = width;
		this.height = height;
		board       = new GameElement[height][width];
		generator   = new Random();
		
		while (numberOfApples > 0) {
			addApple();
			numberOfApples--;
		}
	}

	public void addApple() {
		boolean added = false;
		
		while (!added) {
			int x = generator.nextInt(width);
			int y = generator.nextInt(height);
			
			if (board[y][x] == null) {
				Apple apple = new Apple(x, y);
				board[y][x] = apple;
				added = true;
				if (view != null) {
					view.add(apple);
				}
			}
		}
	}

	public int getWidth() {
		return width;
	}
	
	public int getHeight() {
		return height;
	}
	
	public BoardView createView(Terminal term) {
		view = new BoardView(board, term);
		return view;
	}

	public void add(SnakeElement elem) {
		board[elem.getY()][elem.getX()] = elem;
		view.add(elem);
	}

	public boolean isApple(int x, int y) {
		x %= width;
		y %= height;
		
		return board[y][x] instanceof Apple;
	}

	public boolean isSnake(int x, int y) {
		x %= width;
		y %= height;
		
		return board[y][x] instanceof SnakeElement;
	}

	public void remove(SnakeElement elem) {
		board[elem.getY()][elem.getX()] = null;
		view.remove(elem);
	}
}
