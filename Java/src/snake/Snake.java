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

/**
 * @author smarr
 *
 */
public class Snake {
	
	private SnakeElement head;
	private SnakeElement tail;
	private final Board board;

	public Snake(int x, int y, Board board) {
		head = new SnakeElement(x, y);
		tail = head;
		this.board = board;
		board.add(head);
	}

	public boolean moveUp() {
		int newX = head.getX();
		int newY = overflow(head.getY() - 1, board.getHeight());
		return move(new SnakeElement(newX, newY));
	}

	public boolean moveDown() {
		int newX = head.getX();
		int newY = overflow(head.getY() + 1, board.getHeight());
		return move(new SnakeElement(newX, newY));
	}
	
	public boolean moveLeft() {
		int newX = overflow(head.getX() - 1, board.getWidth());
		int newY = head.getY();
		return move(new SnakeElement(newX, newY));
	}
	
	public boolean moveRight() {
		int newX = overflow(head.getX() + 1, board.getWidth());
		int newY = head.getY();
		return move(new SnakeElement(newX, newY));
	}
	
	private boolean move(SnakeElement newHead) {
		head.setPrev(newHead);
		head = newHead;
		
		if (board.isApple(newHead.getX(), newHead.getY()))
			board.addApple();
		else
		{
			if (board.isSnake(newHead.getX(), newHead.getY()))
				return false;
			
			board.remove(tail);
			tail = tail.getPrev();
		}
		
		board.add(newHead);

		return true;
	}

	
	private int overflow(int val, int max) {
		if (val < 0)
			val = max + val;
		
		val %= max;
		
		return val;
	}
}
