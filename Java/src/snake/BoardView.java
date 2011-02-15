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
 * The View for the game board.
 * Knows how to draw on the terminal.
 * 
 * @author Stefan Marr
 */
public class BoardView {
	
	private final int width;
	private final int height;
	private final Terminal term;
	private Object[][] board;

	public BoardView(Board board, Terminal term) {
		width  	  = board.getWidth();
		height 	  = board.getHeight();
		this.term = term;
		board.setView(this);
	}

	/**
	 * Draws the surrounding border in the following style:
	 * 
	 *   /----\
	 *   |    |
	 *   |    |
	 *   \----/
	 */
	public void drawBoarder() {
		term.setCursor(0, 0);
		term.put("/");
		for (int i = 0; i < width; i++) { term.put("-"); }
		term.put("\\");
		
		for (int i = 0; i < height; i++) {
			term.setCursor(        0, i + 1);
			term.put("|");
			term.setCursor(width + 1, i + 1);
			term.put("|");
		}
		term.put("\n");
		
		term.put("\\");
		for (int i = 0; i < width; i++) { term.put("-"); }
		term.put("/");
	}

	public void add(Apple apple) {
		term.setCursor(apple.getX() + 1, apple.getY() + 1);
		term.put("o");
	}
	
	public void add(SnakeElement elem) {
		term.setCursor(elem.getX() + 1, elem.getY() + 1);
		term.put("#");
	}
	
	public void remove(GameElement elem) {
		term.setCursor(elem.getX() + 1, elem.getY() + 1);
		term.put(" ");
	}

	public void setBoardData(Object[][] board) {
		this.board = board;
	}

	public void updateCompletely() {
		for (Object[] row : board) {
			for (Object elem : row) {
				if (elem instanceof Apple) {
					add((Apple)elem);
				}
			}
		}
	}

}
