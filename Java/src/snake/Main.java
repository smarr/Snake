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
 * Utility class to set up the game for on the standard terminal.
 * 
 * @author Stefan Marr
 */
public class Main {

	/**
	 * Initialize and run the game on the standard terminal.
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		// initialize all required parts
		Terminal term  = new Terminal(System.in, System.out);
		Board board    = new Board(10, 10, 5);
		BoardView view = new BoardView(board, term);
		board.setView(view);
		Snake snake    = new Snake(5, 5, board);
		
		
		// initialize UI
		term.clear();
		view.drawBoarder();
		view.showContent();
		
		// main game loop
		executeGameLoop(term, snake);
		
		// game is done, shut down and clean up
		term.println("GAME OVER");
		
		try { Thread.sleep(5); } catch (InterruptedException e) {  /* Not important here. Program is ending now. */ }
		term.close();
	}

	/**
	 * Main game loop.
	 * 
	 * @param term
	 * @param snake
	 */
	@SuppressWarnings("static-access")
	private static void executeGameLoop(Terminal term, Snake snake) {
		boolean continueGame = true;
		String dir = term.KEY_UP;
		
		while (continueGame) {
			String input = term.get();
			
			// check whether the input is one of our key sequences, and remember direction
			if (term.KEY_UP   .equals(input)
			    || term.KEY_DOWN .equals(input)
			    || term.KEY_LEFT .equals(input)
			    || term.KEY_RIGHT.equals(input)) { dir = input;	}
			
			// decide depending on the direction how to move the snake
			if 		(term.KEY_UP   .equals(dir)) { continueGame = snake.moveUp();    }
			else if (term.KEY_DOWN .equals(dir)) { continueGame = snake.moveDown();  }
			else if (term.KEY_LEFT .equals(dir)) { continueGame = snake.moveLeft();  }
			else if (term.KEY_RIGHT.equals(dir)) { continueGame = snake.moveRight(); }
			
			try { Thread.sleep(250); } catch (InterruptedException e) {  /* Not important here. Will ignore it and just continue looping. */ }			
		}
	}
}
