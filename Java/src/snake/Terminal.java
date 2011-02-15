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

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;

/**
 * Takes care of the interaction with a standard ANSI? terminal.
 * 
 * @author Stefan Marr
 */
public class Terminal {

	public static final String KEY_UP    = new String("\u001B[A");  // ESC + [A
	public static final String KEY_DOWN  = new String("\u001B[B");  // ESC + [B
	public static final String KEY_LEFT  = new String("\u001B[D");  // ESC + [D
	public static final String KEY_RIGHT = new String("\u001B[C");  // ESC + [C
	
	private final InputStreamReader in;
	private final PrintStream out;

	/**
	 * Create a terminal wrapper around an input and output stream,
	 * which are typically System.in and System.out.
	 * 
	 * @param in
	 * @param out
	 */
	public Terminal(InputStream in, PrintStream out) {
		this.in  = new InputStreamReader(in);
		this.out = out;
	}

	/**
	 * Clear the terminal, i.e., remove all output.
	 */
	public void clear() {
		out.print("\u001B[2J");  // "\u001B" is the ASCII code for ESCape
	}

	/**
	 * Output string and add line break.
	 * 
	 * @param string
	 */
	public void println(String string) {
		out.println(string);
	}

	/**
	 * Cleanup and close terminal.
	 * 
	 * REM: currently a NOP since there is not much to do here.
	 */
	public void close() {}

	/**
	 * Read input from standard in.
	 * It is expected to deliver a sequence of characters for every key press.
	 * The console should be set in an appropriate manner to avoid line buffering. 
	 * @return
	 */
	public String get() {
		char[] buffer = new char[1024];
		String result = "";
		
		try {
			int numCharsRead = in.read(buffer);
			result = String.valueOf(buffer, 0, numCharsRead);
		} catch (IOException e) {
			out.println("Could not read a character from input. Should not happen. See details below.");
			e.printStackTrace();
		}
		
		return result;
	}

}
