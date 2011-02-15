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

	public static final String KEY_UP    = new String(new char[] { 27, 91, 65 });  // ESC + [A
	public static final String KEY_DOWN  = new String(new char[] { 27, 91, 66 });  // ESC + [B
	public static final String KEY_LEFT  = new String(new char[] { 27, 91, 68 });  // ESC + [D
	public static final String KEY_RIGHT = new String(new char[] { 27, 91, 67 });  // ESC + [C
	
	private final InputStreamReader in;
	private final PrintStream out;

	public Terminal(InputStream in, PrintStream out) {
		this.in  = new InputStreamReader(in);
		this.out = out;
	}

	public void clear() {
		// TODO Auto-generated method stub
		
	}

	public void println(String string) {
		// TODO Auto-generated method stub
		
	}

	public void close() {
		// TODO Auto-generated method stub
		
	}

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
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return result;
	}

}
