# 
# To change this template, choose Tools | Templates
# and open the template in the editor.
require 'ncurses' 

class Terminal
  KEY_UP =  27.chr + '[A'     # Ncurses::KEY_UP
  KEY_DOWN = 27.chr + '[B'    # Ncurses::KEY_DOWN
  KEY_LEFT = 27.chr + '[D'    # Ncurses::KEY_LEFT
  KEY_RIGHT = 27.chr + '[C'   # Ncurses::KEY_RIGHT
  NOTHING = -1
  
  
  def Terminal.init
    Ncurses.initscr
    Ncurses.cbreak           # provide unbuffered input
    Ncurses.noecho           # turn off input echoing
    Ncurses.nonl             # turn off newline translation
    #Ncurses.stdscr.addstr("Press a key to continue") # output string
    #Ncurses.stdscr.getch                             # get a charachter
    Ncurses.curs_set(0)
    Ncurses.stdscr.nodelay(true)
    Ncurses.stdscr.intrflush(false) # turn off flush-on-interrupt

    @nc = Ncurses
    @screen = Ncurses.stdscr

    
    #Ncurses.stdscr.keypad(true)     # turn on keypad mode
    #
  end
  
  def Terminal.cursorTo x, y
    @screen.move y, x
  end
  #module_function :cursorTo
  
  def Terminal.clear
    @screen.clear
  end
  
  def Terminal.put str
    @screen.addstr str
  end
  
  
  def Terminal.get
    result = ''
    
    last = @screen.getch
    while last != -1
      result += last.chr
      last = @screen.getch
    end

    if result == ''
      return nil
    end
    result
  end
  
  def Terminal.uninit
    Ncurses.curs_set 1
    Ncurses.echo
    Ncurses.nocbreak
    Ncurses.nl
    Ncurses.endwin
  end

end