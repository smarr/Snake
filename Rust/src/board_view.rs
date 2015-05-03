use terminal;
use game_elements::GameElement;
use std::rc::Rc;
use std::cell::RefCell;

pub struct BoardView {
    width:  usize,
    height: usize,
    board:  Rc<RefCell<Vec<Vec<GameElement>>>>,
}

impl BoardView {
    /**
     * Draws the surrounding border in the following style:
     *
     *   /----\
     *   |    |
     *   |    |
     *   \----/
     */
    pub fn draw_boarder(&self) {
        terminal::set_cursor(0, 0);
        terminal::put("/");
        for _ in 0..self.width { terminal::put("-"); }
        terminal::put("\\");

        for i in 0..self.height {
            terminal::set_cursor(             0, i + 1);
            terminal::put("|");
            terminal::set_cursor(self.width + 1, i + 1);
            terminal::put("|");
        }
        terminal::put("\n");

        terminal::put("\\");
        for _ in 0..self.width { terminal::put("-"); }
        terminal::put("/");
    }


    pub fn add(&self, elem: GameElement) {
        match elem {
            GameElement::Apple(p) => {
                terminal::set_cursor(p.get_x() + 1, p.get_y() + 1);
                terminal::put("o");
            },
            GameElement::SnakeElement(s) => {
                let snake = s.borrow();
                terminal::set_cursor(snake.get_x() + 1, snake.get_y() + 1);
                terminal::put("#");
            },
            GameElement::Empty => { () }
        }
    }

    pub fn remove(&self, x: usize, y: usize) {
        terminal::set_cursor(x + 1, y + 1);
        terminal::put(" ");
    }

    pub fn show_content(&self) {
        let board = self.board.borrow();
        for row in board.iter() {
            for e in row.iter() {
                self.add(e.clone())
            }
        }
    }
}

pub fn new(board: Rc<RefCell<Vec<Vec<GameElement>>>>) -> BoardView {
    let b = board.borrow();
    BoardView {width:  b.len(),
               height: b[0].len(),
               board:  board.clone()}
}
