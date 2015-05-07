use std::cell::RefCell;
use std::rc::Rc;

use board::Board;
use game_elements::{SnakeElement, new_snake_element};

pub struct Snake {
    head  : Rc<RefCell<SnakeElement>>,
    tail  : Rc<RefCell<SnakeElement>>,
    board : Rc<RefCell<Board>>
}

impl Snake {
    pub fn move_up(&mut self) -> bool {
        let new_x;
        let new_y;
        {
            let head  = self.head.borrow();
            let board = self.board.borrow();
            new_x = head.get_x();
            new_y = (head.get_y() - 1) % board.get_height();
        }
        self.do_move(new_snake_element(new_x, new_y))
    }

    pub fn move_down(&mut self) -> bool {
        let new_x;
        let new_y;
        {
            let head  = self.head.borrow();
            let board = self.board.borrow();
            new_x = head.get_x();
            new_y = (head.get_y() + 1) % board.get_height();
        }
        self.do_move(new_snake_element(new_x, new_y))
    }

    pub fn move_left(&mut self) -> bool {
        let new_x;
        let new_y;
        {
            let head  = self.head.borrow();
            let board = self.board.borrow();
            new_x = (head.get_x() - 1) % board.get_width();
            new_y = head.get_y();
        }
        self.do_move(new_snake_element(new_x, new_y))
    }

    pub fn move_right(&mut self) -> bool {
        let new_x;
        let new_y;
        {
            let head  = self.head.borrow();
            let board = self.board.borrow();
            new_x = (head.get_x() + 1) % board.get_width();
            new_y = head.get_y();
        }
        self.do_move(new_snake_element(new_x, new_y))
    }

    fn do_move(&mut self, new_head: Rc<RefCell<SnakeElement>>) -> bool {
        let x;
        let y;
        {
            let mut nh = new_head.borrow_mut();
            x = nh.get_x();
            y = nh.get_y();
        }
        {
            let mut head = self.head.borrow_mut();
            head.set_prev(new_head.clone());
        }
        self.head = new_head.clone();

        let mut board = self.board.borrow_mut();

        let is_apple = board.is_apple(x, y);
        if is_apple {
            board.add_apple();
        } else {
            if board.is_snake(x, y) {
                return false;
            }

            board.remove(self.tail.clone());
            let prev;
            {
                let tail = self.tail.borrow();
                prev = tail.get_prev();
            }
            self.tail = prev;
            let mut tail = self.tail.borrow_mut();
            tail.set_prev_to_none();
        }

        board.add(new_head.clone());

        true
    }
}

pub fn new(x: usize, y: usize, board: Rc<RefCell<Board>>) -> Snake {
    let snake_start = new_snake_element(x, y);
    let snake = Snake {head:  snake_start.clone(),
                       tail:  snake_start.clone(),
                       board: board.clone()};
    let b = board.borrow();
    b.add(snake_start);
    snake
}
