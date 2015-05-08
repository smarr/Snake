// THINGS to complain about: `in` keyword, `move` keyword WTF!!!

use std::cell::RefCell;
use std::rc::Rc;

use board::Board;
use game_elements::{SnakeElement, new_snake_element};

pub struct Snake {
    head  : Rc<RefCell<SnakeElement>>,
    tail  : Rc<RefCell<SnakeElement>>,
}

impl Snake {
    pub fn move_up(&mut self, board: &mut Board) -> bool {
        let new_x;
        let new_y;
        {
            let head  = self.head.borrow();
            new_x = head.get_x();
            let mut y = head.get_y();
            if y == 0 { y = board.get_height(); }
            new_y = y - 1;
        }
        self.do_move(new_snake_element(new_x, new_y), board)
    }

    pub fn move_down(&mut self, board: &mut Board) -> bool {
        let new_x;
        let new_y;
        {
            let head  = self.head.borrow();
            new_x = head.get_x();
            let mut y = head.get_y() + 1;
            if y == board.get_height() { y = 0; }
            new_y = y;
        }
        self.do_move(new_snake_element(new_x, new_y), board)
    }

    pub fn move_left(&mut self, board: &mut Board) -> bool {
        let new_x;
        let new_y;
        {
            let head  = self.head.borrow();
            let mut x = head.get_x();
            if x == 0 { x = board.get_width() }
            new_x = x - 1;
            new_y = head.get_y();
        }
        self.do_move(new_snake_element(new_x, new_y), board)
    }

    pub fn move_right(&mut self, board: &mut Board) -> bool {
        let new_x;
        let new_y;
        {
            let head  = self.head.borrow();
            let mut x = head.get_x() + 1;
            if x == board.get_width() { x = 0; }
            new_x = x;
            new_y = head.get_y();
        }
        self.do_move(new_snake_element(new_x, new_y), board)
    }

    fn do_move(&mut self, new_head: Rc<RefCell<SnakeElement>>, board: &mut Board) -> bool {
        let x;
        let y;
        {
            let nh = new_head.borrow();
            x = nh.get_x();
            y = nh.get_y();
        }
        {
            let mut head = self.head.borrow_mut();
            head.set_prev(new_head.clone());
        }
        self.head = new_head.clone();

        if board.is_apple(x, y) {
            board.add_apple();
        } else {
            if board.is_snake(x, y) {
                return false;
            }

            board.remove(self.tail.clone());
            let prev;
            {
                let tail = self.tail.borrow();
                prev = tail.get_prev().unwrap();
            }
            self.tail = prev;
        }

        board.add(new_head.clone());

        true
    }
}

pub fn new(x: usize, y: usize, board: &mut Board) -> Snake {
    let snake_start = new_snake_element(x, y);
    let snake = Snake {head:  snake_start.clone(),
                       tail:  snake_start.clone()};
    board.add(snake_start);
    snake
}
