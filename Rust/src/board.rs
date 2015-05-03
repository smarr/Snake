extern crate rand;

use std::cell::RefCell;
use std::rc::Rc;

use board_view;
use self::rand::Rng;
use game_elements::{GameElement, new_apple, SnakeElement};
use board_view::BoardView;


pub struct Board {
    width:  usize,
    height: usize,
    board:  Rc<RefCell<Vec<Vec<GameElement>>>>,
    random: rand::ThreadRng,
    view:   Rc<BoardView>
}

impl Board {
    pub fn add_apple(&mut self) {
        let mut added = false;

        let mut board = self.board.borrow_mut();
        while !added {
            let x = self.random.gen_range(0, self.width);
            let y = self.random.gen_range(0, self.height);

            added = match board[x][y] {
                GameElement::Empty => {
                    board[x][y] = new_apple(x, y);
                    self.view.add(new_apple(x, y));
                    true
                },
                _ => false
            }
        }
    }

    pub fn get_width(&self) -> usize {
        self.width
    }

    pub fn get_height(&self) -> usize {
        self.height
    }

    pub fn get_view(&self) -> Rc<BoardView> {
        self.view.clone()
    }

    pub fn add(&self, elem: Rc<RefCell<SnakeElement>>) {
        let x;
        let y;
        {   let el = elem.borrow();
            x = el.get_x();
            y = el.get_y();
        }
        let mut board = self.board.borrow_mut();
        board[x][y] = GameElement::SnakeElement(elem.clone());
        self.view.add(GameElement::SnakeElement(elem.clone()))
    }

    pub fn is_apple(&self, x: usize, y: usize) -> bool {
        let _x = x % self.width;
        let _y = y % self.height;

        let board = self.board.borrow();
        match board[_x][_y] {
            GameElement::Apple(_) => true,
            _                     => false
        }
    }

    pub fn is_snake(&self, x: usize, y: usize) -> bool {
        let _x = x % self.width;
        let _y = y % self.height;

        let board = self.board.borrow();
        match board[_x][_y] {
            GameElement::SnakeElement(_) => true,
            _                            => false
        }
    }

    pub fn remove(&self, elem: Rc<RefCell<SnakeElement>>) {
        let e = elem.borrow();
        let mut board = self.board.borrow_mut();
        board[e.get_x()][e.get_y()] = GameElement::Empty;
        self.view.remove(e.get_x(), e.get_y());
    }
}

pub fn new(width: usize, height: usize, num_of_apples: usize) -> Board {
    let board_arr = Rc::new(RefCell::new(vec![vec![GameElement::Empty; height]; width]));
    let mut b = Board {width:  width,
                       height: height,
                       board:  board_arr.clone(),
                       random: rand::thread_rng(),
                       view:   Rc::new(board_view::new(board_arr.clone()))};

    for _ in 0..num_of_apples {
        b.add_apple();
    }
    b
}
