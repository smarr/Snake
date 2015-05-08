extern crate rand;

use std::cell::RefCell;
use std::rc::Rc;

use self::rand::Rng;
use game_elements::{GameElement, new_apple, SnakeElement};
use board_view::BoardViewer;

pub struct Board {
    width:  usize,
    height: usize,
    board:  Vec<Vec<GameElement>>,
    random: rand::ThreadRng
}

impl Board {
    pub fn add_apple(&mut self) {
        let mut added = false;

        while !added {
            let x = self.random.gen_range(0, self.width);
            let y = self.random.gen_range(0, self.height);

            added = match self.board[x][y] {
                GameElement::Empty => {
                    self.board[x][y] = new_apple(x, y);
                    self.view(new_apple(x, y));
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

    pub fn add(&mut self, elem: Rc<RefCell<SnakeElement>>) {
        let x;
        let y;
        {   let el = elem.borrow();
            x = el.get_x();
            y = el.get_y();
        }
        self.board[x][y] = GameElement::SnakeElement(elem.clone());
        self.view(GameElement::SnakeElement(elem.clone()))
    }

    pub fn is_apple(&self, x: usize, y: usize) -> bool {
        let _x = x % self.width;
        let _y = y % self.height;

        match self.board[_x][_y] {
            GameElement::Apple(_) => true,
            _                     => false
        }
    }

    pub fn is_snake(&self, x: usize, y: usize) -> bool {
        let _x = x % self.width;
        let _y = y % self.height;

        match self.board[_x][_y] {
            GameElement::SnakeElement(_) => true,
            _                            => false
        }
    }

    pub fn remove(&mut self, elem: Rc<RefCell<SnakeElement>>) {
        let e = elem.borrow();
        self.board[e.get_x()][e.get_y()] = GameElement::Empty;
        self.hide(e.get_x(), e.get_y());
    }

    pub fn for_each_element<F>(&self, f:F) where F: Fn(GameElement) {
        for row in self.board.iter() {
            for e in row.iter() {
                f(e.clone())
            }
        }
    }
}

pub fn new(width: usize, height: usize, num_of_apples: usize) -> Board {
    let mut b = Board {width:  width,
                       height: height,
                       board:  vec![vec![GameElement::Empty; height]; width],
                       random: rand::thread_rng()};

    for _ in 0..num_of_apples {
        b.add_apple();
    }
    b
}
