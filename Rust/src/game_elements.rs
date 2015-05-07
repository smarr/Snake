use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone)]
struct Point {
    x: usize,
    y: usize
}

impl Point {
    pub fn get_x(&self) -> usize {
        self.x
    }

    pub fn get_y(&self) -> usize {
        self.y
    }
}

pub struct SnakeElement {
    coord: Point,
    prev:  Option<Rc<RefCell<SnakeElement>>>
}

impl SnakeElement {
    pub fn get_x(&self) -> usize {
        self.coord.x
    }

    pub fn get_y(&self) -> usize {
        self.coord.y
    }

    pub fn get_prev(&self) -> Option<Rc<RefCell<SnakeElement>>> {
        let ref e = self.prev;
        e.clone()
    }

    pub fn set_prev(&mut self, prev: Rc<RefCell<SnakeElement>>) {
        self.prev = Some(prev)
    }
}

#[derive(Clone)]
pub enum GameElement {
    Apple(Point),
    SnakeElement(Rc<RefCell<SnakeElement>>),
    Empty
}

pub fn new_apple(x: usize, y: usize) -> GameElement {
    GameElement::Apple(Point {x:x, y:y})
}

pub fn new_snake_element(x: usize, y: usize) -> Rc<RefCell<SnakeElement>> {
    Rc::new(RefCell::new(SnakeElement {
        coord: Point {x:x, y:y},
        prev:  None}))
}
