mod terminal;
mod board;
mod board_view;
mod game_elements;
mod snake;

use std::cell::RefCell;
use std::rc::Rc;
use snake::Snake;
use terminal::ControlKeys;

fn execute_game_loop(snake : &mut Snake) {
    let mut continue_game = true;
    let mut dir;

    while continue_game {
        dir = terminal::get();
        match dir {
            ControlKeys::KeyUp    => continue_game = snake.move_up(),
            ControlKeys::KeyRight => continue_game = snake.move_right(),
            ControlKeys::KeyDown  => continue_game = snake.move_down(),
            ControlKeys::KeyLeft  => continue_game = snake.move_left(),
            _ => ()
        }
    }
}

fn main() {
    // initialize all required parts
    let board = Rc::new(RefCell::new(board::new(10, 10, 5)));
    let view;
    {   let b = board.borrow();
        view = Rc::new(b.get_view());
    }
    let mut snake = snake::new(5, 5, board.clone());

    // initialize UI
    terminal::clear();

    view.draw_boarder();
    view.show_content();

	// main game loop
	execute_game_loop(&mut snake);

	// game is done, shut down and clean up
	terminal::println("GAME OVER");
}
