mod terminal;
mod board;
mod board_view;
mod game_elements;
mod snake;

use board::Board;
use board_view::BoardViewer;
use snake::Snake;
use terminal::ControlKeys;

use std::io;
use std::io::Write;
use std::sync::mpsc::Receiver;
use std::thread;

fn execute_game_loop(snake : &mut Snake, input_recv: Receiver<ControlKeys>, board: &mut Board) {
    let mut continue_game = true;
    let mut dir = ControlKeys::KeyUp;

    while continue_game {
        dir = terminal::get(&input_recv, dir);

        match dir {
            ControlKeys::KeyUp    => continue_game = snake.move_up(board),
            ControlKeys::KeyRight => continue_game = snake.move_right(board),
            ControlKeys::KeyDown  => continue_game = snake.move_down(board),
            ControlKeys::KeyLeft  => continue_game = snake.move_left(board),
            _ => ()
        }
        io::stdout().flush().unwrap();

        thread::sleep_ms(250);
    }
}

fn main() {
    // initialize all required parts
    let input_recv = terminal::init();
    let board = &mut board::new(10, 10, 5);
    let mut snake = snake::new(5, 5, board);

    // initialize UI
    terminal::clear();

    board.draw_boarder();
    board.show_content();

    io::stdout().flush().unwrap();

	// main game loop
	execute_game_loop(&mut snake, input_recv, board);

	// game is done, shut down and clean up
	terminal::println("GAME OVER");
}
