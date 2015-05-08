use terminal;
use game_elements::GameElement;
use board::Board;

pub trait BoardViewer {
    fn draw_boarder(&self);
    fn show_content(&self);

    fn view(&self, elem: GameElement) {
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

    fn hide(&self, x: usize, y: usize) {
        terminal::set_cursor(x + 1, y + 1);
        terminal::put(" ");
    }
}

impl BoardViewer for Board {
    /**
     * Draws the surrounding border in the following style:
     *
     *   /----\
     *   |    |
     *   |    |
     *   \----/
     */
    fn draw_boarder(&self) {
        terminal::set_cursor(0, 0);
        terminal::put("/");
        for _ in 0..self.get_width() { terminal::put("-"); }
        terminal::put("\\");

        for i in 0..self.get_height() {
            terminal::set_cursor(             0, i + 1);
            terminal::put("|");
            terminal::set_cursor(self.get_width() + 1, i + 1);
            terminal::put("|");
        }
        terminal::put("\n");

        terminal::put("\\");
        for _ in 0..self.get_width() { terminal::put("-"); }
        terminal::put("/");
    }

    fn show_content(&self) {
        self.for_each_element(|e| {self.view(e.clone())})
    }
}
