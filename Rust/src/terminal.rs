use std::error::Error;
use std::io::stdin;
use std::io::Read;

pub fn clear() {
    println!("\u{001B}[2J");  // "\u001B" is the ASCII code for ESCape
}

pub fn println(str: &str) {
    println!("{0}", str);
}

pub fn set_cursor(x: usize, y: usize) {
    print!("\u{001B}[{0};{1}H", y + 1, x + 1);
}

pub enum ControlKeys { KeyUp, KeyDown, KeyLeft, KeyRight, Unknown }

/**
 * Read input from standard in.
 * It is expected to deliver a sequence of characters for every key press.
 * The console should be set in an appropriate manner to avoid line buffering.
 */
pub fn get() -> ControlKeys {
    println!("read in");
    let mut reader = stdin();
    let mut s = String::new();

    match reader.read_to_string(&mut s) { // this is still blocking :(
       Err(why) => {
           panic!("couldn't read {}", Error::description(&why))
       },
       Ok(_)    => {
           println!("in {0}", s);
           match &*s {
               "\u{001B}[A" => ControlKeys::KeyUp,
               "\u{001B}[B" => ControlKeys::KeyDown,
               "\u{001B}[D" => ControlKeys::KeyLeft,
               "\u{001B}[C" => ControlKeys::KeyRight,
               _            => ControlKeys::Unknown
           }
       }
    }
}

pub fn put(str: &str) {
    print!("{0}", str);
}
