use std::error::Error;
use std::io::stdin;
use std::io::Read;

use std::thread;
use std::sync::mpsc::{channel, Receiver};

pub fn init() -> Receiver<ControlKeys> {
    let (tx, rx) = channel::<ControlKeys>();
    thread::spawn(move|| {
        loop {
            tx.send(read_stdin()).unwrap();
        }
    });
    rx
}

pub fn get(rx: &Receiver<ControlKeys>, old: ControlKeys) -> ControlKeys {
    match rx.try_recv() {
        Ok(key) => key,
        Err(_)  => old
    }
}

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
fn read_stdin() -> ControlKeys {
    let mut reader = stdin();
    let mut buf = &mut [0u8; 10];

    match reader.read(buf) {
        Err(why) => panic!("couldn't read stdin: {}",
                           Error::description(&why)),
        Ok(size) => {
            // check the ascii values of the escape sequences
            if size == 3 && buf[0] == 27 && buf[1] == 91 {
                if buf[2] == 65 { return ControlKeys::KeyUp;    }
                if buf[2] == 66 { return ControlKeys::KeyDown;  }
                if buf[2] == 67 { return ControlKeys::KeyRight; }
                if buf[2] == 68 { return ControlKeys::KeyLeft;  }
            }
            return ControlKeys::Unknown;
        }
    }
}

pub fn put(str: &str) {
    print!("{0}", str);
}
