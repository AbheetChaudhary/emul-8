use std::fs::File;
use std::io::Read;
mod opcode;
use crate::opcode::Inst;

fn main() {
    let mut file_h = File::open("invaders/invaders.h").unwrap();
    let mut data_h = Vec::<u8>::new();
    file_h.read_to_end(&mut data_h).expect("Reading .h failed");

    let mut pc = 0 as usize;
    loop {
        print!("{:04} ", pc);
        pc += opcode::disassemble8080(&data_h[pc..]).unwrap();

        if pc >= data_h.len() {
            break;
        }
    }
}
