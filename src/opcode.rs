use std::error::Error;
use std::fmt;

/// An instruction which has 2 more bytes worth of arguments
macro_rules! double_arg_inst {
    ($val:literal, $buf:expr) => {{
        let (msb, lsb) = get_bytes($buf).unwrap();
        println!("{:02x} {:02x} {:02x} {}{:02x}{:02x}", ($buf).get(0).unwrap(), lsb, msb, $val, msb, lsb);
        Ok(3)
    }};
}

/// An instruction which has one byte worth of argument
macro_rules! single_arg_inst {
    ($val:literal, $buf:expr) => {{
        let arg = get_arg($buf).unwrap();
        println!("{:02x} {:02x}    {}{:02x}", ($buf).get(0).unwrap(), arg, $val, arg);
        Ok(2)
    }};
}

/// An instruction which does not take any argument
macro_rules! single_byte_inst {
    ($val:literal, $buf:expr) => {{
        println!("{:02x}       {}", ($buf).get(0).unwrap(), $val);
        Ok(1)
    }};
}

#[repr(u8)]
#[derive(Debug)]
// make sure that NOP is first so its implicit discriminant will be zero
pub enum Inst {
    NOP, ACI, ADC, ADD, ADI, ANA, ANI, CALL, CC, CM, CMA, CMC, CMP, CNC, CNZ, CP, CPE, CPI, CPO, CZ, DAA, DAD, DCR, DCX, DI, EI, HLT, IN, INR, INX, JC, JM, JMP, JNC, JNZ, JP, JPE, JPO, JZ, LDA, LDAX, LHLD, LXI, MOV, MVI, ORA, ORI, OUT, PCHL, POP, PUSH, RAL, RAR, RC, RET, RIM, RLC, RM, RNC, RNZ, RP, RPE, RPO, RRC, RST, RZ, SBB, SBI, SHLD, SIM, SPHL, STA, STAX, STC, SUB, SUI, XCHG, XRA, XRI, XTHL,
}

#[derive(Debug)]
pub enum DisassembleError {
    UnexpectedByte,
}

impl Error for DisassembleError {}

impl fmt::Display for DisassembleError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "UnexpectedByte")
    }
}

/// Take a byte stream, look its first byte and based on that, look 1-2 bytes more if needed. 
/// Return total number of bytes read if successful
pub fn disassemble8080(codebuffer: &[u8]) -> Result<usize, DisassembleError> {
    match codebuffer[0] {
        0x00 => single_byte_inst!("NOP", codebuffer),
        0x01 => double_arg_inst!("LXI\tB, #0x", codebuffer),
        0x02 => single_byte_inst!("STAX\tB", codebuffer),
        0x03 => single_byte_inst!("INX\tB", codebuffer),
        0x04 => single_byte_inst!("INR\tB", codebuffer),
        0x05 => single_byte_inst!("DCR\tB", codebuffer),
        0x06 => single_arg_inst!("MVI\tB, #0x", codebuffer),
        0x07 => single_byte_inst!("RLC\tB", codebuffer),
        0x08 => single_byte_inst!("NOP", codebuffer),
        0x09 => single_byte_inst!("DAD\tB", codebuffer),
        0x0a => single_byte_inst!("LDAX\tB", codebuffer),
        0x0b => single_byte_inst!("DCX\tB", codebuffer),
        0x0c => single_byte_inst!("INR\tC", codebuffer),
        0x0d => single_byte_inst!("DCR\tC", codebuffer),
        0x0e => single_arg_inst!("MVI\tC, #0x", codebuffer),
        0x0f => single_byte_inst!("RRC", codebuffer),
        0x10 => single_byte_inst!("NOP", codebuffer),
        0x11 => double_arg_inst!("LXI\tD, #0x", codebuffer),
        0x12 => single_byte_inst!("STAX\tD", codebuffer),
        0x13 => single_byte_inst!("INX\tD", codebuffer),
        0x14 => single_byte_inst!("INR\tD", codebuffer),
        0x15 => single_byte_inst!("DCR\tD", codebuffer),
        0x16 => single_arg_inst!("MVI\tD, #0x", codebuffer),
        0x17 => single_byte_inst!("RAL", codebuffer),
        0x18 => single_byte_inst!("NOP", codebuffer),
        0x19 => single_byte_inst!("DAD\tD", codebuffer),
        0x1a => single_byte_inst!("LDAX\tD", codebuffer),
        0x1b => single_byte_inst!("DXC\tD", codebuffer),
        0x1c => single_byte_inst!("INR\tE", codebuffer),
        0x1d => single_byte_inst!("DCR\tE", codebuffer),
        0x1e => single_arg_inst!("MVI\tE, #0x", codebuffer),
        0x1f => single_byte_inst!("NOP", codebuffer),
        0x20 => single_byte_inst!("RIM", codebuffer),
        0x21 => double_arg_inst!("LXI\tH, #0x", codebuffer),
        0x22 => double_arg_inst!("SHLD\t$", codebuffer),
        0x23 => single_byte_inst!("INX\tH", codebuffer),
        0x24 => single_byte_inst!("INR\tH", codebuffer),
        0x25 => single_byte_inst!("DCR\tH", codebuffer),
        0x26 => single_arg_inst!("MVI\tH, #0x", codebuffer),
        0x27 => single_byte_inst!("DAA", codebuffer),
        0x28 => single_byte_inst!("NOP", codebuffer),
        0x29 => single_byte_inst!("DAD\tH", codebuffer),
        0x2a => double_arg_inst!("LHLD\t$", codebuffer),
        0x2b => single_byte_inst!("DCX\tH", codebuffer),
        0x2c => single_byte_inst!("INR\tL", codebuffer),
        0x2d => single_byte_inst!("DCR\tL", codebuffer),
        0x2e => single_arg_inst!("MVI\tL, #0x", codebuffer),
        0x2f => single_byte_inst!("CMA", codebuffer),
        0x30 => single_byte_inst!("SIM", codebuffer),
        0x31 => double_arg_inst!("LXI\tSP, #0x", codebuffer),
        0x32 => double_arg_inst!("STA\t$", codebuffer),
        0x33 => single_byte_inst!("INX\tSP", codebuffer),
        0x34 => single_byte_inst!("INR\t#0x", codebuffer),
        0x35 => single_byte_inst!("DCR\t#0x", codebuffer),
        0x36 => single_arg_inst!("MVI\tM, #0x", codebuffer),
        0x37 => single_byte_inst!("STC", codebuffer),
        0x38 => single_byte_inst!("NOP", codebuffer),
        0x39 => single_byte_inst!("DAD\tSP", codebuffer),
        0x3a => double_arg_inst!("LDA\t$", codebuffer),
        0x3b => single_byte_inst!("DCX\tSP", codebuffer),
        0x3c => single_byte_inst!("INR\tA", codebuffer),
        0x3d => single_byte_inst!("DCR\tA", codebuffer),
        0x3e => single_byte_inst!("MVI\tA, #0x", codebuffer),
        0x3f => single_byte_inst!("CMC", codebuffer),
        0x40 => single_byte_inst!("MOV\tB, B", codebuffer),
        0x41 => single_byte_inst!("MOV\tB, C", codebuffer),
        0x42 => single_byte_inst!("MOV\tB, D", codebuffer),
        0x43 => single_byte_inst!("MOV\tB, E", codebuffer),
        0x44 => single_byte_inst!("MOV\tB, H", codebuffer),
        0x45 => single_byte_inst!("MOV\tB, L", codebuffer),
        0x46 => single_byte_inst!("MOV\tB, M", codebuffer),
        0x47 => single_byte_inst!("MOV\tB, A", codebuffer),
        0x48 => single_byte_inst!("MOV\tC, B", codebuffer),
        0x49 => single_byte_inst!("MOV\tC, C", codebuffer),
        0x4a => single_byte_inst!("MOV\tC, D", codebuffer),
        0x4b => single_byte_inst!("MOV\tC, E", codebuffer),
        0x4c => single_byte_inst!("MOV\tC, H", codebuffer),
        0x4d => single_byte_inst!("MOV\tC, L", codebuffer),
        0x4e => single_byte_inst!("MOV\tC, M", codebuffer),
        0x4f => single_byte_inst!("MOV\tC, A", codebuffer),
        0x50 => single_byte_inst!("MOV\tD, B", codebuffer),
        0x51 => single_byte_inst!("MOV\tD, C", codebuffer),
        0x52 => single_byte_inst!("MOV\tD, D", codebuffer),
        0x53 => single_byte_inst!("MOV\tD, E", codebuffer),
        0x54 => single_byte_inst!("MOV\tD, H", codebuffer),
        0x55 => single_byte_inst!("MOV\tD, L", codebuffer),
        0x56 => single_byte_inst!("MOV\tD, M", codebuffer),
        0x57 => single_byte_inst!("MOV\tD, A", codebuffer),
        0x58 => single_byte_inst!("MOV\tE, B", codebuffer),
        0x59 => single_byte_inst!("MOV\tE, C", codebuffer),
        0x5a => single_byte_inst!("MOV\tE, D", codebuffer),
        0x5b => single_byte_inst!("MOV\tE, E", codebuffer),
        0x5c => single_byte_inst!("MOV\tE, H", codebuffer),
        0x5d => single_byte_inst!("MOV\tE, L", codebuffer),
        0x5e => single_byte_inst!("MOV\tE, M", codebuffer),
        0x5f => single_byte_inst!("MOV\tE, A", codebuffer),
        0x60 => single_byte_inst!("MOV\tH, B", codebuffer),
        0x61 => single_byte_inst!("MOV\tH, C", codebuffer),
        0x62 => single_byte_inst!("MOV\tH, D", codebuffer),
        0x63 => single_byte_inst!("MOV\tH, E", codebuffer),
        0x64 => single_byte_inst!("MOV\tH, H", codebuffer),
        0x65 => single_byte_inst!("MOV\tH, L", codebuffer),
        0x66 => single_byte_inst!("MOV\tH, M", codebuffer),
        0x67 => single_byte_inst!("MOV\tH, A", codebuffer),
        0x68 => single_byte_inst!("MOV\tL, B", codebuffer),
        0x69 => single_byte_inst!("MOV\tL, C", codebuffer),
        0x6a => single_byte_inst!("MOV\tL, D", codebuffer),
        0x6b => single_byte_inst!("MOV\tL, E", codebuffer),
        0x6c => single_byte_inst!("MOV\tL, H", codebuffer),
        0x6d => single_byte_inst!("MOV\tL, L", codebuffer),
        0x6e => single_byte_inst!("MOV\tL, M", codebuffer),
        0x6f => single_byte_inst!("MOV\tL, A", codebuffer),
        0x70 => single_byte_inst!("MOV\tM, B", codebuffer),
        0x71 => single_byte_inst!("MOV\tM, C", codebuffer),
        0x72 => single_byte_inst!("MOV\tM, D", codebuffer),
        0x73 => single_byte_inst!("MOV\tM, E", codebuffer),
        0x74 => single_byte_inst!("MOV\tM, H", codebuffer),
        0x75 => single_byte_inst!("MOV\tM, L", codebuffer),
        0x76 => single_byte_inst!("HLT", codebuffer),
        0x77 => single_byte_inst!("MOV\tM, A", codebuffer),
        0x78 => single_byte_inst!("MOV\tA, B", codebuffer),
        0x79 => single_byte_inst!("MOV\tA, C", codebuffer),
        0x7a => single_byte_inst!("MOV\tA, D", codebuffer),
        0x7b => single_byte_inst!("MOV\tA, E", codebuffer),
        0x7c => single_byte_inst!("MOV\tA, H", codebuffer),
        0x7d => single_byte_inst!("MOV\tA, L", codebuffer),
        0x7e => single_byte_inst!("MOV\tA, M", codebuffer),
        0x7f => single_byte_inst!("MOV\tA, A", codebuffer),
        0x80 => single_byte_inst!("ADD\tB", codebuffer),
        0x81 => single_byte_inst!("ADD\tC", codebuffer),
        0x82 => single_byte_inst!("ADD\tD", codebuffer),
        0x83 => single_byte_inst!("ADD\tE", codebuffer),
        0x84 => single_byte_inst!("ADD\tH", codebuffer),
        0x85 => single_byte_inst!("ADD\tL", codebuffer),
        0x86 => single_byte_inst!("ADD\tM", codebuffer),
        0x87 => single_byte_inst!("ADD\tA", codebuffer),
        0x88 => single_byte_inst!("ADC\tB", codebuffer),
        0x89 => single_byte_inst!("ADC\tC", codebuffer),
        0x8a => single_byte_inst!("ADC\tD", codebuffer),
        0x8b => single_byte_inst!("ADC\tE", codebuffer),
        0x8c => single_byte_inst!("ADC\tH", codebuffer),
        0x8d => single_byte_inst!("ADC\tL", codebuffer),
        0x8e => single_byte_inst!("ADC\tM", codebuffer),
        0x8f => single_byte_inst!("ADC\tA", codebuffer),
        0x90 => single_byte_inst!("SUB\tB", codebuffer),
        0x91 => single_byte_inst!("SUB\tC", codebuffer),
        0x92 => single_byte_inst!("SUB\tD", codebuffer),
        0x93 => single_byte_inst!("SUB\tE", codebuffer),
        0x94 => single_byte_inst!("SUB\tH", codebuffer),
        0x95 => single_byte_inst!("SUB\tL", codebuffer),
        0x96 => single_byte_inst!("SUB\tM", codebuffer),
        0x97 => single_byte_inst!("SUB\tA", codebuffer),
        0x98 => single_byte_inst!("SBB\tB", codebuffer),
        0x99 => single_byte_inst!("SBB\tC", codebuffer),
        0x9a => single_byte_inst!("SBB\tD", codebuffer),
        0x9b => single_byte_inst!("SBB\tE", codebuffer),
        0x9c => single_byte_inst!("SBB\tH", codebuffer),
        0x9d => single_byte_inst!("SBB\tL", codebuffer),
        0x9e => single_byte_inst!("SBB\tM", codebuffer),
        0x9f => single_byte_inst!("SBB\tA", codebuffer),
        0xa0 => single_byte_inst!("ANA\tB", codebuffer),
        0xa1 => single_byte_inst!("ANA\tC", codebuffer),
        0xa2 => single_byte_inst!("ANA\tD", codebuffer),
        0xa3 => single_byte_inst!("ANA\tE", codebuffer),
        0xa4 => single_byte_inst!("ANA\tH", codebuffer),
        0xa5 => single_byte_inst!("ANA\tL", codebuffer),
        0xa6 => single_byte_inst!("ANA\tM", codebuffer),
        0xa7 => single_byte_inst!("ANA\tA", codebuffer),
        0xa8 => single_byte_inst!("XRA\tB", codebuffer),
        0xa9 => single_byte_inst!("XRA\tC", codebuffer),
        0xaa => single_byte_inst!("XRA\tD", codebuffer),
        0xab => single_byte_inst!("XRA\tE", codebuffer),
        0xac => single_byte_inst!("XRA\tH", codebuffer),
        0xad => single_byte_inst!("XRA\tL", codebuffer),
        0xae => single_byte_inst!("XRA\tM", codebuffer),
        0xaf => single_byte_inst!("XRA\tA", codebuffer),
        0xb0 => single_byte_inst!("ORA\tB", codebuffer),
        0xb1 => single_byte_inst!("ORA\tC", codebuffer),
        0xb2 => single_byte_inst!("ORA\tD", codebuffer),
        0xb3 => single_byte_inst!("ORA\tE", codebuffer),
        0xb4 => single_byte_inst!("ORA\tH", codebuffer),
        0xb5 => single_byte_inst!("ORA\tL", codebuffer),
        0xb6 => single_byte_inst!("ORA\tM", codebuffer),
        0xb7 => single_byte_inst!("ORA\tA", codebuffer),
        0xb8 => single_byte_inst!("CMP\tB", codebuffer),
        0xb9 => single_byte_inst!("CMP\tC", codebuffer),
        0xba => single_byte_inst!("CMP\tD", codebuffer),
        0xbb => single_byte_inst!("CMP\tE", codebuffer),
        0xbc => single_byte_inst!("CMP\tH", codebuffer),
        0xbd => single_byte_inst!("CMP\tL", codebuffer),
        0xbe => single_byte_inst!("CMP\tM", codebuffer),
        0xbf => single_byte_inst!("CMP\tA", codebuffer),
        0xc0 => single_byte_inst!("RNZ", codebuffer),
        0xc1 => single_byte_inst!("POP\tB", codebuffer),
        0xc2 => double_arg_inst!("JNZ\t$", codebuffer),
        0xc3 => double_arg_inst!("JMP\t$", codebuffer),
        0xc4 => double_arg_inst!("CNZ\t$", codebuffer),
        0xc5 => single_byte_inst!("PUSH\tB", codebuffer),
        0xc6 => single_arg_inst!("ADI\t#0x", codebuffer),
        0xc7 => single_byte_inst!("RST\t0", codebuffer),
        0xc8 => single_byte_inst!("RZ", codebuffer),
        0xc9 => single_byte_inst!("RET", codebuffer),
        0xca => double_arg_inst!("JZ\t$", codebuffer),
        0xcb => single_byte_inst!("NOP", codebuffer),
        0xcc => double_arg_inst!("CZ\t$", codebuffer),
        0xcd => double_arg_inst!("CALL\t$", codebuffer),
        0xce => single_arg_inst!("ACI\t#0x", codebuffer),
        0xcf => single_byte_inst!("RST\t1", codebuffer),
        0xd0 => single_byte_inst!("RNC", codebuffer),
        0xd1 => single_byte_inst!("POP\tD", codebuffer),
        0xd2 => double_arg_inst!("JNC\t$", codebuffer),
        0xd3 => single_arg_inst!("OUT\t#0x", codebuffer),
        0xd4 => double_arg_inst!("CNC\t$", codebuffer),
        0xd5 => single_byte_inst!("PUSH\tD", codebuffer),
        0xd6 => single_arg_inst!("SUI\t#0x", codebuffer),
        0xd7 => single_byte_inst!("RST\t2", codebuffer),
        0xd8 => single_byte_inst!("RC", codebuffer),
        0xd9 => single_byte_inst!("NOP", codebuffer),
        0xda => double_arg_inst!("JC\t$", codebuffer),
        0xdb => single_arg_inst!("IN\t#0x", codebuffer),
        0xdc => double_arg_inst!("CC\t$", codebuffer),
        0xdd => single_byte_inst!("NOP", codebuffer),
        0xde => single_arg_inst!("SBI\t#0x", codebuffer),
        0xdf => single_byte_inst!("RST\t3", codebuffer),
        0xe0 => single_byte_inst!("RPO", codebuffer),
        0xe1 => single_byte_inst!("POP\tH", codebuffer),
        0xe2 => double_arg_inst!("JPO\t$", codebuffer),
        0xe3 => single_byte_inst!("XTHL", codebuffer),
        0xe4 => double_arg_inst!("CPO\t$", codebuffer),
        0xe5 => single_byte_inst!("PUSH\tH", codebuffer),
        0xe6 => single_arg_inst!("ANI\t#0x", codebuffer),
        0xe7 => single_byte_inst!("RST\t4", codebuffer),
        0xe8 => single_byte_inst!("RPE", codebuffer),
        0xe9 => single_byte_inst!("PCHL", codebuffer),
        0xea => double_arg_inst!("JPE\t$", codebuffer),
        0xeb => single_byte_inst!("XCHG", codebuffer),
        0xec => double_arg_inst!("CPE\t$", codebuffer),
        0xed => single_byte_inst!("NOP", codebuffer),
        0xee => single_arg_inst!("XRI\t#0x", codebuffer),
        0xef => single_byte_inst!("RST\t5", codebuffer),
        0xf0 => single_byte_inst!("RP", codebuffer),
        0xf1 => single_byte_inst!("POP\tPSW", codebuffer),
        0xf2 => double_arg_inst!("JP\t$", codebuffer),
        0xf3 => single_byte_inst!("DI", codebuffer),
        0xf4 => double_arg_inst!("CP\t$", codebuffer),
        0xf5 => single_byte_inst!("PUSH\tPSW", codebuffer),
        0xf6 => double_arg_inst!("ORI\t#0x", codebuffer),
        0xf7 => single_byte_inst!("RST\t6", codebuffer),
        0xf8 => single_byte_inst!("RM", codebuffer),
        0xf9 => single_byte_inst!("SPHL", codebuffer),
        0xfa => double_arg_inst!("JM\t$", codebuffer),
        0xfb => single_byte_inst!("EI", codebuffer),
        0xfc => double_arg_inst!("CM\t$", codebuffer),
        0xfd => single_byte_inst!("NOP", codebuffer),
        0xfe => double_arg_inst!("CPI\t#0x", codebuffer),
        0xff => single_byte_inst!("RST\t7", codebuffer),
    }
}

/// get the argument bytes of opcodes which take 2 byte argument
fn get_bytes(codebuffer: &[u8]) -> Result<(u8, u8), DisassembleError> {
    let lsb = match codebuffer.get(1) {
        Some(l) => *l,
        None => return Err(DisassembleError::UnexpectedByte),
    };

    let msb = match codebuffer.get(2) {
        Some(m) => *m,
        None => return Err(DisassembleError::UnexpectedByte),
    };

    Ok((msb, lsb))
}

/// get the argument byte of opcodes which take 1 byte argument
fn get_arg(codebuffer: &[u8]) -> Result<u8, DisassembleError> {
    let arg = match codebuffer.get(1) {
        Some(a) => *a,
        None => return Err(DisassembleError::UnexpectedByte),
    };

    Ok(arg)
}
