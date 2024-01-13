#![warn(clippy::pedantic, clippy::nursery)]

use std::fmt::Write;

use std::sync::mpsc::{channel, Receiver, Sender};

use std::thread::spawn;
use std::{
    ops::{BitAnd, Shl},
    time::Instant,
};

use env_logger::init;
use log::{debug, info};
use nannou::color::rgba8;
use nannou::event::{Key, Update};
use nannou::{app, App, Draw, Frame};
use rand::{random, thread_rng, Rng};

#[derive(Debug, Clone, Copy)]
struct Timer {
    set_at: Option<Instant>,
    set_to: u8,
}

impl Timer {
    const fn new() -> Self {
        Self {
            set_at: None,
            set_to: 0,
        }
    }

    #[allow(clippy::cast_possible_truncation)]
    fn value(&self) -> u8 {
        self.set_at.map_or(0, |set_at| {
            let elapsed = set_at.elapsed();
            u64::from(self.set_to).saturating_sub(elapsed.as_secs().saturating_mul(60)) as u8
        })
    }

    fn set(&mut self, value: u8) {
        self.set_at = Some(Instant::now());
        self.set_to = value;
    }
}

struct System {
    memory: [u8; 0x1000],
    registers: [u8; 16],
    address_register: usize,
    stack: Vec<usize>,
    delay_timer: Timer,
    sound_timer: Timer,
    display: [[bool; 64]; 32],
    program_counter: usize,
    pressed_keys: Vec<u8>,
    waiting_for_key: Option<u8>,
    display_tx: Sender<[[bool; 64]; 32]>,
    keys_rx: Receiver<Vec<u8>>,
}

#[derive(Debug, Clone, Copy)]
enum Operation {
    Syscall(usize),
    Clear,
    Return,
    Jump(usize),
    Call(usize),
    SkipIfEqualConst(u8, u8),
    SkipIfNotEqualConst(u8, u8),
    SkipIfEqual(u8, u8),
    SetConst(u8, u8),
    AddConst(u8, u8),
    Set(u8, u8),
    Or(u8, u8),
    And(u8, u8),
    Xor(u8, u8),
    Add(u8, u8),
    Sub(u8, u8),
    ShiftRight(u8),
    SubReverse(u8, u8),
    ShiftLeft(u8),
    SkipIfNotEqual(u8, u8),
    SetAddressRegister(usize),
    JumpOffset(usize),
    Random(u8, u8),
    Draw(u8, u8, u8),
    SkipIfPressed(u8),
    SkipIfNotPressed(u8),
    SetRegisterToDelay(u8),
    WaitForKey(u8),
    SetDelayToRegister(u8),
    SetSoundToRegister(u8),
    AddToAddressRegister(u8),
    SetAddressRegisterToSprite(u8),
    BinaryCodedDecimal(u8),
    DumpRegisters(u8),
    LoadRegisters(u8),
}

impl Operation {
    fn decode(data: [u8; 2]) -> Option<Self> {
        match data {
            [0x00, 0xE0] => Some(Self::Clear),
            [0x00, 0xEE] => Some(Self::Return),
            [upper, lower] if upper >> 4 == 0 => Some(Self::Syscall(
                (upper as usize).bitand(0b0000_1111).shl(8) + lower as usize,
            )),
            [upper, lower] if upper >> 4 == 1 => Some(Self::Jump(
                (upper as usize).bitand(0b0000_1111).shl(8) + lower as usize,
            )),
            [upper, lower] if upper >> 4 == 2 => Some(Self::Call(
                (upper as usize).bitand(0b0000_1111).shl(8) + lower as usize,
            )),
            [upper, lower] if upper >> 4 == 3 => {
                Some(Self::SkipIfEqualConst(upper.bitand(0b0000_1111), lower))
            }
            [upper, lower] if upper >> 4 == 4 => {
                Some(Self::SkipIfNotEqualConst(upper.bitand(0b0000_1111), lower))
            }
            [upper, lower] if upper >> 4 == 5 => {
                Some(Self::SkipIfEqual(upper.bitand(0b0000_1111), lower >> 4))
            }
            [upper, lower] if upper >> 4 == 6 => {
                Some(Self::SetConst(upper.bitand(0b0000_1111), lower))
            }
            [upper, lower] if upper >> 4 == 7 => {
                Some(Self::AddConst(upper.bitand(0b0000_1111), lower))
            }
            [upper, lower] if upper >> 4 == 8 => {
                let x = upper.bitand(0b0000_1111);
                let y = lower >> 4;
                match lower.bitand(0b0000_1111) {
                    0 => Some(Self::Set(x, y)),
                    1 => Some(Self::Or(x, y)),
                    2 => Some(Self::And(x, y)),
                    3 => Some(Self::Xor(x, y)),
                    4 => Some(Self::Add(x, y)),
                    5 => Some(Self::Sub(x, y)),
                    6 => Some(Self::ShiftRight(x)),
                    7 => Some(Self::SubReverse(x, y)),
                    0xE => Some(Self::ShiftLeft(x)),
                    _ => None,
                }
            }
            [upper, lower] if upper >> 4 == 9 => {
                Some(Self::SkipIfNotEqual(upper.bitand(0b0000_1111), lower >> 4))
            }
            [upper, lower] if upper >> 4 == 0xA => Some(Self::SetAddressRegister(
                (upper as usize).bitand(0b0000_1111).shl(8) + lower as usize,
            )),
            [upper, lower] if upper >> 4 == 0xB => Some(Self::JumpOffset(
                (upper as usize).bitand(0b0000_1111).shl(8) + lower as usize,
            )),
            [upper, lower] if upper >> 4 == 0xC => {
                Some(Self::Random(upper.bitand(0b0000_1111), lower))
            }
            [upper, lower] if upper >> 4 == 0xD => Some(Self::Draw(
                upper.bitand(0b0000_1111),
                lower >> 4,
                lower.bitand(0b0000_1111),
            )),
            [upper, lower] if upper >> 4 == 0xE => {
                let x = upper.bitand(0b0000_1111);
                match lower {
                    0x9E => Some(Self::SkipIfPressed(x)),
                    0xA1 => Some(Self::SkipIfNotPressed(x)),
                    _ => None,
                }
            }
            [upper, lower] if upper >> 4 == 0xF => {
                let x = upper.bitand(0b0000_1111);
                match lower {
                    0x07 => Some(Self::SetRegisterToDelay(x)),
                    0x0A => Some(Self::WaitForKey(x)),
                    0x15 => Some(Self::SetDelayToRegister(x)),
                    0x18 => Some(Self::SetSoundToRegister(x)),
                    0x1E => Some(Self::AddToAddressRegister(x)),
                    0x29 => Some(Self::SetAddressRegisterToSprite(x)),
                    0x33 => Some(Self::BinaryCodedDecimal(x)),
                    0x55 => Some(Self::DumpRegisters(x)),
                    0x65 => Some(Self::LoadRegisters(x)),
                    _ => None,
                }
            }
            _ => None,
        }
    }
}

impl System {
    const FONT_START: usize = 0x50;
    const ROM_START: usize = 0x200;
    const MEMORY_SIZE: usize = 0x1000;
    fn new(rom: &[u8], display_tx: Sender<[[bool; 64]; 32]>, keys_rx: Receiver<Vec<u8>>) -> Self {
        let mut rng = thread_rng();
        let mut system = Self {
            memory: [0; Self::MEMORY_SIZE].map(|_| rng.gen()),
            registers: rng.gen(),
            address_register: rng.gen::<usize>() & 0xFFF,
            stack: Vec::new(),
            delay_timer: Timer::new(),
            sound_timer: Timer::new(),
            display: [[false; 64]; 32].map(|row| row.map(|_| rng.gen())),
            program_counter: 0,
            pressed_keys: Vec::new(),
            waiting_for_key: None,
            display_tx,
            keys_rx,
        };
        info!("loading font...");
        let font = include_bytes!("font");
        system.memory[Self::FONT_START..Self::FONT_START + font.len()].copy_from_slice(font);
        info!("loading ROM...");
        system.memory[Self::ROM_START..Self::ROM_START + rom.len()].copy_from_slice(rom);
        system.program_counter = Self::ROM_START;
        system
    }

    #[allow(clippy::too_many_lines)]
    fn execute(&mut self, operation: Operation) {
        match operation {
            Operation::Syscall(_) => unimplemented!(),
            Operation::Clear => {
                self.display = [[false; 64]; 32];
                self.display_tx.send(self.display).unwrap();
            }
            Operation::Return => self.program_counter = self.stack.pop().unwrap(),
            Operation::Jump(address) => self.program_counter = address - 2,
            Operation::Call(address) => {
                self.stack.push(self.program_counter);
                self.program_counter = address - 2;
            }
            Operation::SkipIfEqualConst(register, value) => {
                if self.registers[register as usize] == value {
                    self.program_counter += 2;
                }
            }
            Operation::SkipIfNotEqualConst(register, value) => {
                if self.registers[register as usize] != value {
                    self.program_counter += 2;
                }
            }
            Operation::SkipIfEqual(x, y) => {
                if self.registers[x as usize] == self.registers[y as usize] {
                    self.program_counter += 2;
                }
            }
            Operation::SetConst(register, value) => {
                self.registers[register as usize] = value;
            }
            Operation::AddConst(register, value) => {
                self.registers[register as usize] =
                    self.registers[register as usize].wrapping_add(value);
            }
            Operation::Set(x, y) => {
                self.registers[x as usize] = self.registers[y as usize];
            }
            Operation::Or(x, y) => {
                self.registers[x as usize] |= self.registers[y as usize];
            }
            Operation::And(x, y) => {
                self.registers[x as usize] &= self.registers[y as usize];
            }
            Operation::Xor(x, y) => {
                self.registers[x as usize] ^= self.registers[y as usize];
            }
            Operation::Add(x, y) => {
                let (result, carry) =
                    self.registers[x as usize].overflowing_add(self.registers[y as usize]);
                self.registers[x as usize] = result;
                self.registers[0xF] = u8::from(carry);
            }
            Operation::Sub(x, y) => {
                let (result, borrow) =
                    self.registers[x as usize].overflowing_sub(self.registers[y as usize]);
                self.registers[x as usize] = result;
                self.registers[0xF] = u8::from(!borrow);
            }
            Operation::ShiftRight(register) => {
                let bit = self.registers[register as usize] & 0b0000_0001;
                self.registers[register as usize] >>= 1;
                self.registers[0xF] = bit;
            }
            Operation::SubReverse(x, y) => {
                let (result, borrow) =
                    self.registers[y as usize].overflowing_sub(self.registers[x as usize]);
                self.registers[x as usize] = result;
                self.registers[0xF] = u8::from(!borrow);
            }
            Operation::ShiftLeft(register) => {
                let bit = !self.registers[register as usize] & 0b1000_0000 >> 7;
                self.registers[register as usize] <<= 1;
                self.registers[0xF] = bit;
            }
            Operation::SkipIfNotEqual(x, y) => {
                if self.registers[x as usize] != self.registers[y as usize] {
                    self.program_counter += 2;
                }
            }
            Operation::SetAddressRegister(address) => {
                self.address_register = address;
            }
            Operation::JumpOffset(address) => {
                self.program_counter = address + self.registers[0] as usize;
            }
            Operation::Random(register, value) => {
                self.registers[register as usize] = random::<u8>() & value;
            }
            Operation::Draw(x, y, height) => {
                let x = self.registers[x as usize] as usize;
                let y = self.registers[y as usize] as usize;
                let height = height as usize;
                let mut collision = false;
                for y_offset in 0..height {
                    let row = self.memory[self.address_register + y_offset];
                    for x_offset in 0..8 {
                        let pixel = (row >> (7 - x_offset)) & 0b0000_0001 == 1;
                        let x = (x + x_offset) % 64;
                        let y = (y + y_offset) % 32;
                        let current_pixel = self.display[y][x];
                        self.display[y][x] ^= pixel;
                        collision |= current_pixel && !self.display[y][x];
                    }
                }
                self.registers[0xF] = u8::from(collision);
                self.display_tx.send(self.display).unwrap();
            }
            Operation::SkipIfPressed(register) => {
                if self
                    .pressed_keys
                    .contains(&self.registers[register as usize])
                {
                    self.program_counter += 2;
                }
            }
            Operation::SkipIfNotPressed(register) => {
                if !self
                    .pressed_keys
                    .contains(&self.registers[register as usize])
                {
                    self.program_counter += 2;
                }
            }
            Operation::SetRegisterToDelay(register) => {
                self.registers[register as usize] = self.delay_timer.value();
            }
            Operation::WaitForKey(register) => {
                self.waiting_for_key = Some(self.registers[register as usize]);
            }
            Operation::SetDelayToRegister(register) => {
                self.delay_timer.set(self.registers[register as usize]);
            }
            Operation::SetSoundToRegister(register) => {
                self.sound_timer.set(self.registers[register as usize]);
            }
            Operation::AddToAddressRegister(register) => {
                self.address_register += self.registers[register as usize] as usize;
            }
            Operation::SetAddressRegisterToSprite(register) => {
                self.address_register = self.registers[register as usize] as usize * 5;
            }
            Operation::BinaryCodedDecimal(register) => {
                let value = self.registers[register as usize];
                self.memory[self.address_register] = value / 100;
                self.memory[self.address_register + 1] = (value / 10) % 10;
                self.memory[self.address_register + 2] = value % 10;
            }
            Operation::DumpRegisters(register) => {
                for index in 0..=register {
                    self.memory[self.address_register + index as usize] =
                        self.registers[index as usize];
                }
            }
            Operation::LoadRegisters(register) => {
                for index in 0..=register {
                    self.registers[index as usize] =
                        self.memory[self.address_register + index as usize];
                }
            }
        }
    }

    fn step(&mut self) -> bool {
        if let Ok(keys) = self.keys_rx.try_recv() {
            self.pressed_keys = keys;
        }
        if let Some(key) = self.waiting_for_key {
            if let Some(pressed_key) = self.pressed_keys.first() {
                self.registers[key as usize] = *pressed_key;
                self.waiting_for_key = None;
            } else {
                return false;
            }
        }
        let data = [
            self.memory[self.program_counter],
            self.memory[self.program_counter + 1],
        ];
        debug!("memory:\n{}", {
            let mut out = String::new();
            writeln!(out, "    0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F ").unwrap();
            for (index, row) in self.memory.chunks(0x10).enumerate() {
                write!(out, "{:03X} ", index * 0x10).unwrap();
                for byte in row {
                    write!(out, "{byte:02X} ").unwrap();
                }
                writeln!(out).unwrap();
            }
            out
        });
        info!(
            "decoding operation {}, program counter is {:02X}...",
            data.iter().fold(String::new(), |mut accumulator, byte| {
                write!(accumulator, "{byte:02X}").unwrap();
                accumulator
            }),
            self.program_counter
        );
        let operation = Operation::decode(data).unwrap();
        if let Operation::Jump(address) = operation {
            if address == self.program_counter {
                info!("infinite loop (halt) detected, exiting...");
                return true;
            }
        }
        info!("executing {:?}...", operation);
        self.execute(operation);
        info!(
            "registers are {}...",
            self.registers
                .iter()
                .map(|register| format!("{register:02X}"))
                .collect::<Vec<_>>()
                .join(", ")
        );
        info!("address register is {:03X}...", self.address_register);
        info!("incrementing program counter...");
        self.program_counter += 2;
        false
    }

    fn run(&mut self) {
        loop {
            if self.step() {
                break;
            }
        }
    }
}

const PIXEL_SIZE: u32 = 30;
fn main() {
    info!("initialising logging...");
    init();
    info!("initialising nannou app...");
    app(model)
        .update(update)
        .size(PIXEL_SIZE * 64, PIXEL_SIZE * 32)
        .simple_window(view)
        .run();
}

struct Model {
    display_rx: Receiver<[[bool; 64]; 32]>,
    keys_tx: Sender<Vec<u8>>,
    display: [[bool; 64]; 32],
}

fn model(_: &App) -> Model {
    info!("initialising system...");
    let (display_tx, display_rx) = channel();
    let (keys_tx, keys_rx) = channel();
    let mut system = System::new(include_bytes!("rom.ch8"), display_tx, keys_rx);
    spawn(move || system.run());
    Model {
        display_rx,
        keys_tx,
        display: [[false; 64]; 32],
    }
}

fn update(app: &App, model: &mut Model, _: Update) {
    app.main_window().set_resizable(false);
    model
        .keys_tx
        .send(
            app.keys
                .down
                .iter()
                .filter_map(|key| {
                    [
                        Key::X,
                        Key::Key1,
                        Key::Key2,
                        Key::Key3,
                        Key::Q,
                        Key::W,
                        Key::E,
                        Key::A,
                        Key::S,
                        Key::D,
                        Key::Z,
                        Key::C,
                        Key::Key4,
                        Key::R,
                        Key::F,
                        Key::V,
                    ]
                    .iter()
                    .position(|k| k == key)
                    .map(|index| u8::try_from(index).unwrap())
                })
                .collect(),
        )
        .unwrap();
    if let Ok(display) = model.display_rx.try_recv() {
        model.display = display;
    }
}

#[allow(clippy::cast_precision_loss, clippy::needless_pass_by_value)]
fn view(app: &App, model: &Model, frame: Frame) {
    let draw = Draw::new();
    draw.background().color(rgba8(153, 102, 14, 255));
    for (y, row) in model.display.iter().enumerate() {
        for (x, pixel) in row.iter().enumerate() {
            if *pixel {
                draw.rect()
                    .x_y(
                        (x as f32).mul_add(
                            PIXEL_SIZE as f32,
                            -app.window_rect().w() / 2. - PIXEL_SIZE as f32 / 2.,
                        ),
                        (-(y as f32)).mul_add(
                            PIXEL_SIZE as f32,
                            app.window_rect().h() / 2. + PIXEL_SIZE as f32 / 2.,
                        ),
                    )
                    .w_h(PIXEL_SIZE as f32, PIXEL_SIZE as f32)
                    .color(rgba8(255, 204, 38, 255));
            }
        }
    }
    draw.to_frame(app, &frame).unwrap();
}
