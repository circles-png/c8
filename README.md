# c8

chip-8 emulator in rust, threaded rendering with nannou

load rom by putting a `rom.ch8` binary in the `./src` directory, then run `cargo run` or `cargo run --release`. there is no runtime rom loading so you'll have to recompile to load a new rom. i just used compile-time `include_bytes!`.

the font is stored at `./src/font` and is also loaded at compile time. as per the spec, the font is loaded into memory starting at `0x50`.
