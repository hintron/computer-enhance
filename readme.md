# Computer Enhance 8086 Decoder and Simulator

This is an 8086 instruction decoder and simulator, implemented in the [Rust
programming language][4] by following the [Computer Enhance][1] performance
awareness course by Casey Muratori (see the [class GitHub repo][2] for listings
and for the reference simulator).

This is mainly a personal project for my own benefit. However, I'm hoping to get
this into a state where I can run the real-time operating system (RTOS) I
implemented in C and assembly on an 8086 emulator for a college class.

If you too would like to try your hand at creating an RTOS on the 8086 in C,
take a look at [github.com/hintron/8086-toolchain][3]! This repo contains all
the lectures, 8086 documentation, and lab files needed to implement the "YAK"
RTOS. It also contains an 8086 emulator/debugger, an 8086 compiler, and the NASM
assembler (all easily built from source).


# Quick start

To run and execute an 8086 binary, do:

```
cargo run -- <input-8086-binary> <output-log> -e
```

For example:

```
cargo run -- files/bench-data/listing_0045_challenge_register_movs tmp.txt -e
```

To simply decode an 8086 binary, remove the `-e` flag:

```
cargo run -- <input-8086-binary> <output-log>
```

Available options can be seen via `-h/--help`:
```
cargo run -- -h
```

(Decode-only is currently limited to decoding instructions only, and does not
know how to skip over data.)


# Other docs

* [Running the regressions][5]
* [Benchmarking in Rust][6]
* [Profiling in Rust][7]
* [Using the class reference simulator][8]
* [Viewing assembly of Rust code][9]
* [Debugging Rust code][10]


[1]: https://www.computerenhance.com/
[2]: https://github.com/cmuratori/computer_enhance
[3]: https://github.com/hintron/8086-toolchain
[4]: https://www.rust-lang.org/
[5]: ./docs/regressions.md
[6]: ./docs/benchmarking.md
[7]: ./docs/profiling.md
[8]: ./docs/reference-simulator.md
[9]: ./docs/assembly.md
[10]: ./docs/debugging.md
