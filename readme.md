# Computer Enhance x86 Decoder and Simulator

This is an 8086 instruction decoder and simulator, implemented in the [Rust
programming language][5] by following the [Computer Enhance][1] performance
awareness course by Casey Muratori.

This is mainly a personal project for my own benefit. However, I'm hoping to get
this into a state where I can run the real-time operating system (RTOS) I
implemented in 8086 for a college class.

If you too would like to try your hand at creating an RTOS on the 8086 in C,
take a look at [github.com/hintron/8086-toolchain][4]! This repo contains all
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


# Regressions

To run the regression suite, do:

```
./scripts/regress.sh
cargo test
```

`cargo test` doesn't test much, so most of the testing is done by `regress.sh`.

Note that these regressions need to find `nasm` on your path in order to
assemble the binaries to decode and simulate. I use a version of `nasm` provided
by the [BYU ECEn 425 class 8086 toolchain repo][4].


# Benchmarking

[Criterion][2] is used to benchmark decoding and simulation
([see the Criterion docs][3]).

Build in release mode, and run all benchmarks once, to set a baseline:

```
$ cargo bench
```

Then, make a change to the code. To see the performance difference, specify a
substring of the relevant benchmark name:

```
$ cargo bench 45
    Finished bench [optimized] target(s) in 0.04s
     Running unittests src/lib.rs (target/release/deps/computer_enhance-7363669663bce522)

running 0 tests

test result: ok. 0 passed; 0 failed; 0 ignored; 0 measured; 1 filtered out; finished in 0.00s

     Running unittests src/main.rs (target/release/deps/computer_enhance-c81c3ad28e2db032)

running 0 tests

test result: ok. 0 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s

     Running benches/benchmarks.rs (target/release/deps/benchmarks-05d45e870dfe7f3f)
Gnuplot not found, using plotters backend
Decode-and-simulate of listing 45
                        time:   [18.522 µs 18.690 µs 18.919 µs]
                        change: [+0.9796% +1.4909% +2.2012%] (p = 0.00 < 0.05)
                        Change within noise threshold.
Found 4 outliers among 100 measurements (4.00%)
  2 (2.00%) high mild
  2 (2.00%) high severe
```

The middle value in the `time:` row is the total wall time each iteration took,
on average.

The middle value in the `change:` row shows the percent change compared to the
previous baseline run.


# Profiling - Flamegraphs

We'll use the [flamegraph][6] crate to profile our executable using flamegraphs
in Linux.

Fist, install the flamegraph crate globally:

```shell
cargo install flamegraph
```

Then, enable perf stuff in Linux for flamegraphs:

```shell
echo -1 | sudo tee /proc/sys/kernel/perf_event_paranoid
echo 0 | sudo tee /proc/sys/kernel/kptr_restrict
```
(see [this][7] and [this][8].)

Finally, profile our executable in release mode:

```shell
CARGO_PROFILE_RELEASE_DEBUG=true cargo flamegraph -F 100000 -- files/bench-data/listing_0042_completionist_decode tmp.txt
```

This samples it as much as it can, but currently, it's not super helpful because
writing to the file is the bottleneck, the execution time is small, and I don't
think our decode/execute code is ever really sampled.

[1]: https://www.computerenhance.com/
[2]: https://github.com/bheisler/criterion.rs
[3]: https://bheisler.github.io/criterion.rs/book/index.html
[4]: https://github.com/hintron/8086-toolchain
[5]: https://www.rust-lang.org/
[6]: https://github.com/flamegraph-rs/flamegraph
[7]: https://github.com/flamegraph-rs/flamegraph#enabling-perf-for-use-by-unprivileged-users
[8]: https://users.rust-lang.org/t/flamegraph-shows-every-caller-is-unknown/52408/2
