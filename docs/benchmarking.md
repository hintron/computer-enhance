# Benchmarking

[Criterion][1] is used to benchmark decoding and simulation
([see the Criterion docs][2]).

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

[1]: https://github.com/bheisler/criterion.rs
[2]: https://bheisler.github.io/criterion.rs/book/index.html
