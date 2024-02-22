# Regressions

To run the regression suite, do:

```
./scripts/regress.sh
cargo test
```

`cargo test` doesn't test much, so most of the testing is done by `regress.sh`.

Note that these regressions need to find `nasm` on your path in order to
assemble the binaries to decode and simulate. I use a version of `nasm` provided
by the [BYU ECEn 425 class 8086 toolchain repo][1].

To make sure that the regressions are run before each Git commit, run
`scripts/install-pre-commit.sh` for each instance of this repo that you clone.

[1]: https://github.com/hintron/8086-toolchain
