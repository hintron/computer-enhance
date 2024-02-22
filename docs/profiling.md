# Profiling - Flamegraphs

We'll use the [flamegraph][1] crate to profile our executable using flamegraphs
in Linux.

First, install the flamegraph crate globally:

```shell
cargo install flamegraph
```

Then, enable perf stuff in Linux for flamegraphs:

```shell
echo -1 | sudo tee /proc/sys/kernel/perf_event_paranoid
echo 0 | sudo tee /proc/sys/kernel/kptr_restrict
```
(see [this][2] and [this][3].)

Finally, profile our executable in release mode:

```shell
CARGO_PROFILE_RELEASE_DEBUG=true cargo flamegraph -F 100000 -- files/bench-data/listing_0042_completionist_decode tmp.txt
```

This samples it as much as it can, but currently, it's not super helpful because
writing to the file is the bottleneck, the execution time is small, and I don't
think our decode/execute code is ever really sampled.

[1]: https://github.com/flamegraph-rs/flamegraph
[2]: https://github.com/flamegraph-rs/flamegraph#enabling-perf-for-use-by-unprivileged-users
[3]: https://users.rust-lang.org/t/flamegraph-shows-every-caller-is-unknown/52408/2
