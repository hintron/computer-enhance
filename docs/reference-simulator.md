# Using the class reference simulator

The class reference simulator is available at [the class repo][1].

## Build

To build (on Linux), use Clang (`clang++`) to simply build `sim86.cpp` directly:

```shell
cd <computer-enhance-class-repo>/perfaware/sim86
clang++ sim86.cpp
```

Using GCC (`g++`) doesn't seem to work - there are some compile errors.


## Using

Simply call the built executable directly and point to the 8086 machine code
binary.

To decode:

```shell
 ./a.out <binary>
```

To decode + execute:

```shell
 ./a.out -exec <binary>
```

To decode + execute with cycle timing:

```shell
 ./a.out -exec -showclocks -explainclocks <binary>
```

To see the other options, inspect `sim86.cpp`.

[1]: https://github.com/cmuratori/computer_enhance
[2]: https://bheisler.github.io/criterion.rs/book/index.html
