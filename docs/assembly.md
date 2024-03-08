# Viewing assembly of Rust code

## Generating assembly text

To see the assembly code that Rust compiles down to, do the following:

First, install the [`cargo-show-asm` crate][1] (NOT the outdated `cargo-asm` crate)
```shell
cargo install cargo-show-asm
```

Then, do:

```shell
cargo asm \
--color \
--rust \
--intel \
--lib \
--simplify \
--everything \
> everything.lst
```

* `--color` adds ANSI color codes, which makes the output look nice.
* `--rust` interleaves Rust code in the assembly, so it's easy to see what
corresponds to what.
* `--intel` outputs assembly in the Intel syntax.
* `--lib` selects all code that falls under the lib module.
* `--simplify` removes various non-assembly directives that just clutter things.
* `--everything` will generate assembly for the entire program.

You can also change `--lib` for `--bin` to see the executable startup code. See
`cargo asm --help` for more options.


## Viewing the assembly

To view the listing file with colors in VSCode, be sure to install the
[`ANSI Colors` extension][4] and then do `ANSI TEXT: Open Preview`.

Alternatively, do `less -R main.lst` in a terminal to see the colorized listing
file.


## x86 Assembly Syntax

There are basically two flavors of x86 assembly: AT&T/Linux, and Intel. For a
quick refresher on the syntax, see [x86 assembly language - Syntax][3].

Other x86-64 assembly tips:
* TODO


## llvm-mca

[llvm-mca][2] is a performance analysis tool that can be used to statically
guess the performance of machine code for a specific CPU. It guesses
Instructions Per Cycle (IPC) as well as hardware resource pressure.

To use it, first install the `llvm` Debian package:

```shell
sudo apt install llvm
```

This will install many llvm-based tools, including llvm-mca.

Next, run it like so:

```shell
 cargo asm --mca-intel --lib decode 9 > decode.mca.txt
```


[1]: https://github.com/gnzlbg/cargo-asm
[2]: https://llvm.org/docs/CommandGuide/llvm-mca.html
[3]: https://en.wikipedia.org/wiki/X86_assembly_language#Syntax
[4]: https://marketplace.visualstudio.com/items?itemName=iliazeus.vscode-ansi
