# Debugging Rust code

To graphically debug Rust code interactively, first install the [CodeLLDB][1]
extension in VSCode (and install it remotely for WSL).

Before running the debugger, we want to [set up a release build with debug
symbols][2] so we can profile the final code. By default, a release build excludes
debug symbols. So, to force a release build to include debug symbols, add this
to `Cargo.toml`:

```toml
[profile.release-with-debug]
inherits = "release"
debug = true
```

and run `cargo build --profile=release-with-debug` to manually build it.

Now we need to set up CodeLLDB to start a debug session with the
`release-with-debug` build profile. To e.g. debug my RTOS, create a
`launch.json` file like so:

```json
{
    "version": "0.2.0",
    "configurations": [
        {
            "type": "lldb",
            "request": "launch",
            "name": "RTOS simulator release-with-debug",
            "cargo": {
                "args": [
                    "build",
                    "--profile=release-with-debug",
                    "--bin=computer-enhance",
                    "--package=computer-enhance"
                ],
                "filter": {
                    "name": "computer-enhance",
                    "kind": "bin"
                }
            },
            "args": [
                "${env:HOME}/code/artoss/labs/lab8/artoss.bin",
                "rtos-debug.log",
                "--verbose",
                "--exec",
                "--model-cycles",
                "8086",
                "--ip",
                "0x100",
                "--sp",
                "0xFFFE"
            ],
            "cwd": "${workspaceFolder}"
        },
    ]
}
```

NOTE: If you build it in regular release mode, you won't be able to set
breakpoints!

Now that CodeLLDB and the release-with-debug build are set up, right-click
"Run and Debug" in the left toolbar. Then click the green play button. This will
launch the debugger with the configuration we set up.

You can do the `Toggle Disassembly` command to switch between Rust code and
machine code. Unfortunately, there is no side-by-side synchronization between
Rust code and machine code in CodeLLDB, and the machine code is in AT&T syntax,
not Intel syntax. So, see [Viewing assembly of Rust code][3] for how to print
out assembly in Intel syntax with interleaved Rust code and reference that while
stepping through the code.

[1]: https://marketplace.visualstudio.com/items?itemName=vadimcn.vscode-lldb
[2]: https://stackoverflow.com/questions/38803760/how-to-get-a-release-build-with-debugging-information-when-using-cargo
[3]: ./assembly.md