# Debugging Rust code

To graphically debug Rust code interactively, first install the [CodeLLDB][1]
extension in VSCode (and install it remotely for WSL).

Once that is installed, click Run -> Start Debugging. It will prompt you to
auto-create some configurations in `.vscode/launch.json`, which is a good
starting point.

Edit the args for each launch entry. E.g., for debugging the RTOS, set `args`
like so:

```json
    "args": [
        "${env:HOME}/code/artoss/labs/lab8/artoss.bin",
        "rtos-debug.log",
        "--verbose",
        "--exec",
        "--model-cycles",
        "8086",
        "--initial-ip",
        "0x100",
        "--initial-sp",
        "0xFFFE"
    ],
```

NOTE: If you build it in release mode, you won't be able to set breakpoints!

Set a breakpoint or two, possibly in `main()`.

Right-click the left toolbar and show "Run and Debug". This will control the
debugger and what launch configuration you will use.

[1]: https://marketplace.visualstudio.com/items?itemName=vadimcn.vscode-lldb
