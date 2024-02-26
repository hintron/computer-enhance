#!/bin/bash

# The simulate-regress/ folder is executed with the --no-ip arg, which means
# that the executed output will not print out changes to the IP register. This
# is so I don't have to backport each of the golden outputs for those
# regressions with the correct IP output.

# All the listings in simulate-ip-regress/ and onward will print changes to the
# IP register as well as the final IP register state.

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
PROJECT_DIR=$(realpath "$SCRIPT_DIR/..")
FILE_DIR="$PROJECT_DIR/files"
DECODE_BUILD_DIR="$FILE_DIR/build-decode-regress"
SNAKE_BUILD_DIR="$FILE_DIR/build-snake-regress"
SIMULATE_BUILD_DIR="$FILE_DIR/build-simulate-regress"
SIMULATE_SRC_DIR="$FILE_DIR/simulate-regress"
SIMULATE_IP_BUILD_DIR="$FILE_DIR/build-simulate-ip-regress"
SIMULATE_IP_SRC_DIR="$FILE_DIR/simulate-ip-regress"
SIMULATE_CYCLES_BUILD_DIR="$FILE_DIR/build-simulate-ip-cycles-regress"
SIMULATE_CYCLES_SRC_DIR="$FILE_DIR/simulate-ip-cycles-regress"
SIMULATE_8086_BUILD_DIR="$FILE_DIR/build-simulate-8086-regress"
SIMULATE_8086_SRC_DIR="$FILE_DIR/simulate-8086-regress"
BIN="$PROJECT_DIR/target/debug/computer-enhance"

CHECK_RTOS="false"
if [ "$1" == "rtos" ]; then
    CHECK_RTOS="true"
fi

DATE=$(date +"%Y-%m-%d at %H:%M:%S")
echo "Date: $DATE"

# Build everything in decode-regress and simulate-regress
cd "$FILE_DIR" || exit
"$FILE_DIR/clean.sh"
if ! "$FILE_DIR/make.sh"; then
    exit 1
fi

cd "$SCRIPT_DIR" || exit
if ! cargo build; then
    exit 1
fi

rc=0

# Check the decode of everything in decode-regress
for file in "$DECODE_BUILD_DIR"/*; do
    echo "Checking decode of $file..."
    BASE=$(basename "$file")
    if ! $BIN --verbose "$file" "$DECODE_BUILD_DIR/$BASE-tmp.asm" > "$DECODE_BUILD_DIR/$BASE-tmp.log"; then
        echo "ERROR: Decode program failed for $file"
        rc=1
        break
    fi
    if ! nasm "$DECODE_BUILD_DIR/$BASE-tmp.asm" -o "$DECODE_BUILD_DIR/$BASE-tmp.o"; then
        echo "ERROR: Assembly of decoded output failed for $file"
        rc=1
        break
    fi
    DECODE_GOLDEN_OUTPUT="$DECODE_BUILD_DIR/$BASE-tmp.o"
    DIFF="$DECODE_BUILD_DIR/$BASE-code.diff"
    if ! diff "$DECODE_GOLDEN_OUTPUT" "$file" -u > "$DIFF"; then
        echo "ERROR: Decoded output didn't match golden output."
        echo "See $DIFF"
        echo "ours   (+): $file"
        echo "golden (-): $DECODE_GOLDEN_OUTPUT"
        rc=1
        break
    fi
    rm "$DIFF"
done


# Check the decode of the Snake game
for file in "$SNAKE_BUILD_DIR"/*; do
    echo "Checking decode of $file..."
    BASE=$(basename "$file")
    if ! $BIN --verbose --stop-on-int3 "$file" "$SNAKE_BUILD_DIR/$BASE-tmp.asm" > "$SNAKE_BUILD_DIR/$BASE-tmp.log"; then
        echo "ERROR: Decode program failed for $file"
        rc=1
        break
    fi
    OUR_DECODE_OUTPUT="$SNAKE_BUILD_DIR/$BASE-tmp.o"
    if ! nasm "$SNAKE_BUILD_DIR/$BASE-tmp.asm" -o "$OUR_DECODE_OUTPUT"; then
        echo "ERROR: Assembly of decoded output failed for $file"
        rc=1
        break
    fi
    DIFF="$SNAKE_BUILD_DIR/$BASE-code.diff"
    if ! diff "$OUR_DECODE_OUTPUT" "$file" -u > "$DIFF"; then
        echo "ERROR: Decoded output didn't match golden output."
        echo "See $DIFF"
        echo "ours   (-): $OUR_DECODE_OUTPUT"
        echo "golden (+): $file"
        rc=1
        break
    fi
    rm "$DIFF"
done


# Check the decode and simulation of everything in simulate-regress, but without
# checking the IP register.
for file in "$SIMULATE_BUILD_DIR"/*; do
    # If the glob found nothing, it will be treated as a file, so skip it 
    if [ ! -e "$file" ]; then continue; fi
    echo "Checking simulation (w/o IP) of $file..."
    BASE=$(basename "$file")
    SIMULATE_OUTPUT="$SIMULATE_BUILD_DIR/$BASE-simulate.txt"
    SIMULATE_LOG="$SIMULATE_BUILD_DIR/$BASE-simulate.log"
    if ! $BIN "$file" "$SIMULATE_OUTPUT" --verbose --exec --no-ip > "$SIMULATE_LOG"; then
        echo "ERROR: Simulation of program failed for $file"
        rc=1
        break
    fi
    SIMULATE_GOLDEN_OUTPUT="$SIMULATE_SRC_DIR/$BASE.txt"
    DIFF="$SIMULATE_BUILD_DIR/$BASE-simulate.diff"
    if ! diff "$SIMULATE_GOLDEN_OUTPUT" "$SIMULATE_OUTPUT" -u > "$DIFF"; then
        echo "ERROR: Simulation output didn't match golden output."
        echo "See $DIFF"
        echo "ours   (+): $SIMULATE_OUTPUT"
        echo "golden (-): $SIMULATE_GOLDEN_OUTPUT"
        rc=1
        break
    fi
    rm "$DIFF"

done


# Check the decode and simulation of everything in simulate-ip-regress,
# INCLUDING the IP register
for file in "$SIMULATE_IP_BUILD_DIR"/*; do
    # If the glob found nothing, it will be treated as a file, so skip it 
    if [ ! -e "$file" ]; then continue; fi
    echo "Checking simulation of $file..."
    BASE=$(basename "$file")
    SIMULATE_OUTPUT="$SIMULATE_IP_BUILD_DIR/$BASE-simulate.txt"
    SIMULATE_LOG="$SIMULATE_IP_BUILD_DIR/$BASE-simulate.log"
    if ! $BIN "$file" "$SIMULATE_OUTPUT" --verbose --exec > "$SIMULATE_LOG"; then
        echo "ERROR: Simulation of program failed for $file"
        rc=1
        break
    fi
    SIMULATE_GOLDEN_OUTPUT="$SIMULATE_IP_SRC_DIR/$BASE.txt"
    DIFF="$SIMULATE_IP_BUILD_DIR/$BASE-simulate.diff"
    if ! diff "$SIMULATE_GOLDEN_OUTPUT" "$SIMULATE_OUTPUT" -u > "$DIFF"; then
        echo "ERROR: Simulation output didn't match golden output."
        echo "See $DIFF"
        echo "ours   (+): $SIMULATE_OUTPUT"
        echo "golden (-): $SIMULATE_GOLDEN_OUTPUT"
        rc=1
        break
    fi
    rm "$DIFF"

done

# Check the decode and simulation of everything in simulate-ip-cycles-regress,
# including the IP register AND cycle estimates. Run each file twice - once for
# 8086 cycle estimates, and once for 8088 cycle estimates. Combine both those
# runs into a single output txt file, and compare with the golden txt file.
for file in "$SIMULATE_CYCLES_BUILD_DIR"/*; do
    # If the glob found nothing, it will be treated as a file, so skip it 
    if [ ! -e "$file" ]; then continue; fi
    echo "Checking simulation (w/ 8086 cycles) of $file..."
    BASE=$(basename "$file")
    SIMULATE_OUTPUT="$SIMULATE_CYCLES_BUILD_DIR/$BASE-simulate.txt"
    SIMULATE_LOG_8086="$SIMULATE_CYCLES_BUILD_DIR/$BASE-simulate-8086.log"
    if ! $BIN "$file" "$SIMULATE_OUTPUT" --verbose --exec --model-cycles 8086 --stop-on-ret > "$SIMULATE_LOG_8086"; then
        echo "ERROR: 8086 simulation of program failed for $file"
        rc=1
        break
    fi
    # Append 8088 simulation results to 8086 simulation results
    echo "Checking simulation (w/ 8088 cycles) of $file..."
    SIMULATE_LOG_8088="$SIMULATE_CYCLES_BUILD_DIR/$BASE-simulate-8088.log"
    if ! $BIN "$file" "$SIMULATE_OUTPUT" --verbose --exec --model-cycles 8088 --stop-on-ret > "$SIMULATE_LOG_8088"; then
        echo "ERROR: 8088 simulation of program failed for $file"
        rc=1
        break
    fi
    SIMULATE_GOLDEN_OUTPUT="$SIMULATE_CYCLES_SRC_DIR/$BASE.txt"
    DIFF="$SIMULATE_CYCLES_BUILD_DIR/$BASE-simulate.diff"
    if ! diff "$SIMULATE_GOLDEN_OUTPUT" "$SIMULATE_OUTPUT" -u > "$DIFF"; then
        echo "ERROR: 8086/8088 Simulation output didn't match golden output."
        echo "See $DIFF"
        echo "ours   (+): $SIMULATE_OUTPUT"
        echo "golden (-): $SIMULATE_GOLDEN_OUTPUT"
        rc=1
        break
    fi
    rm "$DIFF"
done

# Check the decode and simulation of everything in simulate-ip-cycles-regress,
# including the IP register AND cycle estimates, but ONLY FOR 8086. 
for file in "$SIMULATE_8086_BUILD_DIR"/*; do
    # If the glob found nothing, it will be treated as a file, so skip it 
    if [ ! -e "$file" ]; then continue; fi
    echo "Checking simulation w/ 8086 cycles of $file..."
    BASE=$(basename "$file")
    SIMULATE_OUTPUT="$SIMULATE_8086_BUILD_DIR/$BASE-simulate.txt"
    SIMULATE_LOG_8086="$SIMULATE_8086_BUILD_DIR/$BASE-simulate.log"
    if ! $BIN "$file" "$SIMULATE_OUTPUT" --verbose --exec --model-cycles 8086 --stop-on-ret > "$SIMULATE_LOG_8086"; then
        echo "ERROR: 8086 simulation of program failed for $file"
        rc=1
        break
    fi
    SIMULATE_GOLDEN_OUTPUT="$SIMULATE_8086_SRC_DIR/$BASE.txt"
    DIFF="$SIMULATE_8086_BUILD_DIR/$BASE-simulate.diff"
    if ! diff "$SIMULATE_GOLDEN_OUTPUT" "$SIMULATE_OUTPUT" -u > "$DIFF"; then
        echo "ERROR: 8086 Simulation output didn't match golden output."
        echo "See $DIFF"
        echo "ours   (+): $SIMULATE_OUTPUT"
        echo "golden (-): $SIMULATE_GOLDEN_OUTPUT"
        rc=1
        break
    fi
    rm "$DIFF"
done


# TODO: Assemble individual files, not final rtos file
# TODO: In handlers_simptris.s, there is:
    # L_handlers_simptris_5:
    # 	DB	".",0
# This looks like an 0x2E, which is an ES segment override prefix (not sure what that does)
# So the problem is that my decoder can't know that this is data rather than an
# instruction. But NASM does when it assembles it. So perhaps this blind decode
# may not work well.
# How do real decoders handle data bytes?

# Use while loop to easily break out
while [ "$CHECK_RTOS" == "true" ]; do
    # Try decoding my ECEn 425 RTOS
    if [ -d "$HOME/code/425_artoss/" ]; then
        RTOS_REPO="$HOME/code/425_artoss/"
    elif [ -d "$HOME/code/artoss/" ]; then
        RTOS_REPO="$HOME/code/artoss/"
    else
        echo "ERROR: Unable to find RTOS dir!"
        rc=1
        break
    fi
    RTOS_DIR="$RTOS_REPO/labs/lab8"
    RTOS_BIN_ORIG="$RTOS_DIR/artoss.bin"
    RTOS_BUILD_DIR="$FILE_DIR/build-rtos-regress"
    # Clear out stale builds
    if [ -d "$RTOS_BUILD_DIR" ]; then
        rm -rf  "$RTOS_BUILD_DIR"
    fi
    mkdir -p "$RTOS_BUILD_DIR"
    RTOS_BIN="$RTOS_BUILD_DIR/artoss.bin"

    cd "$RTOS_DIR" || exit
    make clean
    if ! make; then
        echo "ERROR: Failed to build RTOS"
        rc=1
        break
    fi
    cd "$FILE_DIR" || exit

    if [ ! -f "$RTOS_BIN_ORIG" ]; then
        echo "ERROR: Could not find RTOS binary file '$RTOS_BIN_ORIG'"
        rc=1
        break
    fi

    cp "$RTOS_BIN_ORIG" "$RTOS_BIN"

    echo "Simulating RTOS binary '$RTOS_BIN'..."
    BASE=$(basename "$RTOS_BIN")
    SIMULATE_OUTPUT="$RTOS_BUILD_DIR/$BASE-simulate.txt"
    SIMULATE_LOG_8086="$RTOS_BUILD_DIR/$BASE-tmp.log"
    if ! $BIN "$RTOS_BIN" "$SIMULATE_OUTPUT" --verbose --exec --model-cycles 8086 --stop-on-ret --initial-ip "0x100" > "$SIMULATE_LOG_8086"; then
        echo "ERROR: Decode program failed for $RTOS_BIN"
        rc=1
        break
    fi
    if ! nasm "$RTOS_BUILD_DIR/$BASE-tmp.asm" -o "$RTOS_BUILD_DIR/$BASE-tmp"; then
        echo "ERROR: Assembly of decoded output failed for $file"
        rc=1
        break
    fi
    RTOS_GOLDEN_OUTPUT="$RTOS_BUILD_DIR/$BASE-tmp.o"
    DIFF="$RTOS_BUILD_DIR/$BASE-rtos.diff"
    if ! diff "$RTOS_GOLDEN_OUTPUT" "$file" -u > "$DIFF"; then
        echo "ERROR: Decoded RTOS output didn't match golden output."
        echo "See $DIFF"
        echo "ours   (+): $file"
        echo "golden (-): $RTOS_GOLDEN_OUTPUT"
        rc=1
        break
    fi
    rm "$DIFF"
done


if [ "$rc" == "0" ]; then
    echo "All regressions passed"
else
    echo "ERROR: Regressions failed"
fi
exit $rc
