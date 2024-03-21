#!/bin/bash

# The simulate-regress/ folder is executed with the --no-ip arg, which means
# that the executed output will not print out changes to the IP register. This
# is so I don't have to backport each of the golden outputs for those
# regressions with the correct IP output.

# All the listings in simulate-ip-regress/ and onward will print changes to the
# IP register as well as the final IP register state.

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
PROJECT_DIR=$(realpath "$SCRIPT_DIR/../..")
FILE_DIR="$PROJECT_DIR/emu86rs/files"
BIN_86_DIR="$FILE_DIR/bin86"
BUILD_DIR="$FILE_DIR/build"
SRC_DIR_DECODE="$FILE_DIR/decode-regress"
SRC_DIR_SIMULATE="$FILE_DIR/simulate-regress"
SRC_DIR_SIMULATE_IP="$FILE_DIR/simulate-ip-regress"
SRC_DIR_SIMULATE_CYCLES="$FILE_DIR/simulate-ip-cycles-regress"
SRC_DIR_SIMULATE_8086="$FILE_DIR/simulate-8086-regress"
EMU86RS_BIN="$PROJECT_DIR/target/debug/emu86rs"

DATE=$(date +"%Y-%m-%d at %H:%M:%S")
echo "Date: $DATE"

function help_msg() {
    echo ""
    echo "regress.sh [OPTIONS]"
    echo ""
    echo "Run regressions for emu86rs."
    echo ""
    echo "Options:"
    echo " --no-rtos          Don't run the RTOS regression (RTOS run by default)."
    echo " --rtos             Run only the RTOS regression."
    echo " --snake            Run only the Snake game regression."
    echo ""
}

CHECK_REGULAR="true"
CHECK_RTOS="true"
CHECK_SNAKE="false"

while [[ $# -gt 0 ]]; do
case $1 in
    -h | --help)
        help_msg
        exit
        ;;
    --rtos)
        shift
        CHECK_REGULAR="false"
        ;;
    --no-rtos)
        shift
        CHECK_RTOS="false"
        ;;
    --snake)
        shift
        CHECK_REGULAR="false"
        CHECK_RTOS="false"
        CHECK_SNAKE="true"
        ;;
    *)
        echo "ERROR: Unknown arg '$1'"
        exit 1
        ;;
    esac
done

# See if nasm exists on the system
HAS_NASM="false"
command -v nasm &> /dev/null
if [ $? == 0 ]; then
    nasm -v &> /dev/null
    if [ $? == 0 ]; then
        echo "NASM found"
        HAS_NASM="true"
    fi
fi

# Machines with nasm will rebuild everything by default, to make sure the
# binaries change as the assembly changes. Machines without nasm will use the
# binaries committed to the repo.
if [ "$HAS_NASM" == "true" ]; then
    if ! "$SCRIPT_DIR/rebuild.sh" ; then
        echo "Rebuild failed"
        exit 1
    fi
else
    "$FILE_DIR/clean.sh"
    # Make sure build dir exists, so we can output into it, but empty, so we
    # don't append to existing files
    mkdir -p "$BUILD_DIR"
fi

cd "$SCRIPT_DIR" || exit
echo "Building emulator source code..."
if ! cargo build -q; then
    exit 1
fi

rc=0

if [ "$CHECK_REGULAR" == "true" ]; then
    # Check the decode of everything in decode-regress
    # for GOLDEN_BIN in "$SRC_DIR_DECODE"/*; do
    for file in "$SRC_DIR_DECODE"/*.asm; do
        BASE=$(basename "${file%.*}")
        # Get the built binary corresponding to this .asm file
        GOLDEN_BIN="$BIN_86_DIR/$BASE"
        echo "Checking decode of $GOLDEN_BIN..."
        OUR_ASM="$BUILD_DIR/$BASE-ours.asm"
        OUR_BIN="$BUILD_DIR/$BASE-ours.bin"
        OUR_BIN_TXT="$BUILD_DIR/$BASE-ours.bin.txt"
        GOLDEN_BIN_TXT="$BUILD_DIR/$BASE.bin.txt"
        OUR_LOG="$BUILD_DIR/$BASE-ours.log"
        if ! $EMU86RS_BIN --verbose --decode "$GOLDEN_BIN" "$OUR_ASM" > "$OUR_LOG"; then
            echo "ERROR: Decode program failed for $GOLDEN_BIN"
            rc=1
            break
        fi

        # Skip reassembly check of decoded program, since we don't have nasm
        if [ "$HAS_NASM" != "true" ]; then
            echo "NOTE: No nasm, so skipping reassembly check..."
            continue
        fi

        if ! nasm "$OUR_ASM" -o "$OUR_BIN"; then
            echo "ERROR: Assembly of decoded output failed for $GOLDEN_BIN"
            rc=1
            break
        fi

        # Convert binary to text, 1 byte per line, for diff
        od -Ax -v -t x1 -w1 "$OUR_BIN" > "$OUR_BIN_TXT"
        od -Ax -v -t x1 -w1 "$GOLDEN_BIN" > "$GOLDEN_BIN_TXT"

        DIFF="$BUILD_DIR/$BASE.bin.txt.diff"
        if ! diff "$OUR_BIN_TXT" "$GOLDEN_BIN_TXT" -u > "$DIFF"; then
            echo "ERROR: Decoded output didn't match golden output."
            echo "See $DIFF"
            echo "ours     (-): $OUR_BIN"
            echo "golden   (+): $GOLDEN_BIN"
            rc=1
            break
        fi
        rm "$DIFF"
    done


    # Check the decode and simulation of everything in simulate-regress, but without
    # checking the IP register.
    for file in "$SRC_DIR_SIMULATE"/*.asm; do
        # If the glob found nothing, it will be treated as a file, so skip it 
        if [ ! -e "$file" ]; then continue; fi
        # Get the built binary corresponding to this .asm file
        BASE=$(basename "${file%.*}")
        PROGRAM="$BIN_86_DIR/$BASE"
        echo "Checking simulation (w/o IP) of $PROGRAM..."
        SIMULATE_OUTPUT="$BUILD_DIR/$BASE-simulate.txt"
        SIMULATE_LOG="$BUILD_DIR/$BASE-simulate.log"
        if ! $EMU86RS_BIN "$PROGRAM" "$SIMULATE_OUTPUT" --verbose --no-ip > "$SIMULATE_LOG"; then
            echo "ERROR: Simulation of program failed for $PROGRAM"
            rc=1
            break
        fi
        SIMULATE_GOLDEN_OUTPUT="$SRC_DIR_SIMULATE/$BASE.txt"
        DIFF="$BUILD_DIR/$BASE-simulate.diff"
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
    for file in "$SRC_DIR_SIMULATE_IP"/*.asm; do
        # If the glob found nothing, it will be treated as a file, so skip it 
        if [ ! -e "$file" ]; then continue; fi
        # Get the built binary corresponding to this .asm file
        BASE=$(basename "${file%.*}")
        PROGRAM="$BIN_86_DIR/$BASE"
        echo "Checking simulation of $PROGRAM..."
        SIMULATE_OUTPUT="$BUILD_DIR/$BASE-simulate.txt"
        SIMULATE_LOG="$BUILD_DIR/$BASE-simulate.log"
        if ! $EMU86RS_BIN "$PROGRAM" "$SIMULATE_OUTPUT" --verbose > "$SIMULATE_LOG"; then
            echo "ERROR: Simulation of program failed for $PROGRAM"
            rc=1
            break
        fi
        SIMULATE_GOLDEN_OUTPUT="$SRC_DIR_SIMULATE_IP/$BASE.txt"
        DIFF="$BUILD_DIR/$BASE-simulate.diff"
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
    for file in "$SRC_DIR_SIMULATE_CYCLES"/*.asm; do
        # If the glob found nothing, it will be treated as a file, so skip it 
        if [ ! -e "$file" ]; then continue; fi
        # Get the built binary corresponding to this .asm file
        BASE=$(basename "${file%.*}")
        PROGRAM="$BIN_86_DIR/$BASE"
        echo "Checking simulation (w/ 8086 cycles) of $PROGRAM..."
        SIMULATE_OUTPUT="$BUILD_DIR/$BASE-simulate.txt"
        SIMULATE_LOG_8086="$BUILD_DIR/$BASE-simulate-8086.log"
        if ! $EMU86RS_BIN "$PROGRAM" "$SIMULATE_OUTPUT" --verbose --model-cycles 8086 > "$SIMULATE_LOG_8086"; then
            echo "ERROR: 8086 simulation of program failed for $PROGRAM"
            rc=1
            break
        fi
        # Append 8088 simulation results to 8086 simulation results
        echo "Checking simulation (w/ 8088 cycles) of $PROGRAM..."
        SIMULATE_LOG_8088="$BUILD_DIR/$BASE-simulate-8088.log"
        if ! $EMU86RS_BIN "$PROGRAM" "$SIMULATE_OUTPUT" --verbose --model-cycles 8088 > "$SIMULATE_LOG_8088"; then
            echo "ERROR: 8088 simulation of program failed for $PROGRAM"
            rc=1
            break
        fi
        SIMULATE_GOLDEN_OUTPUT="$SRC_DIR_SIMULATE_CYCLES/$BASE.txt"
        DIFF="$BUILD_DIR/$BASE-simulate.diff"
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
    for file in "$SRC_DIR_SIMULATE_8086"/*.asm; do
        # If the glob found nothing, it will be treated as a file, so skip it 
        if [ ! -e "$file" ]; then continue; fi
        # Get the built binary corresponding to this .asm file
        BASE=$(basename "${file%.*}")
        PROGRAM="$BIN_86_DIR/$BASE"
        echo "Checking simulation w/ 8086 cycles of $PROGRAM..."
        SIMULATE_OUTPUT="$BUILD_DIR/$BASE-simulate.txt"
        SIMULATE_LOG_8086="$BUILD_DIR/$BASE-simulate.log"
        if ! $EMU86RS_BIN "$PROGRAM" "$SIMULATE_OUTPUT" --verbose --model-cycles 8086 --stop-on-ret > "$SIMULATE_LOG_8086"; then
            echo "ERROR: 8086 simulation of program failed for $PROGRAM"
            rc=1
            break
        fi
        SIMULATE_GOLDEN_OUTPUT="$SRC_DIR_SIMULATE_8086/$BASE.txt"
        DIFF="$BUILD_DIR/$BASE-simulate.diff"
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
fi

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
    RTOS_BIN="$BIN_86_DIR/artoss.bin"
    if [ ! -f "$RTOS_BIN" ]; then
        echo "ERROR: Could not find RTOS binary file '$RTOS_BIN'"
        rc=1
        break
    fi

    echo "Simulating RTOS binary '$RTOS_BIN'..."
    BASE=$(basename "$RTOS_BIN")
    SIMULATE_OUTPUT="$BUILD_DIR/$BASE-simulate.txt"
    SIMULATE_LOG_8086="$BUILD_DIR/$BASE-tmp.log"
    if ! $EMU86RS_BIN "$RTOS_BIN" "$SIMULATE_OUTPUT" --verbose --model-cycles 8086 --ip "0x100" --sp "0xFFFE" --exit-after 10000 > "$SIMULATE_LOG_8086"; then
        echo "ERROR: Decode program failed for $RTOS_BIN"
        rc=1
        break
    fi
    break
done


# Execute the Snake game
# Use while loop to easily break out
while [ "$CHECK_SNAKE" == "true" ]; do
    SNAKE_BIN="$BIN_86_DIR/snake"
    BASE=$(basename "$SNAKE_BIN")
    SIMULATE_OUTPUT="$BUILD_DIR/$BASE-simulate.txt"
    echo "Simulating Snake binary '$SNAKE_BIN'..."
    if ! $EMU86RS_BIN "$SNAKE_BIN" "$SIMULATE_OUTPUT" --verbose --stop-on-int3 --exit-after 45000 --display-window > "$BUILD_DIR/$BASE-simulate.log"; then
        echo "ERROR: Simulation failed for $SNAKE_BIN"
        rc=1
        break
    fi
    echo "Finished Snake simulation!"
    break
done



if [ "$rc" == "0" ]; then
    echo "All regressions passed"
else
    echo "ERROR: Regressions failed"
fi
exit $rc
