; -------------------------------------------------------------------
;                              Sna86.
;                     An 8086 simulator snake!
; -------------------------------------------------------------------
;
; Assembly program intended to run in an 8086 simulator with functionaliy equal* to one made for homeworks
; up to Part 1 Episode 10 of the Peformance-Aware Programming Series (https://www.computerenhance.com/)
; by Casey Muratori. Demo - https://youtu.be/s_S4-QHeFMc.
;
; * mov, add, sub, cmp, je and jne was the target instructions. However I also added loop for convenience,
;   and jmp because conditional jumps have limited range! They are both simple enough to implment if you
;   already have the above.
; * int is also needed for a couple of things:
;   - int 0 is used to signal the size of the frame buffer to the simulator. Width is put in ax, height
;     into bx.
;   - int 15h is used for the program to signal it has finished a frame to allow the simulator to draw the
;     frame buffer to the screen
;
; Input is handled via the input "register" at memory address 0xFFFE, with each bit corresponding to an input
; button. Starting from the low bit the buttons are: Up, Down, Left, Right, Menu. Ie the low bit is used for
; the up button, the 5th bit is used for the menu button, and the top 3 bits are unused.
;
; Compiled with `nasm sna86.asm`. Or `nasm sna86.asm -DUSE_TITLE_SCREEN` to use the fancy title screen!
;
; Memory Layout:
; [0x0000]                Program
;                           Code
;                           Data
; [0x0000 + program_size] FrameBuffer
;                           u8 frame_buffer[frame_buffer_num_bytes]
; [¯\_(ツ)_/¯]             Snake
;                           u16 length
;                           u16 head
;                           // x, y coordinates stores as u16s. ie XXYYXXYYXXYYXX
;                           // These could be u8s. But simulator doesnt handle lots of u8 things. so u16 easier for now
;                           u16 *parts 
;
; NOTE(Charles): <label> resb/resw <num_bytes/words>, can be used to reserve space in binary! eg:
; scratch resb 256 ; create 256 byte scratch space.
;
; @TODO
; - Display the score! 
; - Seed the rng with some interrupt to get start value
; - Speed up menu transition with by adding interrupt to specifiy frame pointer. Then flip screens.
; - sometimes on eating an apple the new one can spawn in such a way that to gets eaten right away
;   and you get stuck with no apples?

; NOTE(Charles): SI, DI and the segment registers are not currently used, so my simulator is configured to
; use these for deubgging. It will print the contents of SI, DI each frame. And, if any segment registers
; are set, it will print out the word at that address in memory.

bits 16

%define MEMORY_SCRATCH 0xFF00
%define REGISTER_INPUT 0xFFFE

%define INPUT_UP    0x01
%define INPUT_DOWN  0x02
%define INPUT_LEFT  0x04
%define INPUT_RIGHT 0x08
%define INPUT_MENU  0x10

%define GAME_STATE_MENU 0
%define GAME_STATE_PLAY 1

%define SCREEN_SIZE 64
; @TODO Setting this to 100 giffs big bug! Must be overwriting something somewhere. Pls fix.
%define screen_color 30
%define SCREEN_RGB screen_color, screen_color, screen_color
; Result of the input register mapping. This is probably bad!
%define Direction_up    1
%define Direction_down  2
%define Direction_left  4
%define Direction_right 8

%define SNAKE_START_LENGTH 25
%define SNAKE_R 100
%define SNAKE_G 220
%define SNAKE_B 100
%define SNAKE_RGB SNAKE_R, SNAKE_G, SNAKE_B
%define APPLE_R 250
%define APPLE_G 100
%define APPLE_B 100
%define APPLE_RGB APPLE_R, APPLE_G, APPLE_B

; Simulator maxes out at some number of instructions per frame. Not measured number but at 32x32
; we can just about get 60fps. Above that not so much. Did some basic measurements and looks like
; it's the instruction decode that is the slow part. Thanks Casey ;). Joke obv.
; For snake, don't actually need to draw screen everytime, just draw pixels that need to update!
%define DRAW_SCREEN_EVERY_FRAME 0


; -------------------------------------------------------------------
;                              Macros!
; -------------------------------------------------------------------
; @TODO implement call, ret (probably requires push, pop too) to use actual functions! Can only do macros
; for now.
; NOTE(Charles): Macros seemed like a great idea at first. But holy moly they make keeping track of
; which registers are used hard! (Or I guess only really cos I went and nested macros :roll:)

; Don't have mul yet, just use adds!
; uses cx register as counter (loop instruction), result will be in ax.
; Only works for memory or immediate parameters atm!
; This loops b times so it is consderiably quicker if you put the smaller number in b!
; @TODO Implement mul instruction in simulator!
; ax multiply(a, b)
%macro multiply 2
    mov ax, 0
    mov cx, 0  ; mov then add so can use zero flag
    add cx, %2
    jz %%done
    %%who_needs_mul:
        add ax, %1
        loop %%who_needs_mul
    %%done:
%endmacro

%macro divide 2
    ; @TODO can do a similar thing for divsion using subtraction.
    ; if a is divisible by b, then loop doing a - b - b - b - b until you get 0. Number of loops equal answer!
%endmacro

; This is a no go for a few reasons atm:
; 1) without mul the multiply with large numbers is _very_ slow!
; 2) even if I had mul modding the result so its in a useful range seems hard (impossible?) without div and,
;    more cruicially jl, jg etc
; Sticking with table of random numbers with optional seed from host.
%define LCG_INITIAL_SEED 1234
%macro lcg_generate_random_number 0
    ; Generate a pseudo-random number using a linear congruential generator (LCG)
    ; Algorithm: Xn+1 = (a * Xn + c) mod m
    %define LCG_A 11969
    %define LCG_C 52429
    %define LCG_M 65536 ; 2^16
    multiply LCG_A, [lcg_seed] ; Welp, this is gonna be slow as shit...
    add ax, LCG_C
    mov [lcg_seed], ax ; Store new seed
%endmacro

%define RNG_NUM 1000
; Returns in ax
%macro generate_random_number 0
mov bp, [random_number_index]
add bp, bp ; have to multiply the index by two because the numbers are words!
add bp, rng_4096_1000
mov ax, [bp]
add [random_number_index], word 1
cmp [random_number_index], word RNG_NUM
jne %%end
mov [random_number_index], word 0
%%end:
%endmacro

; Fill a pixel with color.
; void draw_pixel(x, y, r=255, g=255, b=255)
; Hmm this now uses ax, bx and cx (in the multiply macro)
%macro draw_pixel 2-5 255, 255, 255
    ; framebuffer + 4 * (width * y + x)
    multiply [screen_size], %2 ; width * y
    add ax, %1 ; + x
    add ax, ax ; * 2
    add ax, ax ; * 2
    add ax, [frame_buffer]
    mov bx, ax
    mov byte [bx + 0], %5  ; Blue
    mov byte [bx + 1], %4  ; Green
    mov byte [bx + 2], %3  ; Red
    ; mov word [bx], 0xffff
    ; mov word [bx + 2], 0xffff
%endmacro

; Draw pixel with pre flattened coord z, in ax
; Q: Nasm docs say you can overload macros if the number of arguments is different, but I am
; getting a warning if this macro is called draw_pixel too?
%macro draw_pixel_flat 0-3 255, 255, 255
    ; framebuffer + 4 * (flat_coord)
    add ax, ax ; * 2
    add ax, ax ; * 2
    add ax, [frame_buffer]
    mov bx, ax
    mov byte [bx + 0], %3  ; Blue
    mov byte [bx + 1], %2  ; Green
    mov byte [bx + 2], %1  ; Red
    ; mov word [bx], 0xffff
    ; mov word [bx + 2], 0xffff
%endmacro

; Compare pixel pointed to by bp to given immediate r, g, b
%macro cmp_pixel_color 3
; Just checking first word for now @TODO check full rgb
cmp [bp], word (%2 << 8) | %3
%endmacro

; Fewer instructions than doing the nested loop. 
%macro clear_screen 1
    mov bp, [frame_buffer]
    mov cx, [frame_buffer_num_pixels]
    mov al, %1
    mov ah, %1
    %%loop_start:
        mov [bp], ax
        mov [bp + 2], al
        mov [bp + 3], byte 0xFF ; Do you respect alpha?
        add bp, 4
        loop %%loop_start
%endmacro

%macro draw_title_screen 0
; Copy title screen to frame buffer. This is slow
; @TODO Implement ability for program to specificy the frame pointer via interrupt for fast switching!
mov bp, [frame_buffer]
mov bx, title_screen
mov cx, [frame_buffer_num_pixels]
mov di, 0
%%copy_title_screen_loop:
    mov ax, [bx + di]
    mov [bp + di], ax
    mov ax, [bx + di + 2]
    ; mov [bp + di + 2], ax
    ; @HACK It didn't affect me but TheGag96 trying to run this wanted 0xFF in alpha channel.
    ; title screen image has alpha < 0xFF for some pixels. Fix that up here. Should really just fix
    ; the image. Adds an extra frame_buffer_num_pixels instructions.
    mov [bp + di + 2], al
    mov [bp + di + 3], byte 0xFF
    add di, 4
    loop %%copy_title_screen_loop
%endmacro


; -------------------------------------------------------------------
;                     Game Code Starts Here!
; -------------------------------------------------------------------
; NOTE(Charles): Pretty needlessly dynamically done.
multiply [screen_size], [screen_size]
mov [frame_buffer_num_pixels], ax
add ax, ax ; *4
add ax, ax 
mov [frame_buffer_num_bytes], ax

; Set initial game state based on whether title screen is present. 
mov [game_state], word GAME_STATE_PLAY
mov ax, end
mov bx, title_screen
cmp ax, bx
je no_title_screen
mov [game_state], word GAME_STATE_MENU
mov [has_title_screen], word 1
no_title_screen:

; snake_init
mov ax, [program_size]
add ax, [frame_buffer_num_bytes]
mov [snake], ax

; Request frame buffer size using int 0
mov ax, [screen_size]
mov bx, [screen_size]
int 0

%if !DRAW_SCREEN_EVERY_FRAME
; Draw background
; Have to use constant color so that when we are "deleting" snake parts we know what color to use.
; @TODO If did want nicer background could load it into memory separate to frame buffer to refer to!
; Frame buffer sits just after the code.
clear_screen screen_color
%endif

cmp [game_state], word GAME_STATE_PLAY
je start_playing
draw_title_screen
state_title_screen:
mov bp, REGISTER_INPUT
mov ax, [bp]
cmp ax, 0
jne start_playing
jmp update_end

start_playing:
mov [REGISTER_INPUT], word 0 ; clear the input used to start the game so we start in correct direction!
mov [game_state], word GAME_STATE_PLAY
clear_screen screen_color
reset:
mov ax, SNAKE_START_LENGTH
mov [score], word 0

mov bp, [snake]
mov [bp], ax ; snake.length = SNAKE_START_LENGTH
sub ax, 1
mov [bp + 2], ax; snake.head = bla


%define SNAKE_START SCREEN_SIZE / 2
mov [snake_x], word SNAKE_START
sub [snake_x], word 1 ; @HACK to align snake_x with snake parts
mov [snake_y], word SNAKE_START
mov [direction], word Direction_right

; No bounds checking for if the start length puts us off screen!
mov cx, [bp]
add bp, 4 ; bp = snake.parts
init_snake_parts:
    mov bx, SNAKE_START
    mov [bp + 2], bx ; y
    sub bx, cx
    mov [bp], bx ; x
    add bp, 4
    loop init_snake_parts

%macro draw_snake 0
mov bp, [snake]
mov dx, [bp]
add bp, 4 ; point to snake.parts
%%draw_snake_loop:
    draw_pixel [bp], [bp + 2], SNAKE_RGB;, SNAKE_G, SNAKE_B
    add bp, 4
    sub dx, 1
    jnz %%draw_snake_loop
%endmacro
; Should really do this during the init above, but awkward to do while not really fully supporting
; byte operations.
draw_snake

; apple_init
%macro spawn_apple 0
    ; This can potentially infinte loop!
    %%loop:
        generate_random_number
        ; check this position isn't occupied!
        mov bp, ax
        mov bp, bp
        mov bp, bp
        add bp, [frame_buffer]
        cmp_pixel_color SCREEN_RGB
        jne %%loop
    draw_pixel_flat APPLE_RGB
    ; Q: Do I actually even need to store this? Can just check color of frame buffer? I'll be checking frame
    ; buffer to detect self collisions anyway.
    mov [apple_position], ax 
%endmacro
spawn_apple

do_one_frame:
; Read input
mov ax, [REGISTER_INPUT]

; Up Key
cmp ax, INPUT_UP
jne up_not_pressed
; Can't change to up if we are currently going down.
cmp [direction], word Direction_down
je read_input_end
mov [direction], word Direction_up
jmp read_input_end
up_not_pressed:

; Down Key
cmp ax, INPUT_DOWN
jne down_not_pressed
; Can't change to down if we are currently going up.
cmp [direction], word Direction_up
je read_input_end
mov [direction], word Direction_down
jmp read_input_end
down_not_pressed:

; Left Key
cmp ax, INPUT_LEFT
jne left_not_pressed
; Can't change to left if we are currently going right.
cmp [direction], word Direction_right
je read_input_end
mov [direction], word Direction_left
jmp read_input_end
left_not_pressed:

; Right Key
cmp ax, INPUT_RIGHT
jne right_not_pressed
; Can't change to right if we are currently going left.
cmp [direction], word Direction_left
je read_input_end
mov [direction], word Direction_right
jmp read_input_end
right_not_pressed:

; Menu Key
cmp ax, INPUT_MENU
jne menu_not_pressed
mov [game_state], word GAME_STATE_MENU
draw_title_screen
jmp update_end
menu_not_pressed:

read_input_end:


; Update player position
update_player_position:
%define DEBUG_SNAKE_POSITION 0
%if DEBUG_SNAKE_POSITION
draw_pixel [snake_x], [snake_y], screen_color, screen_color, screen_color
%endif

; Handle left/right movement
mov ax, [snake_x]

; Move right
cmp [direction], word Direction_right
je direction_is_right
jmp direction_not_right ; Too big a jump for conditional now!
direction_is_right:
; Update the snake_x
add ax, 1
cmp ax, [screen_size]
jne no_wrap_right
mov ax, 0
no_wrap_right:
mov [snake_x], ax
jmp update_player_position_end
direction_not_right:

; Move left
cmp [direction], word Direction_left
je direction_is_left
jmp direction_not_left
direction_is_left:
; Update the snake_x
add ax, -1
cmp ax, -1
jne no_wrap_left
mov ax, [screen_size]
sub ax, 1
no_wrap_left:
mov [snake_x], ax
jmp update_player_position_end
direction_not_left:

; Handle up/down movement
mov ax, [snake_y]

; Move up
cmp [direction], word Direction_up
je direction_is_up
jmp direction_not_up
direction_is_up:
; Update the snake_y
add ax, 1
cmp ax, [screen_size]
jne no_wrap_up
mov ax, 0
no_wrap_up:
mov [snake_y], ax
jmp update_player_position_end
direction_not_up:

; Move down
cmp [direction], word Direction_down
je direction_is_down 
jmp direction_not_down
direction_is_down:
; Update the snake_y
add ax, -1
cmp ax, -1
jne no_wrap_down
mov ax, [screen_size]
sub ax, 1
no_wrap_down:
mov [snake_y], ax
jmp update_player_position_end
direction_not_down:

update_player_position_end:

; Check what the new x, y contains. Apple, snake or nothing

; Given x, y coord but the address of the pixel into ax
%macro get_pixel 2
multiply [screen_size], %2 ; width * y
add ax, %1 ; + x
add ax, ax ; * 2
add ax, ax ; * 2
add ax, [frame_buffer]
%endmacro

get_pixel [snake_x], [snake_y]
mov bp, ax
cmp_pixel_color SNAKE_RGB
jne i_atent_dead
; Just clearing whole screen for now, will be slow
; @TODO clear all snake parts and apple, not entire screen.
; I actually kind of like the delay when you die that this causes so not doing this because would then need
; to implement that delay!
clear_screen screen_color
jmp reset

i_atent_dead:
; Either we eat an apple and grow, or just move snake. Either way we need the to increment the head!
mov bx, [snake]
mov cx, [bx + 2] ; snake.head
add cx, 1;
cmp cx, [bx]
jne no_wrap_head
mov cx, 0
no_wrap_head:
mov [bx + 2], cx ; snake.head = new_head_index

cmp_pixel_color APPLE_RGB
; @TODO I had to change to unconditnal jump here because lots instrucitnos. but many instrucitons below
; can be removed then this might go back to unconditnoial (if want to save more instructions)
je eaten_an_apple
jmp no_apple

eaten_an_apple:
add [score], word 1
spawn_apple

; Grow the snake!
mov bp, [snake]
add bp, 4 ; bx = snake.parts
; bp -> snake.parts
; cx == snake.head
add cx, cx 
add cx, cx ; cx * 4 (cx is index but size of part is 4 [2xu16])
add bp, cx ; bp = &snake.parts[snake.head]
; move all parts from snake.head (new_index) -> snake.length up one
; @TODO being bit lazy with registers here, can remove some of these instructinos.
mov bx, [snake]
mov cx, [bx]     ; cx = snake.length - snake.head
sub cx, [bx + 2] ; -
add cx, cx ; bx -> snake.parts + snake.length
add cx, cx ; -
mov bx, bp ; -
add bx, cx ; -
add bx, 3
; @TODO re work this to go forward and not be so awkward lol!
realloc_snake_part:
    ; have to do a byte at a time to have negative address displacement I think. But why Am I doing this
    ; backwards at all?!
    mov al, [bx - 4] ; each part is 4 bytes
    mov [bx], al
    sub bx, 1
    cmp bx, bp
    jne realloc_snake_part

mov bp, [snake]
add [bp], word 1 ; snake.length++
; set new head
mov ax, [snake_x]
mov [bx], ax
mov ax, [snake_y]
mov [bx + 2], ax
; draw new head
draw_pixel [bx], [bx + 2], SNAKE_RGB
jmp update_end

no_apple:
; replace_tail_with_head
; @TODO maybe rework where bp, bx point, depending on how eating code plays out!
mov bp, [snake]
add bp, 4 ; bx = snake.parts
; bp -> snake.parts
; cx == snake.head
add cx, cx 
add cx, cx ; cx * 4 (cx is index but size of part is 4 [2 x u16])
add bp, cx ; bp = &snake.parts[snake.head]
; clear tail
draw_pixel [bp], [bp + 2], SCREEN_RGB
; set new head
mov ax, [snake_x]
mov [bp], ax
mov ax, [snake_y]
mov [bp + 2], ax
; draw new head
draw_pixel [bp], [bp + 2], SNAKE_RGB


%if DEBUG_SNAKE_POSITION
draw_pixel [snake_x], [snake_y], 255, 0, 255
%endif

update_end:
; @TMP debugging
mov ax, [score]
mov si, ax

%if DRAW_SCREEN_EVERY_FRAME
; Clear the screen
; Frame buffer sits just after the code.
mov bp, [frame_buffer]

mov dx, [screen_size]
y_loop_start:
    
    mov cx, [screen_size]
    x_loop_start:
        mov byte [bp + 0], screen_color  ; Blue
        mov byte [bp + 1], screen_color  ; Green
        mov byte [bp + 2], cl  ; Red
        ; mov byte [bp + 3], 255 ; Alpha
        add bp, 4
            
        loop x_loop_start
    
    sub dx, 1
    jnz y_loop_start

draw_snake
%endif

; Use int 15 as a hacky way to signal to hosting simualtor to draw frame and sleep till next frame
; There is something about on actualy 8086 used to call int 15h with ah=86h to do micro second sleeps but
; 1) details of it aren't obvious to me but also 2) that would require asm code to keep track of timings!
; mov ah, 86h
; mov cx, micro_seconds
int 15h

; @TODO: Clean up handling of menu screen. This is very spaghetti!
cmp [game_state], word GAME_STATE_PLAY
je is_playing
jmp state_title_screen
is_playing:
; NOTE(Charles): I actually _had_ to implement jmp to change the jnz that was here before because
; conditional jumps only have 1 byte offset. Once I had enough instruciotns I started getting weird assembled
; code. Eg I got a pop and a test instead of the jnz!
jmp do_one_frame


; -------------------------------------------------------------------
;                               Data
; -------------------------------------------------------------------
; Need some way to mark data stuff to not be executed (or even decoded/printed?). Atm int3 will force us
; to break the loop
int3
int3
int3
; NOTE(Charles): The below variable names are really just labels! eg `program_size: dw end` is equivalent.
; gets the size of the program so we can check we arent gonna run out of memory!
program_size dw end
game_state dw 0 ; 0: MENU, 1: PLAY
score dw 0;
has_title_screen dw 0
frame_buffer_num_bytes dw 0
frame_buffer_num_pixels dw 0
frame_buffer dw end
snake dw 0
screen_size dw SCREEN_SIZE; Using a variable instead of a constant as could then dynamically change screen size!
; Technically all the information we need is in the snake struct. But it is much more convienet
; to keep track of the current position in these statics!
snake_x dw 0
snake_y dw 0
apple_position dw 0 ; flattened coordinate to make easier to randomly generate.
direction dw 0
rng_seed dw LCG_INITIAL_SEED
random_number_index dw 0
; rng_limt_number
; ", ".join([str(random.randint(0, 4096)) for _ in range(100)])
; When testing if want first apple to be in path use 2050
rng_4096_10    dw 630, 2080, 3762, 1334, 2310, 2790, 960, 3855, 3001, 1035
rng_4096_100   dw 3818, 2323, 3807, 1082, 2237, 1101, 1694, 1536, 71, 3289, 3184, 3167, 2734, 626, 3559, 2094, 2178, 1158, 4009, 2958, 3170, 367, 3947, 3555, 120, 4021, 645, 1329, 832, 3553, 2819, 2478, 1071, 1754, 1572, 3580, 1230, 2058, 3843, 960, 3460, 1315, 1891, 1112, 2333, 1811, 3124, 436, 1789, 4027, 2957, 1815, 528, 953, 940, 4043, 472, 1978, 2388, 1869, 2830, 2758, 3024, 688, 3538, 1031, 2239, 94, 555, 2960, 4049, 3431, 1205, 663, 395, 1329, 3835, 459, 3404, 995, 632, 2964, 3507, 933, 2154, 2404, 362, 3960, 1083, 1894, 490, 3111, 1871, 2082, 3730, 2426, 2321, 2352, 4074, 3110
rng_4096_1000  dw 1511, 771, 2197, 2256, 1133, 1022, 3580, 1786, 2204, 3182, 713, 2126, 3942, 148, 3438, 2714, 1825, 1861, 2634, 3926, 872, 1327, 2635, 3078, 369, 2189, 1399, 323, 1200, 2015, 1470, 152, 3544, 3251, 618, 2481, 1168, 37, 1255, 790, 3564, 2581, 209, 1921, 1075, 2593, 1197, 2245, 3943, 2359, 4057, 3449, 1581, 3058, 3226, 2498, 2119, 2845, 116, 2892, 1840, 879, 2691, 1581, 678, 1927, 1368, 1377, 2657, 3152, 1401, 243, 1703, 1388, 2612, 979, 3605, 1806, 373, 3295, 1000, 2379, 2886, 1920, 3090, 373, 1284, 3741, 992, 1057, 2990, 1008, 2425, 1679, 3819, 1335, 744, 121, 3746, 1549, 1353, 5, 1999, 359, 1795, 2974, 1875, 1608, 427, 3973, 3719, 1699, 961, 2611, 3426, 383, 1296, 1755, 3989, 4067, 1875, 601, 187, 2960, 2298, 940, 392, 359, 239, 900, 3971, 16, 1611, 355, 1873, 1592, 2253, 1638, 1156, 296, 624, 3490, 2692, 1800, 1167, 1680, 634, 2137, 3783, 3562, 3697, 1542, 694, 1793, 1048, 2890, 197, 2081, 107, 2341, 3734, 2962, 3839, 3139, 462, 2317, 2145, 1612, 1518, 408, 2641, 1436, 1108, 1042, 3067, 1290, 449, 3036, 3298, 1692, 4048, 237, 2517, 1894, 1709, 4084, 761, 780, 3071, 3064, 2022, 904, 3649, 2729, 2067, 4075, 1474, 3837, 2327, 712, 205, 2863, 2320, 1912, 2601, 2853, 472, 2700, 2818, 3969, 3604, 641, 962, 771, 194, 2879, 1980, 2812, 1526, 1832, 464, 2031, 2463, 3605, 296, 2682, 3701, 144, 3420, 4083, 825, 806, 3552, 350, 2672, 1718, 3792, 1830, 695, 2121, 1504, 839, 3134, 3895, 2447, 1217, 3959, 3193, 1464, 2623, 4065, 693, 2894, 3951, 4014, 2211, 4084, 2902, 895, 4052, 2237, 453, 2307, 2980, 3425, 729, 2617, 3410, 546, 318, 1957, 3505, 1283, 1365, 992, 991, 1563, 3827, 1960, 854, 2065, 12, 919, 75, 258, 3221, 4050, 3340, 2196, 2126, 4050, 3546, 434, 2154, 267, 1038, 2627, 1329, 4059, 2963, 3863, 3483, 1778, 1505, 1705, 3973, 5, 513, 2184, 3180, 1625, 2732, 3075, 1663, 1592, 1061, 2606, 3156, 3068, 2833, 2226, 3578, 2795, 3545, 3622, 1835, 804, 3955, 1070, 2165, 886, 2756, 1852, 1658, 1495, 809, 3376, 2316, 1106, 1798, 3085, 4001, 2227, 8, 1026, 770, 3896, 3254, 3316, 1264, 1275, 3383, 391, 3442, 1218, 1210, 3404, 425, 20, 1757, 131, 3305, 1740, 383, 2244, 2011, 4062, 1154, 1615, 1879, 366, 1463, 3660, 1185, 949, 1477, 2423, 2875, 2999, 3166, 43, 865, 675, 3873, 1548, 2585, 1011, 1276, 3314, 553, 2688, 283, 2188, 2496, 3015, 3567, 2239, 954, 1561, 1258, 508, 2821, 524, 2918, 2532, 961, 996, 906, 2553, 2052, 488, 1077, 2383, 1948, 124, 952, 2750, 257, 2174, 893, 46, 117, 3224, 93, 1150, 3615, 1679, 3405, 3306, 1665, 252, 2724, 1608, 76, 1906, 3551, 3141, 2935, 1824, 1652, 1610, 2735, 2049, 788, 1304, 80, 3317, 2477, 2729, 2943, 835, 490, 2714, 2116, 3810, 4005, 3719, 10, 4019, 2855, 244, 3582, 2841, 4087, 3967, 858, 356, 2477, 3716, 2504, 2178, 2568, 3310, 1244, 3190, 194, 2478, 2855, 1445, 3406, 3907, 1273, 2007, 1586, 2258, 1732, 3091, 3399, 2912, 1297, 406, 3768, 3781, 1193, 3704, 2718, 4068, 1517, 219, 2281, 745, 1356, 304, 1843, 3531, 2693, 3534, 132, 2671, 2786, 1006, 2483, 222, 1966, 3385, 782, 548, 1054, 1094, 1096, 2031, 3245, 270, 3701, 3200, 1256, 2138, 468, 2589, 1728, 2435, 2386, 3725, 3834, 287, 3709, 2116, 3061, 1305, 426, 2504, 1500, 409, 2352, 2284, 1028, 850, 3883, 2852, 1955, 3184, 2197, 2228, 2804, 443, 1960, 1311, 208, 2755, 677, 1659, 1611, 2759, 145, 3229, 2895, 2347, 1411, 2203, 2450, 1776, 2406, 1408, 3667, 3671, 811, 281, 2786, 3027, 3365, 2957, 3794, 1407, 1558, 4047, 1972, 169, 351, 2933, 1922, 3476, 3652, 104, 1145, 2807, 4029, 1615, 2147, 149, 937, 319, 3210, 3398, 1118, 1358, 3462, 658, 2106, 3808, 1088, 407, 2195, 1470, 1736, 3714, 735, 1411, 251, 1776, 2035, 2942, 3469, 953, 4082, 2776, 479, 3026, 1812, 1140, 3522, 928, 4083, 2578, 3552, 3296, 3869, 1604, 353, 3300, 1943, 3854, 192, 670, 1455, 3558, 1468, 1018, 880, 1946, 566, 2023, 2962, 4076, 425, 3403, 286, 3803, 640, 2199, 1677, 191, 3167, 2688, 3918, 1829, 2079, 990, 209, 905, 827, 3001, 2805, 795, 1157, 2521, 3645, 2870, 1668, 3812, 79, 1664, 474, 3768, 1823, 957, 3656, 3983, 2151, 3024, 403, 211, 2961, 2675, 1550, 3031, 2846, 79, 996, 290, 3149, 2408, 1137, 1833, 1537, 3047, 4024, 799, 2434, 2573, 3104, 3641, 2401, 1792, 2579, 3077, 3911, 1832, 3519, 449, 1980, 426, 702, 868, 2053, 3675, 2720, 3229, 3853, 1275, 1087, 2210, 2687, 702, 178, 3687, 1269, 2635, 2617, 2718, 1362, 885, 69, 3396, 943, 728, 2895, 2858, 1489, 586, 869, 2915, 2647, 705, 401, 2832, 3295, 2676, 3794, 1709, 2211, 3335, 2423, 2237, 2424, 748, 3382, 1848, 2606, 2534, 1761, 479, 3921, 3738, 3129, 3438, 199, 2560, 2002, 8, 436, 1700, 440, 1169, 2338, 1750, 3915, 129, 2474, 2175, 3285, 1244, 2175, 1380, 2373, 3282, 1071, 2969, 2783, 606, 541, 2715, 2376, 1485, 3649, 296, 146, 2924, 2754, 3801, 3128, 851, 4053, 2960, 858, 3836, 2002, 1008, 446, 1395, 3275, 507, 2923, 3351, 711, 113, 3985, 104, 2605, 3556, 64, 1072, 1266, 3376, 723, 2491, 864, 2797, 645, 2712, 2861, 437, 1971, 2570, 2252, 990, 1709, 824, 2261, 2328, 1583, 2035, 896, 3212, 2876, 3216, 3664, 1087, 435, 979, 1672, 153, 3927, 1089, 1679, 3947, 1720, 1608, 2473, 454, 3613, 2280, 814, 1688, 2237, 3221, 2293, 916, 2821, 2069, 618, 3844, 3767, 185, 371, 1841, 3189, 2642, 1551, 1009, 1594, 53, 2631, 1324, 3080, 1468, 3227, 1164, 2882, 3451, 794, 3220, 2582, 3855, 1934, 1587, 2512, 3210, 2579, 2249, 2088, 522, 2960, 197, 3464, 3191, 2404, 3957, 3824, 127, 3503, 221, 2149, 3360, 3027, 1058, 3787, 2609, 1913, 1302, 1211, 2370, 3646, 1659, 4068, 1535, 2500, 1809, 2654, 3212, 3196, 3503, 323, 2614, 4085, 1044, 796, 2252, 3154, 850, 2669, 1071, 3269, 1885, 4036, 3199, 2090, 1311, 286, 1132, 3425, 332, 3542, 143, 332, 1388, 2999, 1107, 2974, 2478, 1698, 3923, 554, 2318, 1896, 3331, 2217, 1971, 1484, 372, 2163, 116, 4057, 3419, 745, 2044, 1295, 474, 1313, 1916, 3243, 1680, 4086, 3617, 2547, 2203, 2649, 3589, 3630, 1316, 148
; Somethign weird is happening when using 10k. At some point the snake stops clearing its tail. Presumably
; I'm overwriting some memory I shouldn't but I can't see where :shrug:.
; 10k random numbers for snake is proooobably overkill anyway. It's almost as much memory as the frame buffer...
; rng_4096_10000 dw 909, 3655, 4062, 2186, 1144, 2097, 2338, 235, 1071, 3344, 84, 3637, 3035, 1454, 533, 1205, 1369, 2584, 1584, 3986, 2731, 3914, 529, 3891, 1243, 2820, 3718, 292, 6, 1553, 1973, 3302, 2352, 605, 1067, 3239, 3895, 1109, 1330, 552, 1328, 1655, 1923, 2537, 1810, 229, 2226, 2380, 3735, 899, 1613, 3405, 836, 513, 678, 810, 2399, 2944, 1417, 2441, 631, 3153, 3267, 2762, 2748, 2589, 1397, 907, 2083, 1265, 2100, 639, 2386, 2177, 1302, 117, 4010, 373, 3146, 2859, 3139, 2804, 2160, 28, 3609, 3529, 503, 3895, 2422, 3597, 2492, 2417, 2198, 3465, 3229, 2921, 895, 3370, 3750, 247, 826, 3258, 4028, 3672, 2681, 3736, 1486, 3052, 372, 3421, 3662, 4005, 2436, 716, 3568, 1316, 2296, 1110, 162, 3064, 1705, 859, 11, 1587, 3791, 3653, 3071, 2895, 3786, 819, 2674, 2168, 459, 4014, 986, 1148, 2182, 1846, 261, 2616, 2048, 1472, 3647, 1697, 657, 1788, 3526, 111, 315, 2512, 1515, 3502, 3500, 1882, 2965, 3358, 1952, 265, 1141, 842, 1570, 2600, 2109, 725, 2471, 594, 2528, 772, 3014, 143, 1402, 2917, 3019, 728, 2566, 176, 1081, 3830, 343, 2896, 769, 3843, 3015, 1510, 3103, 402, 291, 3474, 2950, 1463, 2828, 3468, 3847, 3461, 3746, 1382, 2049, 1153, 461, 3005, 1220, 2493, 3269, 1133, 1935, 3760, 1172, 1985, 4005, 3417, 1119, 3193, 3289, 3018, 348, 3011, 1266, 1368, 3421, 138, 669, 971, 200, 3364, 2924, 3115, 2478, 2826, 3578, 593, 51, 3404, 1629, 1271, 3261, 1717, 2349, 3068, 2721, 3330, 1007, 1560, 4082, 269, 2532, 3496, 99, 3742, 352, 803, 2889, 3263, 1385, 2954, 332, 1695, 388, 1574, 3780, 3252, 3752, 2275, 1699, 3809, 413, 642, 1866, 3163, 1033, 3691, 214, 1544, 188, 1178, 1234, 1778, 365, 3693, 3508, 936, 417, 2270, 3303, 239, 1301, 3411, 1737, 3156, 3051, 3352, 951, 4023, 1329, 765, 831, 2319, 1146, 2336, 1934, 70, 3255, 2799, 2077, 2474, 1328, 2247, 1762, 820, 3988, 3459, 957, 2103, 2476, 3911, 1886, 1722, 4030, 502, 7, 1772, 3353, 1763, 1116, 2194, 3127, 1636, 2446, 3954, 371, 1503, 147, 3995, 3767, 1692, 541, 3163, 1200, 3213, 2412, 2901, 2028, 2830, 1405, 2989, 3517, 2052, 3381, 3159, 2102, 2736, 3440, 3057, 1904, 3218, 678, 1080, 2256, 2629, 2254, 2572, 3074, 2760, 517, 3510, 561, 550, 4028, 330, 2426, 304, 1315, 1989, 3370, 4080, 3071, 3866, 1456, 4031, 442, 3234, 2728, 2033, 2254, 453, 597, 609, 3483, 3475, 1001, 1823, 3672, 3124, 2634, 414, 2012, 402, 3129, 660, 63, 1091, 3557, 3254, 2034, 1140, 3335, 3092, 2859, 131, 3345, 2525, 782, 3832, 1868, 1974, 590, 4015, 2638, 1217, 1566, 2174, 653, 303, 671, 2841, 3150, 162, 977, 1115, 2856, 1557, 3129, 2100, 3585, 3205, 3608, 1289, 2797, 1201, 2537, 2652, 588, 1853, 3932, 1779, 3831, 3885, 2820, 3466, 4026, 3863, 1281, 1873, 2970, 1165, 3470, 779, 3579, 1059, 2025, 3321, 1339, 2415, 2963, 4082, 3493, 644, 181, 1364, 2338, 3719, 2899, 627, 761, 2780, 2942, 3399, 840, 3437, 1166, 2735, 4075, 2795, 1073, 3051, 2790, 300, 3358, 1581, 2243, 3706, 3410, 1134, 74, 3699, 2820, 2052, 1490, 911, 448, 1838, 2918, 291, 2701, 1965, 4026, 2446, 1036, 3968, 707, 3144, 2599, 1098, 1925, 297, 3584, 3770, 3425, 1115, 3971, 689, 2465, 850, 522, 861, 268, 4050, 867, 1106, 1605, 2376, 2940, 1936, 364, 72, 2461, 2815, 558, 3094, 4025, 254, 2521, 1924, 73, 1187, 127, 3374, 1930, 34, 2103, 3345, 1750, 2242, 3409, 1647, 1476, 3862, 2792, 972, 1602, 2682, 3385, 1651, 3527, 3930, 3737, 3081, 666, 2258, 385, 3208, 2854, 3424, 2511, 2827, 124, 2351, 1192, 2194, 1878, 3489, 1887, 2203, 3729, 1146, 4096, 2380, 391, 1704, 2399, 581, 2894, 2533, 1263, 1580, 3290, 3816, 1386, 755, 1169, 3223, 1124, 2831, 3409, 1590, 2131, 2136, 2541, 643, 1075, 3075, 1016, 2888, 3166, 1373, 2841, 12, 3437, 331, 3892, 3254, 1986, 2418, 2786, 904, 2189, 3372, 166, 3703, 3450, 1621, 3343, 1407, 3084, 44, 3777, 3930, 413, 912, 319, 3091, 481, 2152, 1902, 3262, 2523, 1165, 2553, 602, 80, 3013, 3808, 997, 3894, 1448, 3710, 2828, 3107, 1537, 2898, 3834, 4063, 3628, 3004, 2875, 2949, 1982, 736, 1503, 3071, 113, 2849, 2632, 1089, 1286, 3104, 1362, 2059, 1555, 2823, 434, 237, 58, 2614, 4028, 793, 4069, 1648, 485, 1201, 2254, 823, 2085, 1990, 2280, 879, 2865, 3851, 3154, 616, 1803, 2640, 3738, 1928, 2325, 3009, 2579, 3102, 230, 597, 3433, 1525, 2804, 1699, 2971, 1601, 1533, 1325, 3940, 334, 754, 244, 341, 1071, 3691, 340, 2655, 2630, 1278, 2697, 2868, 2091, 3079, 1726, 1524, 1788, 861, 894, 2751, 1057, 1040, 755, 2341, 2836, 2020, 3532, 3446, 1, 1107, 3125, 3421, 707, 3649, 205, 3163, 3774, 336, 2291, 3553, 39, 2565, 3763, 631, 780, 1630, 3170, 2061, 3853, 2777, 4083, 3723, 291, 1297, 3414, 997, 1683, 3286, 2259, 3396, 1416, 109, 1327, 1855, 2101, 3301, 3090, 3738, 1119, 3473, 994, 3441, 338, 2818, 1967, 2756, 102, 1442, 2691, 3773, 1548, 3127, 2733, 1832, 2702, 2549, 3938, 3096, 1548, 2871, 3295, 3466, 3327, 2942, 3809, 3365, 3153, 1761, 419, 2636, 3117, 1988, 3999, 1307, 3315, 3117, 2023, 1668, 2318, 246, 3811, 1525, 1747, 2254, 3760, 60, 3625, 2702, 131, 563, 4009, 3964, 1584, 2401, 2342, 341, 2522, 2205, 1362, 1975, 2030, 1480, 2016, 4009, 3154, 2614, 123, 3967, 2922, 2790, 2532, 1622, 1286, 1577, 2313, 424, 3853, 376, 3395, 2641, 1789, 466, 1565, 2238, 1764, 2578, 1852, 2283, 3061, 1948, 4004, 2933, 3900, 1709, 2105, 3008, 2855, 3765, 1043, 960, 853, 2018, 1684, 748, 142, 350, 1679, 1056, 1459, 2637, 3471, 2532, 811, 3008, 2552, 2686, 3626, 920, 2893, 1994, 768, 2180, 3599, 1910, 563, 2182, 1295, 2823, 2182, 3592, 3072, 1893, 2561, 491, 2948, 2771, 3952, 159, 846, 2664, 4075, 854, 3088, 1340, 401, 3555, 546, 484, 789, 618, 2901, 3736, 163, 1806, 952, 1271, 3324, 3357, 3144, 3076, 2629, 3758, 189, 1031, 745, 838, 2529, 2128, 3246, 2914, 1495, 3214, 2328, 649, 2752, 2971, 3944, 455, 1236, 20, 2708, 1090, 4022, 511, 2944, 3430, 1003, 2284, 3255, 3710, 1815, 1552, 2082, 2636, 1498, 2225, 3184, 2039, 909, 3740, 2950, 2670, 1077, 2316, 3719, 2767, 1061, 2490, 2788, 3608, 3100, 2961, 2022, 2417, 243, 2033, 3519, 746, 2071, 81, 3704, 3794, 1864, 2374, 2132, 143, 2078, 432, 887, 3090, 2280, 3304, 3814, 299, 1578, 1587, 2291, 2030, 558, 3430, 3099, 3004, 1131, 2490, 1977, 899, 24, 3711, 379, 2763, 3269, 3535, 3539, 417, 1359, 2806, 2543, 3683, 3823, 1710, 3243, 2483, 48, 1021, 94, 1492, 3055, 703, 1215, 2847, 1271, 974, 1686, 3709, 465, 3153, 2078, 3424, 301, 2594, 1766, 825, 1891, 3624, 3748, 334, 1363, 3117, 3462, 3130, 361, 2255, 3645, 3700, 2850, 46, 2547, 2305, 405, 2120, 965, 4077, 2639, 1535, 1227, 834, 1095, 3958, 2248, 532, 801, 2865, 4092, 3037, 1739, 1582, 3467, 3612, 1452, 3773, 2131, 948, 1033, 1326, 3963, 1652, 1407, 345, 2550, 3411, 1192, 3514, 97, 3091, 526, 3023, 2621, 211, 655, 1535, 388, 1733, 534, 702, 352, 277, 3819, 1151, 1053, 3627, 3455, 3538, 3386, 1429, 610, 2072, 960, 2659, 3454, 1263, 2297, 1650, 3706, 1644, 3648, 878, 3302, 2803, 2993, 3626, 3957, 1904, 2443, 1945, 1824, 2399, 2678, 2245, 3746, 345, 1105, 1282, 998, 3495, 670, 672, 307, 2252, 2530, 2518, 526, 321, 1303, 3895, 719, 1630, 2360, 3700, 3834, 2268, 2909, 2166, 901, 3233, 1187, 2371, 30, 3092, 2707, 2636, 3194, 780, 3732, 2432, 1626, 2670, 3872, 3153, 2454, 2766, 3978, 3586, 2931, 1918, 2985, 3989, 4021, 3369, 2045, 1037, 647, 1968, 1886, 980, 2483, 856, 3675, 397, 2082, 2033, 433, 1081, 3921, 4090, 3256, 1232, 2489, 1975, 2422, 2486, 410, 2473, 3420, 2090, 2464, 812, 3173, 1799, 2195, 1708, 742, 2471, 2490, 310, 1527, 1783, 3908, 2582, 3944, 266, 740, 1968, 1408, 2435, 1698, 2091, 1779, 3625, 786, 104, 3670, 3254, 38, 4027, 1388, 2943, 927, 40, 2569, 3323, 2831, 294, 182, 4072, 633, 1829, 2318, 326, 2425, 676, 3383, 3945, 1598, 1065, 3437, 246, 929, 3250, 1966, 3355, 3632, 888, 1773, 2829, 2235, 2769, 2762, 3447, 1168, 3151, 109, 2863, 146, 2717, 2988, 2779, 3325, 2960, 1687, 36, 620, 1175, 140, 1428, 479, 1778, 1291, 2715, 1104, 3022, 1470, 2549, 864, 1535, 297, 3956, 1646, 3190, 3877, 3018, 1331, 1121, 3296, 3515, 2365, 1668, 1791, 1232, 1199, 1480, 3172, 2845, 2030, 2974, 2085, 2834, 138, 3725, 234, 378, 72, 2972, 2421, 2128, 1541, 186, 3163, 58, 681, 3649, 3423, 909, 3351, 452, 3389, 2235, 2598, 502, 307, 1358, 1688, 2176, 3121, 3776, 211, 2157, 591, 1536, 2182, 2435, 2937, 3384, 359, 1151, 2962, 657, 3403, 837, 3341, 1473, 426, 502, 3158, 2227, 2005, 1117, 1179, 4023, 1776, 2917, 1298, 47, 2301, 2691, 3919, 3238, 595, 2363, 3201, 2072, 1423, 1429, 1900, 516, 1215, 2842, 3697, 1928, 2750, 3258, 519, 1313, 130, 413, 3846, 2238, 1592, 2117, 3224, 115, 214, 1850, 2372, 3684, 3943, 2439, 841, 2183, 2045, 4032, 1193, 2488, 513, 3393, 1835, 3863, 3869, 2310, 1467, 3882, 1156, 2768, 2938, 2325, 3142, 1835, 3802, 2518, 1929, 3924, 129, 290, 3416, 3155, 384, 2025, 260, 481, 2215, 2615, 1169, 2118, 2809, 1799, 662, 49, 1574, 2740, 1889, 1469, 486, 2033, 3071, 2947, 3153, 447, 3680, 2083, 1368, 1922, 3470, 4038, 3333, 570, 1434, 2419, 3165, 1410, 1940, 3797, 719, 1018, 208, 1915, 1042, 3843, 3126, 1549, 28, 838, 2542, 1571, 1461, 2437, 3183, 2127, 1368, 3481, 2675, 3017, 784, 1236, 607, 1814, 1432, 1260, 1447, 243, 1299, 2674, 2963, 1964, 3128, 2654, 1918, 414, 2192, 1316, 2779, 1382, 603, 3120, 3047, 3552, 684, 3952, 3529, 2728, 3396, 1487, 1235, 1031, 1052, 737, 1751, 391, 387, 1199, 3494, 4076, 1917, 2689, 4002, 3558, 3911, 1251, 732, 2179, 4046, 923, 2859, 304, 1679, 4052, 2210, 1083, 823, 1073, 1268, 2379, 515, 3094, 2549, 3977, 2516, 727, 59, 254, 515, 1032, 2129, 442, 46, 2128, 1464, 1436, 2321, 2503, 3154, 1427, 3989, 3599, 419, 1451, 3698, 1990, 1211, 1592, 4060, 1230, 3455, 2158, 2042, 3005, 2558, 1870, 3873, 634, 959, 1459, 3211, 3696, 2745, 204, 1823, 143, 3035, 1291, 197, 3141, 870, 3897, 2646, 1192, 198, 1793, 2513, 1229, 1074, 2787, 1556, 3378, 1764, 2890, 2918, 307, 1354, 2858, 2135, 3005, 999, 1697, 1967, 1593, 1447, 2120, 196, 3476, 3546, 451, 2186, 522, 4035, 1461, 960, 1434, 1265, 3607, 894, 3728, 4024, 661, 779, 949, 3245, 127, 1711, 3198, 3647, 3956, 3539, 2291, 3912, 2367, 1814, 1242, 3492, 1019, 1728, 2612, 3275, 686, 861, 2958, 375, 1245, 456, 169, 3287, 4051, 2061, 3077, 59, 3766, 2135, 1326, 2157, 315, 2144, 206, 2130, 353, 737, 993, 2317, 1967, 2056, 2233, 1680, 3692, 1122, 143, 2189, 3770, 3611, 3830, 3525, 699, 325, 1105, 2421, 2424, 2220, 336, 3365, 1142, 3403, 3724, 3167, 1768, 3749, 2037, 1656, 1593, 3092, 1110, 4063, 1979, 3611, 3168, 1202, 3603, 1428, 1692, 2317, 1183, 720, 1261, 1066, 1278, 1500, 164, 572, 3938, 485, 608, 2809, 3056, 3691, 587, 545, 3130, 544, 2356, 2338, 2868, 1463, 3762, 961, 3859, 1246, 2877, 3266, 459, 3301, 1404, 2931, 1795, 1923, 3165, 2111, 1279, 2694, 3024, 3262, 1325, 359, 309, 493, 3423, 3024, 1944, 201, 2143, 1148, 3211, 1214, 2050, 2296, 2528, 3196, 381, 3692, 3111, 1750, 2335, 210, 3866, 3464, 3572, 1208, 343, 995, 3356, 2332, 3386, 3496, 719, 413, 829, 2336, 2039, 3429, 15, 2200, 3367, 1193, 2242, 1349, 4081, 1496, 303, 68, 914, 1660, 339, 237, 362, 114, 2906, 1944, 619, 114, 1569, 339, 1335, 1313, 2500, 1562, 53, 289, 2989, 3213, 349, 3842, 4038, 1559, 2784, 1782, 132, 978, 1033, 1517, 1856, 3568, 2219, 3832, 1736, 2246, 1889, 61, 935, 3737, 94, 1152, 3955, 102, 3055, 2461, 3214, 3367, 120, 443, 339, 1340, 3075, 3083, 14, 3777, 2849, 2920, 3910, 3806, 1235, 962, 1382, 583, 1790, 3514, 1665, 1053, 3668, 1731, 3017, 4080, 2408, 4063, 3679, 3427, 3974, 2974, 4030, 1972, 2852, 3141, 1957, 3392, 3042, 2206, 1971, 1895, 2446, 3484, 1780, 2565, 1647, 3897, 3358, 1552, 2692, 3366, 1783, 779, 3993, 2801, 1830, 1842, 159, 1476, 791, 577, 3753, 2868, 1202, 3192, 2732, 2615, 1674, 2390, 864, 3871, 3518, 239, 3226, 3770, 1776, 2262, 4030, 3389, 524, 1881, 1957, 218, 1467, 1964, 2036, 698, 1496, 1973, 331, 277, 1710, 3734, 2538, 3368, 993, 3779, 2398, 1624, 4077, 3902, 483, 216, 1269, 1035, 2919, 3598, 410, 2579, 881, 3449, 305, 3997, 3211, 155, 513, 2856, 1320, 3739, 1397, 972, 3823, 943, 2319, 1779, 2577, 1751, 3695, 2422, 442, 3962, 3172, 4016, 1870, 3561, 2187, 1113, 738, 3431, 2833, 647, 862, 1007, 3878, 2507, 753, 2614, 142, 975, 3328, 2381, 1271, 2409, 4037, 2793, 2416, 3405, 2785, 2964, 1196, 1858, 2382, 3832, 2559, 1753, 640, 1011, 2353, 3655, 1996, 597, 2759, 549, 3640, 24, 2684, 2921, 2133, 3947, 1985, 1491, 1458, 566, 2609, 3035, 2094, 3046, 2729, 1101, 2888, 1267, 1335, 3588, 1831, 2625, 2660, 1152, 2900, 150, 1883, 3004, 1041, 364, 4087, 1853, 3591, 895, 2699, 655, 1269, 2676, 3256, 3310, 1231, 152, 718, 1595, 1693, 1267, 3907, 542, 1778, 2942, 926, 649, 239, 1038, 3305, 2432, 2604, 3447, 361, 52, 50, 2427, 3439, 2133, 1366, 1102, 3173, 3802, 46, 2026, 2388, 1347, 2368, 2464, 3067, 470, 2229, 2482, 3069, 3334, 402, 1002, 1455, 81, 876, 1615, 3570, 3461, 1288, 2627, 1418, 2572, 1902, 1436, 2289, 1113, 2044, 2947, 2121, 997, 3988, 1517, 3068, 2093, 1466, 556, 2421, 65, 3928, 559, 3392, 497, 3686, 1521, 3170, 2437, 254, 1, 3999, 39, 3081, 3778, 2797, 56, 872, 1513, 2543, 422, 3694, 3591, 4072, 592, 430, 3289, 1181, 1506, 3524, 397, 3781, 2498, 1122, 1621, 3708, 3103, 3187, 2244, 263, 57, 3112, 2747, 3655, 2301, 392, 3181, 1488, 995, 3697, 267, 3022, 1187, 2860, 527, 1001, 3183, 344, 3308, 3231, 2819, 3814, 2072, 3179, 1930, 1474, 3615, 1125, 197, 398, 985, 1719, 1789, 1800, 3037, 444, 941, 1263, 1303, 847, 2624, 1939, 2791, 52, 150, 252, 3061, 1595, 3182, 894, 2475, 325, 1026, 1734, 1393, 923, 66, 1014, 799, 1744, 4074, 2922, 2893, 374, 1174, 703, 981, 280, 1898, 2090, 2391, 1387, 1234, 3782, 726, 1486, 1309, 1859, 1491, 3692, 2296, 2802, 1424, 801, 3808, 3740, 3148, 3694, 3575, 904, 3976, 577, 2608, 3194, 2099, 3756, 744, 990, 1557, 720, 4, 2083, 786, 1949, 1386, 957, 196, 257, 292, 3567, 246, 2136, 2738, 2891, 270, 3836, 2814, 2389, 2087, 552, 3385, 2647, 2172, 3676, 1609, 814, 74, 1411, 3658, 835, 461, 3302, 187, 281, 2595, 1103, 823, 1083, 1221, 864, 3097, 3650, 3402, 2358, 2878, 3177, 937, 4034, 3902, 2953, 1188, 3357, 1372, 1801, 841, 3625, 3084, 588, 2893, 909, 641, 2829, 1441, 1110, 3822, 2352, 3950, 3320, 3145, 2890, 2149, 440, 1219, 1502, 933, 3084, 2387, 2940, 2654, 2589, 2713, 121, 300, 3512, 1914, 1317, 1262, 2022, 374, 3349, 3518, 3362, 3852, 1719, 1146, 564, 685, 3928, 1236, 2847, 2210, 1687, 3664, 1334, 505, 1626, 1099, 2665, 1979, 1162, 2926, 3272, 3448, 2678, 451, 33, 3907, 3902, 3330, 19, 1079, 80, 3138, 2381, 1227, 382, 2541, 840, 1110, 387, 1853, 1490, 2951, 145, 2301, 357, 1048, 1107, 2675, 2383, 3666, 2155, 470, 4014, 2135, 3161, 3795, 904, 342, 3524, 350, 2284, 1697, 909, 359, 1731, 3523, 387, 2041, 264, 3914, 3948, 3925, 190, 2732, 632, 2005, 1122, 3617, 1365, 549, 771, 1474, 596, 1180, 3010, 2338, 3370, 1831, 3587, 852, 1817, 2651, 47, 3098, 2770, 1057, 1928, 2144, 2806, 3579, 2782, 2034, 2427, 1291, 500, 513, 3814, 234, 2210, 3775, 3152, 1666, 3742, 3296, 2356, 3549, 1893, 212, 1114, 522, 1061, 1979, 573, 175, 801, 4003, 1544, 1211, 2206, 3363, 2396, 2386, 3799, 3056, 3405, 2890, 3547, 691, 1703, 15, 892, 1694, 2493, 2372, 3872, 1738, 2957, 3809, 1812, 2487, 1638, 3579, 3402, 3956, 480, 2286, 1830, 762, 2570, 3974, 1499, 866, 365, 3997, 3380, 2120, 2081, 3312, 3330, 2933, 956, 1713, 325, 725, 571, 2945, 1194, 1794, 2092, 56, 440, 866, 3912, 1559, 2240, 2821, 468, 2306, 1458, 1251, 3289, 2973, 4019, 2606, 3150, 3373, 137, 3952, 87, 1901, 693, 3246, 984, 2855, 3197, 1823, 1718, 592, 2910, 2104, 1239, 2914, 1000, 1279, 2362, 1746, 46, 2141, 1779, 1897, 3671, 327, 3497, 2581, 3095, 610, 2989, 1649, 13, 272, 524, 638, 2994, 891, 2170, 2058, 1858, 1193, 298, 381, 912, 4065, 3097, 1395, 1422, 1379, 840, 4021, 409, 1774, 3787, 885, 3722, 3736, 847, 2960, 1665, 3189, 3073, 3322, 389, 2699, 1532, 3410, 1190, 2840, 3088, 2814, 232, 2116, 1106, 3326, 3378, 992, 3800, 2961, 1442, 612, 3669, 3813, 1154, 3506, 4023, 1363, 2725, 1540, 1223, 1874, 2312, 152, 2790, 1914, 3866, 1696, 783, 3166, 20, 594, 491, 2393, 1861, 380, 3461, 1897, 2702, 3319, 3629, 694, 1144, 3101, 1731, 72, 1920, 3360, 3147, 548, 2859, 1259, 2027, 3589, 824, 3687, 920, 1368, 1368, 3221, 2875, 3355, 2973, 2725, 2726, 416, 278, 3691, 628, 784, 882, 357, 537, 83, 567, 527, 3139, 3407, 3828, 2106, 3365, 1411, 1315, 1126, 3533, 965, 2954, 3461, 2616, 930, 1479, 3080, 3346, 1357, 2132, 1897, 733, 2692, 3759, 157, 3061, 778, 2368, 47, 1440, 99, 2966, 2118, 3697, 1071, 2161, 1584, 3877, 1490, 167, 3559, 3986, 846, 2799, 1113, 385, 1713, 1103, 2415, 2870, 2760, 3743, 1160, 1652, 3717, 1220, 2963, 3695, 1258, 2725, 3002, 2878, 2969, 3912, 2103, 1180, 3364, 1282, 265, 1101, 3724, 653, 1936, 120, 504, 3396, 1258, 1164, 2404, 1663, 2762, 2620, 725, 2761, 3974, 281, 3750, 1992, 800, 4086, 3131, 622, 3428, 2546, 1425, 2701, 2506, 387, 888, 2200, 3797, 2497, 729, 491, 3056, 2551, 2069, 3109, 619, 1270, 1065, 1156, 2858, 3689, 544, 25, 3607, 962, 2619, 3646, 2630, 3584, 1568, 4056, 2719, 1311, 240, 3135, 1418, 3882, 2283, 2350, 2282, 2653, 535, 2353, 1753, 426, 3670, 895, 790, 4012, 2015, 20, 1377, 1751, 3766, 2880, 2592, 2221, 3827, 3265, 1853, 2626, 386, 1152, 1253, 2248, 3022, 3100, 2583, 3594, 3005, 305, 763, 1900, 3586, 1420, 3764, 3409, 3350, 702, 3006, 1295, 2536, 3861, 316, 1799, 3607, 1549, 3062, 3889, 382, 1522, 1958, 0, 615, 3945, 176, 1396, 1460, 2466, 3103, 1084, 3432, 3974, 679, 633, 2394, 1315, 2340, 2468, 2313, 593, 826, 2208, 2174, 1703, 2257, 1362, 2386, 3259, 4030, 40, 1712, 822, 897, 1077, 3296, 789, 1724, 3157, 3960, 2695, 3184, 596, 170, 2896, 951, 2324, 1142, 3511, 222, 3879, 2165, 2550, 1405, 563, 2455, 908, 1008, 491, 2941, 1339, 628, 3987, 1138, 3106, 3479, 2494, 3413, 466, 325, 2894, 2666, 1447, 3637, 1078, 2397, 4012, 1901, 2254, 3845, 3168, 1806, 3936, 3964, 2905, 2034, 1266, 867, 94, 2559, 3350, 2450, 3544, 1399, 3779, 3385, 3681, 3252, 1804, 2338, 58, 2735, 2052, 2685, 3857, 1910, 3362, 3416, 3010, 3875, 4086, 3285, 3676, 3119, 307, 1639, 4021, 3447, 826, 2653, 116, 1095, 2644, 276, 1692, 1885, 943, 2435, 1701, 1486, 3120, 2963, 1477, 272, 241, 1139, 1922, 2680, 500, 2606, 641, 2280, 2780, 1346, 1928, 3610, 1290, 3529, 763, 2610, 3186, 29, 2069, 1395, 648, 1275, 3075, 1166, 93, 2176, 2766, 3542, 285, 3614, 2009, 622, 1979, 477, 1019, 690, 1647, 1456, 626, 2606, 1548, 1798, 2992, 1240, 3299, 3715, 2224, 2430, 3408, 3704, 2881, 2062, 29, 2853, 1880, 1687, 565, 3742, 830, 2352, 3809, 272, 597, 2836, 1757, 1397, 2483, 1248, 241, 1271, 624, 3966, 3449, 164, 3920, 1169, 1017, 2367, 2366, 2536, 1020, 2915, 528, 301, 150, 3564, 52, 233, 3212, 3901, 3786, 2932, 1771, 203, 3616, 888, 2200, 387, 1997, 1685, 1989, 1839, 339, 856, 3263, 2374, 14, 1971, 516, 2133, 413, 3668, 992, 3227, 1725, 3076, 1038, 3898, 698, 3497, 1126, 353, 2672, 2468, 1999, 736, 368, 3508, 2257, 2736, 2289, 3722, 724, 4088, 3674, 1934, 1088, 1325, 873, 1996, 2337, 3123, 2303, 758, 1637, 5, 469, 1346, 2475, 285, 3985, 3828, 2794, 1430, 3782, 2122, 3762, 1580, 283, 3385, 446, 3724, 276, 3079, 3808, 3784, 515, 3306, 212, 3570, 2072, 1007, 172, 3394, 1284, 2776, 2708, 2855, 2232, 293, 1509, 2229, 2948, 3565, 790, 3326, 2619, 2789, 3151, 3163, 1643, 36, 1857, 286, 1102, 197, 1916, 1159, 3315, 669, 1064, 2263, 3423, 2299, 1541, 3293, 3190, 266, 2356, 1944, 1156, 3258, 2284, 189, 2072, 1560, 1537, 1339, 2216, 2822, 2415, 1805, 2033, 105, 3243, 2946, 508, 3003, 1301, 2565, 3661, 653, 3446, 1783, 2043, 2992, 3607, 2598, 2285, 1018, 1928, 746, 3388, 2860, 1705, 2381, 2981, 905, 1906, 2769, 3388, 1628, 3926, 628, 2553, 3304, 2707, 3964, 1276, 1073, 505, 30, 1975, 1791, 2399, 3669, 3139, 3380, 2203, 2186, 3540, 3374, 3966, 3664, 2506, 51, 583, 3772, 2528, 455, 1397, 357, 2863, 3707, 2504, 1347, 813, 794, 2631, 1849, 3149, 2087, 3574, 873, 1171, 2315, 2090, 3975, 3216, 3135, 100, 1477, 796, 1117, 3934, 539, 3159, 3321, 1064, 3645, 3498, 412, 136, 977, 2169, 2919, 3979, 3813, 705, 1569, 754, 3366, 3588, 3319, 2905, 3931, 1534, 3562, 3918, 2910, 2692, 2972, 2276, 1511, 2207, 3469, 1837, 112, 1834, 134, 625, 3111, 2019, 1008, 3074, 485, 3824, 1188, 3153, 2365, 2095, 733, 752, 2802, 856, 3186, 1570, 984, 1777, 3888, 915, 3516, 2412, 1974, 3951, 1997, 3094, 102, 1089, 467, 2482, 1276, 2214, 2904, 632, 2143, 2399, 3148, 1821, 615, 3664, 736, 1750, 3711, 3234, 499, 3186, 1739, 3116, 2954, 1521, 3313, 3733, 3303, 811, 891, 60, 45, 2538, 3811, 1167, 2870, 1901, 3917, 1563, 3600, 2767, 1159, 1730, 1057, 3642, 3816, 1410, 2454, 2882, 2862, 3504, 3133, 4002, 1175, 1647, 1821, 63, 2213, 1510, 3589, 238, 855, 1066, 3881, 525, 1648, 3900, 3292, 3236, 2790, 3051, 1893, 2508, 1939, 3119, 696, 3476, 2837, 369, 3864, 3930, 1517, 1936, 772, 1087, 1474, 3235, 2748, 1031, 3331, 3918, 2251, 2372, 1070, 459, 1443, 39, 2181, 1996, 404, 3776, 188, 3824, 153, 2603, 2290, 3055, 940, 3347, 3557, 784, 2874, 2883, 1069, 873, 3336, 1748, 1808, 1157, 3573, 1609, 3642, 3321, 2752, 1502, 885, 1902, 2952, 2652, 3771, 890, 2693, 3611, 4063, 768, 3067, 1289, 953, 2444, 2080, 2507, 1314, 1416, 3973, 784, 2381, 3434, 299, 2503, 3264, 241, 3197, 1563, 3546, 1175, 605, 1909, 3716, 2100, 751, 2922, 476, 3525, 400, 1373, 3196, 3438, 308, 2275, 3055, 1421, 2374, 3012, 1502, 3544, 606, 375, 967, 1702, 4077, 1595, 431, 2694, 1628, 2753, 1461, 1864, 1297, 1993, 3735, 419, 259, 2181, 3119, 2769, 164, 2909, 1560, 924, 191, 2291, 3490, 344, 2622, 3333, 3353, 2625, 1252, 3357, 1031, 3355, 964, 3235, 2084, 2270, 2328, 3298, 3094, 2726, 427, 726, 1507, 3276, 50, 2672, 874, 3340, 182, 2114, 3779, 445, 2129, 3458, 1631, 3404, 3507, 2435, 2036, 2468, 1707, 3182, 2163, 234, 1273, 3850, 1735, 804, 1003, 1622, 2210, 1421, 415, 2389, 3226, 1590, 608, 3048, 953, 3174, 3727, 1385, 1924, 207, 2907, 80, 3064, 2727, 3049, 2231, 393, 2721, 1458, 1754, 3241, 3474, 1290, 3259, 1867, 3359, 2181, 2580, 3595, 2802, 2805, 294, 2798, 4054, 1287, 2364, 3580, 2439, 569, 600, 2116, 2838, 3621, 2271, 2744, 1514, 3364, 3236, 1679, 712, 1079, 467, 2244, 3143, 2074, 148, 1316, 3086, 3587, 1111, 1267, 2293, 1749, 3630, 1483, 3266, 2349, 2302, 2742, 2983, 78, 1133, 259, 2682, 1023, 2949, 2313, 506, 876, 2205, 3406, 3057, 1390, 1697, 3576, 1411, 1247, 1166, 2886, 1968, 2191, 845, 3670, 3093, 2661, 4050, 1283, 693, 2353, 712, 1157, 1855, 1339, 3373, 1266, 3872, 1760, 4035, 2799, 2047, 2641, 1683, 4035, 1402, 2809, 4001, 510, 2621, 604, 2139, 3714, 2782, 2906, 1002, 2005, 1215, 670, 3754, 689, 450, 2798, 3814, 1191, 1410, 571, 760, 3675, 2778, 3256, 3848, 3710, 3661, 3916, 1597, 539, 4056, 41, 2568, 229, 1395, 1465, 1338, 2840, 609, 174, 2053, 679, 2536, 3098, 2330, 2916, 2713, 707, 3227, 1164, 412, 1317, 3207, 1991, 3536, 1247, 3032, 1823, 3698, 3912, 1922, 458, 756, 3387, 447, 2427, 3568, 65, 810, 2809, 2004, 803, 1677, 1921, 1944, 1081, 311, 2865, 3787, 3312, 1238, 3432, 3390, 550, 1090, 2715, 862, 1411, 3426, 3359, 4027, 152, 2716, 1961, 1673, 336, 1443, 2608, 2776, 3596, 1714, 3300, 1646, 1274, 994, 480, 2957, 3525, 2931, 2765, 3430, 802, 2844, 577, 3275, 3510, 878, 2961, 3145, 934, 419, 801, 1151, 2713, 2467, 3679, 1650, 2361, 404, 1309, 3084, 2084, 934, 3437, 252, 304, 534, 4091, 1845, 3996, 2981, 1548, 1411, 2137, 3605, 415, 131, 1302, 3314, 3127, 2471, 3274, 1210, 857, 1095, 2891, 283, 2241, 1535, 89, 1532, 749, 1521, 1670, 2673, 117, 2196, 3419, 1604, 2234, 3348, 2592, 3763, 2979, 3211, 2026, 2063, 361, 1569, 3189, 3192, 112, 3844, 3779, 1849, 2545, 311, 3808, 2172, 3757, 2978, 1829, 1400, 1267, 1419, 3807, 1238, 371, 2782, 179, 3631, 2455, 551, 456, 659, 644, 1299, 269, 3828, 1482, 1204, 1856, 3598, 165, 2338, 664, 2067, 1257, 2888, 2383, 2971, 2652, 1987, 1473, 2223, 3825, 1305, 679, 2063, 1750, 1563, 1228, 1869, 320, 2125, 1241, 336, 783, 75, 1018, 1413, 3484, 3025, 764, 364, 954, 3116, 3422, 410, 1297, 306, 2247, 1982, 2500, 63, 137, 2543, 643, 1649, 1410, 2818, 1665, 1842, 1910, 1026, 2526, 2251, 3058, 541, 619, 1430, 3356, 566, 292, 720, 1711, 1630, 3512, 1070, 3158, 1777, 640, 1340, 3374, 3085, 1511, 1841, 142, 2042, 443, 2440, 3948, 3013, 3073, 3336, 433, 2162, 1532, 3932, 3402, 2630, 881, 38, 3987, 416, 550, 183, 337, 1592, 1437, 765, 429, 2262, 19, 1372, 1953, 1064, 1416, 3830, 3088, 1300, 3262, 1384, 3175, 3661, 3328, 482, 2547, 3504, 1364, 2978, 1054, 4041, 1995, 1424, 3553, 414, 205, 2801, 2549, 3597, 3001, 3981, 851, 1333, 1955, 2380, 1031, 4067, 1669, 2858, 2752, 1923, 3319, 2558, 292, 3539, 2311, 4012, 478, 820, 1990, 1755, 3947, 3987, 627, 3541, 51, 2078, 3681, 594, 935, 2242, 1397, 960, 1454, 2024, 2062, 3035, 3932, 1978, 246, 1919, 1472, 1783, 274, 2342, 3695, 3296, 1910, 2819, 4052, 2786, 1850, 366, 2166, 2318, 2028, 3502, 2286, 890, 3646, 2660, 112, 903, 1743, 893, 2871, 162, 3132, 4057, 3805, 1677, 3351, 3239, 3312, 2886, 101, 3392, 2417, 508, 3991, 3298, 171, 2256, 1086, 1625, 765, 3143, 1374, 2119, 1712, 1084, 1754, 3838, 2524, 1935, 1189, 1963, 2531, 340, 3196, 1583, 3576, 64, 3328, 410, 825, 86, 2742, 1135, 95, 1494, 1738, 1212, 1994, 2594, 3527, 949, 2121, 512, 108, 4032, 545, 3908, 3907, 3946, 1301, 2403, 275, 447, 460, 1052, 3667, 2953, 737, 2046, 3829, 3652, 499, 821, 3939, 1318, 3821, 299, 2558, 2712, 1412, 2870, 2787, 1982, 2998, 1054, 1194, 4005, 976, 1047, 3914, 3347, 1890, 2229, 2740, 316, 3039, 2343, 4057, 345, 3124, 665, 747, 851, 1633, 2101, 1491, 2606, 3033, 210, 104, 1164, 770, 285, 2040, 3038, 3485, 2534, 2551, 2069, 3475, 3684, 1457, 1065, 3060, 3130, 877, 4037, 3772, 3848, 1637, 3272, 3395, 3288, 3779, 1480, 150, 1890, 1774, 1861, 2575, 1863, 3587, 3521, 2906, 1875, 719, 3588, 177, 2526, 2607, 1496, 1048, 3344, 3040, 1523, 1774, 247, 2264, 1869, 3575, 2160, 1817, 3484, 2469, 3333, 549, 1621, 559, 3290, 2720, 3320, 624, 1252, 999, 2181, 3549, 3147, 2117, 2981, 1631, 412, 1963, 468, 3923, 1742, 2455, 3132, 738, 515, 3990, 1063, 3607, 1108, 3165, 3159, 2298, 1814, 1742, 3534, 797, 3114, 3228, 1899, 3470, 3049, 210, 2891, 456, 360, 318, 4061, 124, 2349, 3885, 2911, 3909, 1556, 1048, 1575, 3136, 2236, 209, 868, 983, 2338, 3447, 1465, 1794, 2386, 1438, 192, 2956, 1829, 691, 1881, 2506, 915, 616, 1154, 968, 1480, 2597, 752, 3630, 1703, 2728, 3655, 1312, 2676, 17, 3612, 399, 3333, 1925, 1136, 843, 4071, 3530, 3162, 697, 2431, 1886, 2844, 3059, 2411, 2620, 1105, 2085, 1191, 302, 3590, 3684, 399, 985, 799, 3419, 3492, 1811, 3948, 3003, 1716, 1983, 2746, 1501, 1704, 1577, 2179, 1495, 1246, 2575, 2565, 794, 2758, 2418, 1904, 3624, 3472, 1564, 2807, 2194, 415, 2158, 107, 196, 317, 1539, 3518, 30, 2482, 3519, 3264, 435, 577, 579, 3061, 3336, 3274, 3834, 1459, 2272, 3788, 1480, 650, 2672, 3007, 3295, 3305, 3097, 3760, 3845, 1926, 2891, 3479, 406, 1156, 2976, 1169, 1398, 29, 1045, 447, 949, 690, 2285, 427, 3152, 1108, 1255, 81, 1684, 2775, 2335, 4001, 4095, 2220, 2634, 3606, 3190, 2142, 3469, 4016, 3339, 3319, 3601, 433, 568, 2114, 3199, 2783, 341, 973, 1220, 809, 159, 1276, 2622, 2360, 1622, 1068, 547, 2496, 605, 2122, 1233, 342, 2986, 1045, 3579, 2605, 889, 1384, 2076, 1694, 232, 1583, 71, 1266, 3246, 657, 1065, 451, 3014, 2886, 2519, 2247, 764, 802, 1162, 3706, 2915, 3425, 1703, 2389, 671, 1168, 2534, 75, 3272, 1498, 660, 3040, 3581, 0, 1986, 2995, 2437, 893, 1699, 2620, 2261, 2862, 2942, 3426, 636, 1151, 851, 3767, 401, 3742, 2492, 1890, 230, 98, 2590, 2630, 3407, 1635, 1123, 2350, 963, 2112, 3690, 3331, 3335, 1299, 2186, 104, 295, 3999, 514, 2520, 2349, 3301, 319, 1735, 3623, 3675, 3599, 3750, 3836, 39, 709, 3364, 2379, 503, 1459, 3406, 344, 3263, 2014, 2875, 3963, 2874, 3090, 2539, 1587, 702, 3103, 922, 693, 1338, 2404, 2891, 1221, 1966, 2732, 607, 2272, 3733, 2842, 1363, 860, 2107, 1548, 3954, 763, 2194, 981, 2590, 1236, 2412, 268, 804, 2164, 2162, 717, 3770, 328, 3936, 1206, 1471, 3472, 3710, 1017, 681, 3775, 2218, 3001, 880, 3455, 1235, 3132, 2867, 3884, 2499, 1348, 2945, 3201, 2179, 835, 219, 1288, 331, 1212, 350, 475, 1556, 1211, 1569, 3457, 2861, 3437, 3719, 1861, 2854, 164, 3322, 3475, 1852, 4013, 184, 3306, 2216, 251, 683, 638, 1293, 2870, 3686, 2679, 2838, 1641, 942, 2886, 798, 3870, 1482, 3763, 3491, 2502, 486, 3487, 1515, 2392, 345, 1461, 992, 464, 2894, 1213, 3331, 1699, 2224, 3072, 3694, 3574, 2486, 797, 1681, 2048, 2598, 1957, 3644, 490, 3576, 1384, 2004, 2616, 3767, 518, 3127, 495, 2299, 2223, 3346, 3286, 99, 575, 275, 2545, 898, 1655, 879, 3094, 3333, 3088, 2306, 1068, 671, 3892, 2806, 3156, 1553, 3736, 2858, 2819, 577, 1664, 1092, 3483, 209, 2874, 1856, 3694, 3121, 703, 3315, 2473, 197, 143, 1599, 2539, 2273, 3454, 2776, 3943, 2195, 447, 4092, 1508, 1545, 1021, 3775, 2999, 1065, 2832, 3560, 1144, 1354, 2636, 1074, 3869, 273, 3087, 2605, 3445, 1659, 2898, 1435, 1853, 2610, 1286, 69, 2687, 624, 2271, 3763, 241, 2390, 2481, 1020, 3890, 1154, 416, 2548, 275, 1860, 1488, 844, 840, 4094, 1882, 1400, 1129, 1523, 3139, 1932, 686, 2609, 3267, 1673, 1294, 1976, 546, 1750, 3271, 2610, 3172, 2627, 2726, 49, 553, 1497, 2073, 3428, 80, 962, 541, 1830, 3127, 1899, 419, 3160, 2766, 358, 3021, 2563, 2984, 3860, 2091, 118, 3346, 3349, 2429, 752, 1955, 402, 1663, 1450, 1031, 1184, 2847, 1668, 3539, 2277, 1709, 1846, 3513, 300, 2514, 2425, 136, 2396, 1031, 3019, 1229, 242, 3367, 3559, 3427, 1753, 3751, 4001, 3624, 2033, 3605, 2445, 2729, 1983, 2619, 384, 1390, 3187, 4057, 511, 521, 1867, 3668, 1059, 429, 2462, 3591, 277, 3334, 2350, 995, 3909, 1764, 1681, 1739, 3728, 2494, 533, 2813, 2913, 857, 3111, 3999, 189, 614, 2814, 1447, 133, 3600, 2727, 415, 3985, 3381, 1216, 744, 3692, 410, 106, 185, 103, 1707, 1705, 798, 2250, 2734, 480, 885, 2214, 299, 2698, 2844, 476, 3046, 2579, 3361, 3764, 3481, 236, 1484, 181, 3982, 941, 1518, 2602, 1491, 1841, 3971, 2285, 2821, 3021, 2005, 2651, 500, 339, 1601, 1338, 3952, 637, 1482, 2456, 2126, 1757, 833, 2311, 3431, 1260, 3707, 3146, 592, 479, 3156, 691, 4059, 1155, 1453, 1661, 1896, 1554, 159, 2780, 3414, 344, 3773, 2348, 3682, 3661, 3518, 766, 1529, 1539, 889, 2024, 1732, 69, 1153, 3384, 1454, 1692, 2608, 1074, 554, 3503, 1656, 319, 459, 3787, 818, 898, 3611, 1958, 3602, 1491, 473, 1710, 1258, 248, 3240, 3315, 672, 3144, 2950, 1824, 2641, 513, 146, 901, 1498, 3589, 3680, 2079, 829, 1766, 3700, 1773, 2085, 1293, 2751, 67, 2422, 3837, 2059, 2487, 3256, 2105, 3692, 3032, 4021, 2557, 1835, 1233, 3204, 856, 2313, 2065, 3432, 2053, 1188, 1006, 258, 1180, 2963, 2945, 685, 3998, 1221, 1124, 401, 1289, 3294, 3242, 2329, 1275, 3904, 3764, 186, 2249, 1938, 1, 1564, 295, 3336, 519, 2899, 381, 1692, 3356, 2822, 1962, 2282, 3153, 75, 2073, 3681, 933, 2756, 3036, 3988, 2965, 2699, 1618, 914, 2763, 2245, 2260, 1252, 615, 2277, 861, 1783, 3218, 4076, 2206, 3180, 2668, 73, 3794, 1797, 3449, 286, 3998, 352, 3948, 2770, 3492, 1024, 1551, 578, 3687, 1581, 2042, 1255, 2900, 2744, 2436, 2558, 1008, 3539, 637, 3507, 1087, 972, 163, 52, 3912, 2183, 1779, 3570, 1358, 180, 605, 893, 3862, 1429, 1903, 942, 664, 1943, 2686, 3152, 2183, 2475, 3684, 884, 3294, 2370, 976, 1762, 2570, 1992, 3801, 3223, 959, 628, 2185, 2188, 3524, 2792, 1329, 1988, 604, 3675, 2384, 2193, 3032, 1832, 3657, 3748, 221, 1239, 2396, 1441, 3107, 3319, 4040, 3374, 3972, 91, 3572, 1107, 112, 2605, 2915, 3079, 3785, 3499, 3334, 2974, 363, 736, 3303, 187, 1264, 1632, 2033, 2685, 3329, 1393, 438, 1846, 1130, 1729, 4063, 550, 3523, 2805, 892, 1671, 3616, 1704, 2104, 3136, 2765, 277, 1175, 1510, 1184, 511, 3614, 1287, 924, 1159, 2434, 821, 3583, 371, 4090, 776, 1817, 878, 4005, 1931, 3280, 3019, 3990, 2607, 501, 2503, 1867, 3824, 1790, 2369, 2727, 3652, 3745, 1863, 3105, 3146, 2373, 439, 3465, 1424, 319, 1459, 413, 380, 947, 2044, 745, 3534, 1908, 428, 3894, 2104, 3704, 3408, 3861, 1082, 4087, 1702, 199, 879, 1070, 342, 744, 2372, 2351, 3727, 1456, 2514, 1824, 2250, 2351, 2008, 2753, 3434, 339, 1188, 374, 3939, 1362, 3538, 2745, 3782, 4026, 343, 1323, 792, 418, 1401, 1399, 1983, 2588, 3081, 1470, 1197, 933, 199, 2569, 2166, 2734, 3900, 236, 4061, 3666, 2307, 1172, 3058, 1772, 53, 811, 3859, 2768, 1940, 2584, 604, 2292, 1699, 2673, 3504, 3055, 2804, 927, 3432, 1630, 314, 3730, 791, 3332, 1327, 1612, 1968, 3679, 2636, 2829, 367, 2614, 3821, 1993, 2782, 3515, 3264, 2766, 3519, 3394, 3023, 1112, 970, 3721, 1984, 1510, 145, 581, 295, 144, 3675, 2820, 277, 806, 655, 2736, 637, 695, 1140, 2657, 558, 875, 1715, 1660, 4079, 1582, 916, 680, 2374, 2288, 2776, 2670, 22, 3042, 1901, 2255, 431, 145, 3436, 3466, 1830, 310, 2078, 2234, 179, 3811, 10, 428, 2036, 2938, 1676, 2197, 3089, 3737, 3255, 1444, 2678, 1680, 1401, 2604, 2499, 2254, 1197, 4034, 2691, 4031, 1008, 1264, 3700, 1643, 995, 2547, 147, 2124, 3449, 479, 1565, 867, 2444, 4011, 2097, 2166, 3182, 460, 2594, 3086, 3243, 1938, 1604, 2449, 2999, 906, 1399, 3618, 3778, 3896, 1115, 372, 1900, 3417, 2436, 2341, 1781, 2788, 2374, 1919, 2986, 437, 4023, 150, 598, 3677, 3103, 1075, 3741, 1820, 2060, 2760, 709, 802, 2680, 1119, 3727, 3394, 2539, 3836, 3045, 2802, 244, 3692, 764, 496, 3563, 2155, 3942, 1710, 255, 3398, 2916, 2295, 547, 2409, 622, 186, 2031, 3645, 2826, 3212, 1780, 3092, 2103, 2698, 2560, 3001, 1589, 3600, 15, 870, 3853, 2184, 3663, 1867, 2712, 3526, 3182, 4033, 3006, 3020, 1729, 2797, 2736, 1557, 1449, 102, 517, 2669, 2205, 297, 3151, 1472, 2677, 2633, 562, 3173, 354, 3784, 3916, 1687, 755, 3441, 1955, 4014, 3131, 1832, 2153, 2965, 312, 3577, 762, 1600, 2056, 265, 3423, 1355, 758, 1518, 3130, 3258, 624, 390, 3303, 2921, 359, 3877, 467, 979, 1121, 1589, 347, 2076, 3228, 1210, 1267, 3806, 47, 3168, 2277, 1103, 651, 4087, 3018, 2321, 1483, 3257, 2880, 984, 1072, 3939, 3204, 2268, 2075, 2872, 1583, 1719, 2682, 1975, 2790, 2972, 1651, 1319, 2914, 3560, 2538, 2818, 1252, 1712, 308, 2179, 842, 221, 2146, 496, 795, 2936, 715, 408, 2582, 3635, 2480, 2295, 3355, 2629, 2699, 206, 1521, 3584, 2820, 3222, 2541, 1702, 2266, 3950, 3751, 1298, 1106, 2187, 2202, 3118, 3053, 3303, 3319, 3400, 164, 1530, 1726, 3379, 1684, 2538, 1066, 458, 2123, 2180, 2054, 790, 2639, 2353, 3908, 3097, 3644, 1002, 2116, 2338, 774, 4095, 3524, 2006, 2752, 2036, 3183, 2794, 2073, 4027, 3923, 541, 3209, 1818, 710, 3430, 3439, 785, 3087, 666, 1737, 3173, 3193, 749, 1408, 4076, 24, 3068, 888, 1438, 22, 2340, 2863, 2910, 2611, 3736, 1081, 2084, 1641, 2790, 1235, 3489, 4060, 1485, 1013, 254, 2576, 3872, 2527, 1784, 737, 2116, 186, 1378, 2282, 3652, 2829, 3907, 3537, 696, 2574, 2257, 1261, 217, 200, 737, 2764, 1285, 2482, 1530, 2878, 3897, 1305, 4074, 849, 1802, 3529, 3939, 2179, 1423, 2004, 1795, 256, 543, 209, 2038, 1115, 3887, 525, 183, 3260, 1211, 1575, 642, 2627, 1991, 3111, 1193, 3403, 1706, 3835, 3671, 3784, 0, 713, 2619, 3963, 2934, 1795, 1535, 945, 3160, 1182, 3191, 2511, 2946, 1952, 1198, 470, 1585, 1844, 2794, 2907, 638, 2590, 606, 2920, 329, 1781, 2350, 3057, 4006, 4005, 3921, 3271, 3561, 1718, 98, 993, 1719, 409, 2694, 974, 1791, 3438, 3681, 44, 2648, 4066, 2975, 2086, 714, 1143, 482, 619, 989, 508, 2959, 295, 1668, 2088, 2017, 1153, 3014, 315, 298, 3302, 1550, 1810, 2364, 4006, 1456, 2575, 1665, 2632, 1738, 3169, 3851, 3524, 1374, 2545, 1611, 3342, 3136, 78, 2614, 3381, 827, 1159, 2913, 999, 1959, 1933, 2505, 416, 3108, 2245, 2492, 541, 3765, 770, 2983, 3062, 1327, 562, 1636, 752, 1196, 3490, 2544, 1857, 925, 621, 61, 2823, 1267, 3050, 3208, 1027, 3637, 3961, 2429, 3575, 3882, 982, 820, 1416, 1955, 3306, 2292, 569, 8, 3184, 790, 970, 2183, 739, 1765, 4023, 2727, 149, 3720, 3935, 3027, 3112, 1382, 2436, 4069, 2554, 3058, 2190, 3491, 2099, 352, 3416, 929, 2539, 142, 3204, 124, 230, 1478, 331, 1385, 3675, 1264, 3089, 3628, 854, 3328, 2762, 3711, 3701, 3197, 3747, 675, 3036, 2665, 2215, 3134, 44, 2849, 697, 420, 394, 3837, 3207, 2391, 701, 977, 3194, 670, 2684, 258, 1186, 1010, 4, 1014, 788, 2914, 43, 3261, 1049, 434, 2561, 143, 2318, 387, 1507, 1817, 1726, 3989, 1087, 1436, 1040, 1443, 2049, 2131, 2787, 3792, 3831, 771, 3749, 463, 1344, 1140, 3444, 2856, 553, 2134, 3051, 1885, 1270, 2653, 1342, 987, 4049, 1626, 2115, 3605, 1872, 3189, 1716, 1970, 930, 1331, 3808, 3503, 1221, 3912, 2374, 1336, 4038, 2379, 1868, 853, 1078, 283, 2790, 2035, 3351, 3639, 2988, 2886, 2032, 3606, 3665, 2461, 2093, 2788, 3053, 1423, 3110, 3048, 1223, 2416, 3863, 1039, 463, 3378, 2256, 1249, 2143, 2550, 96, 2387, 3783, 2609, 3504, 1030, 2134, 3255, 2219, 1012, 3488, 2299, 2072, 293, 661, 2853, 1173, 1087, 3978, 1351, 833, 863, 2937, 2287, 1694, 185, 3881, 606, 2832, 1946, 3538, 3759, 1512, 3655, 3015, 1728, 3877, 3042, 3514, 2435, 2360, 1868, 1602, 4026, 1377, 3136, 2672, 826, 1663, 2369, 3037, 782, 1272, 1970, 1743, 1619, 1786, 303, 3161, 2919, 871, 3113, 1774, 97, 508, 1328, 1541, 1604, 3483, 2654, 2018, 3470, 1493, 2528, 3542, 1327, 1098, 3934, 1703, 3496, 3119, 2627, 2762, 2429, 2636, 1138, 1513, 2034, 2812, 2459, 2210, 425, 574, 299, 1087, 2215, 3450, 1190, 1789, 135, 2081, 455, 4056, 3251, 1607, 3775, 1454, 1705, 2814, 3265, 217, 3, 1060, 2216, 2395, 1656, 1595, 1602, 460, 2149, 1953, 736, 843, 1547, 1026, 2373, 4000, 2650, 2907, 2725, 3847, 757, 1515, 452, 2496, 2667, 1727, 3255, 1712, 2966, 1221, 1458, 962, 2980, 1890, 1400, 312, 4095, 1576, 2099, 1044, 295, 2921, 1590, 1757, 636, 1683, 267, 3450, 2036, 1225, 3113, 3419, 1455, 2337, 3679, 3922, 3844, 1468, 2915, 3806, 3805, 2116, 912, 2737, 2270, 4037, 483, 280, 713, 3527, 2710, 2762, 863, 2178, 1919, 1628, 105, 1471, 1442, 704, 1851, 2746, 1752, 41, 1751, 3273, 1008, 2463, 1172, 3230, 3659, 2804, 3292, 1053, 3000, 580, 3934, 3884, 542, 2292, 2564, 3110, 2015, 2621, 1214, 1036, 2515, 2173, 2630, 3267, 3546, 3514, 3271, 4073, 526, 1750, 3592, 1754, 2380, 1153, 3405, 95, 4011, 2020, 1663, 1177, 3719, 142, 2535, 3208, 2123, 3406, 2021, 914, 1033, 308, 3794, 3646, 3963, 225, 934, 2504, 1399, 2804, 2828, 3295, 2363, 953, 1096, 3471, 1806, 3419, 3065, 603, 901, 1916, 2382, 3357, 1715, 3789, 2622, 1071, 1306, 2688, 1690, 1829, 1116, 3144, 1259, 3254, 1879, 301, 3859, 1698, 2685, 1870, 2760, 3613, 3613, 1444, 2076, 440, 2023, 1788, 1262, 121, 3225, 3428, 386, 2323, 3429, 3075, 348, 2060, 1634, 3049, 3909, 4027, 4072, 2468, 3288, 3402, 558, 3198, 2195, 986, 2900, 1651, 56, 2525, 3271, 1544, 2197, 2790, 2231, 3607, 802, 1622, 1843, 832, 2514, 293, 2511, 3922, 1997, 1719, 2622, 216, 3815, 846, 3004, 3495, 3834, 3071, 1630, 1906, 2013, 3376, 3137, 3921, 395, 2494, 2337, 2035, 1569, 3417, 724, 744, 3222, 599, 524, 546, 1677, 1402, 3143, 3267, 3495, 649, 43, 508, 3781, 1924, 2254, 3269, 1505, 3827, 2626, 578, 156, 3797, 3243, 205, 2391, 3235, 97, 635, 3955, 3189, 3563, 1390, 4022, 2086, 939, 2092, 3420, 3621, 3007, 308, 594, 3888, 1304, 769, 3055, 29, 1146, 1844, 2233, 1036, 1808, 2100, 1226, 4047, 2882, 2358, 869, 3386, 2598, 1496, 2323, 1391, 3305, 2398, 3654, 2859, 2828, 2654, 1467, 1324, 3824, 2654, 299, 3196, 1392, 719, 3437, 848, 2559, 1530, 1751, 1564, 410, 3306, 8, 1606, 3187, 884, 1121, 952, 20, 2500, 2358, 1511, 2792, 2168, 966, 502, 236, 1198, 2306, 3120, 2648, 1468, 1083, 1570, 3328, 2202, 3503, 2301, 1689, 2654, 197, 411, 2853, 2916, 1337, 465, 1213, 3816, 886, 2596, 3278, 1651, 884, 247, 2111, 2039, 609, 2938, 3790, 343, 337, 3466, 1648, 3401, 1773, 2388, 902, 1781, 494, 785, 4025, 1529, 1182, 475, 2797, 3916, 3056, 2473, 2564, 2958, 1422, 1174, 3304, 2114, 1531, 4044, 1054, 4022, 3762, 1530, 3461, 1685, 3163, 2387, 4018, 2193, 2360, 199, 1631, 3724, 3020, 3828, 954, 1032, 3860, 379, 3645, 2786, 2463, 1753, 1231, 697, 709, 2101, 221, 2409, 2110, 3626, 365, 1182, 1877, 1990, 2806, 832, 3181, 3166, 2717, 2210, 284, 1919, 2192, 4027, 2551, 2268, 315, 827, 2144, 2670, 2176, 3269, 841, 2454, 3929, 3702, 2891, 3569, 3309, 3139, 1189, 1919, 764, 1562, 2308, 3698, 3020, 1080, 3876, 2458, 2006, 899, 817, 3899, 2209, 3701, 1574, 2438, 4062, 289, 18, 2570, 148, 693, 1188, 2208, 1307, 3902, 1977, 1959, 1951, 2275, 938, 1181, 1775, 3887, 1474, 574, 441, 1222, 2310, 1269, 2605, 2453, 1773, 2181, 2804, 797, 1736, 2577, 3008, 914, 2685, 1076, 2700, 1823, 902, 2305, 3470, 3175, 108, 3412, 3616, 2420, 2199, 4092, 3275, 990, 1505, 701, 1275, 3268, 3371, 1164, 3754, 1008, 3885, 1452, 4094, 1314, 1761, 3596, 3601, 2205, 170, 2887, 194, 3883, 867, 2147, 2953, 2522, 2235, 3853, 823, 2410, 1579, 3009, 303, 1500, 390, 3623, 2604, 3236, 832, 1028, 1449, 1260, 60, 2648, 3362, 2471, 2512, 2338, 3937, 1378, 4040, 1570, 1140, 1991, 1641, 314, 2334, 3266, 1115, 265, 4085, 2371, 466, 913, 1758, 90, 1429, 1824, 3339, 3737, 1012, 3001, 606, 1595, 5, 868, 4082, 1739, 4054, 2172, 4040, 2558, 3748, 1492, 3489, 2253, 2202, 236, 1811, 1015, 3208, 727, 3298, 2713, 2584, 2319, 2709, 1022, 866, 1202, 3729, 203, 3264, 3536, 2061, 2214, 975, 325, 1359, 537, 1498, 1970, 1548, 2966, 1270, 2023, 3979, 1819, 3915, 3782, 843, 1629, 2431, 2982, 1159, 1617, 816, 3405, 839, 1052, 3455, 2368, 2166, 3632, 3412, 2858, 3780, 2970, 2778, 1723, 3055, 2784, 623, 1468, 388, 2945, 2901, 2721, 1354, 3972, 847, 1866, 1900, 3012, 2485, 445, 680, 2757, 2857, 3187, 1509, 1167, 2758, 1059, 3738, 315, 2696, 2117, 1473, 375, 3579, 1358, 2155, 791, 4090, 3306, 482, 3610, 2470, 3214, 3567, 2932, 202, 918, 3412, 1539, 3501, 729, 3910, 1347, 2876, 495, 1961, 3256, 2112, 1717, 659, 719, 1956, 2524, 2048, 543, 2059, 1278, 3614, 1111, 3878, 90, 559, 1257, 3773, 1188, 3453, 1227, 3019, 558, 3273, 1221, 2156, 1870, 2230, 3385, 3098, 3831, 3592, 489, 3289, 2113, 2077, 2084, 1284, 3142, 1022, 3146, 3423, 2425, 754, 321, 3610, 2748, 3256, 1006, 3894, 1860, 287, 856, 586, 377, 1519, 3109, 361, 1713, 3696, 3353, 1786, 3454, 956, 197, 3815, 2258, 1568, 280, 3408, 789, 3145, 3822, 2846, 3644, 420, 2041, 1343, 735, 862, 2854, 2206, 377, 519, 3401, 541, 3349, 3996, 874, 2971, 2296, 3015, 3710, 1166, 836, 40, 3630, 1289, 3895, 3266, 3604, 2340, 3944, 3212, 330, 2071, 3784, 785, 2113, 1780, 330, 1932, 2694, 1422, 1470, 3173, 1, 2750, 2793, 1785, 3474, 1665, 682, 1417, 2598, 3681, 3413, 1963, 2036, 1549, 964, 1151, 2297, 1018, 2949, 3807, 421, 3968, 1069, 2339, 180, 3759, 2259, 1033, 219, 3550, 1999, 518, 1629, 3237, 3916, 3005, 331, 111, 3030, 2274, 3982, 2529, 3213, 883, 3290, 3519, 3686, 1196, 4078, 1046, 2002, 89, 3829, 1404, 2506, 2793, 1432, 2688, 937, 3951, 621, 96, 382, 2235, 2449, 3599, 40, 3068, 3035, 449, 2236, 895, 1216, 3463, 393, 1119, 774, 1557, 2652, 1490, 8, 156, 1948, 2229, 2627, 4016, 725, 797, 1766, 3244, 2019, 3, 3259, 1344, 3954, 3155, 1117, 1023, 3632, 2442, 1771, 402, 4004, 2020, 2584, 3263, 3278, 558, 1866, 1237, 850, 1142, 1424, 944, 803, 3838, 965, 3065, 1544, 1081, 4036, 1629, 57, 1596, 703, 682, 1328, 2833, 3026, 3080, 1793, 1885, 904, 3661, 2483, 575, 2824, 3372, 1267, 1381, 3318, 3583, 2022, 2827, 738, 3995, 3541, 2776, 2397, 873, 1058, 622, 1776, 305, 3026, 1117, 825, 4065, 362, 3576, 1733, 2728, 1049, 3031, 3932, 3373, 738, 3059, 894, 3603, 3153, 929, 2496, 3655, 1210, 1192, 2132, 360, 98, 2805, 3764, 3162, 1862, 968, 337, 2860, 1648, 2177, 1527, 2711, 933, 130, 4082, 932, 2151, 2716, 688, 1408, 733, 3478, 2929, 3562, 3754, 756, 2468, 2028, 2424, 859, 1010, 1564, 918, 2609, 3338, 327, 2839, 69, 2331, 570, 2114, 2172, 2003, 2130, 2554, 76, 1905, 75, 48, 93, 229, 2448, 3730, 1311, 1558, 1100, 1109, 2333, 3403, 3173, 906, 3867, 2684, 1074, 3241, 456, 150, 2976, 1194, 2910, 1644, 3368, 2084, 2118, 1974, 1065, 1218, 2153, 148, 407, 2967, 199, 2138, 2797, 2697, 3767, 2689, 934, 1981, 1362, 3430, 2550, 2329, 1878, 3321, 3840, 3840, 3032, 3082, 1763, 251, 3410, 152, 3659, 471, 379, 3958, 918, 2341, 2821, 3993, 753, 1360, 3275, 257, 2326, 2211, 47, 2123, 2802, 3076, 3383, 2767, 4044, 529, 347, 2065, 2731, 1358, 623, 1051, 2324, 656, 1990, 1184, 269, 1034, 3032, 783, 3762, 1545, 136, 3407, 3024, 3762, 2545, 3610, 2618, 1301, 2441, 3753, 1631, 3127, 1082, 553, 2803, 3806, 652, 3003, 3624, 3374, 827, 2332, 3043, 1821, 100, 636, 3456, 3482, 786, 3944, 1213, 3540, 1639, 774, 1936, 2965, 1114, 161, 979, 2259, 500, 339, 3427, 1392, 3149, 3498, 3042, 2227, 3426, 2465, 2463, 3569, 2698, 3727, 815, 1978, 4027, 3731, 3074, 1746, 3427, 2641, 1542, 601, 963, 4092, 1515, 1026, 1418, 1373, 3645, 3029, 3124, 1316, 3160, 650, 2014, 250, 291, 522, 3685, 2323, 3581, 2668, 3461, 1903, 726, 3570, 3535, 1891, 3817, 3672, 2839, 2523, 1440, 3771, 774, 3193, 603, 2152, 791, 2852, 3260, 2358, 3788, 1984, 3132, 691, 3572, 3786, 1766, 2682, 4089, 1367, 3664, 226, 960, 335, 3321, 432, 185, 2717, 904, 4021, 3253, 883, 2708, 3652, 1803, 2535, 2898, 3185, 1757, 2608, 3807, 3304, 1650, 95, 2320, 2360, 2501, 3620, 460, 2427, 1894, 3884, 3262, 3678, 3187, 3703, 2890, 3510, 2508, 3506, 1482, 2844, 38, 2869, 2829, 1174, 3957, 2836, 3461, 804, 787, 3949, 3656, 1031, 2593, 784, 932, 3604, 2727, 3251, 3595, 1635, 508, 2768, 1011, 1625, 2265, 2066, 3521, 1520, 3273, 969, 2508, 2110, 2978, 3439, 996, 1159, 1681, 1248, 2406, 2389, 3772, 3691, 2328, 975, 2452, 4075, 1373, 3119, 144, 2878, 3282, 2608, 1477, 1139, 1717, 3547, 610, 1216, 2700, 3133, 4046, 3919, 83, 2168, 3094, 853, 1712, 283, 252, 3915, 3230, 3498, 182, 3753, 737, 848, 2787, 1243, 1841, 2642, 613, 148, 603, 1319, 1412, 23, 2600, 3050, 502, 3050, 803, 2929, 1726, 581, 3308, 1267, 3620, 3362, 2421, 980, 3302, 3326, 1496, 2722, 721, 213, 2499, 1266, 357, 3977, 2380, 2726, 3916, 387, 1634, 1214, 1199, 937, 1139, 3142, 1819, 1806, 2893, 1410, 1461, 1580, 65, 2667, 1342, 3990, 3178, 3518, 2848, 1082, 1763, 1215, 1735, 917, 673, 3608, 1734, 2567, 271, 837, 2235, 1141, 1008, 3820, 1372, 1357, 3457, 2954, 1407, 1557, 725, 1108, 2728, 3768, 1044, 2432, 3519, 1727, 392, 949, 3916, 1677, 3404, 2953, 2994, 3705, 913, 3561, 1615, 353, 2411, 891, 1749, 3325, 961, 1683, 2340, 3749, 3008, 3429, 1620, 1291, 1126, 3936, 1130, 2093, 2923, 415, 1689, 2718, 3501, 2321, 437, 3923, 1247, 3459, 1580, 1303, 3281, 3978, 3124, 2500, 307, 1104, 3624, 2875, 1593, 2774, 2473, 2258, 1398, 2335, 2012, 625, 2046, 2487, 1705, 3932, 664, 677, 3486, 3408, 1156, 2945, 1790, 3805, 116, 2178, 3672, 157, 1813, 4016, 872, 1284, 3338, 1736, 3507, 462, 1532, 1056, 3079, 277, 227, 1271, 26, 237, 3353, 1760, 2291, 2485, 2505, 3793, 366, 1938, 1061, 306, 2062, 613, 3513, 120, 3337, 3192, 2232, 2040, 2779, 505, 2296, 2759, 2339, 2355, 998, 1157, 1238, 2290, 2557, 2604, 2797, 3802, 814, 1691, 149, 317, 3241, 296, 531, 3354, 2132, 216, 2036, 3167, 970, 1864, 2913, 1400, 2022, 1178, 1547, 2252, 3887, 3372, 2891, 2991, 1148, 2483, 3970, 2601, 2330, 1007, 234, 1296, 1695, 3419, 2555, 3148, 3556, 3738, 1286, 891, 2703, 2248, 2070, 2479, 2115, 117, 2799, 2578, 1776, 3750, 1086, 2832, 357, 2255, 2592, 45, 3677, 1567, 3183, 2498, 1570, 3275, 1254, 2042, 2871, 3231, 3129, 1416, 1000, 2293, 3131, 2502, 1092, 0, 542, 1704, 3851, 3845, 1544, 1727, 1710, 71, 633, 2570, 3629, 3657, 2358, 3878, 1221, 2041, 403, 868, 3301, 926, 2064, 2565, 3570, 90, 4084, 1793, 894, 995, 1467, 1172, 1399, 3252, 781, 965, 924, 1019, 3638, 1065, 3455, 1406, 1746, 573, 311, 3320, 221, 532, 2309, 1130, 2533, 939, 1421, 657, 3705, 3212, 3103, 1173, 3688, 3075, 1133, 2226, 320, 747, 3527, 2887, 488, 2488, 4093, 1945, 1681, 3121, 3419, 3171, 1171, 2596, 3350, 34, 3591, 1206, 3179, 448, 2381, 1603, 3893, 2727, 1213, 1589, 3751, 3135, 2320, 1197, 3561, 979, 1158, 454, 1123, 1382, 1211, 1598, 973, 417, 4054, 3099, 868, 2928, 1304, 1922, 3705, 163, 198, 2588, 1358, 244, 761, 5, 588, 1143, 1971, 1664, 2686, 1817, 701, 1373, 843, 2673, 5, 3751, 4087, 2585, 2729, 2069, 3593, 3778, 1094, 703, 1745, 3848, 1496, 2503, 2361, 3229, 1829, 1990, 1153, 2827, 1502, 1700, 2998, 1576, 2327, 2561, 3394, 2857, 1194, 3127, 4007, 2712, 2496, 1318, 749, 3764, 417, 1424, 2267, 1892, 2643, 3553, 2167, 663, 602, 2012, 2483, 2168, 680, 2298, 3000, 2282, 2964, 1118, 1012, 212, 3580, 1660, 979, 550, 3213, 520, 2193, 3098, 778, 2247, 149, 3918, 2272, 435, 3655, 2034, 757, 3658, 3975, 750, 1093, 3134, 281, 2802, 1691, 540, 3270, 347, 2057, 3011, 2525, 902, 2614, 1601, 4031, 1193, 1497, 518, 2517, 2584, 582, 3470, 3194, 169, 2585, 1950, 128, 520, 2671, 2633, 1625, 1140, 226, 659, 2115, 136, 2877, 49, 1997, 506, 414, 2770, 115, 2179, 3782, 3842, 320, 1971, 2831, 2939, 2738, 3817, 3842, 334, 659, 2209, 2743, 3684, 1849, 35, 135, 2714, 3415, 2177, 3954, 994, 1156, 1261, 2638, 2705, 970, 1881, 1598, 3510, 2575, 3446, 2119, 3459, 1159, 1313, 2481, 2671, 3672, 405, 3341, 2765, 3680, 2994, 1959, 3758, 2053, 16, 3149, 281, 2375, 2093, 854, 3672, 2440, 2287, 3307, 1781, 3308, 2076, 2253, 543, 2298, 2325, 3265, 658, 2345, 3280, 3314, 3829, 1149, 160, 3071, 2907, 2372, 3341, 3083, 1403, 1294, 2897, 2652, 2779, 1463, 2965, 1147, 380, 600, 3035, 1444, 3086, 2736, 1517, 1307, 305, 750, 2847, 2331, 618, 1946, 87, 183, 1848, 1226, 3251, 194, 3452, 4021, 2140, 2334, 843, 554, 1742, 1898, 4074, 2156, 3526, 3188, 2710, 1179, 416, 78, 2014, 2125, 2916, 947, 226, 367, 394, 3622, 1724, 1251, 0, 1869, 2207, 602, 3091, 831, 2391, 2627, 1544, 3996, 943, 3823, 2530, 3648, 1357, 63, 3442, 116, 2726, 941, 15, 2731, 290, 3225, 3492, 2458, 2976, 3202, 3256, 2926, 3939, 1977, 2476, 1524, 285, 3958, 340, 3795, 3578, 2264, 137, 2770, 1091, 1014, 2443, 2785, 3032, 3673, 2578, 743, 948, 993, 1547, 2625, 2986, 553, 1976, 3852, 2837, 2130, 1407, 1455, 2072, 1017, 2546, 1543, 3439, 1990, 1878, 644, 1018, 1020, 2189, 1290, 913, 2575, 530, 1063, 3247, 2182, 2802, 1690, 3721, 980, 2882, 3925, 272, 2271, 1686, 656, 3293, 3132, 3109, 997, 2194, 2233, 2271, 3195, 3734, 2431, 2333, 889, 4044, 2437, 1633, 3749, 3395, 3573, 257, 277, 1818, 569, 2896, 172, 2082, 537, 1774, 1320, 793, 826, 283, 242, 1909, 3383, 418, 1740, 1634, 2820, 2055, 822, 738, 1561, 3277, 3431, 381, 1447, 3808, 370, 400, 3186, 4055, 3410, 1523, 1674, 3266, 4048, 1048, 3217, 3641, 275, 2845, 3376, 3139, 899, 907, 3755, 830, 585, 2272, 2983, 584, 1586, 4080, 2606, 2872, 2319, 969, 28, 1529, 2357, 1840, 2905, 1695, 1898, 1657, 2198, 2784, 651, 1868, 2892, 2174, 3477, 2821, 420, 3765, 3063, 3517, 2908, 3195, 2303, 2135, 2421, 261, 3069, 2142, 3532, 3433, 1213, 2920, 2417, 511, 2392, 1685, 1907, 2998, 801, 2644, 1377, 2807, 1444, 1867, 2240, 2384, 3072, 3485, 2484, 2356, 797, 1014, 223, 3517, 4068, 1654, 723, 614, 1768, 2867, 223, 1672, 1196, 2516, 3826, 3805, 3716, 1598, 3759, 2405, 3080, 1531, 1342, 739, 1035, 758, 3520, 214, 4012, 378, 2735, 1050, 3473, 3236, 1302, 2683, 438, 3653, 596, 35, 1567, 12, 2940, 2233, 108, 1351, 3488, 1504, 695, 3934, 31, 3407, 2660, 2800, 1876, 864, 4073, 3421, 1454, 2883, 1219, 421, 3586, 3430, 2740, 333, 1415, 3929, 1949, 263, 137, 1563, 1778, 307, 3576, 1019, 3891, 300, 3792, 2135, 35, 2137, 887, 1867, 475, 492, 2906, 846, 766, 2911, 3509, 3518, 3588, 1635, 4034, 3537, 2834, 792, 179, 3195, 3687, 653, 3462, 3790, 1889, 2091, 3813, 471, 3145, 2674, 2460, 2865, 579, 3257, 3043, 3, 2265, 311, 2801, 3354, 250, 350, 688, 3339, 1912, 2141, 2508, 99, 829, 689, 2340, 508, 2172, 1271, 278, 2991, 2969, 923, 3144, 4089, 3072, 3707, 1565, 1614, 1983, 2699, 884, 2307, 3634, 3563, 470, 591, 3484, 567, 1659, 2349, 2650, 798, 1822, 346, 2772, 1300, 2001, 1145, 3681, 3902, 3080, 125, 3856, 2206, 566, 3235, 2987, 1215, 1646, 2440, 3619, 2638, 2885, 2363, 1769, 2512, 828, 2755, 2270, 1469, 3017, 1250, 3372, 2351, 3198, 961, 4028, 3092, 434, 2004, 980, 3866, 3809, 3872, 1905, 2812, 3253, 1431, 3992, 2441, 4075, 98, 1579, 523, 348, 233, 504, 535, 3654, 1228, 348, 3931, 4064, 1915, 3416, 2961, 192, 1402, 1170, 3403, 2563, 764, 229, 2679, 15, 2220, 397, 2937, 3795, 2551, 2111, 3427, 1267, 2256, 2235, 3123, 2145, 2755, 430, 2708, 1216, 1865, 2515, 3895, 1413, 2713, 2344, 1939, 2496, 1774, 2016, 2324, 493, 1185, 2840, 353, 1349, 3615, 692, 3911, 2183, 212, 2676, 3520, 1282, 2769, 3230, 2687, 2708, 3280, 1864, 2727, 2502, 3497, 54, 900, 4077, 2525, 1324, 628, 1364, 165, 3918, 310, 633, 2096, 2869, 2573, 2248, 2594, 2296, 658, 2138, 1581, 2543, 1340, 1334, 1039, 1543, 1276, 3522, 3359, 3909, 3502, 3641, 1421, 4071, 3546, 2440, 445, 1267, 22, 3713, 654, 1827, 3170, 2115, 1802, 722, 2344, 2553, 680, 71, 3537, 3584, 1281, 3552, 2550, 1555, 601, 1903, 1338, 4093, 3806, 2704, 967, 1996, 3539, 46, 2520, 2991, 3530, 436, 446, 1564, 1373, 1960, 2199, 3006, 3155, 1649, 3931, 2609, 795, 3571, 3398, 105, 3824, 3608, 1889, 3468, 2648, 1544, 1683, 1067, 2945, 3854, 136, 269, 3305, 1831, 3539, 1011, 2937, 1600, 1476, 3158, 7, 3330, 1369, 1655, 621, 52, 1649, 2615, 2702, 1079, 2171, 2177, 576, 1320, 390, 993, 1204, 3090, 341, 617, 394, 3729, 2748, 3044, 3559, 1106, 2639, 1895, 1636, 3351, 440, 935, 3954, 3578, 1815, 4059, 2632, 1080, 1644, 1513, 1613, 2973, 3921, 4028, 2181, 2581, 2354, 3362, 4000, 3953, 1655, 690, 3480, 41, 3375, 3519, 3342, 2861, 1635, 1514, 64, 1538, 3551, 2181, 2320, 3098, 2716, 2844, 82, 1557, 4024, 3099, 638, 324, 3944, 343, 2249, 3485, 2043, 1874, 674, 2755, 1228, 3537, 2483, 3720, 1742, 3373, 3209, 2407, 1364, 418, 3857, 3989, 1503, 3301, 2195, 3063, 3023, 3156, 2470, 861, 952, 507, 3756, 2400, 3507, 2478, 1694, 2491, 1247, 3890, 544, 2535, 2554, 2128, 3626, 1146, 570, 1482, 1353, 486, 2924, 2436, 1914, 3141, 901, 3126, 1613, 2872, 727, 3042, 1193, 3606, 1627, 564, 3812, 2982, 581, 3099, 1116, 3391, 3270, 3774, 159, 2298, 2089, 740, 2022, 1480, 887, 896, 501, 1163, 3001, 3860, 1329, 1504, 1814, 3319, 2823, 814, 2566, 2750, 1425, 718, 2778, 284, 3032, 3230, 1498, 568, 287, 3756, 3810, 3368, 1341, 93, 2, 2851, 3541, 2694, 2162, 1168, 1319, 1803, 2154, 3536, 1937, 357, 3930, 2373, 2757, 539, 163, 2014, 32, 1653, 2103, 1186, 983, 3709, 3157, 2890, 2612, 294, 761, 2037, 2955, 398, 1865, 3191, 3637, 341, 2598, 370, 938, 212, 126, 2742, 3953, 3732, 2745, 2307, 2806, 3211, 3549, 3045, 2085, 1500, 2028, 3665, 3930, 995, 584, 2879, 1070, 1806, 293, 1422, 436, 3930, 3934, 1591, 1350, 1150, 603, 1582, 2075, 1442, 2359, 2024, 2507, 1089, 2288, 4077, 2100, 2651, 1271, 1770, 2707, 2684, 1762, 3165, 2959, 1196, 2769, 3655, 2330, 3405, 2374, 688, 600, 3830, 122, 2101, 1469, 2254, 3085, 3748, 2260, 2302, 2194, 1222, 3541, 404, 3265, 1641, 1889, 1214, 2164, 2255, 1965, 2534, 2639, 893, 3194, 857, 863, 874, 1513, 142, 2235, 3324, 1293, 3383, 1893, 3595, 2175, 1923, 2082, 3739, 1413, 559, 2377, 993, 615, 1300, 2702, 181, 2977, 1676, 3957, 3205, 3887, 2576, 740, 1626, 2442, 2454, 2323, 236, 3956, 1367, 1755, 2829, 2845, 1064, 2454, 225, 1884, 409, 2281, 74, 3392, 3532, 4043, 926, 124, 3664, 2955, 2409, 1623, 1668, 3046, 2553, 1489, 2489, 1505, 3222, 1723, 3707, 3126, 903, 2909, 4039, 3788, 3784, 1789, 3462, 1701, 281, 327, 1793, 442, 2091, 226, 1581, 2569, 3522, 1734, 3760, 415, 3790, 1654, 2457, 432, 4017, 3719, 697, 1134, 887, 939, 3774, 1758, 2237, 571, 3516, 1878, 3395, 49, 770, 2921, 1947, 1274, 3320, 2803, 3394, 2364, 1055, 3670, 2201, 3323, 348, 2807, 3760, 3380, 3244, 3755, 2920, 816, 1128, 2186, 1412, 2330, 283, 1328, 3816, 2936, 326, 3393, 2019, 3578, 3227, 3988, 3495, 2334, 3692, 2704, 2223, 399, 1589, 104, 147, 1800, 1976, 142, 3297, 3626, 1182, 128, 113, 1260, 3509, 288, 3471, 148, 3128, 256, 3471, 3030, 144, 748, 1376, 3105, 1814, 4004, 1459, 1731, 302, 645, 1571, 1506, 337, 3621, 369, 600, 704, 24, 3534, 3277, 1330, 3179, 708, 1012, 2789, 1300, 263, 1592, 1781, 2391, 1455, 3517, 804, 1793, 307, 223, 3566, 3950, 2744, 1123, 758, 860, 502, 4046, 1208, 615, 1980, 3307, 2325, 615, 3741, 1469, 813, 3251, 2858, 998, 3851, 3237, 3199, 2839, 3333, 391, 1168, 615, 268, 995, 1418, 1102, 704, 3688, 3820, 3884, 2884, 2492, 3623, 1084, 1195, 3099, 144, 933, 3673, 1185, 518, 1182, 3411, 3495, 1733, 3179, 2167, 541, 1914, 2273, 523, 1821, 443, 1140, 1834, 3370, 2432, 3263, 3095, 2308, 3539, 2412, 1881, 755, 1989, 2722, 1226, 3767, 4027, 257, 1643, 3099, 488, 2869, 1098, 3583, 3549, 2135, 3291, 1611, 3930, 1809, 2244, 2252, 1052, 4059, 790, 1079, 2368, 2129, 4054, 2347, 43, 898, 3757, 2130, 3312, 3395, 1272, 1172, 1094, 947, 2330, 746, 3752, 3588, 1761, 3692, 2945, 472, 3483, 3398, 3816, 2820, 2502, 2982, 136, 660, 1419, 2644, 2460, 1497, 2678, 1558, 3288, 1590, 2194, 2241, 1614, 1119, 2787, 343, 354, 2385, 3570, 1726, 870, 92, 3224, 3779, 847, 262, 1439, 845, 2718, 361, 1138, 2015, 2785, 3687, 928, 3412, 1217, 1686, 3761, 395, 2227, 1462, 1247, 2660, 3829, 2865, 3884, 1463, 707, 3463, 3719, 3795, 2111, 3849, 106, 2472, 2784, 1991, 2549, 69, 1082, 701, 1615, 812, 3185, 3477, 1863, 974, 3123, 1781, 2099, 334, 3202, 2102, 3232, 608, 2346, 4011, 333, 1780, 3664, 1437, 1153, 3435, 891, 2058, 3794, 2148, 1471, 3852, 664, 3343, 2597, 1115, 1699, 2967, 996, 921, 3440, 274, 1543, 2622, 1042, 658, 1592, 3392, 1013, 395, 2497, 2823, 1157, 3042, 574, 1640, 2153, 2421, 3952, 3140, 2187, 3620, 287, 1472, 1119, 377, 3477, 2197, 1857, 1197, 975, 3403, 1019, 3998, 3605, 2186, 507, 3352, 2897, 1644, 3896, 1442, 3601, 3740, 2677, 103, 2218, 2406, 3764, 3201, 564, 3186, 3924, 1450, 536, 1439, 1108, 3089, 3282, 3815, 1979, 1284, 982, 1046, 977, 3455, 1114, 202, 2277, 4090, 2294, 3263, 3535, 825, 2776, 1233, 447, 1541, 1651, 551, 1331, 529, 1696, 1327, 2353, 328, 696, 2843, 626, 3223, 2783, 1426, 464, 571, 298, 3849, 2550, 526, 1985, 132, 1862, 3350, 2859, 1884, 2524, 1965, 3898, 3877, 2467, 3335, 549, 3567, 1398, 1751, 1515, 77, 1049, 1320, 920, 1859, 3415, 1225, 1201, 3093, 3333, 101, 4038, 2936, 4070, 1361, 3343, 1625, 2622, 3183, 3939, 783, 3397, 2144, 4072, 4095, 2984, 1796, 2630, 397, 3716, 2338, 1260, 2846, 932, 2827, 3186, 365, 692, 1179, 2392, 413, 3127, 1338, 505, 3017, 166, 1851, 524, 2868, 397, 243, 3938, 920, 3634, 2380, 929, 1921, 1849, 935, 214, 1541, 2707, 3702, 97, 900, 2405, 926, 4082, 907, 2865, 338, 3027, 1781, 196, 1888, 2892, 174, 281, 3946, 917, 1187, 1687, 3317, 1476, 425, 3539, 2905, 1130, 3828, 313, 1080, 2232, 366, 1450, 1465, 2951, 3460, 1728, 889, 54, 1967, 3369, 3036, 1206, 1533, 4081, 1954, 3773, 3615, 1043, 1055, 3535, 607, 639, 2260, 1630, 3516, 2324, 5, 3640, 3507, 3402, 3781, 2678, 3236, 2007, 1333, 2416, 3018, 2053, 1117, 2187, 2454, 1465, 4033, 257, 112, 2953, 318, 3413, 1323, 1554, 90, 2334, 3796, 1106, 3857, 1786, 663, 4022, 3669, 2818, 796, 2350, 3209, 3718, 1475, 1941, 3821, 3260, 3593, 1801, 2853, 115, 3022, 2161, 3031, 3039, 848, 628, 708, 1345, 1966, 3416, 1804, 1840, 1184, 2923, 1777, 3615, 1409, 2855, 1150, 207, 2947, 3115, 3461, 3503, 1212, 4012, 453, 1516, 2764, 2709, 1720, 717, 3936, 4022, 46, 199, 1475, 3720, 2513, 3950, 2154, 1579, 2590, 1392, 2278, 3645, 1575, 1305, 3396, 3434, 1312, 1753, 1964, 3274, 3542, 1037, 625, 3322, 880, 2102
title_screen:
%ifdef USE_TITLE_SCREEN
%include "sim86_title_screen.dat"
%endif
end:
