; test adds
add ah, 127
add ah, 1
add ah, 1
add ah, 127
add al, 127
add al, 1
add al, 1
add al, 127
add bx, -1
add bx, -1
add bx, 2
mov cl, 1
mov ch, 1
add cl, 255
add ch, 255
add cl, 127
add cl, 127
add ch, 127
add ch, 127
add dl, cl
add dl, ch
add dx, -2

; test subs
sub ah, 127
sub ah, 1
sub ah, 1
sub ah, 127
sub al, 127
sub al, 1
sub al, 1
sub al, 127
sub bx, -1
sub bx, -1
sub bx, 2
mov cl, 1
mov ch, 1
sub cl, 255
sub ch, 255
sub cl, 127
sub cl, 127
sub ch, 127
sub ch, 127
sub dl, cl
sub dl, ch
sub dx, -2

; test lea
lea si, [bp + si + 0x1000]
lea bp, [0x2000]
lea sp, [bp + si]

