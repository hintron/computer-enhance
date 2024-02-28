bits 16

Code:
mov sp, 0x1000
add ax, 1
sub bx, 1
mov cx, ax
xor cx, bx
push ax
push bx
push cx
mov ax, 7
mov bx, 7
mov cx, 7
pop ax
pop bx
pop cx
