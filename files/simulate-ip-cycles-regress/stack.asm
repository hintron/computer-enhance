bits 16

Code:
mov sp, 0x1000
mov ax, 1
mov bx, 2
mov cx, 3
push ax
push bx
push cx
mov ax, 7
mov bx, 7
mov cx, 7
pop cx
pop bx
pop ax
