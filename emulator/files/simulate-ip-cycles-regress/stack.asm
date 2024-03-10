bits 16

Code:
mov sp, 0xFFFE
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
mov dx, -1
add dx, 1
pushf
add dx, 1
add dx, 255
pushf
add dx, 1
popf
popf

mov dx, 0xAAAA
push dx
mov dx, 0xBBBB
push dx
mov dx, 0
pop word [0x100]
mov dx, [0x100]
pop word [0x200]
mov dl, [0x200]
