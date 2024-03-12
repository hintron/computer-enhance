bits 16

Code:
mov ax, -1
mov bx, -1
imul bx

mov ax, -32768
mov bx, -32768
imul bx

mov ax, -32768
mov bx, 32767
imul bx

mov ax, 32767
mov bx, 32767
imul bx

mov ax, -32768
mov bx, -1
imul bx

mov al, 2
mov bl, -3
imul bl

mov al, 2
mov bh, -3
imul bh

mov al, -128
mov bl, -1
imul bl

mov al, -128
mov bh, -1
imul bh
