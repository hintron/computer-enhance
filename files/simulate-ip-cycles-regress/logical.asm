; Test and and test
mov ah, 1
mov al, 3
and al, ah
and al, al
mov ax, -1
test ax, 2
and ax, 254
test ax, 2
mov bl, 1
and bl, 2
mov bh, 2
and bl, bh

; Test xor
xor ax, ax
xor bh, bh
mov bl, 255
xor bl, bl
mov cx, 65535
mov dx, 65534
xor dx, cx
xor dx, cx
xor dx, cx

; Test shifts
mov al, -1
mov cl, 7
shl al, cl
shl al, 1
shl al, 1

mov bx, -1
mov cl, 4
shl bx, cl
sar bx, cl
shl bx, cl
shr bx, cl

mov dx, 1
mov cl, 15
shl dx, cl
sar dx, cl
shr dx, cl
