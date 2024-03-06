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
