bits 16

Code:
jmp di
mov ax, .loc1
mov bx, .loc2
mov cx, .loc3
mov dx, .loc4
jmp .loc4

.loc1:
jmp .loc5
mov ax, 0

.loc2:
jmp .loc1
mov bx, 0

.loc3:
jmp .loc5
mov cx, 0

.loc4:
jmp .loc2
mov dx, 0

.loc5
mov si, 1
