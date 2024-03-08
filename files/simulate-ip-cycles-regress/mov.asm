bits 16

Code:
; Test mem to/from accumulator
; The byte-sized variants were messed up; my RTOS helped me find these
mov ax, 0x5555
mov [0x1000], ax
mov al, 0xAA
mov [0x2000], al
mov ax, 0
mov	al, byte [0x2000]
mov	ax, [0x1000]
