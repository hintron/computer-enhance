bits 16

Code:
mov al, -1
cbw
cwd
mov al, 0
cbw
cwd

mov bx, -1
; reg to reg
xchg bl, ch
xchg bh, cl
; reg16 to acc
xchg ax, cx
; reg to mem
mov [0x100], word -1
mov dx, 0x5555
xchg [0x100], dx
xchg [0x100], dl
; TODO: Fix bug where high byte with mem addr results in incorrect placement
; xchg [0x101], dh

; NOTE: exchanging between same register is currently unsupported
; xchg bl, bh
