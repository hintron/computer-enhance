bits 16

Code:
mov al, -1
cbw
cwd
mov al, 0
cbw
cwd

; Test xchg
; NOTE: xchg always treats the rm field as a source, no matter what order you
; write it in assembly. Do that here so that the instruction printouts match.
mov bx, -1
; reg to reg
xchg bl, ch
xchg bh, cl
; reg16 to acc
xchg ax, cx
; reg to mem
mov [0x100], word -1
mov dx, 0x5555
xchg dx, [0x100]
xchg dl, [0x100]
; TODO: Fix bug where high byte with mem addr results in incorrect placement
; xchg dh, [0x101]

; NOTE: exchanging between same register is currently unsupported
; xchg bl, bh

nop
nop
