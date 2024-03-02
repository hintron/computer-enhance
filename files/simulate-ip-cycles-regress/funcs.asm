bits 16

Code:

; init sp and bp
mov sp, 0x2000
mov bp, 0x1111
; init some other registers with bogus data
mov cx, 0x77
; Call func1
call .func1
; Finish the program
jmp .end

; func1() - no args or locals. Add 1 to result of func2
.func1
push bp
mov bp, sp
; Pass in args x=5, y=10
mov ax, 10
push ax
mov ax, 5
push ax
; Call func2
call .func2
; ; Undo arg stack pushes
; add sp, 4
; Add 1 to result of func2
add ax, 1
; Return ax from func1
;   -- Undo any local vars (there were none)
mov sp, bp
;   -- Restore old bp
pop bp
;   -- Pop return addr into IP
ret

; func2(int x, int y) => result = x + y + 5; return result;
; [bp - 2] is a function-local variable created by push cx
; [bp] is the old bp value
; [bp + 2] is the return address
; [bp + 4] is arg x (5)
; [bp + 6] is arg y (10)
.func2
; Set up function stack frame
push bp
mov bp, sp
; Do calculation all with ax
mov ax, [bp + 4]
add ax, [bp + 6]
add ax, 5
; Create local variable at [bp - 2] with bogus data from cx
push cx
; (contrived) Save result to local variable and move result to ax to return
mov [bp - 2], ax
mov ax, [bp - 2]

; Return ax from func1
;   -- Undo any local vars
mov sp, bp
;   -- Restore old bp
pop bp
;   -- Pop return addr into IP (return val is in ax) and undo arg stack pushes
ret 4


.end
; Add 1 to result of func1
add ax, 1
; ax should now be 22