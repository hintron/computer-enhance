bits 16

Code:

mov dl, 1
add dl, 255
clc

sti
std
stc
cmc
cmc
cli
clc
cld
