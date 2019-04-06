; This is the main file for the scrolling engine
; It will just set the assembler origin, and include the files


org 33025
include "drawmap.asm"
include "rambanks.asm"
include "drawsprite.asm"
include "input.asm"
include "create_shifted_tables.asm"
include "im2.asm"
include "starfield.asm"



SPRITE_START EQU $9000		; we have 4k for sprites, $9000 - $9FFF
MAP_START    EQU $A000		; 4k for the game map, $a000 - $afff
TablaTiles   EQU $B000



