; Limited to 16x16 sprites, with NO mask, printed with OR
; Originally taken from a tutorial by dmsmith, then modified

; Input:
;	DE: sprite number
;	B: X position
;	C: Y position

; Required sprite alignment:
;		full X line first
;
; So, in SevenuP terminology, this means: X char, Char line, Y char

; now, the sprite drawing routine includes a sprite cache, with all required handling


DrawSprite:
		ld a, b
		and 7					; A == rotation required
		ld ixl, a

		rl e
		rl d
		rl e
		rl d
		rl e
		rl d					; sprnum << 3 (Carry was 0)
		push de

		or e
		ld e, a					; DE = (sprnum << 3) | rotation


		ld hl, SprCacheTable
		add hl, de				; HL = SprCacheTable[(sprnum << 3) | rotation]
		ld a, (hl)

		pop de
		push bc
		ld h, $8a				; pointer to the LRU_prev list
		cp 255
		call nz,MoveSpriteToTop                 ; Sprite found in cache, since the value is not 255
							; move to the top of the list and draw
							; we have the cache entry in (LRU_first)

		call z,InsertSpriteInCache		; Sprite not found in cache, rotate and move to the top of the list


		; we have the cache entry in
		; Now draw the sprite
                ; First, calculate the target position: 0x9000+48*LRU_first

                ld a, (LRU_first)
		add a,a
                ld hl, Multiply_by_48
                ld c,a
                ld b,0
                add hl, bc		; HL points to the value in the array
                ld c, (hl)
                inc hl
                ld b, (hl)		; BC = 48 * LRU_first
                ld hl, $9000
                add hl, bc		; hl = $9000 + 48 * LRU_first  <- the place in the sprite cache to get the rotated sprite from.

                pop bc                  ; get X and Y back!

		ld a, 16		; 16-line sprite
		ld (LINECOUNT), a

                ld d, h
                ld e, l                 ; Save the value of HL in DE

		call calcscreenpos
		ld (SCRADD),HL		; save the screen address in SCRADD

                ld h, d
                ld l, e                 ; Restore HL
                ; Clip: three, two or one char to be displayed
	        ld a, b                 ; X position
               	cp $f0                  ; if X < 240...
	        jp c, char3             ; we will display 3 chars
	        and 8                   ; otherwise, if X < 248
                jp z, char2             ; we will display 2 chars
char1:                                   ; finally, X >= 248, display only one char
                ; now calculate screen addresses and stuff, then paint

                ld (lineloop_1char+1), HL		; save pointer


lineloop_1char:
		ld hl, 0		; this will be modified with the right value to load
		ld c, (hl)
		inc hl
		ld b, (hl)
		inc hl			; get the sprite
		ld a, (hl)		; third sprite byte
		inc hl
		ld (lineloop_1char+1), hl	; save HL

		ld hl, (SCRADD)		; get screen address in HL
		ld a, (hl)		; get what is there
		or c			; OR with sprite data
		ld (hl), a		; store... and skip the following two chars
		inc h			; next line

		ld a,h
		and 7
		jr nz, draw_a1_1char		; if the low 3 bits of B are zero
		ld a, l
		add a, 32
		ld l,a
		jr c, draw_a1_1char		; and C + 32 overflows
		ld a,h
		sub 8			; then we go to the next third of the screen
		ld h,a
draw_a1_1char:
		ld (SCRADD),hl		; store the screen address again
		ld hl, LINECOUNT
		dec (hl)
		jp nz, lineloop_1char		; go to next line
                ret

char2:
                ld (lineloop_2char+1), HL		; save pointer


lineloop_2char:
		ld hl, 0		; this will be modified with the right value to load
		ld c, (hl)
		inc hl
		ld b, (hl)
		inc hl			; get the sprite
		ld a, (hl)		; third sprite byte
		inc hl
		ld (lineloop_2char+1), hl	; save HL

		ld hl, (SCRADD)		; get screen address in HL
		ld a, (hl)		; get what is there
		or c			; OR with sprite data
		ld (hl), a		; store
		inc l			; next char

		ld a, (hl)		; repeat for the next line
		or b
		ld (hl), a

		dec l
		inc h			; next line

		ld a,h
		and 7
		jr nz, draw_a1_2char		; if the low 3 bits of B are zero
		ld a, l
		add a, 32
		ld l,a
		jr c, draw_a1_2char		; and C + 32 overflows
		ld a,h
		sub 8			; then we go to the next third of the screen
		ld h,a
draw_a1_2char:
		ld (SCRADD),hl		; store the screen address again
		ld hl, LINECOUNT
		dec (hl)
		jp nz, lineloop_2char		; go to next line
		ret



char3:          ; This is the most common case, just 3 bytes because we are on the middle of the screen
                ; now calculate screen addresses and stuff, then paint
                ld (lineloop+1), HL		; save pointer

lineloop:
		ld hl, 0		; this will be modified with the right value to load
		ld c, (hl)
		inc hl
		ld b, (hl)
		inc hl			; get the sprite
		ld a, (hl)
		ld (data+1),a		; third sprite byte
		inc hl
		ld (lineloop+1), hl	; save HL

		ld hl, (SCRADD)		; get screen address in HL
		ld a, (hl)		; get what is there
		or c			; OR with sprite data
		ld (hl), a		; store
		inc l			; next char

		ld a, (hl)		; repeat for the next line
		or b
		ld (hl), a
		inc l

		ld a, (hl)
data:		or 0			; 0 will be replaced by what was loaded before
		ld (hl), a

		dec l
		dec l
		inc h			; next line

		ld a,h
		and 7
		jr nz, draw_a1		; if the low 3 bits of B are zero
		ld a, l
		add a, 32
		ld l,a
		jr c, draw_a1		; and C + 32 overflows
		ld a,h
		sub 8			; then we go to the next third of the screen
		ld h,a
draw_a1:
		ld (SCRADD),hl		; store the screen address again
		ld hl, LINECOUNT
		dec (hl)
		jp nz, lineloop		; go to next line

		ret

; Same as DrawSprite, but only for the main ship
; Changes:
;	1- No clipping needed, always display the full sprite
;	2- Use also for collision checks, update variable collision with the result
; Input:
;	DE: sprite number
;	B: X position
;	C: Y position


DrawSprite_Ship:
		xor a
		ld (Ship_Collision), a			; 0 == no collision

		ld a, b
		and 7					; A == rotation required
		ld ixl, a

		rl e
		rl d
		rl e
		rl d
		rl e
		rl d					; sprnum << 3 (Carry was 0)
		push de

		or e
		ld e, a					; DE = (sprnum << 3) | rotation


		ld hl, SprCacheTable
		add hl, de				; HL = SprCacheTable[(sprnum << 3) | rotation]
		ld a, (hl)

		pop de
		push bc
		ld h, $8a				; pointer to the LRU_prev list
		cp 255
		call nz,MoveSpriteToTop                 ; Sprite found in cache, since the value is not 255
							; move to the top of the list and draw
							; we have the cache entry in (LRU_first)

		call z,InsertSpriteInCache		; Sprite not found in cache, rotate and move to the top of the list


		; we have the cache entry in
		; Now draw the sprite
                ; First, calculate the target position: 0x9000+48*LRU_first

                ld a, (LRU_first)
		add a,a
                ld hl, Multiply_by_48
                ld c,a
                ld b,0
                add hl, bc		; HL points to the value in the array
                ld c, (hl)
                inc hl
                ld b, (hl)		; BC = 48 * LRU_first
                ld hl, $9000
                add hl, bc		; hl = $9000 + 48 * LRU_first  <- the place in the sprite cache to get the rotated sprite from.

                pop bc                  ; get X and Y back!

		ld a, 16		; 16-line sprite
		ld (LINECOUNT), a

                ld d, h
                ld e, l                 ; Save the value of HL in DE

		call calcscreenpos
		ld (SCRADD),HL		; save the screen address in SCRADD

                ld h, d
                ld l, e                 ; Restore HL

                ; This is the most common case, just 3 bytes because we are on the middle of the screen
                ; now calculate screen addresses and stuff, then paint
                ld (lineloop_ship+1), HL		; save pointer
lineloop_ship:
		ld hl, 0		; this will be modified with the right value to load
		ld c, (hl)
		inc hl
		ld b, (hl)
		inc hl			; get the sprite
		ld a, (hl)
		ld (data_ship+1),a		; third sprite byte
		ld (data2_ship+1),a		; third sprite byte
		inc hl
		ld (lineloop_ship+1), hl	; save HL

		ld hl, (SCRADD)		; get screen address in HL
		ld a, (hl)		; get what is there
		and c			; if A & C is not zero, there is a collision
		jr z, byte1_nocollision
		ld a, 1
		ld (Ship_Collision), a
byte1_nocollision:		
		ld a, (hl)
		or c			; OR with sprite data
		ld (hl), a		; store
		inc l			; next char

		ld a, (hl)		; get what is there
		and b			; if A & B is not zero, there is a collision
		jr z, byte2_nocollision
		ld a, 1
		ld (Ship_Collision), a
byte2_nocollision:
		ld a, (hl)		; repeat for the next line
		or b
		ld (hl), a
		inc l

		ld a, (hl)		; get what is there
data2_ship:	and 0			; if A & B is not zero, there is a collision
		jr z, byte3_nocollision
		ld a, 1
		ld (Ship_Collision), a
byte3_nocollision:

		ld a, (hl)
data_ship:	or 0			; 0 will be replaced by what was loaded before
		ld (hl), a

		dec l
		dec l
		inc h			; next line

		ld a,h
		and 7
		jr nz, draw_a1_ship		; if the low 3 bits of B are zero
		ld a, l
		add a, 32
		ld l,a
		jr c, draw_a1_ship		; and C + 32 overflows
		ld a,h
		sub 8			; then we go to the next third of the screen
		ld h,a
draw_a1_ship:
		ld (SCRADD),hl		; store the screen address again
		ld hl, LINECOUNT
		dec (hl)
		jr nz, lineloop_ship		; go to next line
		ret


; Insert sprite in cache. This means
;  1. Allocate cache entry for the combination
;  2. Rotate sprite and move to the appropriate place in memory
;
;  Input: DE: sprnum * 8
;	  IXl: rotation
;
;  Output: IX: pointer to the sprite, already rotated

InsertSpriteInCache:
			push hl
			ld a, (LRU_last)
			add a, a		; MappingTable + 2*LRU_last
			ld hl, MappingTable
			add a,l
			inc a
			ld l, a			; HL points to the high byte of the current entry
			ld a, (hl)
			and a			; If a==0, this entry was unused
			jr z, insert_unused_entry
						; The entry is used, so we need to clean up
			ld b, a
			dec hl
			ld a, (hl)
			ld c, a			; BC points to the sprnum | rotation entry. It should be LRU_last now, we will reset to 255
			ld a, 255
			ld (bc), a
insert_unused_entry:
			ld b, d
			ld a, e
			or ixl
			ld c, a			; BC = sprnum <<3 | rotation
			ld hl, SprCacheTable
			add hl, bc		; HL has now the address
			ld c, l
			ld b, h			; BC has it
			ld a, (LRU_last)
			add a, a		; MappingTable + 2*LRU_last
			ld hl, MappingTable
			add a,l
			ld l, a
			ld (hl), c
			inc hl
			ld (hl), b		; store the address back
			pop hl


			ld a, (LRU_last)
			ld l, a
			ld a, (hl)		; A == LRU_newlast,  LRU_newlast = LRU_prev[LRU_last];
			ld (hl), LRU_LASTENTRY  ;  LRU_prev[LRU_last] = LRU_LASTENTRY;
			ld l, LRU_LASTENTRY
			ld (hl), a		;  LRU_prev[LRU_LASTENTRY] = LRU_newlast;

			ex af, af'
			ld a, (LRU_first)
			ld l, a
			ld a, (LRU_last)
			ld c, a
			ld (hl), c		; LRU_prev[LRU_first] = LRU_last;
			inc h			; pointer to the LRU_next list, clear carry flag
			ld l, c			; c == LRU_last
			ld a, (LRU_first)
			ld b, a
			ex af, af'

			ld (hl), b		; LRU_next[LRU_last] = LRU_first;
			ld l, a
			ld (hl), LRU_LASTENTRY  ; LRU_next[LRU_newlast] = LRU_LASTENTRY;
			ld l, LRU_LASTENTRY
			ld (hl), c		; LRU_next[LRU_LASTENTRY] = LRU_last;

			ld hl, SprCacheTable
			ld ixh,c			; ixh == LRU_last

			ex af, af'		; use alternate A
			ld a, ixl		; A' = rotation

			ld b, d					; BC= sprnum << 3
			or e
			ld c, a					; HL = (sprnum << 3) | rotation

			add hl, bc				; hl = SprCacheTable[value]

			ex af, af'				; normal A again

			ld c, ixh
			ld (hl), c				; SprCacheTable[value]=LRU_last

			ld b, a					; save LRU_newlast

			ld a, (LRU_last)
			ld (LRU_first), a			; LRU_first = LRU_last;
			ld a,b
			ld (LRU_last),a				;  LRU_last = LRU_newlast, A is still LRU_newlast


			; Now we should rotate the sprite and really write it there
			; First, calculate the target position: 0x9000+48*SprCacheTable[value]
			; C is LRU_last == SprCacheTable[value]

			ld hl, Multiply_by_48
			ld b,0
			sla c			; to index, we need LRU_LAST * 2
			add hl, bc		; HL points to the value in the array
			ld c, (hl)
			inc hl

			ld a, (hl)		; AC = 48 * LRU_last
			add a,$90
			ld h,a
			ld l,c			; HL = $9000 + 48 * LRU_last  <- the place in the sprite cache to store the rotated sprite


			; The target position is HL, now rotate!
			ld (SCRADD),HL		; save the target address in SCRADD


			; aqui hay que seleccionar el banco de RAM con los sprites!!!

			ld 	a, (23388)		; Variable del sistema con el valor previo
			ld 	(SAVE_RAMBANK), a
			and 	$f8			; conservar bits altos
			or 	SPRITES_BANK			; ponemos la p�gina 1 arriba
			ld 	bc, $7ffd		; Puerto en el que escribir
			di     		
			ld	(23388),a	;Actualizar variable del sistema
     			out	(c),a		;Direccionar
			ei

			ld hl, $C000
			and a			; clear carry flag
			rl e
			rl d
			rl e
			rl d
			;rl e
			;rl d			; DE = sprnum *64
			add hl, de		; HL = first position for the sprite
			ld (insert_lineloop+1),hl


			ld a, 16		; 16-line sprite
			ld (LINECOUNT), a

insert_lineloop:	ld hl, 0		; this address will be modified
			ld c, (hl)
			inc hl
			ld b, (hl)		; one sprite line
			inc hl
			ld (insert_lineloop+1),hl


			ld a, ixl
			or a
			jr z, insert_skiprotate	; if no rotation is needed, skip this

			ld l,a			; l= loop counter
			xor a			; clear carry flag, clear a,since if will be shifted to the image

insert_rotateloop:	rr c
			rr b
			rra
			dec l
			jr nz, 	insert_rotateloop		; at the end, we have BCa with the rotated sprite

insert_skiprotate:	ld hl, (SCRADD)		; get screen address in HL
			ld (hl), c
			inc hl
			ld (hl), b		; write BC (sprite) in cache
			inc hl
			ld (hl), a		; write A (last byte of sprite) in cache
			inc hl

			ld (SCRADD),hl		; store the write address again
			ld hl, LINECOUNT
			dec (hl)
			jr nz, insert_lineloop		; go to next line

			; aqui hay que seleccionar el banco de RAM con los sprites!!!
			ld 	a,(SAVE_RAMBANK)
			ld 	bc, $7ffd		; Puerto en el que escribir
			di
     			ld	(23388),a	;Actualizar variable del sistema
     			out	(c),a		;Direccionar
			ei

			ret

; Move sprite to top of the cache.
;  Input: A: entry to move to the top of the cache
;
;	C: prev
;	B: next
;	E: entry


MoveSpriteToTop: ; First, if the entry is already at the top of the cache, skip
                ld e, a                ; Copy entry to E
                ld a, (LRU_first)
                cp e
                jr nz, checklast
                inc a                  ; sets flag to not zero
                ret
                
checklast:
                ld a, (LRU_last)
                cp e
                jr nz, moveactually    
                ; If we are moving the last entry to the top of the cache
                ; we need to adjust LRU_last, or we will screw the cache
                ld l, a
                ld a, (hl)              ; A == LRU_prev[LRU_last], or the new LRU_last
                ld (LRU_last),a         ; and now, continue with the movement

moveactually:
		ld a, e			; E = entry
		ld l, a			; A ==entry
		ld c, (hl)		; C == prev = LRU_prev[entry];
		ld (hl), LRU_LASTENTRY  ; LRU_prev[entry] = LRU_LASTENTRY;
		ld a, (LRU_first)
		ld l, a
		ld (hl), e		;   LRU_prev[LRU_first] = entry;  50

		inc h			; pointer to the LRU_next list
		ld l, e
		ld b, (hl)		; B== next = LRU_next[entry];
		ld (hl),a		; LRU_next[entry] = LRU_first;
		ld l, LRU_LASTENTRY
		ld (hl), e		; LRU_next[LRU_LASTENTRY] = entry;
		ld l, c
		ld (hl), b		; LRU_next[prev] = next; 54

		dec h			; pointer to the LRU_prev list, flags not zero
		ld l,b
		ld (hl),c		;   LRU_prev[next] = prev;

		ld a, e
		ld (LRU_first),a	;    LRU_first = entry;

 		ret			; Total: 143 T-states for a cache hit

; Initialize sprite cache list
; No entry, no output
; Modifies: BC, DE, HL, A

InitSprCacheList:
                 ; First, initialize the Sprite Cache Table with 255
                 ld hl, SprCacheTable
                 ld de, SprCacheTable+1
                 ld (hl),255
                 ld bc, 1023
                 ldir
		 ; Initialize the mapping table with zeroes
                 ld hl, MappingTable
                 ld de, MappingTable+1
                 ld (hl),0
                 ld bc, 169
                 ldir

                 ; Third, pre-populate the LRU_next and LRU_prev arrays
                 ;unsigned char LRU_next[43]={1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,0};
                 ;unsigned char LRU_prev[43]={42,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41};
                 ld hl, LRU_prev+1
                 ld de, LRU_next
                 ld b, 85
                 ld a, 1
                 ld c, 0

loop_InitSprCache:
                  ld (hl), c
                  ld (de), a
                  inc hl
                  inc de
                  inc a
                  inc c
                  djnz loop_InitSprCache
                  ; The final touches
                  dec a                 ; A is already 86, so decrement to make it 85
                  ld (LRU_prev), a
                  xor a
                  ld (de), a
		  ; Finally, set LRU_first and LRU_last to their proper values
		  ld (LRU_first), a
		  ld a, 84
		  ld (LRU_last), a
		  ret



; Calculate screen position
; INPUT
;	B: X position
;	C: Y position
; OUTPUT:
;	HL: screen position


calcscreenpos:
                ld a, c			; 4
		and $07			; 7  <-the 3 lowest bits are the line within a char
		ld h,a			; 4
		ld a,c			; 4  <- the top 2 bits are the screen third
		rra			; 4
		rra			; 4
		rra			; 4
		and $18			; 7
		or h			; 4
		or $C0			; 4	<- If the start address is 16384, this should be $40
		ld h,a			; 4 (total 50 t-states) H has the high byte of the address 
		
		ld a,b			;4
		rra			;4
		rra			;4
		rra			;4
		and $1f			;7  <- the top 5 bits are the char pos. The low 3 bits are the pixel pos
		ld l,a			;4
		ld a,c			;4
		rla			;4
		rla			;4
		and $e0			;7
		or l			;4
		ld l,a			;4 (total 54 t-states) L has the low byte of the address
		ret


; Definitions for sprite cache addresses

SprCacheTable 	EQU $8C00		; sprite cache table, 1K
LRU_next      	EQU $8B00		; cache list next pointers, 86 bytes used (some bytes wasted!)
LRU_prev      	EQU $8A00		; cache list prev pointers, 86 bytes used (some bytes wasted!)
MappingTable	EQU $8B56		; mapping from cache entries to the sprnum | rotation used. We are using the full address
LRU_first	db 0
LRU_last	db 84			; pointers to the first and last entry in the cache
SCRADD          dw 0
LINECOUNT       db 0
SAVE_RAMBANK	DB 0
Ship_Collision		db 0

SPRITES_BANK	EQU 1
LRU_LASTENTRY   EQU 85
;Multiply_by_96  dw 0,96,192,288,384,480,576,672,768,864,960,1056,1152,1248,1344,1440,1536,1632,1728,1824,1920,2016,2112,2208,2304,2400,2496,2592,2688,2784,2880,2976,3072,3168,3264,3360,3456,3552,3648,3744,3840,3936
Multiply_by_48   dw 0, 48, 96, 144, 192, 240, 288, 336, 384, 432, 480, 528, 576, 624, 672, 720, 768, 816, 864, 912, 960, 1008, 1056, 1104, 1152, 1200, 1248, 1296
		 dw 1344, 1392, 1440, 1488, 1536, 1584, 1632, 1680, 1728, 1776, 1824, 1872, 1920, 1968, 2016, 2064, 2112, 2160, 2208, 2256, 2304, 2352, 2400, 2448
		 dw 2496, 2544, 2592, 2640, 2688, 2736, 2784, 2832, 2880, 2928, 2976, 3024, 3072, 3120, 3168, 3216, 3264, 3312, 3360, 3408, 3456, 3504, 3552, 3600
	         dw 3648, 3696, 3744, 3792, 3840, 3888, 3936, 3984, 4032, 4080, 4128
