NSTARS	EQU 24		; number of stars per plane

starshl_slow 	ds NSTARS*2	; we will hold the address (dw) directly
starshl_medium 	ds NSTARS*2 

;stars_pixels_slow   ds NSTARS
;stars_pixels_medium ds NSTARS
stars_pixels_slow   EQU $8A57
stars_pixels_medium EQU $8A57+NSTARS


; the starfield will be randomly generated. Y will be 0-127, X will be 0-255

; Supporting function
; IX: starshl_X
; DE: stars_pixels_X
; a: number of stars

stars_var	db 0
star_iteration  db 0

genstars:	ld (stars_var), a
		call rand
		ld a, l ; random value for X
		ld b, a	; force it to be an even number
		ld c, h	; random value for Y, in 0..127			
		call calcscreenpos
		ld (ix), l
		inc ix
		ld (ix),h		; Store address in array
		inc ix
		call genpixels		; genpixels already stores in (DE)
		inc de
		ld a, (stars_var)
		dec a
		ld (stars_var), a
		jp nz, genstars
		ret

; Another supporting function
; INPUT
;	DE: address of pixel char
;	 B: X for pixel

genpixels:
		ld a, b		; we have the byte in A
		and $7			;
		ld b, a			; C has the pixel addr
		ld a, $80
genpix_shiftloop:
		rrca			; rotate right
		dec b
		jr nz, genpix_shiftloop   ; until we reach the end		
		ld (de), a		; store 
		ret


; UPDATE STAR POSITION

; Inner loop
; INPUT:
;	DE: starshl
;	HL: starshl_pixels
;	 B: NSTARS

update_loop:
	xor a
	ld a, (hl)
	rla				; rotate right 1 pixel
	jr nc, nojump_char			; we have not moved to the char left

	or 1
	ld (hl), a
	ld a, (de)
	and $e0		; mask out the column
	ld c, a		; and store in C
	ld a, (de)
	dec a		; update value 1 char left
	and $1f		; only keep the column
	or c		; and get the value back
	ld (de), a	; and store back
	jr jump_char

nojump_char:
	ld (hl), a
jump_char:
	inc hl
	inc de
	inc de
	djnz update_loop
	ret




MoveStarfield:
	ld de, starshl_slow
	ld hl, stars_pixels_slow
	ld b, NSTARS
	call update_loop


	ld a, (star_iteration)
	xor 1
	and a
	ld (star_iteration), a
	ret z				; we update this once every 2 pixels

	ld de, starshl_medium
	ld hl, stars_pixels_medium
	ld b, NSTARS
	call update_loop
	ret


; Generate starfield

GenStarfield:	

		ld ix, starshl_slow
		ld de, stars_pixels_slow 
		ld a, NSTARS
		call genstars
		ld ix, starshl_medium
		ld de, stars_pixels_medium
		ld a, NSTARS
		call genstars

		ret

; Display starfield

Display_starfield:
	ld hl, starshl_slow
	ld de, stars_pixels_slow
	ld a, NSTARS*2

DisplayLoop:
	ex af, af'
	ld c, (hl)
	inc hl
	ld b, (hl)
	inc hl		; HL has the exact address in screen

	ld a, (bc)
	and a
	jp nz, disp_dontdraw

	ld a, (de)	; fetch pixel
	ld (bc), a	; place pixel on screen

disp_dontdraw:
	inc de
	ex af, af'
	dec a
	jp nz, DisplayLoop		; continue with loop
	ret



rand_seed	dw 24581

; Blatant copy from the z88dk RAND routine

rand:
   ld   hl,(rand_seed)
   ld   a,h
   add  a,a                     ;Set highest bit of seed to carry
   rl   l                       ;rotate L left (C<=L<=C)
   rl   h                       ;rotate H left (C<=L<=C)
   add  a,a                     ;Set second highest bit of seed to carry
   rl   l
   rl   h
   add  a,a
   rl   l
   rl   h
   ld   bc,$7415
   add  hl,bc                   ;Add $7415 to HL
   ld   (rand_seed),hl
   res     7,h                  ;force to be +ve
   ret
