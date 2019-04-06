org 49152


NSTARS	EQU 32		; number of stars per plane

include "maindefs.asm"

; INPUT:
;	DE: last score. If higher than any of the current high scores
;	    we will ask the player to enter his-her name

mainmenu:
	call check_hiscore
	;  generate starfield
	call generate_starfield

	; load title screen
	ld hl, genesis_title
	ld de, 16384
	call depack
	call menu_display_inertia_status
	call menu_display_sound_status

menu_loop: 
	ld a, (fire_pressed_wait)
	and a
	jr z, menu_move_stars
	dec a
	ld (fire_pressed_wait), a
	ret z	
menu_move_stars:
	;move starfield
	ld a, (timer)
	inc a
	ld (timer), a
	jr nz, menu_nochange
	ld a, (screen_to_show)
	inc a
	cp 3
	jr nz, menu_nowrap
	xor a			; wrap from 2 to 0
menu_nowrap:
	ld (screen_to_show), a
	jr z, menu_puttitle
	cp 1
	jr z, menu_puthiscores
menu_putcredits:
	ld hl, 16384
	ld (hl), 0
	ld de, 16385
	ld bc, 6911
	ldir	
	ld hl, credits_bkg
	ld de, 16384
	call depack
	jr menu_nochange
menu_puthiscores:
	call showhiscores
	call menu_display_inertia_status
	call menu_display_sound_status
	jr menu_nochange
menu_puttitle:
	ld hl, 16384
	ld (hl), 0
	ld de, 16385
	ld bc, 6911
	ldir
	ld hl, genesis_title
	ld de, 16384
	call depack
	call menu_display_inertia_status
	call menu_display_sound_status
menu_nochange:
	halt
	call check_firepress
	jr nc, menu_nofirepressed		; fire not pressde
; fire pressed
	ld a, (fire_pressed_wait)
	and a
	jr nz, menu_nofirepressed		; ignore multiple fires
; Play sound
	ld hl,  FX_START_GAME
	push hl
        call _wyz_effect
        pop hl
	ld a, 20
	ld (fire_pressed_wait), a
menu_nofirepressed:
	; if we are at the credits screen, do not check for inertia or any other cheat!
	ld a, (screen_to_show)
	cp 2
	jr z, menu_continue
	call check_cheats
	jr nc, menu_check_sound
; Play sound
	ld hl, FX_MENU_INERTIA
	push hl
        call _wyz_effect
        pop hl

	ld a, (inertia_cheat)
	and a
	jr nz, menu_inertia_off
	ld b, 160
	ld c, 160
	ld ix, string_on
	call PrintString
	jr menu_continue
menu_inertia_off:
	ld b, 160
	ld c, 160
	ld ix, string_off
	call PrintString
menu_check_sound:
	call check_sound
	jr nc, menu_continue

	ld hl, FX_MENU_INERTIA
	push hl
        call _wyz_effect
        pop hl

	call menu_display_sound_status
	
menu_continue:
	call erase_starfield
	call move_starfield
	call display_starfield

	ld a, (screen_to_show)
	and a
	jp nz, menu_loop		; only update the animation if we are on the title screen

	ld a, (anim_frame)	; This little piece of code will change the animation position every 8 frames
	inc a
	ld (anim_frame), a
	and $3F
	call DrawStarAnim

	jp menu_loop
	ret

menu_display_inertia_status:
	ld a, (inertia_cheat)
	and a
	jr nz, start_inertia_off
	ld b, 160
	ld c, 160
	ld ix, string_on
	call PrintString
	ret

menu_display_sound_status:
	ld a, (sound_selection)
	and a
	jr z, menu_print_silence
	dec a
	jr z, menu_print_musiconly
	dec a
	jr z, menu_print_sfxonly
menu_print_music_and_sound:
	ld b, 80
	ld c, 184
	ld ix, string_music_and_fx
	call PrintString	
	ret
menu_print_sfxonly:
	ld b, 80
	ld c, 184
	ld ix, string_fx
	call PrintString	
	ret
menu_print_musiconly:
	ld b, 80
	ld c, 184
	ld ix, string_music
	call PrintString	
	ret
menu_print_silence:
	ld b, 80
	ld c, 184
	ld ix, string_silence
	call PrintString
	ret
start_inertia_off:
	ld b, 160
	ld c, 160
	ld ix, string_off
	call PrintString
	ret

screen_to_show db 0		; 0: title screen; 1: hi scores; 2: credits
timer db 0

hiscore_names:  db 93,94,94,95,96,91,91,91,0
		db "sejuan",91,91,0
		db "anjuel",91,91,0
		db "pagantip",0
		db "wyz",91,91,91,91,91,0
		db "tbrazil",91,0		; 6 high scores for now

hiscore_values: dw 2000, 1500, 1000, 500, 250, 100
hiscore_string ds 5

; INPUT:  DE: new potential hi score
check_hiscore:
	ld hl, hiscore_values
	ld a, 6
loop_hiscores:
	ld c, (hl)
	inc hl
	ld b, (hl)
	inc hl
	ex de, hl		; HL now has the high score, and DE the pointer
	push hl
	push de
	sbc hl, bc		; if bc is higher than hl, no new hi score
	pop hl
	pop de			; we POP in reverse order!
	jr nc, hiscore_found
	dec a
	jr nz, loop_hiscores
nohiscore_found:
	ret			; no new high score, just exit
hiscore_found:
	; in this case, we found the high score. We have to push the high scores down
	dec a
	jr z, hiscore_last ; A -1 scores we need to move
	push af
	push de
	add a, a  	     ; do it twice (words)
	ld b, a
	ld hl, hiscore_values+11
	ld de, hiscore_values+9
pushscore_loop:
	ld a, (de)
	ld (hl), a
	dec hl
	dec de
	djnz pushscore_loop	
	pop de
	ld (hl), d
	dec hl
	ld (hl), e	    ; and put the new high score
	pop af		    ; get again the number of scores to move
	push de 
	ld b, a
	add a, a	    ; A * 2
	add a, a	    ; A * 4
	add a, a	    ; A * 8
	add a, b	    ; A * 9 (9 bytes per name)
	ld de, hiscore_names + 53
	ld hl, hiscore_names + 44
	ld c, a
	ld b, 0
	lddr		    ; copy down all names	
	pop de
	jr hiscore_pushed
hiscore_last:
	ld hl, hiscore_values+11
	ld (hl), d
	dec hl
	ld (hl), e
hiscore_pushed:
	; now go backwards in the name list, and find the first score equal to the high score
	; DE still has the high score
	ld ix, hiscore_values+11
	ld iy, hiscore_names+45
	ld b, 5
findname_loop:
	ld a, (ix)
	dec ix
	ld h, a
	ld a, (ix)
	dec ix
	ld l, a
	xor a
	sbc hl, de
	jr z, foundname
	dec iy
	dec iy
	dec iy
	dec iy
	dec iy
	dec iy
	dec iy
	dec iy
	dec iy
	djnz findname_loop
foundname:
	ld (iy+0), 92
	ld (iy+1), 91
	ld (iy+2), 91
	ld (iy+3), 91
	ld (iy+4), 91
	ld (iy+5), 91
	ld (iy+6), 91
	ld (iy+7), 91
	push iy
	call showhiscores
	call menu_display_inertia_status
	pop hl
	ld b, 0				; B will be the counter for the current position
readloop:
	push bc
	push hl
	call SCAN_KEYBOARD		; read keyboard in A
	pop hl
	pop bc
	cp  13				; 13 is ENTER
	jr nz, read_noenter
	ld a, (hl)
	cp 92
	jr nz, read_exit				; do nothing if we are in the last char
	ld (hl), 91			; else, substitute the cursor by a space
read_exit:
        ; Play sound
	ld hl,  FX_START_GAME
	push hl
        call _wyz_effect
        pop hl
	ret
read_noenter:
	cp 'a'
	jr c, read_checkzero			; if less than a, check for zero (delete)
	cp 'z' + 1
	jr nc, read_checkdelete		; if higher than z, check for delete
	ld (hl), a			; store the new key press
	ld a, b
	cp 7
	jr z, read_continue		; don't go beyond 8 characters
	inc hl
	ld (hl),92
	inc b

	push bc
        push hl
	ld hl, FX_MENU_INERTIA
	push hl
        call _wyz_effect
        pop hl
        pop hl
        pop bc

	jr read_continue
read_checkzero:
	cp '0'
	jr z, read_deletepressed
read_checkdelete:
	cp 255
	jr nz, readloop
read_deletepressed:
	ld a, b
	and a
	jr z, readloop
	dec b
	ld (hl), 91
	dec hl
	ld (hl), 92
read_continue:
	push bc
	push hl
	call showhiscores_loop
	pop hl
	pop bc
	jr readloop
	ret


showhiscores:
	; load title screen
	ld hl, title_hiscores
	ld de, 16384
	call depack

showhiscores_loop:
	; loop through the hi score names
	ld b, 64
	ld c, 64
	ld ix, hiscore_names
	call PrintString
	ld b, 64
	ld c, 80
	ld ix, hiscore_names+9
	call PrintString
	ld b, 64
	ld c, 96
	ld ix, hiscore_names+18
	call PrintString
	ld b, 64
	ld c, 112
	ld ix, hiscore_names+27
	call PrintString
	ld b, 64
	ld c, 128
	ld ix, hiscore_names+36
	call PrintString
	ld b, 64
	ld c, 144
	ld ix, hiscore_names+45
	call PrintString

	; loop through the hi score names
	ld hl, (hiscore_values)
	ld ix, hiscore_string
	ld c, 64
	call printhi
	ld hl, (hiscore_values+2)
	ld ix, hiscore_string
	ld c, 80
	call printhi
	ld hl, (hiscore_values+4)
	ld ix, hiscore_string
	ld c, 96
	call printhi
	ld hl, (hiscore_values+6)
	ld ix, hiscore_string
	ld c, 112
	call printhi
	ld hl, (hiscore_values+8)
	ld ix, hiscore_string
	ld c, 128
	call printhi
	ld hl, (hiscore_values+10)
	ld ix, hiscore_string
	ld c, 144
	call printhi
	ret


; INPUT:
;	HL: hi score value
;	IX: hiscore_string
;	C: value in Y
printhi:
	push bc
	call decompose_5digit
	pop bc
	ld b, 152
	ld a, (ix+0)
	push bc	
	call PrintNumber
	pop bc
	ld b, 160
	ld a, (ix+1)
	push bc	
	call PrintNumber
	pop bc
	ld b, 168
	ld a, (ix+2)
	push bc	
	call PrintNumber
	pop bc
	ld b, 176
	ld a, (ix+3)
	push bc	
	call PrintNumber
	pop bc
	ld b, 184
	ld a, (ix+4)
	call PrintNumber
	ret




; Divide HL by BC
;
; HL: number
; BC: divider
; DE: result (HL / BC)
; HL: remainder

divide_large:
    xor a
    ld de, 0
divide_loop:
    sbc hl, bc
    jp c, divide_completed
    inc de
    jp divide_loop    
divide_completed:
    add hl, bc
    ret

; INPUT: HL: number
; 	 IX: string with the number to print

decompose_5digit:
	ld bc, 10000		; get the fifth digit
	call divide_large	; E has the result, HL the remainder
	ld (ix+0), e
	ld bc, 1000
	call divide_large
	ld (ix+1), e
	ld bc, 100
	call divide_large
	ld (ix+2), e
	ld bc, 10
	call divide_large
	ld (ix+3), e
	ld (ix+4), l		; L has the last remainder
	ret	


anim_frame db $0


JOY_KEMPSTON	EQU 0
JOY_SINCLAIR1	EQU 1
JOY_SINCLAIR2	EQU 2
JOY_KEYS	EQU 3

selected_joystick db 0
inertia_cheat	db 0

; Read joysticks, set carry if fire is pressed
check_firepress:
read_sinclair1_joystick:
	       	ld bc, $effe
	       	in c, (c)  ; Leemos solo la fila 6-0. Los bits a 0 están pulsados
	       	xor a
sinclair1_fire:
	       	rr c
	       	jr c,read_sinclair2_joystick
	       	; Sinclair 1 fire detected
	       	ld a, JOY_SINCLAIR1
		ld (selected_joystick),a
		scf 
		ret
read_sinclair2_joystick:
	       	ld bc, $f7fe

	      	in c, (c)  ; Leemos solo la fila 1-5. Los bits a 0 están pulsados
	       	ld a,c
		and $10
		jr nz, read_kempston
	       	; Sinclair 2 fire detected
	       	ld a, JOY_SINCLAIR2
		ld (selected_joystick),a
		scf 
		ret
read_kempston:
		ld c, 31
		in c, (c)		
		ld a, 255
		cp c
		jr z, read_keyb	; if the value read is 255, then there is no kempston interface
	    	ld a,c
		and $10
		jr z, read_keyb
	       	; Kempston fire detected
	       	ld a, JOY_KEMPSTON
		ld (selected_joystick),a
		scf 
		ret
read_keyb:
		ld bc, $7ffe	; ready to read!
		in a,(c)	; get the row in e
		and $01
		jr nz, no_fire
		; Keyboard fire (SPACE) detected
	       	ld a, JOY_KEYS
		ld (selected_joystick),a
		scf 
		ret
no_fire:
		xor a	; clear carry
		ret		

; Read certain keys to enable cheats, set carry if any cheat was enabled

key_i_pressed	db 0

check_cheats:
		ld bc, $dffe	; read row containing I
		in a, (c)
		and $04
		jr nz, no_cheat_pressed
		ld a, (key_i_pressed)
		and a
		jr z, cheat_pressed
		xor a	; clear carry		
		ret
cheat_pressed:
		ld a, (inertia_cheat)
		xor 1
		ld (inertia_cheat), a
		ld a, 1
		ld (key_i_pressed), a
		scf
		ret
no_cheat_pressed:
		ld a, (key_i_pressed)
		and a
		ret z	; key was already released, and carry flag is now 0
		xor a	; clear carry		
		ld (key_i_pressed), a
		ret

; Read keyboard to find if we want to change sound

key_s_pressed db 0
check_sound:
		ld bc, $fdfe	; read row containing S
		in a, (c)
		and $02
		jr nz, no_sound_pressed
		ld a, (key_s_pressed)
		and a
		jr z, sound_pressed
		xor a	; clear carry		
		ret
sound_pressed:
		ld a, (sound_selection)
		inc a
		and 3
		ld (sound_selection), a
		ld a, 1
		ld (key_s_pressed), a
		scf
		ret
no_sound_pressed:
		ld a, (key_s_pressed)
		and a
		ret z	; key was already released, and carry flag is now 0
		xor a	; clear carry		
		ld (key_s_pressed), a
		ret

; the starfield will be randomly generated. Y will be 32-160, X will be 0-255

; Supporting function
genstars:
		push hl
		push bc
		call rand
		pop bc
		ld a, l ; random value for X
		ld c, h	; random value for Y, in 0..127
		pop hl
		ld (hl), a
		inc hl

		ld a, c ; random value for Y, in 0..127
		add a, 28 ; now it is in 20..147

		ld (de), a
		inc de
		djnz genstars
		ret

; Another supporting function
genpixels:
		ld a, (hl)		; we have the byte in A
		and $7			;
		ld c, a			; C has the pixel addr
		ld a, $80
genpix_shiftloop:
		rrca			; rotate right
		dec c
		jr nz, genpix_shiftloop   ; until we reach the end		
		ld (de), a		; store 
		inc hl
		inc de
		djnz genpixels
		ret


generate_starfield:	

		ld hl, starsx_slow
		ld de, starsy_slow
		ld b, NSTARS
		call genstars
		ld hl, starsx_medium
		ld de, starsy_medium
		ld b, NSTARS
		call genstars
		ld hl, starsx_fast
		ld de, starsy_fast
		ld b, NSTARS
		call genstars

; we have generated the addresses, now we have to generate the pixels...

		ld hl, starsx_slow
		ld de, stars_pixels_slow
		ld b, NSTARS
		call genpixels

		; Medium speed pixels
		ld hl, starsx_medium
		ld de, stars_pixels_medium
		ld b, NSTARS
		call genpixels

		; Fast pixels
		ld hl, starsx_fast
		ld de, stars_pixels_fast
		ld b, NSTARS
		call genpixels

		ret

; ERASE STARS


erase_starfield:
	ld ix, starsx_slow
	ld iy, starsy_slow
	ld d, NSTARS
	call EraseLoop
	ld ix, starsx_medium
	ld iy, starsy_medium
	ld d, NSTARS
	call EraseLoop
	ld ix, starsx_fast
	ld iy, starsy_fast
	ld d, NSTARS
	call EraseLoop
	ret


EraseLoop:
	ld a, (iy)	; Y position
	rlca
	rlca		; multiply by 4
	ld l, a
	and 3
	add a, 88	; 88*256=22528, start of attributes
	ld h, a
	ld a, l
	and 224		; mask low byte of Y*32
	ld l, a
	ld a, (ix)	; X position
	rrca
	rrca
	rrca		; divide by 8 to create the char addr
	and $1F		; mask so that it is not more than 31
	add a, l	; add to low byte
	ld l, a		; HL=address of attribute
	ld a, (hl)

	cp 7
	jp nz, erase_donterase

	ld a, (ix)
	ld b, a		; X position
	ld a, (iy)
	ld c, a		; Y position
	call CalcScreenPos
	xor a
	ld (hl), a	; place pixel on screen

erase_donterase:
	inc ix
	inc iy
	
	dec d
	jp nz, EraseLoop		; continue with loop
	ret





; UPDATE STAR POSITION

move_starfield:
	ld ix, starsx_slow
	ld hl, stars_pixels_slow
	ld b, NSTARS
updateslow_loop:
	inc (ix)			; update value 1 pixel right
	ld a, (hl)
	rrca				; rotate right 1 pixel
	ld (hl), a
	inc hl
	inc ix
	djnz updateslow_loop


	ld ix, starsx_medium
	ld hl, stars_pixels_medium
	ld b, NSTARS
updatemedium_loop:
	inc (ix)			; update value 2 pixels right
	inc (ix)
	ld a, (hl)
	rrca				; rotate right 2 pixels
	rrca
	ld (hl), a
	inc hl
	inc ix
	djnz updatemedium_loop



	ld ix, starsx_fast
	ld hl, stars_pixels_fast
	ld b, NSTARS
updatefast_loop:
	inc (ix)			; update value 4 pixels right
	inc (ix)
	inc (ix)
	inc (ix)
	ld a, (hl)
	rrca				; rotate right 1 pixels
	rrca
	rrca
	rrca
	ld (hl), a
	inc hl
	inc ix
	djnz updatefast_loop

	ret


; DISPLAY STARFIELD

starfield_variable db 0

display_starfield:
	ld ix, starsx_slow
	ld iy, starsy_slow
	ld de, stars_pixels_slow
	ld a, NSTARS
	ld (starfield_variable), a
	call DisplayLoop
	
	ld ix, starsx_medium
	ld iy, starsy_medium
	ld de, stars_pixels_medium
	ld a, NSTARS
	ld (starfield_variable), a
	call DisplayLoop

	ld ix, starsx_fast
	ld iy, starsy_fast
	ld de, stars_pixels_fast
	ld a, NSTARS
	ld (starfield_variable), a
	call DisplayLoop
	ret

DisplayLoop:
	ld a, (iy)	; Y position
	rlca
	rlca		; multiply by 4
	ld l, a
	and 3
	add a, 88	; 88*256=22528, start of attributes
	ld h, a
	ld a, l
	and 224		; mask low byte of x*32
	ld l, a
	ld a, (ix)	; X position
	rrca
	rrca
	rrca		; divide by 8 to create the char addr
	and $1F		; mask so that it is not more than 31
	add a, l	; add to low byte
	ld l, a		; HL=address of attribute
	ld a, (hl)

	cp 7
	jp nz, disp_dontdraw

	ld a, (ix)
	ld b, a		; X position
	ld a, (iy)
	ld c, a		; Y position
	call CalcScreenPos
	ld a, (de)
	ld (hl), a	; place pixel on screen

disp_dontdraw:

	inc ix
	inc iy
	inc de
	
	ld a, (starfield_variable)
	dec a
	ld (starfield_variable), a
	jp nz, DisplayLoop		; continue with loop
	ret


	


starsx_slow 	ds NSTARS
starsx_medium 	ds NSTARS
starsx_fast	ds NSTARS

starsy_slow 	ds NSTARS
starsy_medium 	ds NSTARS
starsy_fast	ds NSTARS

stars_pixels_slow   ds NSTARS
stars_pixels_medium ds NSTARS
stars_pixels_fast   ds NSTARS

star_anim:
	DEFB	  0,  0,  0,  0,  0,  0,  0,  0		; first frame
	DEFB	  0,  0,  0,  0,  0,  0,  0,  0
	DEFB	  0,  0,  0,  0,  0,  0,  0,  0
	DEFB	  0,  0,  0,  0,  0,  0,  0,  0

	DEFB	  0,  0,  0,  0,  0,  0,  0,  0		; second frame
	DEFB	  0,  0,  0,  1,  0,  1,  1, 22
	DEFB	  1,  0,  0,  0,  0,  0,  0,  0
	DEFB	  0,  0,  0,  0,  0,  0,  0,128

	DEFB	  0,  0,  0,  0,  0,  0,  0,  0		; third frame
	DEFB	  0,  0,  0,  1,  8,  5,  3, 22
	DEFB	  1,  1,  0,  0,  0,  0,  0,  0
	DEFB	  0,  0,  0,  0,  0,  0,  0,192

	DEFB	  0,  0,  0,  0,  0,  0,  0,  1		; fourth frame
	DEFB	128,  0, 32,  1,  8,  5,  3,150
	DEFB	  1,  1,  0,  1,  0,  0,  0,  0
	DEFB	  0,  0,  0,  0,  0,  0,  0,208

anim_order: 	db	0,0,0,0,0,0,0,0
	    	db	1,1,1,1,2,2,2,2
		db 	2,2,3,3,3,3,3,3
		db	3,3,3,3,3,3,3,3
		db	3,3,3,3,3,3,3,3
		db	2,2,2,2,2,2,1,1
		db	1,1,0,0,0,0,0,0
		db	0,0,0,0,0,0,0,0
		db	0,0,0,0,0,0,0,0

; INPUT: 
; 	A: Animation position (0-7)

DrawStarAnim:	;  draw the animation
	ld hl, anim_order
	ld d,0
	ld e,a
	add hl, de
	ld a, (hl)	; A holds the animation position
	ld hl, 0
	and a
	jr z, noadd
	ld de, 32
dr_chooseanimpos:
	add hl, de
	dec a
	jr nz, dr_chooseanimpos
noadd:
	ld de, star_anim
	add hl, de
	ex de, hl
	ld b, 4
	ld c, 3
	push de
	call PrintTile
	pop de

	ld hl, 8
	add hl, de
	ex de, hl
	ld b, 4
	ld c, 4
	push de
	call PrintTile
	pop de

	ld hl, 8
	add hl, de
	ex de, hl
	ld b, 4
	ld c, 5
	push de
	call PrintTile
	pop de

	ld hl, 8
	add hl, de
	ex de, hl
	ld b, 5
	ld c, 4
	push de
	call PrintTile
	pop de
	ret

; DE = puntero al tile
; B  = X (0-31)
; C  = Y (0-23)
PrintTile:
	ld hl, TileScAddress	; address table
	ld a, c
	add a,c			; C = 2*Y, to address the table
	ld c,a
	ld a, b			; A = X
	ld b,0			; Clear B for the addition
	add hl, bc		; hl = address of the first tile
	ld c, (hl)
	inc hl
	ld b, (hl)		; BC = Address
	ld l,a			; hl = X
	ld h, 0
	add hl, bc		; hl = tile address in memory
	ld a, (de)
	ld (hl), a
	inc h
	inc de
	ld a, (de)
	ld (hl), a
	inc h
	inc de
	ld a, (de)
	ld (hl), a
	inc h
	inc de
	ld a, (de)
	ld (hl), a
	inc h
	inc de
	ld a, (de)
	ld (hl), a
	inc h
	inc de
	ld a, (de)
	ld (hl), a
	inc h
	inc de
	ld a, (de)
	ld (hl), a
	inc h
	inc de
	ld a, (de)
	ld (hl), a	
	ret


TileScAddress:	; Screen address for each tile start
	dw 16384 ; Y = 0
	dw 16416 ; Y = 1
	dw 16448 ; Y = 2
	dw 16480 ; Y = 3

	dw 16512 ; Y = 4
	dw 16544 ; Y = 5
	dw 16576 ; Y = 6
	dw 16608 ; Y = 7

	dw 18432 ; Y = 8
	dw 18464 ; Y = 9
	dw 18496 ; Y = 10
	dw 18528 ; Y = 11
	dw 18560 ; Y = 12
	dw 18592 ; Y = 13
	dw 18624 ; Y = 14
	dw 18656 ; Y = 15

	dw 20480 ; Y = 16
	dw 20512 ; Y = 17
	dw 20544 ; Y = 18
	dw 20576 ; Y = 19
	dw 20608 ; Y = 20
	dw 20640 ; Y = 21
	dw 20672 ; Y = 22
	dw 20704 ; Y = 23



; Input:
;	B: X position
;	C: Y position
; Output:
;        HL: screen address
;        B:  bit position

CalcScreenPos:
     ld a, c
     and $07			; 7  <-the 3 lowest bits are the line within a char
     ld h,a			; 4
     ld a,c			; 4  <- the top 2 bits are the screen third
     rra			; 4
     rra			; 4
     rra			; 4
     and $18			; 7
     or h			; 4
     or $40			; 4	<- If the start address is 16384, this should be $40
     ld h,a			; 4 (total 50 t-states) H has the high byte of the address

     ld a, b			;4
     rra			;4
     rra			;4
     rra			;4
     and $1f			;7  <- the top 5 bits are the char pos. The low 3 bits are the pixel pos
     ld l,a			;4
     ld a,c      		;4
     rla			;4
     rla			;4
     and $e0			;7
     or l			;4
     ld l,a			;4 (total 54 t-states) L has the low byte of the address
                                ; HL has the address in RAM of the char

     ld a, b
     and $07                    ; the lowest three bits are the bit position
     inc a

     ld b,a                      ; b has the number of bits to shift 1
     ld a,$80
     dec b
     jr z, postloop
shiftloop:
     rrca
     dec b
     jr nz, shiftloop
postloop:
     ld b, a
     ret




; aPPack decompressor
; original source by dwedit
; very slightly adapted by utopian
; optimized by Metalbrain

;hl = source
;de = dest

depack:		ld	ixl,128
apbranch1:	ldi
aploop0:	ld	ixh,1		;LWM = 0
aploop:		call 	ap_getbit
		jr 	nc,apbranch1
		call 	ap_getbit
		jr 	nc,apbranch2
		ld 	b,0
		call 	ap_getbit
		jr 	nc,apbranch3
		ld	c,16		;get an offset
apget4bits:	call 	ap_getbit
		rl 	c
		jr	nc,apget4bits
		jr 	nz,apbranch4
		ld 	a,b
apwritebyte:	ld 	(de),a		;write a 0
		inc 	de
		jr	aploop0
apbranch4:	and	a
		ex 	de,hl 		;write a previous byte (1-15 away from dest)
		sbc 	hl,bc
		ld 	a,(hl)
		add	hl,bc
		ex 	de,hl
		jr	apwritebyte
apbranch3:	ld 	c,(hl)		;use 7 bit offset, length = 2 or 3
		inc 	hl
		rr 	c
		ret 	z		;if a zero is encountered here, it is EOF
		ld	a,2
		adc	a,b
		push 	hl
		ld	iyh,b
		ld	iyl,c
		ld 	h,d
		ld 	l,e
		sbc 	hl,bc
		ld 	c,a
		jr	ap_finishup2
apbranch2:	call 	ap_getgamma	;use a gamma code * 256 for offset, another gamma code for length
		dec 	c
		ld	a,c
		sub	ixh
		jr 	z,ap_r0_gamma		;if gamma code is 2, use old r0 offset,
		dec 	a
		;do I even need this code?
		;bc=bc*256+(hl), lazy 16bit way
		ld 	b,a
		ld 	c,(hl)
		inc 	hl
		ld	iyh,b
		ld	iyl,c

		push 	bc
		
		call 	ap_getgamma

		ex 	(sp),hl		;bc = len, hl=offs
		push 	de
		ex 	de,hl

		ld	a,4
		cp	d
		jr 	nc,apskip2
		inc 	bc
		or	a
apskip2:	ld 	hl,127
		sbc 	hl,de
		jr 	c,apskip3
		inc 	bc
		inc 	bc
apskip3:	pop 	hl		;bc = len, de = offs, hl=junk
		push 	hl
		or 	a
ap_finishup:	sbc 	hl,de
		pop 	de		;hl=dest-offs, bc=len, de = dest
ap_finishup2:	ldir
		pop 	hl
		ld	ixh,b
		jr 	aploop

ap_r0_gamma:	call 	ap_getgamma		;and a new gamma code for length
		push 	hl
		push 	de
		ex	de,hl

		ld	d,iyh
		ld	e,iyl
		jr 	ap_finishup


ap_getbit:	ld	a,ixl
		add	a,a
		ld	ixl,a
		ret	nz
		ld	a,(hl)
		inc	hl
		rla
		ld	ixl,a
		ret

ap_getgamma:	ld 	bc,1
ap_getgammaloop: call 	ap_getbit
		rl 	c
		rl 	b
		call 	ap_getbit
		jr 	c,ap_getgammaloop
		ret

rand_seed	dw 12345

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


; Input:
;	B: X position
;	C: Y position
; Output:
;        HL: screen address

CalcScreenPos_Char:
     ld a,c			; 4  <- the top 2 bits are the screen third
     rra			; 4
     rra			; 4
     rra			; 4
     and $18			; 7
     or $40			; 4	<- If the start address is 16384, this should be $40
     ld h,a			; 4 (total 31 t-states) H has the high byte of the address

     ld a, b			;4
     rra			;4
     rra			;4
     rra			;4
     and $1f			;7  <- the top 5 bits are the char pos. The low 3 bits are the pixel pos
     ld l,a			;4
     ld a,c      		;4
     rla			;4
     rla			;4
     and $e0			;7
     or l			;4
     ld l,a			;4 (total 54 t-states) L has the low byte of the address
                                ; HL has the address in RAM of the char
     ret

; Print normal char on screen (1x1), from a base number 0..9
; INPUT:
;		B: position in X (pixels)
;		C: position in Y (pixels)
;		A: number to print

PrintNumber:
	push af
	call CalcScreenPos_Char	; Screen position in HL
	ex de, hl
	ld hl, numfont
	pop af
	rlca
	rlca
	rlca
	and $f8			; multiply by 8
	ld c, a
	ld b, 0
	add hl, bc		
	ex de, hl		; HL points to the screen position, DE to the first row of the number
	ld b, 8
printnum_loop:
	ld a, (de)
	inc de
	ld (hl), a
	inc h
	djnz printnum_loop
	ret




; Print string on screen
; INPUT:
;		B: position in X (pixels)
;		C: position in Y (pixels)
;		IX: pointer to string, terminated by 0


PrintString:
	call CalcScreenPos_Char	; Screen position in HL
	ex de, hl

string_outerloop:
	ld a, (ix+0)
	and a
	ret z			; terminate with null
	sub 'a'			; a is the first char
	rlca
	rlca
	rlca
	and $f8			; multiply by 8

	ld hl, charfont
	ld c, a
	ld b, 0
	add hl, bc	
	
	ex de, hl		; HL points to the screen position, DE to the first row of the char
	push hl
	ld b, 8
string_innerloop:
	ld a, (de)
	inc de
	ld (hl), a
	inc h
	djnz string_innerloop

	pop de
	inc de			; next char
	inc ix
	jp string_outerloop

	ret


string_on db "n",91,0
string_off db "ff",0



string_silence		db 91,91,"silence",91,91,91,0
string_music_and_fx	db "music",91,"and",91,"fx",0
string_music		db 91,"music",91,"only",91,0
string_fx		db 91,91,"fx",91,"only",91,91,91,0

; Character font
charfont:
	DEFB	  0,127,  1,127, 65,127,  0,  0
	DEFB	 64,127, 65, 65, 65,127,  0,  0
	DEFB	  0,127, 64, 64, 64,127,  0,  0
	DEFB	  1,127, 65, 65, 65,127,  0,  0
	DEFB	  0,127, 65,127, 64,127,  0,  0
	DEFB	  0,127, 64,127, 64, 64,  0,  0
	DEFB	  0,127, 65, 65,127,  1,127,  0
	DEFB	 64,127, 65, 65, 65, 65,  0,  0
	DEFB	  0,  8,  8,  8,  8,  8,  0,  0
	DEFB	  0,  2,  2,  2,  2,  2,126,  0
	DEFB	  0, 65, 65,126, 65, 65,  0,  0
	DEFB	  0, 64, 64, 64, 64,127,  0,  0
	DEFB	  0,127, 73, 73, 73, 73,  0,  0
	DEFB	  0,127, 65, 65, 65, 65,  0,  0
	DEFB	  0,127, 65, 65, 65,127,  0,  0
	DEFB	  0,127, 65, 65, 65,127, 64,  0
	DEFB	  0,127, 65, 65, 65,127,  1,  0

	DEFB	  0,127, 64, 64, 64, 64,  0,  0
	DEFB	  0,127, 64,127,  1,127,  0,  0
	DEFB	 64,126, 64, 64, 64,127,  0,  0
	DEFB	  0, 65, 65, 65, 65,127,  0,  0
	DEFB	  0, 65, 65, 34, 20,  8,  0,  0
	DEFB	  0, 73, 73, 73, 73,127,  0,  0
	DEFB	  0, 65, 34, 28, 34, 65,  0,  0
	DEFB	  0, 65, 65, 65,127,  1,127,  0
	DEFB	  0,127,  3, 28, 96,127,  0,  0
	DEFB	  0,  0,  0,  0,  0,  0,  0,  0		; ARTIFICIAL char for the space, number 91
	DEFB	$7E,$7E,$7E,$7E,$7E,$7E,$7E,$7E		; ARTIFICIAL char for the cursor, number 92
	DEFB	 60, 98, 98,126, 98, 98, 98,  0		; inverted A, number 93
	DEFB	124,  6,  6, 62,  6,  6,  6,  0		; inverted F, number 94
	DEFB	 60, 98, 98, 98, 98, 98, 60,  0		; inverted O, number 95
	DEFB	124, 64, 64, 64, 70, 70,124,  0		; inverted J, number 96

; Number font
numfont:
	DEFB	  0,127, 65, 73, 65,127,  0,  0
	DEFB	  0, 24,  8,  8,  8,  8,  0,  0
	DEFB	  0,127,  1,127, 64,127,  0,  0
	DEFB	  0,127,  1,127,  1,127,  0,  0
	DEFB	  0, 65, 65,127,  1,  1,  0,  0
	DEFB	  0,127, 64,127,  1,127,  0,  0
	DEFB	  0,127, 64,127, 65,127,  0,  0
	DEFB	  0,127,  1,  1,  1,  1,  0,  0
	DEFB	  0,127, 65,127, 65,127,  0,  0
	DEFB	  0,127, 65,127,  1,127,  0,  0

; Scan the keyboard to find a single keypress
; Input: n/a
; Output: key scan code, in A

KeyCodes:
   defb 255,'z','x','c','v'      ; CAPS SHIFT, Z, X, C, V
   defb 'a','s','d','f','g'      ; A, S, D, F, G
   defb 'q','w','e','r','t'      ; Q, W, E, R, T
   defb '1','2','3','4','5'      ; 1, 2, 3, 4, 5
   defb '0','9','8','7','6'      ; 0, 9, 8, 7, 6
   defb 'p','o','i','u','y'      ; P, O, I, U, Y
   defb 13,'l','k','j','h'       ; ENTER, L, K, J, H
   defb ' ',254,'m','n','b'      ; SPACE, SYM SHIFT, M, N, B


SCAN_KEYBOARD:
	LD BC, $FEFE	; This is the first row, we will later scan all of them
	LD HL,KeyCodes  ; Let's go to the KeyCode table
	LD A,8		; loop counter
	
scan_loop:
	IN E, (C)	; Read the row status
	LD D, 5		; We just need to do it 5 times per scan line
find_keypress:
	RR E
	JR NC, keyfound	; we found a pressed key!	
	INC HL		; if not, go to the next scan code
	DEC D
	JR NZ, find_keypress ; try next key
	RLC B
	DEC A
	JR NZ, scan_loop	; back to the scan loop. This will repeat forever until a key press is found					
	JR SCAN_KEYBOARD	; if not, restart again		
keyfound:
	LD A,(HL)	; This is the scan code. We are not going back to the main loop, so we can reuse A
	PUSH AF
waitforrelease:
       	XOR A
        IN A, (C)  
       	CPL 
       	AND $1F
       	JR NZ, waitforrelease ; some key in this row is still pressed                   
       	POP AF
	RET       

fire_pressed_wait	db 0			; If we have pressed fire, wait for two seconds before exiting (to allow effect to play)
sound_selection		db 3			; 0: Silence; 1: Music only; 2: SFX ony; 3: Musix+SFX

genesis_title INCBIN "genesis_title.bin"
title_hiscores INCBIN "title_hiscores.bin"
credits_bkg INCBIN "credits_bkg.bin"

org 57343
db 0
; just to have the last byte
