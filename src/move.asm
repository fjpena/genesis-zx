org $E000

include	"maindefs.asm"

; Simple go left movement
; param1: speed

move_left:
; Entry: HL is the struct Enemy
	ld a, (hl)		; a == e->x
	ld d, h
	ld e, l			; save e in DE
	ld bc, 6
	add hl, bc		; HL == e->param1
	sub (hl)		; substract param1
	jr c, goneleft		; gone by the left
	ld (de), a
	ret

goneleft:
	ex de, hl		; HL has now e->x
	ld bc, 2
	add hl, bc
	xor a
	ld (hl), a		; e->sprnum = 0
	ret


; Simple go left movement, expire after a certain amount of frames
; param1: speed
; param2: frames to expire

move_left_expire:
; Entry: HL is the struct Enemy
	ld a, (hl)		; a == e->x
	ld d, h
	ld e, l			; save e in DE
	ld bc, 6
	add hl, bc		; HL == e->param1
	sub (hl)		; substract param1
	jr c, goneleft4		; gone by the left
	ld (de), a
	inc hl			; point to e->param2
	dec (hl)
	jr z, goneleft4		; die after a certain amount of frames
	ret

goneleft4
	ex de, hl		; HL has now e->x
	ld bc, 2
	add hl, bc
	xor a
	ld (hl), a		; e->sprnum = 0
	ret

; Simple go right movement
; param1: speed

move_right:
; Entry: HL is the struct Enemy
	ld a, (hl)		; a == e->x
	ld d, h
	ld e, l			; save e in DE
	ld bc, 6
	add hl, bc		; HL == e->param1
	add a, (hl)		; add param1
	ld b, 239
	cp b
	jr nc, goneright	; gone by the right
	ld (de), a
	ret
goneright
	ex de, hl		; HL has now e->x
	ld bc, 2
	add hl, bc
	xor a
	ld (hl), a		; e->sprnum = 0
	ret

; ping-pong movement
; param1: 0 (move left), 1 (move right)
; param2: speed

move_pingpong:
; Entry: HL is the struct Enemy
	ld d, h
	ld e, l			; save e in DE
	ld bc, 6
	add hl, bc		; HL == e->param1
	ld a, (hl)
	and a
	ld h,d
	ld l,e
	jr nz, pingpong_right
pingpong_left			; moving left
	ld a, (hl)		; a == e->x
	ld bc, 7
	add hl, bc		; HL == e->param2
	sub (hl)		; substract param2
	jr c, pingpong_goneleft		; gone by the left
	ld (de), a
	ret
pingpong_goneleft
	ex de, hl		; HL has now e->x
	ld bc, 6
	add hl, bc
	ld a, 1
	ld (hl), a		; e->param1 = 1
	ret
pingpong_right
	ld a, (hl)		; a == e->x
	ld bc, 7
	add hl, bc		; HL == e->param2
	add a,(hl)		; add param2
	ld b, 239
	cp b
	jr nc, pingpong_goneright	; gone by the right
	ld (de), a
	ret
pingpong_goneright
	ex de, hl		; HL has now e->x
	ld bc, 6
	add hl, bc
	xor a
	ld (hl), a		; e->param1 = 0
	ret

; ping-pong movement for final enemy 4
; param1: 0 (move left), 1 (move right)
; param2: speed

move_pingpong_final4:
; Entry: HL is the struct Enemy
	ld d, h
	ld e, l			; save e in DE
	ld bc, 6
	add hl, bc		; HL == e->param1
;	add (hl)		; add param1
	ld a, (hl)
	and a
	ld h,d
	ld l,e
	jr nz, pingpong4_right
pingpong4_left			; moving left
	ld a, (hl)		; a == e->x
	ld bc, 7
	add hl, bc		; HL == e->param2
	sub (hl)		; substract param2
	jr c, pingpong4_goneleft		; gone by the left
	ld (de), a
	ret
pingpong4_goneleft
	ex de, hl		; HL has now e->x
	ld bc, 6
	add hl, bc
	ld a, 1
	ld (hl), a		; e->param1 = 1
	ret

pingpong4_right
	ld a, (hl)		; a == e->x
	ld bc, 7
	add hl, bc		; HL == e->param2
	add a,(hl)		; add param2
	ld b, 239-72
	cp b
	jr nc, pingpong4_goneright	; gone by the right
	ld (de), a
	ret
pingpong4_goneright
	ex de, hl		; HL has now e->x
	ld bc, 6
	add hl, bc
	xor a
	ld (hl), a		; e->param1 = 0
	ret

; Movement type for explosions
; e->param1 is the counter for the explosion, goes from 4 down to 0

move_explosion:
	ld d, h
	ld e, l			; save e in DE
	ld bc, 6
	add hl, bc		; HL == e->param1
	dec (hl)		; decrement param1
	jr z, end_explosion
	ld bc, -4
	add hl, bc		; HL == e->sprnum
	inc (hl)
	ret
end_explosion
	ld bc, -4
	add hl, bc		; HL == e->sprnum
	xor a
	ld (hl), a		
	ret

; Furby movement
; This is a 4-step movement
; 	0: walk left for a while (X frames)
; 	1: Look left, then right, while standing (X frames)
;	2: Jump, then fall
;	3: When back to the floor, continue walking

; Param2 defines the state
move_furby:
	; STEP 1: move left

	ld a, (hl)		; a == e->x
	ld d, h
	ld e, l			; save e in DE
	ld bc, 6
	add hl, bc		; HL == e->param1
	sub (hl)		; substract param1
	jr c, furby_goneleft		; gone by the left
	ld (de), a
	jp furby_animate
furby_goneleft
	ld h, d
	ld l, e		; HL has now e->x
	ld bc, 2
	add hl, bc
	xor a
	ld (hl), a		; e->sprnum = 0
	ret
furby_animate	
	; STEP 2: process animation and move in Y
	inc hl			; HL now points to param2
	inc (hl)	
	ld a, (hl)
	cp $20			
	jp c, furby_anim1
	cp $40
	jp c, furby_anim2
	cp $60
	jp c, furby_anim3
	cp $80
	ret nc
furby_anim4
	inc de
	ld a, (de)
	add a, 2
	ld (de), a		; e->y += 2
	inc de
	inc de
	ld a, (de)		; a = e->type
	ld hl, _enemy_sprites
	ld c, a
	ld b, 0
	add hl, bc
	ld a, (hl)		
	add a, 9		; a=enemy_sprites[e->type] + 9
	dec de			
	ld (de), a		; e->sprnum = enemy_sprites[e->type] + 9;
	ret
furby_anim3
	inc de
	ld a, (de)
	add a, -2
	ld (de), a		; e->y -= 2
	inc de
	inc de
	ld a, (de)		; a = e->type
	ld hl, _enemy_sprites
	ld c, a
	ld b, 0
	add hl, bc
	ld a, (hl)		
	add a, 8		; a=enemy_sprites[e->type] + 8
	dec de			
	ld (de), a		; e->sprnum = enemy_sprites[e->type] + 8;
	ret
furby_anim2
	inc de
	inc de
	inc de
	ld a, (de)		; a = e->type
	push hl
	ld hl, _enemy_sprites
	ld c, a
	ld b, 0
	add hl, bc
	ld a, (hl)		
	add a, 4		; a=enemy_sprites[e->type] + 4
	pop hl			; HL points to e->param2
	ld c, a
	ld a, (hl)
	rrca
	rrca
	and 3
	add a, c		; a = enemy_sprites[e->type] + 4 + ((e->param2 >>2)& 0x3);
	dec de	
	ld (de), a		; e->sprnum = enemy_sprites[e->type] + 4 + ((e->param2 >>2)& 0x3);
	dec de
	dec de			; DE points back to e->x
	ld a, (de)
	dec hl			; HL points to e->param1
	add a, (hl)
	sub 2			
	ld (de), a
	ret
furby_anim1
	inc de
	inc de
	inc de
	ld a, (de)		; a = e->type
	push hl
	ld hl, _enemy_sprites
	ld c, a
	ld b, 0
	add hl, bc
	ld a, (hl)		; a=enemy_sprites[e->type]
	pop hl			; HL points to e->param2
	ld c, a
	ld a, (hl)
	rrca
	rrca
	rrca
	and 3
	add a, c		; a = enemy_sprites[e->type]  + ((e->param2 >>3)& 0x3);
	dec de	
	ld (de), a		; e->sprnum = enemy_sprites[e->type] + ((e->param2 >>3)& 0x3);	
	ret

; Kamikaze movement
; 2-phase movement
; 	1: While changing animation, move towards ship
;	2: once the animation reaches the end, fly directly to ship
; Always 4 animation frames
; param1 defines speed in X

move_kamikaze:
	; STEP 1: move left

	ld a, (hl)		; a == e->x
	ld d, h
	ld e, l			; save e in DE
	ld bc, 6
	add hl, bc		; HL == e->param1
	sub (hl)		; substract param1
	jr c, kamikaze_goneleft		; gone by the left
	ld (de), a
	jp kamikaze_moveiny
kamikaze_goneleft
	ld h, d
	ld l, e		; HL has now e->x
	ld bc, 2
	add hl, bc
	xor a
	ld (hl), a		; e->sprnum = 0
	ret
	; STEP 2: move in Y

kamikaze_moveiny
	inc de			; DE points to e->y
	ld a, (de)
	ld hl, _ship_y		
	cp (hl)			; compare e->y with ship_y
	jr c, kamikaze_moveup
	jr z, kamikaze_animate	; if we are exactly in the same Y, do nothing
kamikaze_movedown		; e->y > ship_y, so go down
	sub 4			; This is a bit subtle. We substract 4 to add 2, so we actually substract 2 and store
kamikaze_moveup
	add a, 2
	ld (de), a		; store the new Y
        ; let us check if the difference is 1 in terms of Y. If so, just make it the same
        sub (hl)
        inc a
        cp 3
        jr nc, kamikaze_animate
        ld a, (hl)
        ld (de), a

	; STEP 3: animate ship

kamikaze_animate
	ld h, d
	ld l, e			; HL also points to e->y
	ld bc, 6
	add hl, bc		; HL points now to e->param2
	ld a, (hl)
	cp $1f
	jr nc, kamikaze_noinc
	inc a
	ld (hl), a		; e->param2 is lower than $1f, so increment
kamikaze_noinc
	rra
	rra
	rra
	and 3		; a = ((e->param2>>3) & 3) , we do not care about the upper bits so we can use rra
	ld ixh, a			; store in IXh
	ld bc, -4
	add hl, bc		; now HL points to e->type
	ld a, (hl)
	ld hl, _enemy_sprites
	ld c, a
	ld b, 0
	add hl, bc
	ld a, (hl)		; a = enemy_sprites[e->type]
	add a, ixh		; a = enemy_sprites[e->type] + ((e->param2>>3) & 3);
	inc de			; DE pointed to e->y, now it points to e->sprnum
	ld (de), a
	ret

; Homing missile movement
; param1 defines speed in X
; param2 defines the enemy to target

move_homing:
	; STEP 1: move right

	ld a, (hl)		; a == e->x
	ld d, h
	ld e, l			; save e in DE
	ld bc, 6
	add hl, bc		; HL == e->param1
	add a,(hl)		; add param1
	ld b, 239
	cp b
	jr nc, homing_goneright	; gone by the right
	ld (de), a
	jp homing_moveiny
homing_goneright
	ld h, d
	ld l, e		; HL has now e->x
	ld bc, 2
	add hl, bc
	xor a
	ld (hl), a		; e->sprnum = 0
	ret

	; STEP 2: move in Y

homing_moveiny
	inc de			; DE points to e->y
	ld a, (de)
	ex af, af'		; switch to alternate af
	inc hl			; HL points to e->param2
	ld a, (hl)
	ld bc, 12		
	ld hl, _active_enemies+1
	and a
	jr z, homing_nomultiply
homing_multiplyby12
	add hl, bc
	dec a
	jr nz, homing_multiplyby12
homing_nomultiply
	; HL now points to active_enemies[param2].y
	ex af, af'		; A == e->y
	cp (hl)			; compare e->y with enemy->y
	jr c, homing_moveup
	ret z 			; if we are exactly in the same Y, do nothing
homing_movedown		; e->y > ship_y, so go down
	sub 4			; This is a bit subtle. We substract 4 to add 2, so we actually substract 2 and store
homing_moveup
	add a, 2
	ld (de), a		; store the new Y
        ; let us check if the difference is 1 in terms of Y. If so, just make it the same
        sub (hl)
        inc a
        cp 3
        ret nc
        ld a, (hl)
        ld (de), a
	ret

; Wave movement
; This is just a sinus movement
; param1: speed in X
; param2: maximum displacement in Y
; param3: counter for movement until param2
; param4: sign (-1 or 1)

move_wave:
	; STEP 0: initialize
	push hl
	ld bc, 9
	add hl, bc
	ld a, (hl)
	and a
	jr nz, wave_initialized
	ld a, 1
	ld (hl), a			; if (e->param4 == 0) e->param4 = 1
wave_initialized
	pop hl
	ld a, (hl)		; a == e->x
	ld d, h
	ld e, l			; save e in DE
	ld bc, 6
	add hl, bc		; HL == e->param1
	sub (hl)		; substract param1
	jr c, wave_goneleft		; gone by the left
	ld (de), a

	inc de			; DE points to e->y
	inc hl
	inc hl
	inc hl			; HL points to e->param4
	ld a, (de)
	add a, (hl)		; A = e->y + e->param4
	ld (de), a		; e->y += e->param4
	dec hl
	inc (hl)		; e->param3 ++
	ld a, (hl)
	dec hl
	cp (hl)
	ret nz			; e-> param3 < e-> param2, so no need to do anything else
	xor a
	inc hl
	ld (hl), a		; e->param3 = 0
	inc hl			; HL points to e->param4
	ld a, (hl)
	neg
	ld (hl), a		; e->param4 = -e->param4
	ret

wave_goneleft
	ex de, hl		; HL has now e->x
	inc hl
	inc hl	
	xor a
	ld (hl), a		; e->sprnum = 0
	ret
	

; Move left while animating
; e->param1: speed of the movement to the left
; e->param2: animation length

move_leftanim:
	; STEP 0: initialize
	ld bc, 9
	add hl, bc
	ld a, (hl)
	and a
	jr nz, anim_initialized
	ld a, 1
	ld (hl), a
anim_initialized
	ld bc, -9 
	add hl, bc

	; STEP 1: move left

	ld a, (hl)		; a == e->x
	ld d, h
	ld e, l			; save e in DE
	ld bc, 6
	add hl, bc		; HL == e->param1
	sub (hl)		; substract param1
	jr c, leftanim_goneleft		; gone by the left
	ld (de), a
	jp leftanim_update
leftanim_goneleft
	ld h, d
	ld l, e		; HL has now e->x
	ld bc, 2
	add hl, bc
	xor a
	ld (hl), a		; e->sprnum = 0
	ret
	
	; STEP 2: update animation every 8 frames
leftanim_update
	ld a, d
	ld ixh, a
	ld a, e
	ld ixl, a		;  IX points to e->x

	ld a, (ix + 8)
	add a, (ix + 9)
	ld (ix+8), a		;  e->param3+=e->param4;
	and 7
	jr nz, leftanim_nottoupdate
	ld a, (ix + 9)
	add a, (ix + 2)
	ld (ix + 2), a		; e->sprnum+=e->param4
leftanim_nottoupdate	
	ld a, (ix + 8)
	srl a
	srl a
	srl a			; a = e->param3>>3
	sub (ix + 7)		; 
	cp -1
	jr nz, leftanim_noswitchside			
	ld (ix + 9) , a		; e->param4 = -1;
leftanim_noswitchside
	ld a, (ix + 8)
	and a
	ret nz
	ld a, 1
	ld (ix+9),a		
	ret	

; Simple go up & left movement
; param1: speed in Y
; param2: speed in X

move_upleft:
; Entry: HL is the struct Enemy
	ld a, (hl)		; a == e->x
	ld d, h
	ld e, l			; save e in DE
	ld bc, 7
	add hl, bc		; HL == e->param2
	sub (hl)		; substract param1
	jr c, goneleft2		; gone by the left
	ld (de), a
	inc de			; DE points to e->y
	ld a, (de)
	dec hl			; HL == e->param1
	sub (hl)		; substract param1
	jr c, goneup
	ld (de), a
	ret

goneleft2
	inc de			;  point to e->y
goneup
	inc de			; e-> sprnum
	xor a
	ld (de), a		; e->sprnum=0
	ret		


; Simple go right movement
; param1: speed

move_upright:
; Entry: HL is the struct Enemy
	ld a, (hl)		; a == e->x
	ld d, h
	ld e, l			; save e in DE
	ld bc, 6
	add hl, bc		; HL == e->param1
	add a,(hl)		; add param1
	ld b, 239
	cp b
	jr nc, goneright2	; gone by the right
	ld (de), a
	inc de			; DE points to e->y
	ld a, (de)
	sub (hl)		; substract param1
	jr c, goneup2
	ld (de), a
	ret
goneright2
	ex de, hl		; HL has now e->x
	ld bc, 2
	add hl, bc
	xor a
	ld (hl), a		; e->sprnum = 0
	ret
goneup2
	inc de
	xor a
	ld (de), a		; e->sprnum=0
	ret		


; Simple go down-left movement
; param1: speed in Y 
; param2: speed in X

move_downleft:
	ld a, (hl)		; a == e->x
	ld d, h
	ld e, l
	ld bc, 7
	add hl, bc		; HL == e->param2
	sub (hl)
	jr c, goneleft3		; gone by the left
	ld (de), a		
	inc de			; point at e->y
	ld a, (de)		; a == e->y
	dec hl			; HL == e->param1
	add a,(hl)		; add param1
	ld b, 111
	cp b
	jr nc, gonedown		; gone down
	ld (de), a
	ret
goneleft3
	inc de
gonedown
	inc de			; DE points now to e->sprnum
	xor a
	ld (de), a
	ret


; Simple go down-right movement
; param1: speed in Y 
; param2: speed in X
move_downright:
	ld a, (hl)		; a == e->x
	ld d, h
	ld e, l
	ld bc, 7
	add hl, bc		; HL == e->param2
	add a,(hl)
	ld b, 239
	cp b
	jr nc, goneright3	; gone by the right
	ld (de), a		
	inc de			; point at e->y
	ld a, (de)		; a == e->y
	dec hl			; HL == e->param1
	add a,(hl)		; add param1
	ld b, 111
	cp b
	jr nc, gonedown2		; gone down
	ld (de), a
	ret
goneright3
	inc de
gonedown2
	inc de			; DE points now to e->sprnum
	xor a
	ld (de), a
	ret


; Simple go up
; param1: speed

move_up:
; Entry: HL is the struct Enemy
	inc hl
	ld a, (hl)		; a == e->y
	ld d, h
	ld e, l			; save e in DE
	ld bc, 5
	add hl, bc		; HL == e->param1
	sub (hl)		; substract param1
	jr c, goneup3
	ld (de), a
	ret
goneup3
	inc de
	xor a
	ld (de), a		; e->sprnum=0
	ret		


; Movement towards a defined target (ONLY FOR SHOOTS, it uses some dirty tricks such as reusing unused variables for shoots that make this function unsuitable for enemies)
; param1: X speed, integer part. Bit 0 defines sign: 0 is positive, 1 is negative
; param2: X speed, fractional part.
; param3: Y speed, integer part. Bit 0 defines sign: 0 is positive, 1 is negative
; param4: Y speed, fractional part.
; type:   fractional side of e->x
; energy: fractional side of e->y

move_target:
	push hl
	pop ix		; IX will point to the Entity structure. Anything else will make it very complex to manage

	ld a, (ix+6)	; param1
	and $80
	jr nz, target_decx
target_incx
	ld h, (ix+6)	;param1
	ld l, (ix+7)	;param2
	ld d, (ix+0)	; X
	ld e, (ix+3)	; type == fractional part of X
	add hl, de
	ld (ix+0), h
	ld (ix+3), l
	ld a, h
	ld b, 239
	cp b
	jr nc, target_goneright; gone by the right
	jr target_movey		; continue with the Y coord movement
target_decx
	ld h, (ix+0)	; X
	ld l, (ix+3)	; type == fractional part of X
	ld a, (ix+6)	; param1
	and $7f
	ld d, a
	ld e, (ix+7)	; param2
	xor a
	sbc hl, de
	jr c, target_goneleft	
	ld (ix+0), h
	ld (ix+3), l	
target_movey
	ld a, (ix+8)	; param3
	and $80
	jr nz, target_decy
target_incy
	ld h, (ix+8)	;param3
	ld l, (ix+9)	;param4
	ld d, (ix+1)	; Y
	ld e, (ix+5)	; energy == fractional part of Y
	add hl, de
	ld (ix+1), h
	ld (ix+5), l
	ld a, h
	ld b, 111
	cp b
	jr nc, target_gonedown  ; gone down
	ret			; completed
target_decy
	ld h, (ix+1)	; Y
	ld l, (ix+5)	; energy == fractional part of Y
	ld a, (ix+8)	; param1
	and $7f
	ld d, a
	ld e, (ix+9)	; param2
	xor a
	sbc hl, de
	jr c, target_goneup
	ld (ix+1), h
	ld (ix+5), l	
	ret
target_goneright
target_goneleft
target_goneup
target_gonedown
	xor a
	ld (ix+2), a 		; e->sprnum = 0
	ret


; Kamikaze movement, without animation
; param1 defines speed in X

move_kamikaze_noanim:
	; STEP 1: move left

	ld a, (hl)		; a == e->x
	ld d, h
	ld e, l			; save e in DE
	ld bc, 6
	add hl, bc		; HL == e->param1
	sub (hl)		; substract param1
	jr c, kamikaze2_goneleft		; gone by the left
	ld (de), a
	jp kamikaze2_moveiny
kamikaze2_goneleft
	ld h, d
	ld l, e		; HL has now e->x
	ld bc, 2
	add hl, bc
	xor a
	ld (hl), a		; e->sprnum = 0
	ret
	; STEP 2: move in Y
kamikaze2_moveiny
	inc de			; DE points to e->y
	ld a, (de)
	ld hl, _ship_y		
	cp (hl)			; compare e->y with ship_y
	jr c, kamikaze2_moveup
	ret z 			; if we are exactly in the same Y, do nothing
kamikaze2_movedown		; e->y > ship_y, so go down
	sub 4			; This is a bit subtle. We substract 4 to add 2, so we actually substract 2 and store
kamikaze2_moveup
	add a, 2
	ld (de), a		; store the new Y
        ; let us check if the difference is 1 in terms of Y. If so, just make it the same
        sub (hl)
        inc a
        cp 3
        ret nc
        ld a, (hl)
        ld (de), a
	ret

; Wave movement, without any check for enemy going out of bounds
; This is just a sinus movement
; param1: speed in X
; param2: maximum displacement in Y
; param3: counter for movement until param2
; param4: sign (-1 or 1)

move_wave_nogoneleft:
	; STEP 0: initialize
	push hl
	ld bc, 9
	add hl, bc
	ld a, (hl)
	and a
	jr nz, wave2_initialized
	ld a, 1
	ld (hl), a			; if (e->param4 == 0) e->param4 = 1
wave2_initialized
	pop hl
	ld a, (hl)		; a == e->x
	ld d, h
	ld e, l			; save e in DE
	ld bc, 6
	add hl, bc		; HL == e->param1
	sub (hl)		; substract param1

	ld (de), a
	inc de			; DE points to e->y
	inc hl
	inc hl
	inc hl			; HL points to e->param4
	ld a, (de)
	add a, (hl)		; A = e->y + e->param4
	ld (de), a		; e->y += e->param4
	dec hl
	inc (hl)		; e->param3 ++
	ld a, (hl)
	dec hl
	cp (hl)
	ret nz			; e-> param3 < e-> param2, so no need to do anything else
	xor a
	inc hl
	ld (hl), a		; e->param3 = 0
	inc hl			; HL points to e->param4
	ld a, (hl)
	neg
	ld (hl), a		; e->param4 = -e->param4
	ret

; Move casco's missile, in 3 steps:
; Step 1: move up, for 4 frames (first animation frame)
; Step 2: move up-left, for 4 more frames (second animation frame)
; Step 3: move left, until it goes out (third animation frame)
; param1: speed 
; param2: will be used as animation counter

move_missile:
	ld d, h
	ld e, l
	ld bc, 7
	add hl, bc		; HL points to e->param2
	ld a, (hl)
	cp 8
	jr nc, missile_goleft
	cp 4
	jr nc, missile_goupleft
missile_goup:
	ld h, d
	ld l, e
	inc hl
	ld a, (hl)		; a == e->y
	ld bc, 5
	add hl, bc		; HL == e->param1
	sub (hl)		; substract param1
	jr c, missile_goneup	
	inc de
	ld (de), a
	dec de
	jr missile_incparam2
missile_goneup:
	inc de
	inc de
	xor a
	ld (de), a		; e->sprnum=0
	ret		
missile_goleft:
	cp 8
	jr nz, missile_goleftalready
	ld bc, -5
	add hl, bc
	inc (hl)		; next sprite
missile_goleftalready:
	ld h, d
	ld l, e
	ld a, (hl)		; a == e->x
	ld bc, 6
	add hl, bc		; HL == e->param1
	sub (hl)		; substract param1
	jr c, missile_goneleft2	; gone by the left
	ld (de), a
	jr missile_incparam2

missile_goneleft2:
	inc de
	inc de
	xor a
	ld (de), a		; e->sprnum = 0
	ret


missile_goupleft:
	cp 4
	jr nz, missile_goupleftalready
	ld bc, -5
	add hl, bc
	inc (hl)		; next sprite
missile_goupleftalready:
	ld h, d
	ld l, e
	ld a, (hl)		; a == e->x
	ld bc, 6
	add hl, bc		; HL == e->param1
	sub (hl)		; substract param1
	jr c, missile_goneleft	; gone by the left
	ld (de), a
	inc de			; DE points to e->y
	ld a, (de)
	sub (hl)		; substract param1
	jr c, missile_goneup2
	ld (de), a
	dec de
	jr missile_incparam2
missile_goneleft:
	inc de			;  point to e->y
missile_goneup2:
	inc de			; e-> sprnum
	xor a
	ld (de), a		; e->sprnum=0
	ret	
missile_incparam2:
	ld h,d
	ld l,e
	ld bc, 7
	add hl, bc
	inc (hl)
	ret	

; Kamikaze movement for egg bisho
; 2-phase movement
; 	1: While changing animation, move towards ship
;	2: once the animation reaches the end, fly directly to ship
; Always 3 animation frames
; param1 defines speed in X

move_kamikaze_2:
	; STEP 1: move left

	ld a, (hl)		; a == e->x
	ld d, h
	ld e, l			; save e in DE
	ld bc, 6
	add hl, bc		; HL == e->param1
	sub (hl)		; substract param1
	jr c, kamikaze3_goneleft		; gone by the left
	ld (de), a
	jp kamikaze3_moveiny
kamikaze3_goneleft:
	ld h, d
	ld l, e		; HL has now e->x
	ld bc, 2
	add hl, bc
	xor a
	ld (hl), a		; e->sprnum = 0
	ret
	; STEP 2: move in Y
kamikaze3_moveiny:
	inc de			; DE points to e->y
	ld a, (de)
	ld hl, _ship_y		
	cp (hl)			; compare e->y with ship_y
	jr c, kamikaze3_moveup
	jr z, kamikaze3_animate	; if we are exactly in the same Y, do nothing
kamikaze3_movedown:		; e->y > ship_y, so go down
	sub 4			; This is a bit subtle. We substract 4 to add 2, so we actually substract 2 and store
kamikaze3_moveup:
	add a, 2
	ld (de), a		; store the new Y
        ; let us check if the difference is 1 in terms of Y. If so, just make it the same
        sub (hl)
        inc a
        cp 3
        jr nc, kamikaze3_animate
        ld a, (hl)
        ld (de), a

	; STEP 3: animate egg

kamikaze3_animate:
	ld h, d
	ld l, e			; HL also points to e->y
	ld bc, 6
	add hl, bc		; HL points now to e->param2
	ld a, (hl)
	cp $18
	ret nc
	inc a
	ld (hl), a		; e->param2 is lower than $1f, so increment
kamikaze3_noinc:
	and 7
	ret nz		; do not change animation if not needed
	ld bc, -5
	add hl, bc		; now HL points to e->sprnum
	inc (hl)
	ret


; find enemy to follow
; param1 defines sprite id of enemy to follow
; param2 stores pointer to enemy to follow, 255 if not initialized
move_find_enemy_to_follow:
	push hl
	ld d, h
	ld e, l
	ld bc, 7
	add hl, bc		; HL points to e->param2
	ld a, (hl)
	cp 255
	jr nz, find_already_initialized
	; Initialize: find enemy sprite id
	push de
	ld de, 12
	dec hl
	ld a, (hl)
	ld b, a			; sprite id in B
	ld c, 0			; C will serve as counter
	ld hl, _active_enemies+2
find_loop:
	ld a, (hl)
	cp b
	jr z, find_found
	inc c
	ld a, c
	cp 8			; MAX_ENEMIES
	jp z, find_not_found	; IF C==8, we have not found the sprite... this is an issue
	add hl, de		; go to next enemy
	jr find_loop		; and loop
find_found:			; C holds the counter
	pop de
	ld h, d
	ld l, e
	inc hl
	inc hl
	inc hl
	inc hl
	inc hl
	inc hl
	inc hl
	ld a, c
	ld (hl), a		; store the pointer to the enemy we have to follow
find_already_initialized:
	pop hl
	scf			; set carry flag		
	ret	
find_not_found:
	pop de
	pop hl
	inc de
	inc de
	xor a			; carry flag clear
	ld (de), a		; make this enemy disappear
	ret			


; Follow another enemy, at its right (+16,0)
; param1 defines sprite id of enemy to follow
; param2 stores pointer to enemy to follow, 255 if not initialized

move_follow_right: 
	call move_find_enemy_to_follow
	ret nc			; return if we did not find the enemy
	ld d, h
	ld e, l
	ld bc, 7
	add hl, bc
	ld a, (hl)
	inc a
	ld hl, 	_active_enemies-12
	ld bc, 12
fright_loop2:
	add hl, bc
	dec a
	jr nz, fright_loop2	; position at the reference enemy
	ld a, (hl)		; reference->x
	add a, 16
	ld (de), a		; e->x = reference->x+16
	inc hl
	inc de
	ld a, (hl)
	ld (de), a		; e->y = reference->y
	inc hl
	inc de
	ld a, (hl)
	and a
	ret nz
	ld (de), a		; if the original enemy disappears, delete this one as well
	ret

; Follow another enemy, below (0,+16)
; param1 defines sprite id of enemy to follow
; param2 stores pointer to enemy to follow, 255 if not initialized

move_follow_down: 
	call move_find_enemy_to_follow
	ret nc			; return if we did not find the enemy
	ld d, h
	ld e, l
	ld bc, 7
	add hl, bc
	ld a, (hl)
	inc a
	ld hl, 	_active_enemies-12
	ld bc, 12
fdown_loop:
	add hl, bc
	dec a
	jr nz, fdown_loop	; position at the reference enemy
	ld a, (hl)		; reference->x
	ld (de), a		; e->x = reference->x
	inc hl
	inc de
	ld a, (hl)
	add a, 16
	ld (de), a		; e->y = reference->y+16
	inc hl
	inc de
	ld a, (hl)
	and a
	ret nz
	ld (de), a		; if the original enemy disappears, delete this one as well
	ret


; Follow another enemy, at its down-right (+16,+16)
; param1 defines sprite id of enemy to follow
; param2 stores pointer to enemy to follow, 255 if not initialized

move_follow_downright: 
	call move_find_enemy_to_follow
	ret nc			; return if we did not find the enemy
	ld d, h
	ld e, l
	ld bc, 7
	add hl, bc
	ld a, (hl)
	inc a
	ld hl, 	_active_enemies-12
	ld bc, 12
fdownright_loop:
	add hl, bc
	dec a
	jr nz, fdownright_loop	; position at the reference enemy
	ld a, (hl)		; reference->x
	add a, 16
	ld (de), a		; e->x = reference->x + 16
	inc hl
	inc de
	ld a, (hl)
	add a, 16
	ld (de), a		; e->y = reference->y+16
	inc hl
	inc de
	ld a, (hl)
	and a
	ret nz
	ld (de), a		; if the original enemy disappears, delete this one as well
	ret

; Simple go left movement, with decreasing acceleration
; param1: speed, will be decreased to zero on every step

move_left_accel:
; Entry: HL is the struct Enemy
	ld a, (hl)		; a == e->x
	ld d, h
	ld e, l			; save e in DE
	ld bc, 6
	add hl, bc		; HL == e->param1
	sub (hl)		; substract param1
	jr c, goneleft_accel	; gone by the left
	ld (de), a
	ld a, (hl)
	and a
	ret z			; return if speed is zero	
	dec (hl)		; decrease acceleration
	ret
goneleft_accel:
	ex de, hl		; HL has now e->x
	ld bc, 2
	add hl, bc
	xor a
	ld (hl), a		; e->sprnum = 0
	ret


; Simple go right movement, with decreasing acceleration
; param1: speed, will be decreased to zero on every step

move_right_accel:
; Entry: HL is the struct Enemy
	ld a, (hl)		; a == e->x
	ld d, h
	ld e, l			; save e in DE
	ld bc, 6
	add hl, bc		; HL == e->param1
	add a, (hl)		; add param1
	ld b, 239
	cp b
	jr nc, goneright_accel	; gone by the right
	ld (de), a
	ld a, (hl)
	and a
	ret z			; return if speed is zero
	dec (hl)
	ret
goneright_accel:
	ex de, hl		; HL has now e->x
	ld bc, 2
	add hl, bc
	xor a
	ld (hl), a		; e->sprnum = 0
	ret



; Dummy movement, does nothing
move_none:
	ret

org 61439
	db 0		; Last byte!
