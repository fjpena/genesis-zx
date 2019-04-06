org 61440

include "maindefs.asm"

; Simple global variable to hold created entities, allowing modification

_new_e	ds 8		; struct Enemy new_e

; Supporting functions

; INPUT
;	H: x_ship
;	L: y_ship
;	D: x_enemy	
;	E: y_enemy

; OUTPUT
;	HL: speed_x   ( H: integer part, L fractional part). Bit 0 of H defines sign (0: positive, 1: negative)
;	DE: speed_y   ( D: integer part, E fractional part). Bit 0 of D defines sign (0: positive, 1: negative)
calcdirectshoot
   	ld iy, 0
   	ld a, h
   	sub d
   	jr nc, x_positive
x_negative
   	neg
   	ld iyl, $80   ; 
x_positive
   	ld b, a      ; B = abs(dx)
   	ld a, l
   	sub e
   	jr nc, y_positive
y_negative
   	neg
   	ld iyh, $80
y_positive
	ld c, a      ; C = abs (dy)
	ld c, a		; C = abs (dy)
	ld a, b
	cp c
	jr c, dy_greater_dx
dx_greater_dy
	ld a, 4
	or iyl
	ld h, a
	ld l, 0			; speed_x = 1 (dx / dx)

	ld d, b
	ld e, c
	push bc
	call Div8		; speed_y = abs(dy) / abs (dx), sign is stored elsewhere
	pop bc
	rl d
	rla
	rl d
	rla
	ld e, d
	or iyh
	ld d, a			; speed_y
	ret

dy_greater_dx
	ld a, c
	and a
	jr z, dy_is_zero
	ld d, c
	ld e, b
	push bc
	call Div8		; speed_x = abs(dx) / abs (dy), sign is stored elsewhere
	pop bc
	rl d
	rla
	rl d
	rla
	or iyl
	ld h, a
	ld l, d			; speed_x
	ld e, 0
	ld a, 2
	or iyh
	ld d, a			; speed_y = 1 (dy/dy)
	ret

dy_is_zero
	ld a, 4
	or iyl
	ld h, a
	ld l, 0
	ld de, 0
	ret			; Special case when x_ship equals x_enemy

;Divide 8-bit values
;In: Divide E by divider D
;Out: A = result, D = rest
;
Div8:
    xor a
    ld b,8
Div8_Loop:
    rl e
    rla
    sub d
    jr nc,Div8_NoAdd
    add a,d
Div8_NoAdd:
    djnz Div8_Loop
    ld d,a
    ld a,e
    rla
    cpl
    ret


; Dummy behavior, does nothing

behav_none:
	ret

; Shoot once, left direction

behav_shoot_left:
	ld d, h
	ld e, l			; save e in DE
	ld bc, 11
	add hl, bc		; HL == e->behav_param
	ld a, (hl)
	and a
	ret nz			; if e->behav_param is not zero, just return

	; NewEnemyShoot(e->x, e->y, MOVE_LEFT, SHOT_ENEMY)

	push hl
	ld a, (de)
	ld c, a
	ld b, 0
	push bc			; e->x
	inc de
	ld a, (de)
	ld c, a
	push bc			; e->y
	ld bc, MOVE_LEFT
	push bc			; MOVE_LEFT
	ld bc, SHOT_ENEMY
	push bc			; SHOT_ENEMY
	call _NewEnemyShoot
	pop bc
	pop bc
	pop bc
	pop bc
	; upon return, HL holds the pointer to the new enemy shoot
	pop hl			; HL is pointing to e->behav_param
	ld a, 1
	ld (hl), a
	ret

; Shoot once, left direction, when the ship is within a (-10,+10) Y distance

behav_shoot_left_Y:
	ld d, h
	ld e, l			; save e in DE
	ld bc, 11
	add hl, bc		; HL == e->behav_param
	ld a, (hl)
	and a
	ret nz			; if e->behav_param is not zero, just return

	inc de
	ld a, (de)		; A = e->y
	ld hl, _ship_y
	sub (hl)
	jr nc, y_nocarry
	neg
y_nocarry:
	cp 10
	ret nc			; if the distance is greater than 10, do not shoot

	dec de
	ld h, d
	ld l, e
	ld bc, 11
	add hl, bc		; HL == e->behav_param

	; NewEnemyShoot(e->x, e->y, MOVE_LEFT, SHOT_ENEMY)

	push hl
	ld a, (de)
	ld c, a
	ld b, 0
	push bc			; e->x
	inc de
	ld a, (de)
	ld c, a
	push bc			; e->y
	ld bc, MOVE_LEFT
	push bc			; MOVE_LEFT
	ld bc, SHOT_ENEMY
	push bc			; SHOT_ENEMY
	call _NewEnemyShoot
	pop bc
	pop bc
	pop bc
	pop bc
	; upon return, HL holds the pointer to the new enemy shoot
	pop hl			; HL is pointing to e->behav_param
	ld a, 1
	ld (hl), a
	ret


; Shoot once, left direction, but wait 20 frames to do so

behav_shoot_left_wait:
	ld d, h
	ld e, l			; save e in DE
	ld bc, 11
	add hl, bc		; HL == e->behav_param
	ld a, (hl)
	cp 20
	ret nc			; if e->behav_param > 20, we did it already
	inc a
	ld (hl), a
	cp 20
	ret nz			; we will only shoot when e->behav_param is 20

	; NewEnemyShoot(e->x, e->y, MOVE_LEFT, SHOT_ENEMY)
	ld a, (de)
	ld c, a
	ld b, 0
	push bc			; e->x
	inc de
	ld a, (de)
	ld c, a
	push bc			; e->y
	ld bc, MOVE_LEFT
	push bc			; MOVE_LEFT
	ld bc, SHOT_ENEMY
	push bc			; SHOT_ENEMY
	call _NewEnemyShoot
	pop bc
	pop bc
	pop bc
	pop bc
	ret
	; upon return, HL holds the pointer to the new enemy shoot
	; anyway, we do not care


; Shoot at ship, left direction, from ground
; Uses: behav_param: counter, will shoot three times

behav_shoot_target_left:
	ld d, h
	ld e, l			; save e in DE
	ld bc, 11
	add hl, bc		; HL == e->behav_param
 	ld a, (hl)
   	and a			; A == 0
   	jr z, check_first_shoot_left
   	dec a			; A == 1
   	jr z, check_second_shoot_left
   	dec a			; A == 2
	ret nz
check_third_shoot_left
	ld a, (de)
	cp 100
	ret nc
	inc (hl)		; e->behav_param++
	jr left_new_shoot
check_second_shoot_left
	ld a, (de)
	cp 170
	ret nc
	inc (hl)		; e->behav_param++
	jr left_new_shoot
check_first_shoot_left
	ld a, (de)
	cp 220
	ret nc
	inc (hl)		; e->behav_param++
left_new_shoot
	; NewEnemyShoot(e->x, e->y, MOVE_UPLEFT, SHOT_CANNON_LEFT);
	sub 8
	ld c, a
	ld b, 0
	push bc			; e->x
	inc de
	ld a, (de)
	sub 8
	ld c, a
	push bc			; e->y
	ld bc, MOVE_UPLEFT
	push bc			; MOVE_UPLEFT
	ld bc, SHOT_CANNON_LEFT
	push bc			; SHOT_CANNON_LEFT
	call _NewEnemyShoot
	pop bc
	pop bc
	pop bc
	pop bc
	; upon return, HL holds the pointer to the new enemy shoot
	ld a, l
	or h
	ret z			; If HL == 0, then skip, as the enemy shoot could not be created

	ld bc, 6
	add hl, bc
	ld (hl), 4		; behav_e->param1=4
	inc hl
	ld (hl), 4		; behav_e->param2=4
	ret

; Shoot at ship, right direction, from ground
; Uses: behav_param: counter, will shoot three times

behav_shoot_target_right:
	ld d, h
	ld e, l			; save e in DE
	ld bc, 11
	add hl, bc		; HL == e->behav_param
	ld a, (hl)
	cp 0
	jr z, check_first_shoot_right
	cp 1
	jr z, check_second_shoot_right
	cp 2
	ret nz
check_third_shoot_right
	ld a, (de)
	cp 40
	ret nc
	inc (hl)		; e->behav_param++
	jr right_new_shoot
check_second_shoot_right
	ld a, (de)
	cp 90
	ret nc
	inc (hl)		; e->behav_param++
	jr right_new_shoot
check_first_shoot_right
	ld a, (de)
	cp 220
	ret nc
	inc (hl)		; e->behav_param++
right_new_shoot
	; NewEnemyShoot(e->x, e->y, MOVE_UPRIGHT, SHOT_CANNON_RIGHT);
	add a, 8
	ld c, a
	ld b, 0
	push bc			; e->x
	inc de
	ld a, (de)
	sub 8
	ld c, a
	push bc			; e->y
	ld bc, MOVE_UPRIGHT
	push bc			; MOVE_UPRIGHT
	ld bc, SHOT_CANNON_RIGHT
	push bc			; SHOT_CANNON_RIGHT
	call _NewEnemyShoot
	pop bc
	pop bc
	pop bc
	pop bc
	; upon return, HL holds the pointer to the new enemy shoot
	ld a, l
	or h
	ret z			; If HL == 0, then skip, as the enemy shoot could not be created

	ld bc, 6
	add hl, bc
	ld (hl), 4		; behav_e->param1=4
	ret


; Shoot at ship, point at target (kamikaze bomb)
; behav_param: counter, will shoot every 16 frames

behav_shoot_target:
	push hl
	ld de, 11
	add hl, de
	ld a, (hl)
	inc a
	ld (hl), a
	pop hl
	and $1F
	cp $1f		; will shoot every 32 frames
	ret nz
	; now we have to shoot. First, generate shoot
	push hl
	;NewEnemyShoot(e->x, e->y+14, MOVE_TARGET, SHOT_MEGA);
	ld e, (hl)
	ld d, 0
	push de		; e->x
	inc hl
	ld a, (hl)
	add a, 14
	ld e, a
	push de		; e->y+14
	ld e, MOVE_TARGET
	push de
	ld e, SHOT_MEGA
	push de
	call _NewEnemyShoot_FX	; HL holds the pointer to the new enemy shoot
	pop de
	pop de
	pop de
	pop de

	; Then, generate parameters for the shoot
	push hl
	pop ix			; IX holds the pointer to the new enemy shoot
	pop hl			; get the original HL (entity) back

	ld a, ixh
	or ixl
	ret z			; If IX == 0, then skip, as the enemy shoot could not be created

	ld d, (hl)		; D==x_enemy
	inc hl
	ld e, (hl)		; E==y_enemy
	ld a, (_ship_x)
	add a, 16
	ld h, a			; H = x_ship
	ld a, (_ship_y)
	ld l, a			; L = y_ship
	call calcdirectshoot

	ld (ix+6), h
	ld (ix+7), l
	ld (ix+8), d
	ld (ix+9), e
	ld (ix+3), 0
	ld (ix+5), 0

	ret


; Behavior for power ups
; It is pretty simple: if we are exploding, and we are in the last
; explosion frame, turn into a power up

behav_powerup:
	inc hl
	inc hl
	inc hl
	inc hl			; check movement_type
	ld a, (hl)
	cp MOVE_EXPLOSION
	ret nz			; if we are not exploding, just exit
	; if we are exploding, check param1, which will specify the current frame. If it is one... just generate the new power up
	inc hl
	inc hl
	ld a, (hl)
	cp 1
	ret nz			; only if param1==1 we will do something

	inc hl			; param2 is the sprite number of the power up to generate
	ld d, (hl)		; D == param2
	ld bc, -7
	add hl, bc		
	ld e, (hl)			; E == x
	inc hl
	ld a, (hl)		; A == y

	ld hl, _power_up
	ld (hl), e
	inc hl
	ld (hl), a
	inc hl
	ld (hl), d
	inc hl
	inc hl
	ld (hl), MOVE_LEFT
	inc hl
	inc hl
	ld (hl), 2
	inc hl
	ld (hl), d
	
	ld hl, FX_HIT_CAPSULE
        push hl
        call _wyz_effect
        pop hl
	ret

; Behavior for three-position turrets
; They check the ship X position. If it is between a -30,30 px range
; they are in the middle position, otherwise they point at ship
; And they shoot from time to time :)

; Uses: behav_param: counter, will shoot three times

behav_turret:
	ld a, (_ship_x)		; get ship X position
	ld b, (hl)		; B = e->x
	sub b
	jr nc, turret_nocarry	; ship_x - e->x is greater or equal than zero
turret_carry
	neg			; we convert A to a positive value
	cp 30 
	jr c, turret_center	; set the center position
	ld a, ENEMY_TURRET	; this is the sprite for the look-left position
	jr turret_done
turret_nocarry
	cp 30 
	jr c, turret_center	; set the center position
	ld a, ENEMY_TURRET+2	; this is the sprite for the look-right position
	jr turret_done
turret_center
	ld a, ENEMY_TURRET+1	; this is the sprite for the look-up position
turret_done
	inc hl
	inc hl
	ld (hl), a		; we have just set the new sprite

	dec hl
	dec hl
	
	ld d, h
	ld e, l			; save e in DE
	ld bc, 11
	add hl, bc		; HL == e->behav_param
	ld a, (hl)
	cp 0
	jr z, check_first_shoot_turret
	cp 1
	jr z, check_second_shoot_turret
	cp 2
	ret nz
check_third_shoot_turret
	ld a, (de)
	cp 40
	ret nc
	inc (hl)		; e->behav_param++
	jr turret_new_shoot
check_second_shoot_turret
	ld a, (de)
	cp 90
	ret nc
	inc (hl)		; e->behav_param++
	jr turret_new_shoot
check_first_shoot_turret
	ld a, (de)
	cp 220
	ret nc
	inc (hl)		; e->behav_param++
turret_new_shoot
	ld bc, -11
	add hl, bc		; make HL point back to e->x

	push hl
	;NewEnemyShoot(e->x, e->y-8, MOVE_TARGET, SHOT_MEGA);
	ld e, (hl)
	ld d, 0
	push de		; e->x
	inc hl
	ld a, (hl)
	sub 8
	ld e, a
	push de		; e->y-8
	ld e, MOVE_TARGET
	push de
	ld e, SHOT_MEGA
	push de
	call _NewEnemyShoot_FX	; HL holds the pointer to the new enemy shoot
	pop de
	pop de
	pop de
	pop de

	; Then, generate parameters for the shoot
	push hl
	pop ix			; IX holds the pointer to the new enemy shoot
	pop hl			; get the original HL (entity) back

	ld a, ixh
	or ixl
	ret z			; If IX == 0, then skip, as the enemy shoot could not be created

	ld d, (hl)		; D==x_enemy
	inc hl
	ld e, (hl)		; E==y_enemy
	ld a, (_ship_x)
	add a, 16
	ld h, a			; H = x_ship
	ld a, (_ship_y)
	ld l, a			; L = y_ship
	call calcdirectshoot

	ld (ix+6), h
	ld (ix+7), l
	ld (ix+8), d
	ld (ix+9), e
	ld (ix+3), 0
	ld (ix+5), 0
	ret


; Behavior for asteroids
; It is pretty stupid, just check if we are in the first frame of an explosion
; and if so, change the sprite for the asteroid explosion one
behav_asteroid:
	inc hl
	inc hl
	inc hl
	inc hl			; check movement_type
	ld a, (hl)
	cp MOVE_EXPLOSION
	ret nz			; if we are not exploding, just exit
	; if we are exploding, check param1, which will specify the current frame. If it is 4... just switch sprites
	inc hl
	inc hl
	ld a, (hl)
	cp 4
	ret nz			; only if param1==4 we will do something
	dec hl
	dec hl
	dec hl
	dec hl
	ld (hl), ASTEROID_EXPL	; new sprite for the explosion!
	ret

; Behavior for final enemy 1
behav_fenemy1:
	; First, check if we have been defeated

	ld d, h
	ld e, l			; save e in DE

	inc hl
	inc hl
	ld a, (hl)
	and a
	jr nz, final1_start
	ld a, (_fenemy_defeat)
	and a
	jr nz, inc_f1_defeated			; if the enemy is defeated, there is nothing left to do for us but increase the count
	inc a
	ld (_fenemy_defeat),a		; final_enemy_defeated=1, now make the rest of it explode
	ld bc, 12	
	add hl, bc				; we are pointint at sprite2->sprnum
        ld (hl), EXPLOSION_SPR
	inc hl
	inc hl
	ld (hl), MOVE_EXPLOSION
	inc hl
	inc hl
	ld (hl), 4				; param1
	ld bc, 8
	add hl, bc				; sprite3
        ld (hl), EXPLOSION_SPR
	inc hl
	inc hl
	ld (hl), MOVE_EXPLOSION
	inc hl
	inc hl
	ld (hl), 4				; param1
	add hl, bc				; sprite4
        ld (hl), EXPLOSION_SPR
	inc hl
	inc hl
	ld (hl), MOVE_EXPLOSION
	inc hl
	inc hl
	ld (hl), 4				; param1
	ret

inc_f1_defeated
	inc a
	ld (_fenemy_defeat), a
	ret

final1_start
	ld h, d
	ld l, e

	; as a first step, animate UFO
	ld bc, 11
	add hl, bc		; HL == e->behav_param
	ld a, (hl)
	and $C			; e->behav_param & 0xC, that will be the animation 
	add a, 22		; this is the first sprite for this animation
	ld bc, -9	
	add hl, bc		; hl == e->sprnum
	ld (hl),a
	ld bc, 12
	add hl, bc
	inc a
	ld (hl), a
	add hl, bc
	inc a
	ld (hl), a
	add hl, bc
	inc a
	ld (hl), a

	ld h, d
	ld l, e
	ld bc, 11
	add hl, bc		; HL == e->behav_param
	ld a, (hl)
	and a
	jr nz, dec_behav_param_1

; Generate new enemy...
	push hl
	push de

	ld hl, _new_e
        ld a, (de)     ; A has e->x
        ld (hl), a
	inc hl
        inc hl  ; point to new_e->y
        inc de
        ld a, (de)
	add a, 20
        ld (hl), a
        inc hl         ; point to new_e->enemy_type
        ld (hl), 14     ; ENEMY_FINAL1_SHOOT
        inc hl         ; point to new_e->movent
        ld (hl), MOVE_KAMIKAZE_NOANIM
        inc hl         ; point to new_e->energy
        ld (hl), 2
        inc hl         ; point to new_e->param1
        ld (hl), 8
        inc hl         ; point to new_e->param2
        ld (hl), 0
	ld hl, _new_e
	push hl
	ld bc, 4
	push bc
	call _NewEnemy
	pop bc
	pop bc         ; we pop BC because we want to keep HL
	pop de
	push de
	ld a, h
	and l
	jr z, notgenerated_1       ; If the result returned was 0, the enemy could not be generated
	ld a, (de)
	add a, 8
	ld (hl), a               ; the X was not like that...

notgenerated_1
        pop de
        pop hl
	ld a, 32
	ld (hl), a
	jr move_rest_final1
dec_behav_param_1
	dec (hl)		; e->behav_param--;
move_rest_final1		; move the rest of the final enemy according to sprite 0
	ld h, d
	ld l, e
	inc hl
	ld a, (hl)		; e->y
	ld bc, 12	; sizeof (struct entity)
	add hl, bc
	ld (hl), a
	add hl, bc
	add a, 16
	ld (hl), a
	add hl, bc
	ld (hl), a
	; copy the movement to all sprites
	ret



; Behavior for final enemy 4
behav_fenemy4
	; First, check if we have been defeated

	ld d, h
	ld e, l			; save e in DE

	inc hl
	inc hl
	ld a, (hl)
	and a
	jr nz, final4_start
	ld bc, 12
	add hl, bc
	ld a, (hl)
	and a
	jr nz, final4_start
	ld a, (_fenemy_defeat)
	and a
	jr nz, inc_f4_defeated			; if the enemy is defeated, there is nothing left to do for us but increase the count
	inc a
	ld (_fenemy_defeat),a		; final_enemy_defeated=1, now make the rest of it explode
	add hl, bc				; we are pointint at sprite3->sprnum
        ld (hl), EXPLOSION_SPR
	inc hl
	inc hl
	ld (hl), MOVE_EXPLOSION
	inc hl
	inc hl
	ld (hl), 4				; param1
	ld bc, 8
	add hl, bc				; sprite4
        ld (hl), EXPLOSION_SPR
	inc hl
	inc hl
	ld (hl), MOVE_EXPLOSION
	inc hl
	inc hl
	ld (hl), 4				; param1
	add hl, bc				; sprite5
        ld (hl), EXPLOSION_SPR
	inc hl
	inc hl
	ld (hl), MOVE_EXPLOSION
	inc hl
	inc hl
	ld (hl), 4				; param1
	add hl, bc				; sprite6
        ld (hl), EXPLOSION_SPR
	inc hl
	inc hl
	ld (hl), MOVE_EXPLOSION
	inc hl
	inc hl
	ld (hl), 4				; param1
	ret

inc_f4_defeated
	inc a
	ld (_fenemy_defeat), a
	ret

final4_start
	ld h, d
	ld l, e
	ld bc, 11
	add hl, bc		; HL == e->behav_param
	ld a, (hl)
	and a
	jp nz, dec_behav_param

; NewEnemyShoot(e->x, e->y, MOVE_LEFT, SHOT_BASIC);

; Generate new enemy...
	push hl
	push de

	ld h, d
	ld l, e
	ld a, (hl)
	cp 17
	jr c, final4_generatesecond	; if X < 17, do not generate 
	inc hl
	inc hl
	ld a, (hl)
	and a
	jr z, final4_generatesecond	; in  this case, the sprite does not exist

	ld hl, _new_e
        ld a, (de)     ; A has e->x
        ld (hl), a
        inc hl
        inc hl         ; point to new_e->y
        inc de
        ld a, (de)
        ld (hl), a
        inc hl         ; point to new_e->enemy_type
        ld (hl), 7     ; ENEMY_FINAL4_EYE
        inc hl         ; point to new_e->movent
        ld (hl), MOVE_LEFT
        inc hl         ; point to new_e->energy
        ld (hl), 0
        inc hl         ; point to new_e->param1
        ld (hl), 5
        inc hl         ; point to new_e->param2
        ld (hl), 0
	ld hl, _new_e
	push hl
	ld bc, 6
	push bc
	call _NewEnemy
	pop bc
	pop bc         ; we pop BC because we want to keep HL
	pop de
	push de
	ld a, h
	and a
	jr z, notgenerated       ; If the result returned was 0, the enemy could not be generated
	ld a, (de)
	sub 16
	ld (hl), a               ; the X was not like that...

final4_generatesecond
	pop de
	pop hl
	push hl
	push de

	ld h, d
	ld l, e
	ld bc, 14		; sizeof (struct Entity) + 2
	add hl, bc
	ld a, (hl)
	and a
	jr z, notgenerated	; in  this case, the sprite does not exist

	ld hl, _new_e
        ld a, (de)     ; A has e->x
        ld (hl), a
        inc hl
        inc hl         ; point to new_e->y
        inc de
        ld a, (de)
        ld (hl), a
        inc hl         ; point to new_e->enemy_type
        ld (hl), 7     ; ENEMY_FINAL4_EYE
        inc hl         ; point to new_e->movent
        ld (hl), MOVE_LEFT
        inc hl         ; point to new_e->energy
        ld (hl), 0
        inc hl         ; point to new_e->param1
        ld (hl), 5
        inc hl         ; point to new_e->param2
        ld (hl), 0
	ld hl, _new_e
	push hl
	ld bc, 6
	push bc
	call _NewEnemy
	pop bc
	pop bc         ; we pop BC because we want to keep HL
	pop de
	push de
	ld a, h
	and a
	jr z, notgenerated       ; If the result returned was 0, the enemy could not be generated
	ld a, (de)
        add a, 16
	ld (hl), a               ; the X was not like that...

notgenerated
        pop de
        pop hl

	ld a, 40
	ld (hl), a
	jr move_rest_final4
dec_behav_param
	dec (hl)		; e->behav_param--;
move_rest_final4		; move the rest of the final enemy according to sprite 0
	ld a, (de)		; e->x
	ld h, d
	ld l, e
	ld bc, 12	; sizeof (struct entity)
	add hl, bc
	add a, 32
	ld (hl), a
	add hl, bc
	add a, -32
	ld (hl), a
	add hl, bc
	add a, 16
	ld (hl), a
	add hl, bc
	add a, 16
	ld (hl), a
	add hl, bc
	add a, 16
	ld (hl), a	; copy the movement to all sprites
	ret




; Behavior for final enemy 2
behav_fenemy2:
	; First, check if we have been defeated

	ld d, h
	ld e, l			; save e in DE

	inc hl
	inc hl
	ld a, (hl)
	and a
	jr nz, final2_start
	ld a, (_fenemy_defeat)
	and a
	jr nz, inc_f2_defeated			; if the enemy is defeated, there is nothing left to do for us but increase the count
	inc a
	ld (_fenemy_defeat),a		; final_enemy_defeated=1, now make the rest of it explode
	ld bc, 12	
	add hl, bc				; we are pointint at sprite2->sprnum
      ld (hl), EXPLOSION_SPR
	inc hl
	inc hl
	ld (hl), MOVE_EXPLOSION
	inc hl
	inc hl
	ld (hl), 4				; param1
	ld bc, 8
	add hl, bc				; sprite3
      ld (hl), EXPLOSION_SPR
	inc hl
	inc hl
	ld (hl), MOVE_EXPLOSION
	inc hl
	inc hl
	ld (hl), 4				; param1
	add hl, bc				; sprite4
      ld (hl), EXPLOSION_SPR
	inc hl
	inc hl
	ld (hl), MOVE_EXPLOSION
	inc hl
	inc hl
	ld (hl), 4				; param1
	ret

inc_f2_defeated:
	inc a
	ld (_fenemy_defeat), a
	ret

final2_start:
	; before doing anything else, just check if we are already dying. Otherwise some nasty things could happen
	ld h, d
	ld l, e
	ld bc, 4
	add hl, bc
	ld a, (hl)		; A == e->movement
	cp MOVE_EXPLOSION
	ret z			; just return if exploding!

	ld h, d
	ld l, e
	ld bc, 11
	add hl, bc		; HL == e->behav_param	
	ld a, (hl)	
	inc a
	ld (hl), a
	cp 128
	jp nc, shooting_final2		; if behav_param > 127, then we are shooting
	and  $1f
	jr nz, nochangedirection_final2	; will change X direction every 32 frames
	ld bc, -5
	add hl, bc
	ld a, (hl)
	neg
	ld (hl), a			; change direction in X
	inc hl
	inc hl
	inc hl
	ld a, (hl)
	neg
	ld (hl), a			; also switch Y direction, to form a circle
	jp move_rest_final2
nochangedirection_final2:	
	ld a, (hl)
	cp 127
	jp nz, move_rest_final2	; we will shoot every 256 behav_params
doshoot_final2:
	; First, remove shield, then process shoot
	push  hl
	ld bc, -7
	add hl, bc
	ld a, MOVE_NONE	; freeze during shoot
	ld (hl), a

	ld h, d
	ld l, e
	ld bc, 50
	add hl, bc		; point to sprnum for the first shield sprite
	ld bc, 12
	xor a
	ld (hl), a
	add hl, bc
	ld (hl), a
	add hl, bc
	ld (hl), a

	; There will be two types of shoot
	; If we have > 50% of the energy, shoot large lasers
	; else, shoot many small bombs, one of them directed to our position

	ld h, d
	ld l, e

	ld bc, 5
	add hl, bc
	ld a, (hl)
	ld bc, -5
	add hl, bc
	cp 10		; when the enemy is dying, it will be more dangerous
	jr c, shoot2_final2
shoot1_final2:
	push de
	ld a, (hl)
	sub 18
	ld c, a
	ld b, 0
	push bc			; e->x
	inc hl
	ld a, (hl)
	add a, 8
	ld c, a
	push bc			; e->y
	ld bc, MOVE_LEFT
	push bc			; MOVE_LEFT
	ld bc, SHOT_LASER
	push bc			; SHOT_ENEMY
	call _NewEnemyShoot_FX
	pop bc
	pop bc
	pop bc
	pop bc
	pop de

	ld h, d
	ld l, e
	push de
	ld a, (hl)
	sub 26
	ld c, a
	ld b, 0
	push bc			; e->x
	inc hl
	ld a, (hl)
	add a, 8
	ld c, a
	push bc			; e->y
	ld bc, MOVE_LEFT
	push bc			; MOVE_LEFT
	ld bc, SHOT_LASER
	push bc			; SHOT_ENEMY
	call _NewEnemyShoot
	pop bc
	pop bc
	pop bc
	pop bc
	pop de
	; upon return, HL holds the pointer to the new enemy shoot
	jp shoot2_done
shoot2_final2: ; 3 shoots, in different directions
	push de
	ld a, (hl)
	sub 18
	ld c, a
	ld b, 0
	push bc			; e->x
	inc hl
	ld a, (hl)
	add a, 16
	ld c, a
	push bc			; e->y
	ld bc, MOVE_LEFT
	push bc			; MOVE_LEFT
	ld bc, SHOT_MEGA
	push bc			; SHOT_ENEMY
	call _NewEnemyShoot_FX
	pop de			; de = SHOOT_MEGA
	pop bc			; bc = MOVE_LEFT
	pop iy			; iy = e->y
	pop ix			; ix = e->x

	ld bc, MOVE_UPLEFT
	push ix
	push iy
	push bc
	push de
	call _NewEnemyShoot	; upon return, HL holds the pointer to the new enemy shoot

	; upon return, HL holds the pointer to the new enemy shoot
	ld a, l
	or h
	jp z, shoot2_skip1			; If HL == 0, then skip, as the enemy shoot could not be created

	ld bc, 6
	add hl, bc
	ld a, 4
	ld (hl), a		; e->param1=4
	inc hl
	ld (hl), a		; e->param1=4
shoot2_skip1:
	pop de			; de = SHOOT_MEGA
	pop bc			; bc = MOVE_LEFT
	pop iy			; iy = e->y
	pop ix			; ix = e->x

	ld bc, MOVE_DOWNLEFT
	push ix
	push iy
	push bc
	push de
	call _NewEnemyShoot	; upon return, HL holds the pointer to the new enemy shoot
	; upon return, HL holds the pointer to the new enemy shoot
	ld a, l
	or h
	jp z, shoot2_skip2			; If HL == 0, then skip, as the enemy shoot could not be created

	ld bc, 6
	add hl, bc
	ld a, 4
	ld (hl), a
	inc hl
	ld (hl), a
	pop de			; de = SHOOT_MEGA
	pop bc			; bc = MOVE_LEFT
	pop iy			; iy = e->y
	pop ix			; ix = e->x

shoot2_skip2:
	ld bc, MOVE_TARGET
	push ix
	push iy
	push bc
	push de
	call _NewEnemyShoot	; upon return, HL holds the pointer to the new enemy shoot
	pop de
	pop de
	pop de			; DE = e->y
	pop bc			; BC = e->x

	; Then, generate parameters for the shoot
	push hl
	pop ix			; IX holds the pointer to the new enemy shoot

	ld a, ixh
	or ixl
	ret z			; If IX == 0, then skip, as the enemy shoot could not be created

	ld d, c		; D==x_enemy,  E is already ==y_enemy
	ld a, (_ship_x)
	add a, 16
	ld h, a			; H = x_ship
	ld a, (_ship_y)
	ld l, a			; L = y_ship
	call calcdirectshoot

	ld (ix+6), h
	ld (ix+7), l
	ld (ix+8), d
	ld (ix+9), e
	ld (ix+3), 0
	ld (ix+5), 0
	pop de




shoot2_done:
	pop hl
shooting_final2:
	ld a, (hl)
	inc a
	ld (hl), a
	cp 160
	jr c, move_rest_final2
restore_sprites_final2:
	ld a, 255
	ld (hl), a
	ld bc, -7
	add hl, bc
	ld a, MOVE_WAVE_NOGONELEFT	; move again
	ld (hl), a
	ld h, d
	ld l, e
	ld bc, 50
	add hl, bc		; point to sprnum for the first shield sprite
	ld a, 26
	ld (hl), a
	ld bc, 12
	add hl, bc
	ld a, 27
	ld (hl), a
	add hl, bc
	ld a, 28
	ld (hl), a
	
move_rest_final2:		; move the rest of the final enemy according to sprite 0
	ld h, d
	ld l, e
	ld a, (hl)		; e->x
	inc hl
	ex af, af'
	ld a, (hl)		; e->y
	ld bc, 11	; sizeof (struct entity) - 1 
	add hl, bc
	ex af, af'
	add a, 16
	ld (hl), a
	ex af, af'
	inc hl
	ld (hl), a

	add hl, bc
	ex af, af'
	sub 16
	ld (hl), a
	ex af, af'
	inc hl
	add a, 16
	ld (hl), a

	add hl, bc
	ex af, af'
	add a, 16
	ld (hl), a
	ex af, af'
	inc hl
	ld (hl), a

	add hl, bc		; this is the shield part
	ex af, af'		; we have to move -24 pixels in X, -22 in Y
	sub 24
	ld (hl), a
	ex af, af'
	sub 22
	inc hl
	ld (hl), a

	add hl, bc
	ex af, af'
	ld (hl), a
	ex af, af'
	add a, 16
	inc hl
	ld (hl), a

	add hl, bc
	ex af, af'
	ld (hl), a
	ex af, af'
	add a, 16
	inc hl
	ld (hl), a
	

	; copy the movement to all sprites
	ret


; Behavior for final enemy 3
behav_fenemy3:
	; First, check if we have been defeated

	ld d, h
	ld e, l			; save e in DE

	inc hl
	inc hl
	ld a, (hl)
	and a
	jr nz, final3_start
	ld a, (_fenemy_defeat)
	and a
	jr nz, inc_f3_defeated			; if the enemy is defeated, there is nothing left to do for us but increase the count
	inc a
	ld (_fenemy_defeat),a		; final_enemy_defeated=1, now make the rest of it explode
	ld bc, 12	
	add hl, bc				; we are pointint at sprite2->sprnum
        ld (hl), EXPLOSION_SPR
	inc hl
	inc hl
	ld (hl), MOVE_EXPLOSION
	inc hl
	inc hl
	ld (hl), 4				; param1
	ld bc, 8
	add hl, bc				; sprite3
	ld (hl), EXPLOSION_SPR
	inc hl
	inc hl
	ld (hl), MOVE_EXPLOSION
	inc hl
	inc hl
	ld (hl), 4				; param1
	add hl, bc				; sprite4
        ld (hl), EXPLOSION_SPR
	inc hl
	inc hl
	ld (hl), MOVE_EXPLOSION
	inc hl
	inc hl
	ld (hl), 4				; param1
	ret

inc_f3_defeated:
	inc a
	ld (_fenemy_defeat), a
	ret

final3_start:
	; before doing anything else, just check if we are already dying. Otherwise some nasty things could happen
	ld h, d
	ld l, e
	ld bc, 4
	add hl, bc
	ld a, (hl)		; A == e->movement
	cp MOVE_EXPLOSION
	ret z			; just return if exploding!

	ld h, d
	ld l, e

	; as a first step, animate alien
	ld bc, 11
	add hl, bc		; HL == e->behav_param
	ld a, (hl)
	and $C			; e->behav_param & 0xC, that will be the animation 
	add a, 22		; this is the first sprite for this animation
	ld bc, -9	
	add hl, bc		; hl == e->sprnum
	ld (hl),a
	ld bc, 12
	add hl, bc
	inc a
	ld (hl), a
	add hl, bc
	inc a
	ld (hl), a
	add hl, bc
	inc a
	ld (hl), a


	ld h, d
	ld l, e
	ld bc, 11
	add hl, bc		; HL == e->behav_param
	ld a, (hl)
	and $03f
	jr nz, dec_behav_param_3 ; shoot every 64 frames

; Generate new shoot

shoot1_final3:
	push hl
	push de
	ld h, d
	ld l, e
	ld a, (hl)
	sub 18
	ld c, a
	ld b, 0
	push bc			; e->x
	inc hl
	ld a, (hl)
	add a, 8
	ld c, a
	push bc			; e->y
	ld bc, MOVE_LEFT
	push bc			; MOVE_LEFT
	ld bc, SHOT_LASER
	push bc			; SHOT_ENEMY
	call _NewEnemyShoot_FX
	pop bc
	pop bc
	pop bc
	pop bc
	pop de
	; upon return, HL holds the pointer to the new enemy shoot

	ld h, d
	ld l, e
	push de
	ld a, (hl)
	sub 26
	ld c, a
	ld b, 0
	push bc			; e->x
	inc hl
	ld a, (hl)
	add a, 8
	ld c, a
	push bc			; e->y
	ld bc, MOVE_LEFT
	push bc			; MOVE_LEFT
	ld bc, SHOT_LASER
	push bc			; SHOT_ENEMY
	call _NewEnemyShoot
	pop bc
	pop bc
	pop bc
	pop bc

        ld hl, FX_LASER
        push hl
        call _wyz_effect
        pop hl


	pop de
	pop hl
	; upon return, HL holds the pointer to the new enemy shoot



dec_behav_param_3:
	dec (hl)		; e->behav_param--;
	ld a, (hl)
	ld h, d
	ld l, e
	ld bc, 4
	add hl, bc		; move to e->movement
	and $30
	cp $10
	jr z, make_goright_final3
	cp $20
	jr z, make_goleft_final3
	cp $30
	jr nz, move_rest_final3
make_kamikaze_final3:
	ld (hl), MOVE_KAMIKAZE_NOANIM
	inc hl
	inc hl
	ld (hl), 0
	jr move_rest_final3
make_goleft_final3:
	ld (hl), MOVE_LEFT
	inc hl
	inc hl
	ld (hl), 10
	jr move_rest_final3
make_goright_final3:
	ld (hl), MOVE_RIGHT
	inc hl
	inc hl
	ld (hl), 5
move_rest_final3:		; move the rest of the final enemy according to sprite 0
	ld h, d
	ld l, e
	ld a, (hl)		; e->x
	inc hl
	ex af, af'
	ld a, (hl)		; e->y
	cp 96
	jr c, noadjust_final3	; since the final enemy is 2x sprites, we need to make sure we do not overwrite the score table
	ld a, 96
	ld (hl), a
noadjust_final3:
	ld bc, 11	; sizeof (struct entity) - 1 
	add hl, bc
	ex af, af'
	add a, 16
	ld (hl), a
	ex af, af'
	inc hl
	ld (hl), a

	add hl, bc
	ex af, af'
	sub 16
	ld (hl), a
	ex af, af'
	inc hl
	add a, 16
	ld (hl), a

	add hl, bc
	ex af, af'
	add a, 16
	ld (hl), a
	ex af, af'
	inc hl
	ld (hl), a


	; copy the movement to all sprites
	ret


; Wait until ship X distance is < 30, then jump
; While jumping, it will be animated
; Uses: behav_param: y speed, it will be 6 initially, then decremented every frame until -6
;       param4: simple toogle: 0 means movement not started, 1 means movement started
behav_saltarin:
	push hl	
	ld bc, 9
	add hl, bc
	ld a, (hl)
	pop hl
	and a
	jr nz, saltarin_alreadyjumping
saltarin_notjumping:
	ld a, (_ship_x)
	sub (hl)
	jr nc, saltarin_nocarry	; ship_x - e->x is greater or equal than zero
saltarin_carry:
	neg			; we convert A to a positive value
saltarin_nocarry:
	cp 30 
	ret nc			; if the distance is greater than 30, we just do not care
saltarin_jump:
	push hl
	ld bc, 9
	add hl, bc		; point to e->param4
	ld (hl), 1		; we are jumping!
	inc hl
	inc hl
	ld (hl), -10		; set the vertical speed to 10
	pop hl
saltarin_alreadyjumping:	
	; when jumping, the value of e->behav_param changes the animation
	; if > 2, second sprite
	; if < 2 but > -2, third sprite
	; if < -2, fourth sprite
	ld d, h
	ld e, l
	ld bc, 11
	add hl, bc	; point to e->behav_param
	ld a, (hl)
	cp 10
	jr z, saltarin_goingdown
	ld h, d
	ld l, e	
	inc hl
	add a,(hl)	; A + (HL) is the new position
	ld (hl), a	
	ld bc, 10
	add hl, bc	; point to e->behav_param
	ld a, (hl)
	inc a
	ld (hl), a
	ld bc, -9
	add hl, bc		; point to e->sprnum
	cp 246
	jr nc, saltarin_anim1	; if < -3
	cp 3
	jr nc, saltarin_anim3   ; if > 3
saltarin_anim2:
	ld (hl), ENEMY_SALTARIN+2
	ret
saltarin_anim3:
	ld (hl), ENEMY_SALTARIN+3
	ret
saltarin_anim1:
	ld (hl), ENEMY_SALTARIN+1
	ret
saltarin_goingdown:
	ld h, d
	ld l, e	
	inc hl
	add a,(hl)	; A + (HL) is the new position
	cp 112
	jr nc, saltarin_gonedown		; if the position is >= 112, the enemy is gone
	ld (hl), a
	ret
saltarin_gonedown:
	inc hl			; HL points now to e->sprnum
	xor a
	ld (hl), a
	ret


; Wait until ship X distance is < 30, then jump
; While jumping, it will be animated
; Uses: behav_param: y speed, it will be 6 initially, then decremented every frame until -6
;       param4: simple toogle: 0 means movement not started, 1 means movement started
behav_saltarin_bis:
	push hl	
	ld bc, 9
	add hl, bc
	ld a, (hl)
	pop hl
	and a
	jr nz, saltarinbis_alreadyjumping
saltarinbis_notjumping:
	ld a, (_ship_x)
	sub (hl)
	jr nc, saltarinbis_nocarry	; ship_x - e->x is greater or equal than zero
saltarinbis_carry:
	neg			; we convert A to a positive value
saltarinbis_nocarry:
	cp 30 
	ret nc			; if the distance is greater than 30, we just do not care
saltarinbis_jump:
	push hl
	ld bc, 9
	add hl, bc		; point to e->param4
	ld (hl), 1		; we are jumping!
	inc hl
	inc hl
	ld (hl), -10		; set the vertical speed to 10
	pop hl
saltarinbis_alreadyjumping:	
	; when jumping, the value of e->behav_param changes the animation
	; if > 2, second sprite
	; if < 2 but > -2, third sprite
	; if < -2, fourth sprite
	ld d, h
	ld e, l
	ld bc, 11
	add hl, bc	; point to e->behav_param
	ld a, (hl)
	cp 10
	jr z, saltarinbis_goingdown
	ld h, d
	ld l, e	
	inc hl
	add a,(hl)	; A + (HL) is the new position
	ld (hl), a	
	ld bc, 10
	add hl, bc	; point to e->behav_param
	ld a, (hl)
	inc a
	ld (hl), a
	ld bc, -9

	add hl, bc		; point to e->sprnum
	cp 246
	jr nc, saltarinbis_anim1	; if < -3
	cp 3
	jr nc, saltarinbis_anim3   ; if > 3
saltarinbis_anim2:
	ld (hl), ENEMY_SALTARIN_BIS+2
	ret
saltarinbis_anim3:
	ld (hl), ENEMY_SALTARIN_BIS+3
	ret
saltarinbis_anim1:
	ld (hl), ENEMY_SALTARIN_BIS+1
	ret
saltarinbis_goingdown:
	ld h, d
	ld l, e	
	inc hl
	add a,(hl)	; A + (HL) is the new position
	cp 112
	jr nc, saltarinbis_gonedown		; if the position is >= 112, the enemy is gone
	ld (hl), a
	ret
saltarinbis_gonedown:
	inc hl			; HL points now to e->sprnum
	xor a
	ld (hl), a
	ret




; Casco is walking left. Every 24 frames stop, shoot a missile, wait for 8 frames and continue
; Uses: behav_param: counter
behav_casco:
	ld d, h
	ld e, l	
	ld bc, 11
	add hl, bc
	ld a, (hl)	; a = e->behav_param
	inc a
	cp 24
	jr z, casco_shoot
	cp 32
	jr nz, casco_storeandcont
casco_moveagain:
	xor a
	ld (hl), a	; e->behav_param = 0
	ld bc, -7
	add hl, bc	; HL points to e->movement
	ld (hl), MOVE_LEFTANIM
	inc hl
	inc hl
	ld (hl), 3	; move with animation, change speed
	ret
casco_shoot:
	ld (hl), a	; store e->behav_param
	ld bc, -7
	add hl, bc	; HL points to e->movement
	ld (hl), MOVE_LEFT
	inc hl
	inc hl
	ld (hl), 2	; move without, at the background speed
	; now we should create the shoot and stuff

	; NewEnemyShoot(e->x, e->y, MOVE_LEFT, SHOT_ENEMY)

	ld a, (de)
	ld c, a
	ld b, 0
	push bc			; e->x
	inc de
	ld a, (de)
	ld c, a
	push bc			; e->y
	ld bc, MOVE_MISSILE
	push bc			; MOVE_MISSILE
	ld bc, SHOT_CASCO_MISSILE
	push bc			; SHOT_CASCO_MISSILE
	call _NewEnemyShoot_FX
	pop bc
	pop bc
	pop bc
	pop bc
	; upon return, HL holds the pointer to the new enemy shoot
	ld a, l
	or h
	ret z                    ; If HL == 0, then skip, as the enemy shoot could not be created

	ld bc, 6
	add hl, bc
	ld (hl), 4		; move sloooowly
	inc hl
	ld (hl), 0		; param2=0

	ret
casco_storeandcont:
	ld (hl), a
	ret



; Casco is walking left. Every 24 frames stop, shoot a missile, wait for 8 frames and continue
; Uses: behav_param: counter
behav_casco_bis:
	ld d, h
	ld e, l
	ld bc, 11
	add hl, bc
	ld a, (hl)	; a = e->behav_param
	inc a
	cp 24
	jr z, cascobis_shoot
	cp 32
	jr nz, cascobis_storeandcont
cascobis_moveagain:
	xor a
	ld (hl), a	; e->behav_param = 0
	ld bc, -7
	add hl, bc	; HL points to e->movement
	ld (hl), MOVE_LEFTANIM
	inc hl
	inc hl
	ld (hl), 3	; move with animation, change speed
	ret
cascobis_shoot:
	ld (hl), a	; store e->behav_param
	ld bc, -7
	add hl, bc	; HL points to e->movement
	ld (hl), MOVE_LEFT
	inc hl
	inc hl
	ld (hl), 2	; move without, at the background speed
	; now we should create the shoot and stuff

	; NewEnemyShoot(e->x, e->y, MOVE_LEFT, SHOT_ENEMY)

	ld a, (de)
	ld c, a
	ld b, 0
	push bc			; e->x
	inc de
	ld a, (de)
	ld c, a
	push bc			; e->y
	ld bc, MOVE_MISSILE
	push bc			; MOVE_MISSILE
	ld bc, SHOT_CASCO_MISSILE_BIS
	push bc			; SHOT_CASCO_MISSILE
	call _NewEnemyShoot_FX
	pop bc
	pop bc
	pop bc
	pop bc
	; upon return, HL holds the pointer to the new enemy shoot
	ld a, l
	or h
	ret z                    ; If HL == 0, then skip, as the enemy shoot could not be created

	ld bc, 6
	add hl, bc
	ld (hl), 4		; move sloooowly
	inc hl
	ld (hl), 0		; param2=0

	ret
cascobis_storeandcont:
	ld (hl), a
	ret

; Just wait until the animation reachs frame 7, then move towards ship
; behav_param: counter, will wait until it reaches 57

behav_egg:
	ld d, h
	ld e, l
	ld bc, 11
	add hl, bc
	inc (hl)
	ld a, (hl)	; a = e->behav_param
	cp 57
	ret nz
	; now, change the movement to move_left
	ld h, d
	ld l, e
	ld bc, 4
	add hl, bc
	ld (hl), MOVE_KAMIKAZE_2
	inc hl
	inc hl
	ld (hl), 6	; move fast
	ret

; Ugly guy is constantly jumping
; Uses: behav_param: y speed, it will be 4 initially, then decremented every frame until -4, then cycle
behav_uglyguy:
	ld d, h
	ld e, l
	ld bc, 11
	add hl, bc	; point to e->behav_param
	ld a, (hl)
	dec a
	cp -5
	jr nz, ugluguy_nochange
	ld a, 4
ugluguy_nochange:
	ld (hl), a
	ld h, d
	ld l, e
	inc hl
	add a,(hl)	; A + (HL) is the new position
	ld (hl), a

	ret


; Behavior for final enemy 5
behav_fenemy5:
	; First, check if we have been defeated

	ld d, h
	ld e, l			; save e in DE

	inc hl
	inc hl
	ld a, (hl)
	and a
	jr nz, final5_start
	ld a, (_fenemy_defeat)
	and a
	jr nz, inc_f5_defeated			; if the enemy is defeated, there is nothing left to do for us but increase the count
	inc a
	ld (_fenemy_defeat),a		; final_enemy_defeated=1, now make the rest of it explode
	ld bc, 12	
	add hl, bc				; we are pointint at sprite2->sprnum
        ld (hl), EXPLOSION_SPR
	inc hl
	inc hl
	ld (hl), MOVE_EXPLOSION
	inc hl
	inc hl
	ld (hl), 4				; param1
	ld bc, 8
	add hl, bc				; sprite3
	ld (hl), EXPLOSION_SPR
	inc hl
	inc hl
	ld (hl), MOVE_EXPLOSION
	inc hl
	inc hl
	ld (hl), 4				; param1
	add hl, bc				; sprite4
        ld (hl), EXPLOSION_SPR
	inc hl
	inc hl
	ld (hl), MOVE_EXPLOSION
	inc hl
	inc hl
	ld (hl), 4				; param1
	ret

inc_f5_defeated:
	inc a
	ld (_fenemy_defeat), a
	ret

final5_start:
	; before doing anything else, just check if we are already dying. Otherwise some nasty things could happen
	ld h, d
	ld l, e
	ld bc, 4
	add hl, bc
	ld a, (hl)		; A == e->movement
	cp MOVE_EXPLOSION
	ret z			; just return if exploding!

	ld h, d
	ld l, e

	; as a first step, animate alien
	ld bc, 11
	add hl, bc		; HL == e->behav_param
	ld a, (hl)
	and $C			; e->behav_param & 0xC, that will be the animation 
	add a, 22		; this is the first sprite for this animation
	ld bc, -9	
	add hl, bc		; hl == e->sprnum
	ld (hl),a
	ld bc, 12
	add hl, bc
	inc a
	ld (hl), a
	add hl, bc
	inc a
	ld (hl), a
	add hl, bc
	inc a
	ld (hl), a

	ld h, d
	ld l, e
	ld bc, 11
	add hl, bc		; HL == e->behav_param
	ld a, (hl)
	and $0f
	jr nz, dec_behav_param_5 ; shoot every 32 frames
; Generate new shoot

shoot_final5:
	push hl
	push de
	ld h, d
	ld l, e
	ld a, (hl)
	sub 18
	ld c, a
	ld b, 0
	push bc			; e->x
	inc hl
	ld a, (hl)
	ld c, a
	push bc			; e->y
	ld bc, MOVE_LEFT
	push bc			; MOVE_LEFT
	ld bc, SHOT_MEGA
	push bc			; SHOT_ENEMY
	call _NewEnemyShoot_FX
	pop bc
	pop bc
	pop bc
	pop bc
	pop de
	; upon return, HL holds the pointer to the new enemy shoot
	pop hl



dec_behav_param_5:
	inc (hl)		; e->behav_param++;
	ld a, (hl)
;	and $7F
	ld h, d
	ld l, e
	ld bc, 4
	add hl, bc		; move to e->movement
	
	cp $10
	jr z, make_godownleft_final5
	cp $20
	jr z, make_goleft_final5
	cp $30
	jr z, make_goup_final5
	cp $40
	jr z, make_godown_final5
	cp $50
	jr z, make_goright_final5
	cp $60
	jr z, make_goupright_final5
	cp $70
	jr nz, move_rest_final5


make_stop_final5:
	ld (hl), MOVE_NONE
	jr move_rest_final5
make_godownleft_final5:
	ld (hl), MOVE_DOWNLEFT
	inc hl
	inc hl
	ld (hl), 3
	inc hl
	ld (hl), 3
	jr move_rest_final5
make_goupright_final5:
	ld (hl), MOVE_UPRIGHT
	inc hl
	inc hl
	ld (hl), 3
	jr move_rest_final5
make_goleft_final5:
	ld (hl), MOVE_LEFT
	inc hl
	inc hl
	ld (hl), 5
	jr move_rest_final5
make_goright_final5:
	ld (hl), MOVE_RIGHT
	inc hl
	inc hl
	ld (hl), 5
	jr move_rest_final5
make_goup_final5:
	ld (hl), MOVE_UP
	inc hl
	inc hl
	ld (hl), 3
	jr move_rest_final5
make_godown_final5:
	ld (hl), MOVE_DOWNLEFT
	inc hl
	inc hl
	ld (hl), 3
	inc hl
	ld (hl), 0
	jr move_rest_final5


move_rest_final5:		; move the rest of the final enemy according to sprite 0
	ld h, d
	ld l, e
	ld a, (hl)		; e->x
	inc hl
	ex af, af'
	ld a, (hl)		; e->y

	ld bc, 11
	add hl, bc
	ex af, af'
	add a, 16
	ld (hl), a
	ex af, af'
	inc hl
	ld (hl), a

	add hl, bc
	ex af, af'
	sub 16
	ld (hl), a
	ex af, af'
	inc hl
	add a, 16
	ld (hl), a

	add hl, bc
	ex af, af'
	add a, 16
	ld (hl), a
	ex af, af'
	inc hl
	ld (hl), a

ret




; Behavior for final enemy 6
behav_fenemy6:
	; First, check if we have been defeated

	ld d, h
	ld e, l			; save e in DE

	inc hl
	inc hl
	ld a, (hl)
	and a
	jr nz, final6_start
	ld a, (_fenemy_defeat)
	and a
	jr nz, inc_f6_defeated			; if the enemy is defeated, there is nothing left to do for us but increase the count
	inc a
	ld (_fenemy_defeat),a		; final_enemy_defeated=1, now make the rest of it explode
	ld bc, 12	
	add hl, bc				; we are pointint at sprite2->sprnum
        ld (hl), EXPLOSION_SPR
	inc hl
	inc hl
	ld (hl), MOVE_EXPLOSION
	inc hl
	inc hl
	ld (hl), 4				; param1
	ld bc, 8
	add hl, bc				; sprite3
	ld (hl), EXPLOSION_SPR
	inc hl
	inc hl
	ld (hl), MOVE_EXPLOSION
	inc hl
	inc hl
	ld (hl), 4				; param1
	add hl, bc				; sprite4
        ld (hl), EXPLOSION_SPR
	inc hl
	inc hl
	ld (hl), MOVE_EXPLOSION
	inc hl
	inc hl
	ld (hl), 4				; param1
	ret

inc_f6_defeated:
	inc a
	ld (_fenemy_defeat), a
	ret

final6_start:
	; before doing anything else, just check if we are already dying. Otherwise some nasty things could happen
	ld h, d
	ld l, e
	ld bc, 4
	add hl, bc
	ld a, (hl)		; A == e->movement
	cp MOVE_EXPLOSION
	ret z			; just return if exploding!

	; as a first step, animate robot
	ld h, d
	ld l, e
	ld bc, 11
	add hl, bc		; HL == e->behav_param
	ld a, (hl)
	and $F			; e->behav_param & 0xF
				; for A = 0-1, first animation step
				; for A = 2-3, second animation step
				; for A = 4-5, third animation step
				; for A = 6-15, still animation
	cp 2
	jp c, final6_animate1
	cp 4
	jp c, final6_animate2
	cp 6
	jp c, final6_animate3
final6_animate_still:
	ld a, 22
	jp final6_storeanimation
final6_animate1:
	ld a, 26
	jp final6_storeanimation
final6_animate2:
	ld a, 30
	jp final6_storeanimation
final6_animate3:
	ld a, 34

final6_storeanimation:
	ld bc, -9	
	add hl, bc		; hl == e->sprnum
	ld (hl),a
	ld bc, 12
	add hl, bc
	inc a
	ld (hl), a
	add hl, bc
	inc a
	ld (hl), a
	add hl, bc
	inc a
	ld (hl), a




	ld h, d
	ld l, e
	ld bc, 11
	add hl, bc		; HL == e->behav_param
	ld a, (hl)
	and $0f
	cp 2
	jp nz, inc_behav_param_6 ; shoot every 16 frames
; Generate new shoot
; If the energy is below 50%, shoot homing missiles instead of simple lasers
	ld bc, -6
	add hl, bc
	ld a, (hl)
	ld bc, 6
	add hl, bc		; set HL back to e->behav_param, we need it later on
	cp 25
	jp c, shoot_final6_lowenergy

shoot_final6:
	push hl
	push de
	ld h, d
	ld l, e
	ld a, (hl)
	sub 10
	ld c, a
	ld b, 0
	push bc			; e->x
	inc hl
	ld a, (hl)
	add a, 8
	ld c, a
	push bc			; e->y
	ld bc, MOVE_LEFT
	push bc			; MOVE_LEFT
	ld bc, SHOT_MEGAMISSILE_2
	push bc			; SHOT_ENEMY
	call _NewEnemyShoot_FX
	pop bc
	pop bc
	pop bc
	pop bc
	pop de
	; upon return, HL holds the pointer to the new enemy shoot

	ld h, d
	ld l, e
	push de
	ld a, (hl)
	sub 26
	ld c, a
	ld b, 0
	push bc			; e->x
	inc hl
	ld a, (hl)
	add a, 8
	ld c, a
	push bc			; e->y
	ld bc, MOVE_LEFT
	push bc			; MOVE_LEFT
	ld bc, SHOT_MEGAMISSILE_1
	push bc			; SHOT_ENEMY
	call _NewEnemyShoot
	pop bc
	pop bc
	pop bc
	pop bc
	pop de
	pop hl
	; upon return, HL holds the pointer to the new enemy shoot
	jp inc_behav_param_6


shoot_final6_lowenergy:
	push hl
	push de
	ld h, d
	ld l, e
	ld a, (hl)
	sub 18
	ld c, a
	ld b, 0
	push bc			; e->x
	inc hl
	ld a, (hl)
	ld c, a
	push bc			; e->y
	ld bc, MOVE_TARGET
	push bc			; MOVE_TARGET
	ld bc, SHOT_MEGA
	push bc			; SHOT_ENEMY
	call _NewEnemyShoot_FX
	pop de
	pop de
	pop de			; DE = e->y
	pop bc			; BC = e->x

	; upon return, HL holds the pointer to the new enemy shot
	; Then, generate parameters for the shoot
	push hl
	pop ix			; IX holds the pointer to the new enemy shoot

	ld a, ixh
	or ixl
	jp z, shoot_final6_low_skip1			; If IX == 0, then skip, as the enemy shoot could not be created

	ld d, c		; D==x_enemy,  E is already ==y_enemy
	ld a, (_ship_x)
	add a, 16
	ld h, a			; H = x_ship
	ld a, (_ship_y)
	ld l, a			; L = y_ship
	call calcdirectshoot

	ld (ix+6), h
	ld (ix+7), l
	ld (ix+8), d
	ld (ix+9), e
	ld (ix+3), 0
	ld (ix+5), 0


shoot_final6_low_skip1:
	pop de
	ld h, d
	ld l, e
	push de
	ld a, (hl)
	sub 18
	ld c, a
	ld b, 0
	push bc			; e->x
	inc hl
	ld a, (hl)
	add a, 16
	ld c, a
	push bc			; e->y
	ld bc, MOVE_TARGET
	push bc			; MOVE_TARGET
	ld bc, SHOT_MEGA
	push bc			; SHOT_ENEMY
	call _NewEnemyShoot
	pop de
	pop de
	pop de			; DE = e->y
	pop bc			; BC = e->x

	; upon return, HL holds the pointer to the new enemy shot
	; Then, generate parameters for the shoot
	push hl
	pop ix			; IX holds the pointer to the new enemy shoot

	ld a, ixh
	or ixl
	jp z, shoot_final6_low_skip2			; If IX == 0, then skip, as the enemy shoot could not be created

	ld d, c		; D==x_enemy,  E is already ==y_enemy
	ld a, (_ship_x)
	add a, 16
	ld h, a			; H = x_ship
	ld a, (_ship_y)
	ld l, a			; L = y_ship
	call calcdirectshoot

	ld (ix+6), h
	ld (ix+7), l
	ld (ix+8), d
	ld (ix+9), e
	ld (ix+3), 0
	ld (ix+5), 0

shoot_final6_low_skip2:
	pop de
	pop hl

inc_behav_param_6:
	inc (hl)		; e->behav_param++;
	ld a, (hl)
	and $7F
	ld h, d
	ld l, e
	ld bc, 4
	add hl, bc		; move to e->movement
	
	cp $10
	jr z, make_godownleft_final6
	cp $20
	jr z, make_goupleft_final6
	cp $30
	jr z, make_goleft_final6
	cp $38
	jr z, make_goright_final6
	cp $50
	jr z, make_goleft2_final6
	cp $60
	jr nz, move_rest_final6


make_stop_final6:
	ld (hl), MOVE_NONE
	jr move_rest_final6
make_godownleft_final6:
	ld (hl), MOVE_DOWNLEFT
	inc hl
	inc hl
	ld (hl), 3
	inc hl
	ld (hl), 2
	jr move_rest_final6
make_goupleft_final6:
	ld (hl), MOVE_UPLEFT
	inc hl
	inc hl
	ld (hl), 3
	inc hl
	ld (hl), 2
	jr move_rest_final6
make_goleft_final6:
	ld (hl), MOVE_LEFT
	inc hl
	inc hl
	ld (hl), 8
	jr move_rest_final6
make_goright_final6:
	ld (hl), MOVE_RIGHT
	inc hl
	inc hl
	ld (hl), 8
	jr move_rest_final6
make_goleft2_final6:
	ld (hl), MOVE_LEFT
	inc hl
	inc hl
	ld (hl), 4
	jr move_rest_final6


move_rest_final6:		; move the rest of the final enemy according to sprite 0
	ld h, d
	ld l, e
	ld a, (hl)		; e->x
	inc hl
	ex af, af'
	ld a, (hl)		; e->y

	ld bc, 11
	add hl, bc
	ex af, af'
	add a, 16
	ld (hl), a
	ex af, af'
	inc hl
	ld (hl), a

	add hl, bc
	ex af, af'
	sub 16
	ld (hl), a
	ex af, af'
	inc hl
	add a, 16
	ld (hl), a

	add hl, bc
	ex af, af'
	add a, 16
	ld (hl), a
	ex af, af'
	inc hl
	ld (hl), a

	ret



; behaviour for enemies following another enemy
; basically check if the enemy is exploding, and if so explode as well

behav_follow:
	ld d, h
	ld e, l			; save e in DE
	ld bc, 7
	add hl, bc		; HL == e->param2
	ld a, (hl)
	cp 255
	ret z			; if e->param2 is 255, just return

	inc a
	ld hl, 	_active_enemies-12
	ld bc, 12
follow_loop:
	add hl, bc
	dec a
	jr nz, follow_loop	; position at the reference enemy
	
	ld bc,4
	add hl, bc		; HL == enemy->movement
	ld a, (hl)
	cp MOVE_EXPLOSION
	ret nz			; return if it is not exploding
; enemy is exploding, copy parameters
	dec hl
	dec hl			; point at enemy->sprnum
	inc de
	inc de			; point at enemy->sprnum
	ld a, (hl)
	ld (de), a
	inc hl
	inc hl
	inc de
	inc de			; point at e->movement
	ld a, (hl)
	ld (de), a
	inc hl
	inc hl
	inc de
	inc de			; point at e->param1
	ld a, (hl)
	ld (de), a
	ret
	

; Just like final enemy in level 1, shoot an enemy every now and then
; currently, every 64 frames

behav_final1_l7:
	ld d, h
	ld e, l
	ld bc, 11
	add hl, bc		; HL == e->behav_param
	ld a, (hl)
	and $1f			; shoot once every 16 frames
	cp 15
	jr nz, inc_behav_param_final1_l7

; Generate new enemy...
	push hl
	push de

	ld hl, _new_e
        ld a, (de)     ; A has e->x
        ld (hl), a
	inc hl
        inc hl  ; point to new_e->y
        inc de
        ld a, (de)
	add a, 20
        ld (hl), a
        inc hl         ; point to new_e->enemy_type
        ld (hl), 32    ; ENEMY_SHIP1_LEVEL7
        inc hl         ; point to new_e->movent
        ld (hl), MOVE_KAMIKAZE_NOANIM
        inc hl         ; point to new_e->energy
        ld (hl), 1
        inc hl         ; point to new_e->param1
        ld (hl), 4
        inc hl         ; point to new_e->param2
        ld (hl), 0
	ld hl, _new_e
	push hl
	ld bc, 4
	push bc
	call _NewEnemy
	pop bc
	pop bc         ; we pop BC because we want to keep HL
	pop de
	push de
	ld a, h
	and l
	jr z, notgenerated_f1_l7       ; If the result returned was 0, the enemy could not be generated
	ld a, (de)
	add a, 8
	ld (hl), a               ; the X was not like that...

notgenerated_f1_l7:
        pop de
        pop hl

inc_behav_param_final1_l7:
	inc (hl)		; e->behav_param++;
	ret



; Behavior for final enemy 7
behav_fenemy7:
	; First, check if we have been defeated

	ld d, h
	ld e, l			; save e in DE

	inc hl
	inc hl
	ld a, (hl)
	and a
	jr nz, final7_start
	ld a, (_fenemy_defeat)
	and a
	jr nz, inc_f7_defeated			; if the enemy is defeated, there is nothing left to do for us but increase the count
	inc a
	ld (_fenemy_defeat),a		; final_enemy_defeated=1, now make the rest of it explode
	ld bc, 12	
	add hl, bc				; we are pointint at sprite2->sprnum
        ld (hl), EXPLOSION_SPR
	inc hl
	inc hl
	ld (hl), MOVE_EXPLOSION
	inc hl
	inc hl
	ld (hl), 4				; param1
	ld bc, 8
	add hl, bc				; sprite3
        ld (hl), EXPLOSION_SPR
	inc hl
	inc hl
	ld (hl), MOVE_EXPLOSION
	inc hl
	inc hl
	ld (hl), 4				; param1
	add hl, bc				; sprite4
        ld (hl), EXPLOSION_SPR
	inc hl
	inc hl
	ld (hl), MOVE_EXPLOSION
	inc hl
	inc hl
	ld (hl), 4				; param1
	ret

inc_f7_defeated:
	inc a
	ld (_fenemy_defeat), a
	ret

final7_start:
	; before doing anything else, just check if we are already dying. Otherwise some nasty things could happen
	ld h, d
	ld l, e
	ld bc, 4
	add hl, bc
	ld a, (hl)		; A == e->movement
	cp MOVE_EXPLOSION
	ret z			; just return if exploding!


	ld h, d
	ld l, e
	; as a first step, animate alien
	ld bc, 11
	add hl, bc		; HL == e->behav_param
	ld a, (hl)
	and $C			; e->behav_param & 0xC, that will be the animation 
	add a, 22		; this is the first sprite for this animation
	ld bc, -9	
	add hl, bc		; hl == e->sprnum
	ld (hl),a
	ld bc, 12
	add hl, bc
	inc a
	ld (hl), a
	add hl, bc
	inc a
	ld (hl), a
	add hl, bc
	inc a
	ld (hl), a

	ld h, d
	ld l, e
	ld bc, 11
	add hl, bc		; HL == e->behav_param
	ld a, (hl)
	and a
	jp z, final7_shoot1
	cp 64
	jp z, final7_shoot2
	cp 140
	jp z, final7_shoot3
	cp 192			; CHECK HERE when we should shoot, possibly with several different shoot types
	jp nz, inc_behav_param_7

final7_shoot4: ; 3 homing lasers

	push hl
	push de
	ld h, d
	ld l, e
	ld a, (hl)
	sub 18
	ld c, a
	ld b, 0
	push bc			; e->x
	inc hl
	ld a, (hl)
	ld c, a
	push bc			; e->y
	ld bc, MOVE_TARGET
	push bc			; MOVE_TARGET
	ld bc, SHOT_MEGA
	push bc			; SHOT_ENEMY
	call _NewEnemyShoot_FX
	pop de
	pop de
	pop de			; DE = e->y
	pop bc			; BC = e->x

	; upon return, HL holds the pointer to the new enemy shot
	; Then, generate parameters for the shoot
	push hl
	pop ix			; IX holds the pointer to the new enemy shoot

	ld a, ixh
	or ixl
	jp z, final7_shoot4_skip1			; If IX == 0, then skip, as the enemy shoot could not be created

	ld d, c		; D==x_enemy,  E is already ==y_enemy
	ld a, (_ship_x)
	add a, 16
	ld h, a			; H = x_ship
	ld a, (_ship_y)
	ld l, a			; L = y_ship
	call calcdirectshoot

	ld (ix+6), h
	ld (ix+7), l
	ld (ix+8), d
	ld (ix+9), e
	ld (ix+3), 0
	ld (ix+5), 0

final7_shoot4_skip1:
	pop de
	ld h, d
	ld l, e
	push de
	ld a, (hl)
	sub 18
	ld c, a
	ld b, 0
	push bc			; e->x
	inc hl
	ld a, (hl)
	add a, 16
	ld c, a
	push bc			; e->y
	ld bc, MOVE_TARGET
	push bc			; MOVE_TARGET
	ld bc, SHOT_MEGA
	push bc			; SHOT_ENEMY
	call _NewEnemyShoot
	pop de
	pop de
	pop de			; DE = e->y
	pop bc			; BC = e->x

	; upon return, HL holds the pointer to the new enemy shot
	; Then, generate parameters for the shoot
	push hl
	pop ix			; IX holds the pointer to the new enemy shoot

	ld a, ixh
	or ixl
	jp z, final7_shoot4_skip2			; If IX == 0, then skip, as the enemy shoot could not be created

	ld d, c		; D==x_enemy,  E is already ==y_enemy
	ld a, (_ship_x)
	add a, 16
	ld h, a			; H = x_ship
	ld a, (_ship_y)
	ld l, a			; L = y_ship
	call calcdirectshoot

	ld (ix+6), h
	ld (ix+7), l
	ld (ix+8), d
	ld (ix+9), e
	ld (ix+3), 0
	ld (ix+5), 0

final7_shoot4_skip2:
	pop de
	ld h, d
	ld l, e
	push de
	ld a, (hl)
	sub 18
	ld c, a
	ld b, 0
	push bc			; e->x
	inc hl
	ld a, (hl)
	sub 16
	ld c, a
	push bc			; e->y
	ld bc, MOVE_TARGET
	push bc			; MOVE_TARGET
	ld bc, SHOT_MEGA
	push bc			; SHOT_ENEMY
	call _NewEnemyShoot
	pop de
	pop de
	pop de			; DE = e->y
	pop bc			; BC = e->x

	; upon return, HL holds the pointer to the new enemy shot
	; Then, generate parameters for the shoot
	push hl
	pop ix			; IX holds the pointer to the new enemy shoot

	ld a, ixh
	or ixl
	jp z, final7_shoot4_skip3			; If IX == 0, then skip, as the enemy shoot could not be created

	ld d, c		; D==x_enemy,  E is already ==y_enemy
	ld a, (_ship_x)
	add a, 16
	ld h, a			; H = x_ship
	ld a, (_ship_y)
	ld l, a			; L = y_ship
	call calcdirectshoot

	ld (ix+6), h
	ld (ix+7), l
	ld (ix+8), d
	ld (ix+9), e
	ld (ix+3), 0
	ld (ix+5), 0

final7_shoot4_skip3:
	pop de
	pop hl
	jp inc_behav_param_7


final7_shoot3:; shoot 3: 4 simple shots, up-left, up-left, down-left, down-left
	push hl
	push de
	ld h, d
	ld l, e
	ld a, (hl)
	sub 18
	ld c, a
	ld b, 0
	push bc			; e->x
	inc hl
	ld a, (hl)
	add a, 16
	ld c, a
	push bc			; e->y
	ld bc, MOVE_UPLEFT
	push bc			; MOVE_UPLEFT
	ld bc, SHOT_MEGA
	push bc			; SHOT_ENEMY
	call _NewEnemyShoot_FX

	; upon return, HL holds the pointer to the new enemy shoot
	ld a, l
	or h
	jp z, final7_shoot3_skip1			; If HL == 0, then skip, as the enemy shoot could not be created

	ld bc, 6
	add hl, bc
	ld (hl), 1		; e->param1=2
	inc hl
	ld (hl), 6		; e->param2=4	
final7_shoot3_skip1:
	pop de			; de = SHOOT_MEGA
	pop bc			; bc = MOVE_UPLEFT
	pop iy			; iy = e->y
	pop ix			; ix = e->x
	push ix
	push iy
	push bc
	push de

	call _NewEnemyShoot	; upon return, HL holds the pointer to the new enemy shoot

	; upon return, HL holds the pointer to the new enemy shoot
	ld a, l
	or h
	jp z, final7_shoot3_skip2			; If HL == 0, then skip, as the enemy shoot could not be created

	ld bc, 6
	add hl, bc
	ld (hl), 2		; e->param1=2
	inc hl
	ld (hl), 4		; e->param2=4	

final7_shoot3_skip2:
	pop de			; de = SHOOT_MEGA
	pop bc			; bc = MOVE_LEFT
	pop iy			; iy = e->y
	pop ix			; ix = e->x

	ld bc, MOVE_DOWNLEFT
	push ix
	push iy
	push bc
	push de
	call _NewEnemyShoot	; upon return, HL holds the pointer to the new enemy shoot
	; upon return, HL holds the pointer to the new enemy shoot
	ld a, l
	or h
	jp z, final7_shoot3_skip3			; If HL == 0, then skip, as the enemy shoot could not be created

	ld bc, 6
	add hl, bc
	ld (hl), 2
	inc hl
	ld (hl), 4
final7_shoot3_skip3:
	pop de			; de = SHOOT_MEGA
	pop bc			; bc = MOVE_LEFT
	pop iy			; iy = e->y
	pop ix			; ix = e->x
	push ix
	push iy
	push bc
	push de
	call _NewEnemyShoot	; upon return, HL holds the pointer to the new enemy shoot
	; upon return, HL holds the pointer to the new enemy shoot
	ld a, l
	or h
	jp z, final7_shoot3_skip4			; If HL == 0, then skip, as the enemy shoot could not be created

	ld bc, 6
	add hl, bc
	ld (hl), 1
	inc hl
	ld (hl), 6

final7_shoot3_skip4:
	pop de			; de = SHOOT_MEGA
	pop bc			; bc = MOVE_LEFT
	pop iy			; iy = e->y
	pop ix			; ix = e->x

	pop de
	pop hl
	jp inc_behav_param_7

final7_shoot2:
	push hl
	push de

	ld h, d
	ld l, e
	ld a, (hl)
	sub 18
	ld c, a
	ld b, 0
	push bc			; e->x
	inc hl
	ld a, (hl)
	add a, 16
	ld c, a
	push bc			; e->y
	ld bc, MOVE_UPLEFT
	push bc			; MOVE_LEFT
	ld bc, SHOT_LASER
	push bc			; SHOT_ENEMY
	call _NewEnemyShoot
	pop de			; de = SHOOT_MEGA
	pop bc			; bc = MOVE_LEFT
	pop iy			; iy = e->y
	pop ix			; ix = e->x

	; upon return, HL holds the pointer to the new enemy shoot
	ld a, l
	or h
	jp z, final7_shoot2_skip1			; If HL == 0, then skip, as the enemy shoot could not be created

	ld bc, 6
	add hl, bc
	ld (hl), 1		; e->param1=2
	inc hl
	ld (hl), 6		; e->param2=4	
final7_shoot2_skip1:
	ld bc, MOVE_DOWNLEFT
	push ix
	push iy
	push bc
	push de
	call _NewEnemyShoot	; upon return, HL holds the pointer to the new enemy shoot
	; upon return, HL holds the pointer to the new enemy shoot
	ld a, l
	or h
	jp z, final7_shoot2_skip2			; If HL == 0, then skip, as the enemy shoot could not be created

	ld bc, 6
	add hl, bc
	ld (hl), 1
	inc hl
	ld (hl), 6
final7_shoot2_skip2:
        ld hl, FX_LASER
        push hl
        call _wyz_effect
        pop hl

	pop de			; de = SHOOT_MEGA
	pop bc			; bc = MOVE_LEFT
	pop iy			; iy = e->y
	pop ix			; ix = e->x

	pop de
	pop hl
	jp inc_behav_param_7
final7_shoot1:	; shoot 1: 3 simple shots, up-left, left, down-left
	push hl
	push de

	ld h, d
	ld l, e
	ld a, (hl)
	sub 18
	ld c, a
	ld b, 0
	push bc			; e->x
	inc hl
	ld a, (hl)
	add a, 16
	ld c, a
	push bc			; e->y
	ld bc, MOVE_LEFT
	push bc			; MOVE_LEFT
	ld bc, SHOT_MEGA
	push bc			; SHOT_ENEMY
	call _NewEnemyShoot_FX
	pop de			; de = SHOOT_MEGA
	pop bc			; bc = MOVE_LEFT
	pop iy			; iy = e->y
	pop ix			; ix = e->x

	ld bc, MOVE_UPLEFT
	push ix
	push iy
	push bc
	push de
	call _NewEnemyShoot	; upon return, HL holds the pointer to the new enemy shoot

	; upon return, HL holds the pointer to the new enemy shoot
	ld a, l
	or h
	jp z, final7_shoot1_skip1			; If HL == 0, then skip, as the enemy shoot could not be created

	ld bc, 6
	add hl, bc
	ld (hl), 2		; e->param1=2
	inc hl
	ld (hl), 4		; e->param2=4	
final7_shoot1_skip1:
	pop de			; de = SHOOT_MEGA
	pop bc			; bc = MOVE_LEFT
	pop iy			; iy = e->y
	pop ix			; ix = e->x

	ld bc, MOVE_DOWNLEFT
	push ix
	push iy
	push bc
	push de
	call _NewEnemyShoot	; upon return, HL holds the pointer to the new enemy shoot
	; upon return, HL holds the pointer to the new enemy shoot
	ld a, l
	or h
	jp z, final7_shoot1_skip2			; If HL == 0, then skip, as the enemy shoot could not be created

	ld bc, 6
	add hl, bc
	ld (hl), 2
	inc hl
	ld (hl), 4
final7_shoot1_skip2:
	pop de			; de = SHOOT_MEGA
	pop bc			; bc = MOVE_LEFT
	pop iy			; iy = e->y
	pop ix			; ix = e->x

	pop de
	pop hl

inc_behav_param_7:
	inc (hl)		; e->behav_param++;

	ld a, (hl)
	ld h, d
	ld l, e
	ld bc, 4
	add hl, bc		; move to e->movement
	
	and a
	jr z, make_wave_final7
	cp $10
	jr z, make_goleft_final7
	cp $20
	jr z, make_goright_final7
	cp $30
	jr z, make_stop_final7
	cp $40
	jr z, make_wave_final7
	cp $50
	jr z, make_downleft_final7
	cp $60
	jr z, make_upleft_final7
	cp $70
	jr z, make_upright_final7
	cp $80
	jr z, make_downright_final7
	cp $90
	jr nz, move_rest_final7

make_stop_final7:
	ld (hl), MOVE_NONE
	jr move_rest_final7
make_wave_final7:
	ld (hl), MOVE_WAVE
	inc hl
	inc hl
	ld (hl), 0
	inc hl
	ld (hl), 4
	jr move_rest_final7

make_goleft_final7:
	ld (hl), MOVE_LEFT_ACCEL
	inc hl
	inc hl
	ld (hl), 16
	jr move_rest_final7
make_goright_final7:
	ld (hl), MOVE_RIGHT_ACCEL
	inc hl
	inc hl
	ld (hl), 16
	jr move_rest_final7

make_downleft_final7:
	ld (hl), MOVE_DOWNLEFT
	inc hl
	inc hl
	ld (hl), 2
	inc hl
	ld (hl), 2
	jr move_rest_final7
make_upleft_final7:
	ld (hl), MOVE_UPLEFT
	inc hl
	inc hl
	ld (hl), 2
	inc hl
	ld (hl), 8
	jr move_rest_final7
make_upright_final7:
	ld (hl), MOVE_UPRIGHT
	inc hl
	inc hl
	ld (hl), 2
	jr move_rest_final7
make_downright_final7:
	ld (hl), MOVE_DOWNRIGHT
	inc hl
	inc hl
	ld (hl), 2
	inc hl
	ld (hl), 8
	jr move_rest_final7


move_rest_final7:		; move the rest of the final enemy according to sprite 0
	ld h, d
	ld l, e
	ld a, (hl)		; e->x
	inc hl
	ex af, af'
	ld a, (hl)		; e->y

	ld bc, 11
	add hl, bc
	ex af, af'
	add a, 16
	ld (hl), a
	ex af, af'
	inc hl
	ld (hl), a

	add hl, bc
	ex af, af'
	sub 16
	ld (hl), a
	ex af, af'
	inc hl
	add a, 16
	ld (hl), a

	add hl, bc
	ex af, af'
	add a, 16
	ld (hl), a
	ex af, af'
	inc hl
	ld (hl), a
	ret


rand_seed	dw 63059

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
