#include "structs.h"
#include "movement.h"
#include "behavior.h"
#include "constants.h"
#include "sprdefs.h"
#include "engine.h"

#pragma output STACKPTR=24600

// Game variables
unsigned char ship_x;
unsigned char ship_y;
char speed_x;
char speed_y;
unsigned char frames_to_shoot;		// Can we shoot now?
unsigned char frames_fire_pressed;	// Number of frames where the FIRE button is pressed: when pressed for 6 frames, we will launch the BFB (if available)
unsigned char current_weapon;		// What is our current weapon?
unsigned char current_weapon_sprite;	// Sprite for our current weapon
unsigned char current_weapon_energy;	// Energy for the current weapon
unsigned char available_superbombs;	// Number of available superbombs (kill everything on screen)
unsigned char life_counter;		// How many lifes do we have?
unsigned char credit_counter;		// Start with 3 credits!
unsigned char end_game;			// Game will be really over at this time
unsigned char current_screen;
unsigned char map_xpos;
unsigned char respawn_xpos;		// xpos to respawn after dying
unsigned char map_displacement;  // Displacement in tile: 0000YYXX, where XX is the displacement in pixels (0, 1==2, 2==4, 3==6), and YY is the displacement in chars (0-2).
unsigned char max_shoots; 	// Maximum number of shoots allowed with the current weapon
unsigned char mayday;		// We have been shot!
unsigned char current_level;	// level we are playing
unsigned char previous_level;	// level we were at before getting killed or moved to a new level
unsigned char level_color;	// Color for the current level
unsigned int score;		// Need we say more?
unsigned int hiscore;		// Highest score
unsigned int next_extralife;	// Score to get the next extra life
unsigned char update_score;	// We must update the score at the next frame or two
unsigned char update_superb;	// We must update the number of superbombs
unsigned char update_life;	// We must update the number of lifess
// Final enemy
unsigned char final_enemy_active;
unsigned char final_enemy_components;		// How many sprites in the enemy
unsigned char fenemy_defeat;		// Have we beaten it?
unsigned char fenemy_activation_counter;

// Ship sprites, used for dying animation
unsigned char ship0spr;

// Temporary definition of structs. If they work as-is, we will move them to an include file
unsigned char CurLevel_XLength;		// Length in X of the current level
unsigned char CurLevel_NTiles;		// Number of tiles for the current level
unsigned int keys[]={KEY_Q,KEY_A,KEY_O,KEY_P,KEY_SPACE};
unsigned char joy;
unsigned char joystick_type;            // Which joystick are we using
unsigned char inertia_cheat;		// are we cheating?
unsigned char border_color;		// For some silly effects


// Array of existing enemies and shoots (max 8 enemies for now)

struct Entity active_enemies[MAX_ENEMIES];
struct Entity my_active_shoots[MAX_ENEMIES];
struct Entity enemy_active_shoots[MAX_ENEMIES];
struct Entity power_up;			// Only one powerup active at a time...

// Array of enemy locations in the game
// We use the same Entity structure, with just one difference: X now means the tile

//struct Enemy enemy_locations[128]; // up to 128 enemies per level, 8 bytes per enemy: 1K
struct Enemy *enemy_locations = 0xF800; // up to 256 enemies per level, 8 bytes per enemy: 2K, WARNING: this is placed in RAM 4
unsigned char new_enemy;

// Loop counters and temporary variables

unsigned char sound_selection;
unsigned char dummy_b;
unsigned int dummy_i;
unsigned char frameskip_counter;

int __FASTCALL__ read_joystick(int joytype)
{
#asm
	ld a, l
	ld hl, _keys
	call get_joystick
	ld h,0
	ld l,a
#endasm
}

// WYZ player functions

void wyz_load_music (unsigned char mzk_number)
{

#asm
	        ld hl, 2
		add hl, sp
		ld a, (hl)              ; A gets the song number
		push af

		di
		call CARGA_CANCION	
		ei

		ld a, 3
		ld (SOUND_SFX), a	; by default we select sound and sfx, unless changed later

	        pop af
		ld c, a			; C gets the song number
		ld b, 0

		ld hl, _fxchannel_music
		add hl, bc
		ld a, (hl)
		ld (FX_CHANNEL),a
		call STOP_FX
#endasm

}

void wyz_stop_music(void)
{

	#asm
		di	
		CALL STOP_PLAYER
		ei
		halt
	#endasm

}

void wyz_play(void)
{
	#asm
		call WYZ_PLAY
	#endasm
}

void wyz_effect(unsigned char effect)
{
     #asm
	ld a, (23388)
	and $07			; keep low bits
	push af			; save current state
	ld b, 0
	di
	call setrambank		; go to ram bank 0 to play FX
	ei

        ld hl, 4
	add hl, sp
	ld b, (hl)              ; B gets the effect number
        call LOAD_FX

	di
	pop af			; recover RAM bank state
	ld b, a
	call setrambank		; go back to normal state
	ei
     #endasm
}


void gameISR(void)
{
  joy=read_joystick(joystick_type);
#asm
	ld a,r
	jp p, noscreenswitch	; the highest bit of R is 0, no screen switch yet
	call switchscreen	; switch screen
	ld a, r
   	and $7f
   	ld r,a	; clear the highest bit of the R register. It will be used to flag for a screen switch
.noscreenswitch
	ld a, (23388)
	and $07			; keep low bits
	push af			; save current state
	ld b, 0
	call setrambank		; go to ram bank 0 for the music ISR
	call WYZ_PLAY
	pop af
	ld b, a
	call setrambank		; go back to normal state
	ld a, (_frameskip_counter)
	inc a
	ld (_frameskip_counter), a
	ld a, (_border_color)
	and a
	ret z
	dec a
	ld (_border_color), a
	out ($fe), a
#endasm
}

// Print large char on screen (2x1), from a base number 0..9
// INPUT:
//		B: position in X (pixels)
//		C: position in Y (pixels)
//		A: number to print

#asm
.PrintLargeNumber
	push af
	call CalcScreenPos_Char	; Screen position in HL
	ex de, hl
	ld hl, _number_font
	pop af
	rlca
	rlca
	rlca
	and $f8			; multiply by 8
	ld c, a
	ld b, 0
	add hl, bc		
	ex de, hl		; HL points to the screen position, DE to the first row of the number
	push hl
	ld b, 4
.printlarge_loop1
	ld a, (de)
	inc de
	ld (hl), a
	inc h
	ld (hl), a
	inc h
	djnz printlarge_loop1
	pop hl
	ld bc, 32
	add hl, bc
	ld b, 4 
.printlarge_loop2
	ld a, (de)
	inc de
	ld (hl), a
	inc h
	ld (hl), a
	inc h
	djnz printlarge_loop2
	ret
#endasm

// Decompose a 5-digit number in single digits
// INPUT:	HL: number
//		IX: pointer to string where the individual numbers will be located

#asm
; Divide HL by BC
;
; HL: number
; BC: divider
; DE: result (HL / BC)
; HL: remainder

.divide_large
    xor a
    ld de, 0
.divide_loop
    sbc hl, bc
    jp c, divide_completed
    inc de
    jp divide_loop    
.divide_completed
    add hl, bc
    ret

.decompose_5digit
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
#endasm

unsigned char numbers[5];	// 5 chars is the maximum number of digits we will print


// Print 2-digit number, large
// INPUT:
//		B: position in X (pixels)
//		C: position in Y (pixels)
//		A: number to print



#asm
.PrintLarge_2
	push bc
	ld e, a
	ld d, 10
	call Div8		; A = number /10 (first digit), D = remainder (second digit)	
	ld hl, _numbers
	ld (hl), a
	inc hl
	ld (hl), d	
	pop bc
	push bc
	push hl
	call PrintLargeNumber	; Print first number
	pop hl
	pop bc
	ld a, 8
	add a, b
	ld b, a			; move 8 pixels right
	ld a, (hl)		; get second digit
	call PrintLargeNumber	; Print second number
	ret
#endasm

// Print large char on screen (2x1), from a base number 0..9, on a screen based at $4000
// INPUT:
//		B: position in X (pixels)
//		C: position in Y (pixels)
//		A: number to print

#asm
.PrintLargeNumber_4000
	push af
	call CalcScreenPos_Char	; Screen position in HL
	ld a, h
	sub $80
	ld h, a
	ex de, hl
	ld hl, _number_font
	pop af
	rlca
	rlca
	rlca
	and $f8			; multiply by 8
	ld c, a
	ld b, 0
	add hl, bc		
	ex de, hl		; HL points to the screen position, DE to the first row of the number
	push hl
	ld b, 4
.printlarge4000_loop1
	ld a, (de)
	inc de
	ld (hl), a
	inc h
	ld (hl), a
	inc h
	djnz printlarge4000_loop1
	pop hl
	ld bc, 32
	add hl, bc
	ld b, 4 
.printlarge4000_loop2
	ld a, (de)
	inc de
	ld (hl), a
	inc h
	ld (hl), a
	inc h
	djnz printlarge4000_loop2
	ret
#endasm

// Print 2-digit number, large, on a screen based at $4000
// INPUT:
//		B: position in X (pixels)
//		C: position in Y (pixels)
//		A: number to print



#asm
.PrintLarge_2_4000
	push bc
	ld e, a
	ld d, 10
	call Div8		; A = number /10 (first digit), D = remainder (second digit)	
	ld hl, _numbers
	ld (hl), a
	inc hl
	ld (hl), d	
	pop bc
	push bc
	push hl
	call PrintLargeNumber_4000	; Print first number
	pop hl
	pop bc
	ld a, 8
	add a, b
	ld b, a			; move 8 pixels right
	ld a, (hl)		; get second digit
	call PrintLargeNumber_4000	; Print second number
	ret
#endasm

// Print 5-digit number, large
// INPUT:
//		B: position in X (pixels)
//		C: position in Y (pixels)
//		HL: number to print

#asm
.PrintLarge_5
	ld ix, _numbers
	push bc
	call decompose_5digit
	pop bc

	ld a, (ix+0)
	push bc
	call PrintLargeNumber
	pop bc
	ld a, 8
	add a, b
	ld b, a

	ld a, (ix+1)
	push bc
	call PrintLargeNumber
	pop bc
	ld a, 8
	add a, b
	ld b, a

	ld a, (ix+2)
	push bc
	call PrintLargeNumber
	pop bc
	ld a, 8
	add a, b
	ld b, a

	ld a, (ix+3)
	push bc
	call PrintLargeNumber
	pop bc
	ld a, 8
	add a, b
	ld b, a

	ld a, (ix+4)
	call PrintLargeNumber
	ret
#endasm

// Print 5-digit number, small
// INPUT:
//		B: position in X (pixels)
//		C: position in Y (pixels)
//		HL: number to print

#asm

.PrintSmall_5
	ld ix, _numbers
	push bc
	call decompose_5digit
	pop bc
	ld a, (ix+0)
	push bc
	call PrintNumber
	pop bc
	ld a, 8
	add a, b
	ld b, a

	ld a, (ix+1)
	push bc
	call PrintNumber
	pop bc
	ld a, 8
	add a, b
	ld b, a

	ld a, (ix+2)
	push bc
	call PrintNumber
	pop bc
	ld a, 8
	add a, b
	ld b, a

	ld a, (ix+3)
	push bc
	call PrintNumber
	pop bc
	ld a, 8
	add a, b
	ld b, a

	ld a, (ix+4)
	call PrintNumber
	ret


; Print normal char on screen (1x1), from a base number 0..9
; INPUT:
;		B: position in X (pixels)
;		C: position in Y (pixels)
;		A: number to print

.PrintNumber
	push af
	call CalcScreenPos_Char	; Screen position in HL
	ex de, hl
	ld hl, _number_font
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
.printnum_loop
	ld a, (de)
	inc de
	ld (hl), a
	inc h
	djnz printnum_loop
	ret

; Print normal char on screen (1x1), from a base number 0..9
; Use $4000 as the base screen address
; INPUT:
;		B: position in X (pixels)
;		C: position in Y (pixels)
;		A: number to print

.PrintNumber_4000
	push af
	call CalcScreenPos_Char	; Screen position in HL
	ld a, h
	sub $80
	ld h, a
	ex de, hl
	ld hl, _number_font
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
.printnum_4000_loop
	ld a, (de)
	inc de
	ld (hl), a
	inc h
	djnz printnum_4000_loop
	ret
#endasm




// Load a block of sprites to the current working set
// dstspr (0-63): first sprite to load
// sourcespr (0-448): source to load from
// numspr (1-64): number of sprites to load

void LoadSprBlock(unsigned char dstspr, unsigned int sourcespr, unsigned char numspr)
{
#asm
	di
	ld a, (23388)
	and $07			; keep low bits
	push af			; save current state	
	ld b, 1
	call setrambank		; go to ram bank 1 to manage the sprites
	
	ld hl, 8
	add hl, sp
	ld e, (hl)
	dec hl
	ld d, (hl)		; DE gets dstspr
	dec hl
	ld c, (hl)
	dec hl
	ld b, (hl)		; BC gets sourcespr
	dec hl
	ld a, (hl)		; A gets numspr
	ld h, b
	ld l, c			; HL gets sourcespr now

	; basically, we copy (numspr * 32) bytes, from (49152 + 2048 + (sourcespr*32)) to 49152 + (firstspr*32)
	rrca
	rrca
	rrca
	ld c, a
	and $1f
	ld b, a
	ld a, c
	and $e0
	ld c, a			; BC = numspr *32

	ld a, e
	rrca
	rrca
	rrca
	ld e, a
	and $1f
	ld d, a
	ld a, e
	and $e0
	ld e, a			; DE = firstspr*32
	push hl
	ld hl, 49152
	add hl, de	
	ex de, hl		; DE = (firstspr*32) + 49152
	pop hl
	push de			; we will need DE for some calculations
	
	ld a, l
	rrca
	rrca
	rrca
	ld e, a
	and $1f
	ld d, a
	ld a, e
	and $e0
	ld e, a			; DE = (low byte of sourcespr)*32

	ld a, h
	rrca
	rrca
	rrca
	and $e0
	or d
	ld h, a
	ld l, e
	ld de, 49152+2048
	add hl, de		; HL = 49152 + 2048 + (sourcespr*32))
	pop de		

	ldir			; copy all stuff

	pop af			; recover RAM bank state
	ld b, a
	call setrambank		; go back to normal state
	ei
#endasm
}

// Levels have compressed data, let's see how it goes when loading them

unsigned char *level_pointer;
unsigned int *int_pointer;
unsigned int length_tiles;
unsigned int length_map;


void load_level(unsigned char level)		
{

#asm
	di
	push bc
	ld b, 4
	call setrambank		; Levels in RAM4
	pop bc
#endasm

	level_pointer=(unsigned char*)level_address[level];
	CurLevel_NTiles = *(level_pointer)++;
	CurLevel_XLength = *(level_pointer)++;	// Load the basic level data
	int_pointer=level_pointer;
	length_tiles = *(int_pointer);
	level_pointer+=2;
	int_pointer=level_pointer;
	length_map = *(int_pointer);
	level_pointer+=2;
	
	// Prepare tiles, preshift them, store them in their final location
	// We will use the map area as decompression buffer
#asm
	
	ld de, $a000
	ld hl, (_level_pointer)
	call depack

	ld de, $a000
	ld a, (_CurLevel_NTiles)
	ld b,a
	call CreaTablaTiles
#endasm

	// now, copy the map (uncompress) to its final location
	level_pointer += length_tiles;
#asm
	ld de, $a000
	ld hl, (_level_pointer)
	call depack
#endasm
        // Copy enemy table
        level_pointer=(unsigned char*)enemy_address[level];
        dummy_i = *level_pointer; // Number of enemies in level
        level_pointer++;
        dummy_i *= sizeof (struct Enemy);

#asm
	ld de, (_enemy_locations)
	ld hl, (_level_pointer)
        ld bc, (_dummy_i)
        ldir


	ld b, 0
	call setrambank		; vuelta a la normalidad
	ei
#endasm
}

#asm
.CleanSCRC000
	ld hl, $C000
	ld de, $C001
	ld (hl),0
	ld bc, 4095
	ldir
	ld hl, $d800
	ld de, $d801
	ld a, (_level_color)
	ld (hl), a
	ld bc, 511
	ldir
	ret
#endasm

void clean_screen(void)
{
#asm
	di

	ld b, 5
	call setrambank		; la ponemos en $c000 - $ffff	
	call CleanSCRC000


	ld b, 7
	call setrambank		; la ponemos en $c000 - $ffff	
	call CleanSCRC000	

	ld b, 0
	call setrambank		; vuelta a la normalidad
	ei

#endasm
}

void load_background(void)
{
#asm
	di

	ld b, 3			; Static images in RAM3
	call setrambank		; la ponemos en $c000 - $ffff	
	ld hl, $c000
	ld de, 16384
	call depack

	ld b, 7
	call setrambank		; la ponemos en $c000 - $ffff
	ld hl, 16384
	ld de, $c000
	ld bc, 6912
	ldir			; copy the background to the alternate screen

	; Print number of remaining lives, current score, high score and current level
	
	ld b, 72
	ld c, 136
	ld a, (_life_counter)
	call PrintLarge_2

	ld b, 72
	ld c, 152
	ld a, (_available_superbombs)
	call PrintLarge_2

	
	ld b, 120
	ld c, 144
	ld hl, (_score)
	call PrintLarge_5
	
	ld b, 184
	ld c, 152
	ld hl, (_hiscore)
	call PrintSmall_5

	ld b, 80
	ld c, 176
	ld a, (_current_level)
	inc a
	call PrintNumber

	; Same on the other screen!

	ld b, 5
	call setrambank		; la ponemos en $c000 - $ffff
	ld b, 72
	ld c, 136
	ld a, (_life_counter)
	call PrintLarge_2

	ld b, 72
	ld c, 152
	ld a, (_available_superbombs)
	call PrintLarge_2

	ld b, 120
	ld c, 144
	ld hl, (_score)
	call PrintLarge_5

	ld b, 184
	ld c, 152
	ld hl, (_hiscore)
	call PrintSmall_5

	ld b, 80
	ld c, 176
	ld a, (_current_level)
	inc a
	call PrintNumber

	ld b, 0
	call setrambank		; back to reality
	ei
#endasm


}



void DrawGameMap(void)
{
#asm
	ld a, (_current_screen)	; cargamos la pantalla
	ld b, a
	call setrambank		; la ponemos el $c000 - $ffff
	ld a, (_current_screen)
	xor 2			; 5 xor 2 = 7; 7 xor 2 = 5
	ld (_current_screen), a	; intercambiamos la pantalla en la que vamos a escribir



	ld a, (_CurLevel_XLength)
	ld h,0
	ld l,a
	ld d, $a0
	ld a, (_map_xpos)
	ld e, a				; the map will always start at $a000, so the displacement will always be in E
	ld a, (_map_displacement)
	and $03
	ld c, a				; displacement in pixels within tile
	ld a, (_map_displacement)
	and $0C
	rrca
	rrca
	ld b, a				; displacement in chars within tile
	call DrawMap
#endasm
}

void DrawShip(void)
{
// First draw the ship
#asm
	ld a, (_ship_x)
	ld b, a
	ld a, (_ship_y)
	ld c,a
	ld a, (_ship0spr)	
	ld d, 0
	ld e, a		; _ship01
	call drawsprite_ship
	call _CheckShipCollision

	ld a, (_ship_x)
	add a,16
	ld b, a
	ld a, (_ship_y)
	ld c,a
	ld a, (_ship0spr)
	inc a
	ld d, 0
	ld e, a		;_ship02
	call drawsprite_ship
	call _CheckShipCollision

	ld a, (_max_shoots)
	ld b, a
	ld hl, _my_active_shoots

.drawshoots_loop
	push bc
	inc hl
	inc hl
	ld a, (hl)
	and a
	jr z, loop_shoots

	ld d, 0
	ld e, (hl)
	dec hl
	ld c, (hl)
	dec hl
	ld b, (hl); bc = (x << 8) | y

	push hl
	call drawsprite	
	pop hl
	inc hl
	inc hl
.loop_shoots
	ld bc, 10		; 12 = sizeof(struct Entity)	
	add hl, bc
	pop bc
	djnz drawshoots_loop	; continue loop


.draw_powerups
	ld hl, _power_up
	inc hl
	inc hl
	ld a, (hl)
	and a
	ret z			; if the power up is not active, just quit

	ld d, 0
	ld e, (hl)
	dec hl
	ld c, (hl)
	dec hl
	ld b, (hl); bc = (x << 8) | y

	call drawsprite		; draw the power up, and that is it
#endasm



/*
	for(i=0;i<max_shoots;i++)
	{
		if (my_active_shoots[i].sprnum)	// This shoot is active
		{
			dummy_i = my_active_shoots[i].x;
			dummy_i <<= 8;			// Move X to the upper byte
			dummy_i |= my_active_shoots[i].y;

			dummy_i2 = my_active_shoots[i].sprnum;
			#asm
				ld bc, (_dummy_i)
				ld de, (_dummy_i2)
				call drawsprite
			#endasm
		}
	}*/
}

void DrawEnemies(void)
{
// Now, display all enemies, shoots, etc
#asm
	ld b, MAX_ENEMIES
	ld ix, _active_enemies
	ld iy, _enemy_active_shoots

.drawenemy_loop
	push bc
	ld a, (ix+2)
	and a
	jr z, loop_enemy_shoot

	ld b, (ix+0)
	ld c, (ix+1)	; bc = (x << 8) | y
	ld d, 0
	ld e, (ix+2)
	push ix
	call drawsprite	
	pop ix

.loop_enemy_shoot
	ld a, (iy+2)
	and a
	jr z, loop_enemy_continue

	ld b, (iy+0)
	ld c, (iy+1)	; bc = (x << 8) | y
	ld d, 0
	ld e, (iy+2)
	push ix
	call drawsprite	
	pop ix

.loop_enemy_continue
	ld bc, 12		; 12 = sizeof(struct Entity)	
	add ix, bc
	add iy, bc		; go to the next entity
	pop bc
	djnz drawenemy_loop	; continue loop

#endasm

/*

	for(i=0;i<MAX_ENEMIES;i++)
	{
		if (active_enemies[i].sprnum)	// This enemy is active
		{
			dummy_i = active_enemies[i].x;
			dummy_i <<= 8;			// Move X to the upper byte
			dummy_i |= active_enemies[i].y;

			dummy_i2 = active_enemies[i].sprnum;
			#asm
				ld bc, (_dummy_i)
				ld de, (_dummy_i2)
				call drawsprite
			#endasm
		}

		if (enemy_active_shoots[i].sprnum)	// This enemy shot is active
		{
			dummy_i = enemy_active_shoots[i].x;
			dummy_i <<= 8;			// Move X to the upper byte
			dummy_i |= enemy_active_shoots[i].y;

			dummy_i2 = enemy_active_shoots[i].sprnum;
			#asm
				ld bc, (_dummy_i)
				ld de, (_dummy_i2)
				call drawsprite
			#endasm
		}
	}*/
}

void activate_final_enemy(void)
{
     if(!fenemy_activation_counter)
     {
#asm
	di
	push bc
	ld b, 4
	call setrambank		; Levels in RAM4
	pop bc
#endasm

	level_pointer=(unsigned char*)finalenemy_address[current_level];
	dummy_i = final_enemy_components = *(level_pointer)++;	// Load the basic final enemy data
        // Copy to enemy table (we are overwriting, which should not be a problem)
        dummy_i *= sizeof (struct Enemy);

#asm
	ld de, (_enemy_locations)
	ld hl, (_level_pointer)
        ld bc, (_dummy_i)
        ldir
#endasm

#asm
	ld a, (_final_enemy_components)
	ld b, a
	ld hl, _active_enemies
	ld de, (_enemy_locations)
.initfenemyloop
	ld a, (de)		; fenemy->x
	ld (hl), a		; active_enemies[i].x=fenemy->x;
	inc hl
	inc de
	inc de
	ld a, (de)		; fenemy->y
	ld (hl), a		; active_enemies[i].y=fenemy->y;
	inc hl
	inc de
	ld (hl), EXPLOSION_SPR	;active_enemies[i].sprnum=EXPLOSION_SPR;	
	inc hl
	ld a, (de)		; fenemy->enemy_type
	ld (hl),a		; active_enemies[i].type= fenemy->enemy_type;
	inc hl
	inc de	
	ld (hl), MOVE_EXPLOSION		; active_enemies[i].movement=MOVE_EXPLOSION;
	inc hl
	inc de
;	ld a, (de)		; 	fenemy->energy;
;	ld (hl), a		; active_enemies[i].energy = fenemy->energy;
	ld (hl), 0		; active_enemies[i].energy = 0, so the enemy is only vulnerable after the explosions
	inc hl
	inc de
	ld (hl), 4		;active_enemies[i].param1=4;  
	inc hl
	inc de
	ld a, (de)
	ld (hl), a		; active_enemies[i].param2=fenemy->param2;
	inc hl
	inc de			; DE now points to the next enemy
	ld (hl), 0		; active_enemies[i].param3=0;
	inc hl
	ld (hl), 0		; active_enemies[i].param4=0;
	inc hl
	ld (hl), BEHAV_DO_NOTHING ;	 active_enemies[i].behavior = BEHAV_DO_NOTHING;
	inc hl
	ld (hl), 0		; active_enemies[i].behav_param=0;
	inc hl			; HL now points to the next enemy
	djnz initfenemyloop
#endasm





/*	 for(i=0;i<final_enemy_components;i++)
	 {
	      active_enemies[i].sprnum=EXPLOSION_SPR;	// exploding!
    	      active_enemies[i].movement=MOVE_EXPLOSION;
              active_enemies[i].param1=4;               // 4 frames to explode

	      fenemy = &(enemy_locations[i]);
	      active_enemies[i].type= fenemy->enemy_type;
	      active_enemies[i].behavior = BEHAV_DO_NOTHING;
	      active_enemies[i].x=fenemy->x;
	      active_enemies[i].y=fenemy->y;
	      active_enemies[i].energy = fenemy->energy;
	      active_enemies[i].param2=fenemy->param2;
	      active_enemies[i].param3=0;
	      active_enemies[i].param4=0;
	      active_enemies[i].behav_param=0;
	 }*/


#asm
	ld b, 0
	call setrambank		; vuelta a la normalidad
	ei
#endasm

 // Load the sprite block for the final enemy
 LoadSprBlock(22,finalspr[current_level],finalspr_count[current_level]);
 #asm
	call InitSprCacheList	; finally, re-initialize sprite cache list to ensure there are no problems!
 #endasm

 wyz_stop_music();
 wyz_load_music(finalenemy_music[current_level]);
#asm
 ld a, (_sound_selection)
 ld (SOUND_SFX), a
#endasm

     }

#asm
	ld a, (_fenemy_activation_counter)
	inc a 
	ld (_fenemy_activation_counter), a
	and $3
	jp nz, checkifcounteris60
	ld a, (_final_enemy_components)
	ld b, a
	ld ix, _active_enemies
	ld de, 12
.renewexplosion_counter
	ld (ix+2), EXPLOSION_SPR	; exploding
	ld (ix+4), MOVE_EXPLOSION
	ld (ix+6), 4			; 4 frames to explode
	add ix, de
	djnz renewexplosion_counter

.checkifcounteris60

#endasm

/*
     fenemy_activation_counter++;
     if((fenemy_activation_counter & 3) == 0)
     {
	 for(i=0;i<final_enemy_components;i++)
	 {
	      active_enemies[i].sprnum=EXPLOSION_SPR;	// exploding!
    	      active_enemies[i].movement=MOVE_EXPLOSION;
              active_enemies[i].param1=4;               // 4 frames to explode
         }
     }
*/
     if (fenemy_activation_counter == 60)
     {
#asm
	di
	push bc
	ld b, 4
	call setrambank		; Levels in RAM4
	pop bc
#endasm


#asm
	ld a, (_final_enemy_components)
	ld b, a
	ld hl, _active_enemies
	ld ix, (_enemy_locations)
.initfenemyloop_part2
	inc hl
	inc hl
	push hl
	ld hl, _finalenemy_sprites
	ld a, (ix+3)		; fenemy->enemy_type
	ld e, a
	ld d, 0
	add hl, de
	ld a, (hl)		; A = finalenemy_sprites[fenemy->enemy_type];
	pop hl
	ld (hl), a		; active_enemies[i].sprnum=finalenemy_sprites[fenemy->enemy_type];
	inc hl
	inc hl
	ld a, (ix+4)
	ld (hl), a		; active_enemies[i].movement=fenemy->movement;
	inc hl
	ld a, (ix+5)
	ld (hl), a		; active_enemies[i].energy=fenemy->energy;
	inc hl
	ld a, (ix+6)
	ld (hl), a		; active_enemies[i].param1=fenemy->param1;
	ld de, 8
	add ix, de		; enemy_locations[i+1]
	ld de, 6
	add hl, de		; active_enemies[i+1]
	djnz initfenemyloop_part2
#endasm


	/* for(i=0;i<final_enemy_components;i++)
	 {
	      fenemy = &(enemy_locations[i]);
	      active_enemies[i].sprnum=finalenemy_sprites[fenemy->enemy_type];
	      active_enemies[i].movement=fenemy->movement;
	      active_enemies[i].param1=fenemy->param1;
	 }*/
	 final_enemy_active=1;
#asm
	ld b, 0
	call setrambank		; vuelta a la normalidad
	ei
#endasm
     }
}


// Calculate the number of available shoots

unsigned char AvailShoots(void)
{
#asm
	ld hl, _my_active_shoots+2
	ld a, (_max_shoots)
	ld b, a
	ld c, 0
	ld de, 12	; sizeof (struct Entity)
.availshoots_loop
	ld a, (hl)
	and a
	jr nz, shoot_active
	inc c
.shoot_active
	add hl, de
	djnz availshoots_loop
	ld h, 0
	ld l, c
	ret
#endasm

}

struct Entity * NewShoot(unsigned char x, unsigned char y)
{
#asm
	ld hl, 4
	add hl, sp
	ld a, (hl)
	and a
	jp z, nonewshoot		; Cannot create a shoot at X=0, it means we are at the end of the screen

	ld hl, 2
	add hl, sp
	ld a, (hl)
	cp 113
	jp nc, nonewshoot		; If Y > 112, also skip the shoot creation


	ld ix, _my_active_shoots
	ld a, (_max_shoots)
	ld b, a

.searchshoot_loop
	ld a, (ix+2)
	and a
	jr nz, next_searchshoot
	; sprnum is 0, so a candidate was found
	ld a, (_current_weapon_sprite)
	ld (ix+2), a		;current_e->sprnum = current_weapon_sprite;

	ld hl, 4
	add hl, sp
	ld a, (hl)
	ld (ix+0), a		; current_e->x = x;
	dec hl
	dec hl			; hl points to Y
	ld a, (hl)
	ld (ix+1), a		; current_e->y = y
	ld a, MOVE_RIGHT
	ld (ix+4), a		;  current_e->movement = MOVE_RIGHT;

	ld a, (_current_weapon)
	ld hl, _shoot_speed

	ld e, a
	ld d, 0
	add hl, de
	ld a, (hl)		; A= shoot_speed[current_weapon]


;	ld a, 8
	ld (ix+6), a		; current_e->param1 = shoot_speed[current_weapon];   // Fast for now, let's see the rest
	ld a, ixh
	ld h, a
	ld a, ixl
	ld l, a			; HL points to the entity created
	ret
.next_searchshoot
	ld de, 12
	add ix, de
	djnz searchshoot_loop
.nonewshoot
	ld hl, 0		; return NULL if all shoots are in use
#endasm
/*        if(!x) return;  // Cannot create a shoot at X=0, it means we are at the end of the screen

	for (i=0;i<max_shoots;i++)
	{
		current_e=&my_active_shoots[i];
		if(current_e->sprnum == 0) // Found a candidate
		{

			current_e->sprnum = current_weapon_sprite;	// FIXME: Substitute with some variable holding the current shoot, defining shoot energy, etc
			current_e->x = x;
			current_e->y = y;
			current_e->movement = MOVE_RIGHT;
			current_e->param1 = 8;   // Fast for now, let's see the rest
			return;
		}
	}*/
}

#asm 
._NewEnemyShoot_FX
        ld hl, FX_DOUBLE_SHOOT
        push hl
        call _wyz_effect
        pop hl
#endasm

struct Entity * NewEnemyShoot(unsigned char x, unsigned char y, unsigned char movement, unsigned char sprite)
{
#asm
	ld hl, 8
	add hl, sp
	ld a, (hl)
	and a
	jr z, nonewenemy		; Cannot create a shoot at X=0, it means we are at the end of the screen

       	ld hl, 6
       	add hl, sp
	ld a, (hl)
	cp 113
	jr nc, nonewenemy		; If Y > 112, also skip the shoot creation

	ld ix, _enemy_active_shoots
	ld a, MAX_ENEMIES
	ld b, a

.searchenemyshoot_loop
	ld a, (ix+2)
	and a
	jr nz, next_searchenemyshoot
	; sprnum is 0, so a candidate was found

	ld hl, 8
	add hl, sp
	ld a, (hl)
	ld (ix+0), a		; current_e->x = x;
	dec hl
	dec hl			; hl points to Y
	ld a, (hl)
	ld (ix+1), a		; current_e->y = y
	dec hl
	dec hl
	ld a, (hl)		;
	ld (ix+4), a		;  current_e->movement = movement;
	ld a, 8
	ld (ix+6), a		; current_e->param1 = 8;   // Fast for now, let's see the rest
	dec hl
	dec hl
	ld a, (hl)
	ld (ix+2), a		; current_e->sprnum = sprnum;
	ld a, ixh
	ld h, a
	ld a, ixl
	ld l, a			; HL points to the entity created
	ret
.next_searchenemyshoot
	ld de, 12
	add ix, de
	djnz searchenemyshoot_loop
.nonewenemy	
	ld hl, 0		; return NULL if all shoots are in use
#endasm


/*
        if(!x) return;  // Cannot create a shoot at X=0, it means we are at the end of the screen

	for (i=0;i<MAX_ENEMIES;i++)
	{
		current_e=&enemy_active_shoots[i];
		if(current_e->sprnum == 0) // Found a candidate
		{
			current_e->sprnum = current_weapon;	// FIXME: Substitute with some variable holding the current shoot, defining shoot energy, etc
			current_e->x = x;
			current_e->y = y;
			current_e->movement = type;
			current_e->param1 = 8;   // Fast for now, let's see the rest
			return;
		}
	}*/
}


// Generate the enemies starting from "first"... Useful for final enemies

struct Entity * NewEnemy(struct Enemy *e, unsigned char first)
{
#asm
	ld hl, 2
	add hl, sp
	ld b, (hl)		; B = first
	inc hl
	inc hl
	ld e, (hl)
	inc hl
	ld d, (hl)
	ld ixh, d
	ld ixl, e		; IX = e
	ld hl, _active_enemies-12
	ld de, 12
	ld a, b			; Save first in A
	inc b			
.gotofirst
	add hl, de
	djnz, gotofirst

	ld b, a
	inc hl
	inc hl
.findemptyenemy
	ld a, (hl)
	and a
	jp nz, enemynotempty
; Empty slot found, populate it!
	dec hl
	dec hl			; point to e->X

	push hl			; Save HL
	ld a, (_map_xpos)
	neg
	add a, (ix+0)		; A = e->x - map_xpos
	ld l, a
	ld h, 0
	xor a
	rl l
	rl h
	rl l
	rl h
	rl l
	rl h			; HL = HL*8
	ld d, h
	ld e, l
	add hl, de
	add hl, de		; HL = HL*24, (e->x - map_xpos) * 24)
	ld e, (ix+1)
	ld d, 0			; DE=e->e_xdesp
	add hl, de		; HL = (e->x - map_xpos) * 24) + e->x_desp
	ld a, (_map_displacement)
	rlca			; the highest bit of map_displacement is always 0, so in effect it is << 1
	ld e, a
	sbc hl, de		; HL = ((e->x - map_xpos) * 24) + e->x_desp - (map_displacement<<1), it should be less than 256
	ex de, hl
	pop hl
	ld (hl), e		;  active_enemies[i].x=((e->x - map_xpos) * 24) + e->x_desp - (map_displacement<<1);
	inc hl
	ld a, (ix+2)
	ld (hl), a		;  active_enemies[i].y=e->y;
	inc hl
	push hl
	ld hl, _enemy_sprites
	ld a, (ix+3)		; e->enemy_type
	ld e, a
	ld d, 0
	add hl, de
	ld a, (hl)		; A = enemy_sprites[e->enemy_type]
	pop hl
	ld (hl), a		; active_enemies[i].sprnum=enemy_sprites[e->enemy_type];
	inc hl
	ld a, (ix+3)		; e->enemy_type
	ld (hl), a		; active_enemies[i].type= e->enemy_type;
	inc hl
	ld a, (ix+4)		; e->movement
	ld (hl), a		; active_enemies[i].movement=e->movement;
	inc hl
	ld a, (ix+5)	
	ld (hl), a		; active_enemies[i].energy = e->energy;
	inc hl
	ld a, (ix+6)
	ld (hl), a		; active_enemies[i].param1=e->param1;
	inc hl
	ld a, (ix+7)
	ld (hl), a		; active_enemies[i].param1=e->param2;
	xor a
	inc hl
	ld (hl), a		; active_enemies[i].param3=0;
	inc hl
	ld (hl), a		; active_enemies[i].param4=0;
	inc hl
	ld a, (ix+3)		; e->enemy_type
	push hl
	ld hl, _behavior_types
	ld e, a
	add hl, de
	ld a, (hl)		; A = behavior_types[e->enemy_type];
	pop hl
	ld (hl), a		; active_enemies[i].behavior = behavior_types[e->enemy_type];
	inc hl
	xor a
	ld (hl), a		; active_enemies[i].behav_param=0;
	ld de, -11
	add hl, de
	ret			; return &(active_enemies[i])

.enemynotempty
	ld de, 12
	add hl, de
	inc b
	ld a, MAX_ENEMIES-1
	cp b
	jp nc, findemptyenemy

	ld hl, 0		; return 0 if no empty enemy was found
#endasm


/*
  for(i=first;i<MAX_ENEMIES;i++)
  {
   if(active_enemies[i].sprnum == 0)
   {
      active_enemies[i].sprnum=enemy_sprites[e->enemy_type];
      active_enemies[i].type= e->enemy_type;
      active_enemies[i].behavior = behavior_types[e->enemy_type];
      active_enemies[i].x=((e->x - map_xpos) * 24) + e->x_desp - (map_displacement<<1);
      active_enemies[i].y=e->y;
      active_enemies[i].energy = e->energy;
      active_enemies[i].movement=e->movement;
      active_enemies[i].param1=e->param1;
      active_enemies[i].param2=e->param2;
      active_enemies[i].param3=0;
      active_enemies[i].param4=0;
      active_enemies[i].behav_param=0;
      return &(active_enemies[i]);
   }
  }
  return (struct Entity *)0;*/
}

char shoot_xchar, shoot_ychar;
//char enemy_xchar, enemy_ychar;

unsigned char shoot_hits_enemy(struct Entity *shoot)
{
#asm
    pop bc
    pop hl
    push hl
    push bc                           ; HL holds the pointer to the shoot struct

    ld a, (hl)
    rrca
    rrca
    rrca
    and $1f
    ld (_shoot_xchar), a                ; shoot_xchar = shoot->x / 8
    inc hl
    ld a, (hl)
    rrca
    rrca
    rrca
    and $1f
    ld (_shoot_ychar), a                ; shoot_ychar = shoot->y / 8

    ld hl,_active_enemies
    ld b, MAX_ENEMIES
    ld c, 0              ; C will serve as an enemy counter
.loop_hitenemy
    push bc
    inc hl
    inc hl
    ld a, (hl)
    dec hl
    dec hl
    and a
    jr z, cont_loop_hitenemy
    ; not zero, so this is an active enemy

    ld a, (hl)  ; current_e->x
    rrca
    rrca
    rrca
    and $1f
    ld ix, _shoot_xchar
    sub (ix+0)
    ld d, a     ; D =    (current_e->x) / 8 -   shoot_xchar
    inc hl      ; current_e->y
    ld a, (hl)  ; current_e->y
    dec hl
    rrca
    rrca
    rrca
    and $1f
    ld ix, _shoot_ychar
    sub (ix+0)
    ld e, a     ; E =    (current_e->y) / 8 -   shoot_ychar

    ; we want to check if abs(D) and abs(E) is 0 or 1. So we add one, and if the value as
    ; unsigned is less than 3, we got it
    inc d
    inc e
    ld a, 2
    cp d
    jr c, cont_loop_hitenemy
    cp e
    jr c, cont_loop_hitenemy

    ; our shoot has hit an enemy, so we exit with the enemy value
    pop bc          ; to restore the stack
    ld h, 0
    ld l, c
    ret

.cont_loop_hitenemy
    ld bc, 12
    add hl, bc
    pop bc
    inc c     ; next enemy
    djnz loop_hitenemy
    ld h, 0
    ld l, 255
    ret
#endasm


/*
 shoot_xchar = (shoot->x) / 8;
 shoot_ychar = (shoot->y) / 8;

 for(j=0;j<MAX_ENEMIES;j++)	// For now, a crude bounding box collision detection. If it works...
 {
  current_e = &active_enemies[j];
  if(current_e->sprnum)	// This is a real active enemy
  {
	 enemy_xchar = (current_e->x) / 8;
	 enemy_ychar = (current_e->y) / 8;

	 enemy_xchar -= shoot_xchar;
	 enemy_ychar -= shoot_ychar;

	 if((enemy_xchar < 2) && (enemy_xchar > -2))
	 {
	  if((enemy_ychar < 2) && (enemy_ychar > -2))	return j;
	 }
  }
 }
 return 255; */

}

// Go to every enemy on screen and substract 5 to its energy
// Sould kill them all!

void Do_Superbomb(void)
{
#asm
	ld a, 7
	ld (_border_color), a		; set border effect

	ld b, MAX_ENEMIES
	ld hl, _active_enemies+2
	ld de, 12			; sizeof (struct enemy)

.superbomb_loop
	ld a, (hl)
	and a
	jr z, continue_loop_superbomb

	inc hl
	inc hl
	inc hl				; energy			
	ld a, (hl)		
	and a				; If energy is 0, ignore (not killable or already dead)
	jr z, superbomb_shot_completed
	sub 5
	jr c, superbomb_enemy_dead
	jr z, superbomb_enemy_dead	; if the energy is 0 or < 0, it is dead
	ld (hl), a			; new energy
	jr superbomb_shot_completed
.superbomb_enemy_dead
	ld (hl), 0			; enemy is dead
        inc hl                               ; param1
        ld (hl), 4
	dec hl
	dec hl					; movement
        ld (hl), MOVE_EXPLOSION
	dec hl
	dec hl                             ; sprnum
        ld (hl), EXPLOSION_SPR
.continue_loop_superbomb
	add hl, de
	djnz superbomb_loop
	ret
.superbomb_shot_completed
	dec hl
	dec hl
	dec hl
	jr continue_loop_superbomb		; make sure we are at energy before adding 12
#endasm
}


#asm
; Input:
;	B: X position
;	C: Y position
; Output:
;        HL: screen address
;        B:  bit position

.CalcScreenPos
     ld a, c
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
.shiftloop
     rrca
     dec b
     jr nz, shiftloop
.postloop
     ld b, a
     ret



; Input:
;	B: X position
;	C: Y position
; Output:
;        HL: screen address

.CalcScreenPos_Char
     ld a,c			; 4  <- the top 2 bits are the screen third
     rra			; 4
     rra			; 4
     rra			; 4
     and $18			; 7
     or $C0			; 4	<- If the start address is 16384, this should be $40
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



;Divide 8-bit values
;In: Divide E by divider D
;Out: A = result, D = rest
;
.Div8
    xor a
    ld b,8
.Div8_Loop
    rl e
    rla
    sub d
    jr nc,Div8_NoAdd
    add a,d
.Div8_NoAdd
    djnz Div8_Loop
    ld d,a
    ld a,e
    rla
    cpl
    ret

#endasm


/*
void CheckShipCollision(void)
{


#asm

    ld a, (_ship_x)
    add a, 2
    ld b, a
    ld a, (_ship_y)
    add a, 3
    ld c,a
    call CalcScreenPos          ; check upper left corner
    ld a, (hl)
    and b
    jp nz, collision
    inc hl
    ld a, (hl)
    and a
    jp nz, collision		; middle char    
    inc hl
    inc hl                  ; the upper right corner is always 3 characters to the right
    ld a, (hl)
    and b
    jp nz, collision

    ld a, (_ship_x)
    add a, 2
    ld b, a
    inc c
    inc c
    call CalcScreenPos          ; check the 5th pixel
    ld a, (hl)
    and b
    jp nz, collision
    inc hl
    ld a, (hl)
    and a
    jp nz, collision		; middle char    
    inc hl
    inc hl 			; the upper right corner is always 3 characters to the right
    ld a, (hl)
    and b
    jp nz, collision

    ld a, (_ship_x)
    add a, 2
    ld b, a
    inc c
    inc c
    call CalcScreenPos          ; check the 7th pixel
    ld a, (hl)
    and b
    jp nz, collision
    inc hl
    ld a, (hl)
    and a
    jp nz, collision		; middle char    
    inc hl
    inc hl 			; the upper right corner is always 3 characters to the right
    ld a, (hl)
    and b
    jp nz, collision

    ld a, (_ship_x)
    add a, 2
    ld b, a
    inc c
    inc c
    call CalcScreenPos          ; check the 9th pixel
    ld a, (hl)
    and b
    jr nz, collision
    inc hl
    ld a, (hl)
    and a
    jp nz, collision		; middle char    
    inc hl
    inc hl 			; the upper right corner is always 3 characters to the right
    ld a, (hl)
    and b
    jr nz, collision

    ld a, (_ship_x)
    add a, 2
    ld b, a
    inc c
    inc c
    call CalcScreenPos          ; check the 11th pixel
    ld a, (hl)
    and b
    jr nz, collision
    inc hl
    ld a, (hl)
    and a
    jp nz, collision		; middle char    
    inc hl
    inc hl 			; the upper right corner is always 3 characters to the right
    ld a, (hl)
    and b
    jr nz, collision

    ld a, (_ship_x)
    add a, 2
    ld b, a
    inc c
    inc c
    call CalcScreenPos          ; check the 13th pixel (lower left corner)
    ld a, (hl)
    and b
    jr nz, collision
    inc hl
    ld a, (hl)
    and a
    jp nz, collision		; middle char    
    inc hl
    inc hl 			; the upper right corner is always 3 characters to the right
    ld a, (hl)
    and b
    jr nz, collision



    ld a, (_ship_x)
    add a, 15
    ld b, a
    ld a, (_ship_y)
    add a, 3
    ld c,a
    call CalcScreenPos          ; check upper center
    ld a, (hl)
    and b
    jr nz, collision

    ld a, (_ship_x)
    add a, 15







    ld b, a
    ld a, (_ship_y)
    add a, 7
    ld c,a
    call CalcScreenPos          ; check middle center
    ld a, (hl)
    and b
    jr nz, collision

    ld a, (_ship_x)
    add a, 15
    ld b, a
    ld a, (_ship_y)
    add a, 13
    ld c,a
    call CalcScreenPos          ; check lower center
    ld a, (hl)
    and b
    jr nz, collision


    ret                          ; no collision
.collision
    ld a, (_mayday)
    and a
    ret nz			; if mayday is already non zero, do not duplicate
;    inc a			; NORMAL BEHAVIOR
   nop				; CHEAT!!!!!
    ld (_mayday),a		; mayday, we have been hit!

#endasm

}*/


void CheckShipCollision(void)
{
#asm
	ld a, (Ship_Collision)
	and a
	ret z
.collision
	ld a, (_mayday)
	and a
	ret nz			; if mayday is already non zero, do not duplicate
.cheathere
	inc a			; NORMAL BEHAVIOR
;	nop				; CHEAT!!!!!
	ld (_mayday),a		; mayday, we have been hit!
#endasm


}


void CheckShootCollision(void)
{
#asm

    ld ix, _my_active_shoots    ; pointer to the structure
    ld a, (_max_shoots)
    ld iyh, a                     ; IYh will be the counter
    call checkshootloop

    ld ix, _enemy_active_shoots
    ld a, MAX_ENEMIES
    ld iyh, a
    call checkshootloop		; check the same, for the enemy shoots
#endasm
}

#asm
.checkshootloop
    ld a, (ix+2)		; get sprnum
    and a
    jp z, continue_collisionloop 	; ignore non-existing shoots

    ld a, (ix+0)                 
    add a, 12
    ld e, a			; E == shoot_x
    ld d, 24
    call Div8			; A holds x /24, D holds x % 24
    ld e, a
    ld a, (_map_xpos)
    add a, e			
    ld c, a			; C = x_tile = map_xpos + x/24 
    ld a, (_map_displacement)
    add a, a			; a * 2
    add a, d			; a = map_disp + x %24
    cp 24
    jp c, no_overflow		; a < 24
    inc c			; if a >= 24, increment x_tile
.no_overflow
    ld a, (ix+1)                ; a = shoot_y, c = shoot_x (in tile terms)
    add a, 4			; middle of the shoot
    sra a
    sra a
    sra a			; a = shoot_y (in tile terms), c = shoot_x (in tile terms)









    ld h, $a0
    ld l, c			; hl holds the position of shoot_x (in tile terms) in the map
    ex af, af
    ld a, (_CurLevel_XLength)
    ld e,a
    xor a
    ld d, a
    ex af, af			; we need to add DE shoot_y times
    and a
    jp z, endloop_addde		; if a is zero, no need to add

.loop_addde
    add hl, de
    dec a
    jr nz, loop_addde		; at the end, HL has the tile address
.endloop_addde
    ld a, (hl)
    and a
    jr z, noshootcollision1
    xor a
    ld (ix+2), a                ; my_active_shoots[i].sprnum=0
    jp continue_collisionloop

.noshootcollision1		; now check again, for the lower side of the shoot
    ld a, (ix+1)                ; a = shoot_y, c = shoot_x (in tile terms)
    add a, 12			; middle of the shoot
    sra a
    sra a
    sra a			; a = shoot_y (in tile terms), c = shoot_x (in tile terms)
    ld h, $a0
    ld l, c			; hl holds the position of shoot_x (in tile terms) in the map
    ex af, af
    ld a, (_CurLevel_XLength)
    ld e,a
    xor a
    ld d, a
    ex af, af			; we need to add DE shoot_y times
    and a
    jp z, endloop_addde2		; if a is zero, no need to add

.loop_addde2
    add hl, de
    dec a
    jr nz, loop_addde2		; at the end, HL has the tile address
.endloop_addde2
    ld a, (hl)
    and a
    jr z, continue_collisionloop
    xor a
    ld (ix+2), a                ; my_active_shoots[i].sprnum=0
.continue_collisionloop
    ld bc, 12			; 12 == sizeof (struct Entity)
    add ix, bc
    dec iyh
    jp nz, checkshootloop
    ret
#endasm





void ShowBombBar(void)
{
#asm
	ld hl, 56025		; position in memory for the attributes of the progress bar
	ld a, (_frames_fire_pressed)
	sra a			; and divide by 2

.bar_onloop
	and a
	jr z, bar_off
	ld (hl), 0x03
	inc hl
	dec a
	jr bar_onloop
.bar_off	
	ld a, (_frames_fire_pressed)
	sub 12
	neg
	sra a
.bar_offloop
	and a
	ret z
	ld (hl), 0x01
	inc hl
	dec a
	jr bar_offloop
#endasm
}


void gameloop(void)
{


 while(mayday<7)
 {
#asm
waitvblank:
	ld a,r
	jp m, waitvblank	; while the screen has not been switched, we cannot continue
waitframeskip:
	ld a, (_frameskip_counter)
	cp 2
	jp c, waitframeskip	; we must wait until enough frames has passed
	xor a
	ld (_frameskip_counter), a

	;halt
	di

	call _DrawGameMap	; draw map, interrupts must be disabled

	ld a, (_current_level)
	cp 2
	jr z, noduplicatewyzplay

	ld b, 0
	call setrambank		; we have missed one interrupt for sure, so call player
	call WYZ_PLAY

.noduplicatewyzplay
	ld a, (_current_screen)
	xor 2			; 5 xor 2 = 7; 7 xor 2 = 5
	ld b, a
	call setrambank		; restore the previous paging state
	ei

	call _DrawEnemies	; draw enemies and enemy shoots (also valid for final enemies and their shoots)
	call _DrawShip		; draw ship, my shoots and power ups: at the same time, check ship collision
;        call _CheckShipCollision ; check HERE if the ship hits the background or an enemy
        call _CheckShootCollision
	call MoveStarfield	; update starfield
	call Display_starfield	; now display it

	call _ShowBombBar	; display progress bar for superbomb

	; Do we need to update the score?
	ld a, (_update_score)
	and a
	jr z, noupdatescore
	dec a
	ld (_update_score), a
	ld b, 120
	ld c, 144
	ld hl, (_score)
	call PrintLarge_5
.noupdatescore
	; Do we need to update the number of superbombs?
	ld a, (_update_superb)
	and a
	jr z, noupdatesuperb
	dec a
	ld (_update_superb), a
	ld b, 72
	ld c, 152
	ld a, (_available_superbombs)
	call PrintLarge_2
.noupdatesuperb
	; Do we need to update the number of lifes?
	ld a, (_update_life)
	and a
	jr z, noupdatelife
	dec a
	ld (_update_life), a
	ld b, 72
	ld c, 136
	ld a, (_life_counter)
	call PrintLarge_2
.noupdatelife
	ld a,r
	or $80
	ld r,a			; set the highest bit of R to 1, so switch screen!!!!

	di
	ld b, 0
	call setrambank		; leave the RAM bank 0 in place before continuing
	ei
#endasm

  if(!mayday) 	// only check joystick if we are not dying
  {
	if(ship0spr == 6) ship0spr = 8;
	else	ship0spr=0;

	 if(inertia_cheat)
	 {
	  if(joy & JOY_UP)
	  {
		speed_y = -4;
		ship0spr=2;
	  }
	  else if(joy & JOY_DOWN)
	  {
		speed_y=4;
		ship0spr=4;
	  }
	  else speed_y=0;
	  
	  if(joy & JOY_LEFT) speed_x = -4;
	  else  if(joy & JOY_RIGHT)	speed_x = 4;
	    else speed_x=0;
	 }
	 else
	 {

	  if(joy & JOY_UP)
	  {
		if(speed_y > -4) speed_y --;
		ship0spr=2;
	  }
	  else if(joy & JOY_DOWN)
	  {
		if(speed_y < 4) speed_y ++;
		ship0spr=4;
	  }
	  else if(speed_y > 0) speed_y--;	// No keys pressed, decrease speed
	   else if(speed_y < 0) speed_y++;

	  if(joy & JOY_LEFT)
	  {
		if(speed_x > -4) speed_x --;
	  }
	  else  if(joy & JOY_RIGHT)
	  {
		if(speed_x < 4) speed_x++;
	  }
	  else if(speed_x > 0) speed_x--;
	   else if(speed_x < 0) speed_x++;
	 }


	  if (joy & JOY_FIRE)
  	  {
#asm
	; increment frames_fire_pressed, unless it is 12 already

	ld a, (_frames_fire_pressed)
	inc a
	cp 13
	jr nz, store_frames_fire_pressed
	dec a
.store_frames_fire_pressed
	ld (_frames_fire_pressed), a
#endasm	

		if(!frames_to_shoot)
		{
			dummy_b = 1;
			#asm
				ld a, (_current_weapon)
				and a
				jp z, shot_case0
				cp 1
				jp z, shot_case1
				cp 2
				jp z, shot_case2
				cp 3
				jp z, shot_case3
				cp 4
				jp z, shot_case4
				cp 5
				jp nz, shot_noshoot
			.shot_case5			; Megashot
					call _AvailShoots
					ld a, l 
					cp 8
					jp c, shot_noshoot	; less than 8 shoots available
					; Play sound
					ld b, FX_DISPARO_MULTI
		                        call LOAD_FX

					ld a, SHOT_BASIC
					ld (_current_weapon_sprite), a
					ld a, (_ship_x)
					add a, 20
					ld h, 0
					ld l, a
					push hl
					ld a, (_ship_y)
					ld l, a


					push hl
					call _NewShoot		;NewShoot(ship_x+20, ship_y)	
					pop de


					pop hl	

					ld a, SHOT_MEGA
					ld (_current_weapon_sprite), a
					ld a, (_ship_x)
					add a, 20
					ld h, 0

					ld l, a
					push hl			; ship_x+20
					ld a, (_ship_y)
					sub 12
					jr c, case5_skip_shot2
					ld l, a			; ship_y-12
					push hl
					call _NewShoot		;NewShoot(ship_x+20, ship_y-12)
					ld a, l
					or h
					jr z, case5_shot2_not_created	; if the shot was not created, skip
					inc hl
					inc hl
					inc hl
					inc hl
					ld (HL), MOVE_UPRIGHT
.case5_shot2_not_created
					pop bc
.case5_skip_shot2
					pop bc

					ld a, (_ship_x)
					add a, 20
					ld h, 0
					ld l, a
					push hl			; ship_x+20
					ld a, (_ship_y)
					add a, 12		; ship_y +12
					ld l, a
					push hl
					call _NewShoot		;NewShoot(ship_x+20, ship_y+12)
					ld a, l
					or h
					jr z, case5_shot3_not_created	; if the shot was not created, skip
					inc hl
					inc hl
					inc hl
					inc hl
					ld (HL), MOVE_DOWNRIGHT
					inc hl
					inc hl
					inc hl
					ld (hl), 8		; e->param2=8
.case5_shot3_not_created
					pop de
					pop bc

					ld a, (_ship_x)
					ld h, 0
					ld l, a
					push hl			; ship_x
					ld a, (_ship_y)
					sub 12	
					jr c, case5_skip_shot4	
					ld l, a
					push hl			; ship_y-12
					call _NewShoot		;NewShoot(ship_x, ship_y-12)
					ld a, l
					or h
					jr z, case5_shot4_not_created	; if the shot was not created, skip
					inc hl
					inc hl
					inc hl
					inc hl
					ld (HL), MOVE_UPLEFT
					inc hl
					inc hl
					inc hl
					ld (hl), 8		; e->param2=8
.case5_shot4_not_created
					pop de
.case5_skip_shot4
					pop bc

					ld a, (_ship_x)
					ld h, 0
					ld l, a
					push hl			; ship_x
					ld a, (_ship_y)
					add a, 12		; ship_y +12
					ld l, a
					push hl
					call _NewShoot		;NewShoot(ship_x, ship_y+12)
					ld a, l
					or h
					jr z, case5_shot5_not_created	; if the shot was not created, skip
					inc hl
					inc hl
					inc hl
					inc hl
					ld (HL), MOVE_DOWNLEFT
					inc hl
					inc hl
					inc hl
					ld (hl), 8		; e->param2=8
.case5_shot5_not_created
					pop de
					pop bc


					ld a, (_ship_x)
					sub 12
					jr c, case5_skip_shot6
					ld h, 0
					ld l, a
					push hl			; ship_x-12
					ld a, (_ship_y)
					ld l, a
					push hl			; ship_y
					call _NewShoot		;NewShoot(ship_x-12, ship_y)
					ld a, l
					or h
					jr z, case5_shot6_not_created	; if the shot was not created, skip
					inc hl
					inc hl
					inc hl
					inc hl
					ld (HL), MOVE_LEFT	
.case5_shot6_not_created
					pop de
					pop bc
.case5_skip_shot6

		
					ld a, (_ship_x)
					add a, 12
					ld h, 0
					ld l, a
					push hl			; ship_x+12
					ld a, (_ship_y)
					sub 12
					jr c, case5_skip_shot7
					ld l, a
					push hl			; ship_y-12
					call _NewShoot		;NewShoot(ship_x+12, ship_y-12)
					ld a, l
					or h
					jr z, case5_shot7_not_created	; if the shot was not created, skip
					inc hl
					inc hl
					inc hl
					inc hl
					ld (HL), MOVE_UP
.case5_shot7_not_created
					pop de
.case5_skip_shot7
					pop bc

					ld a, (_ship_x)
					add a, 12
					ld h, 0
					ld l, a
					push hl			; ship_x+12
					ld a, (_ship_y)
					add a, 12
					ld l, a
					push hl

					call _NewShoot		;NewShoot(ship_x+12, ship_y+12)
					ld a, l
					or h
					jr z, case5_shot8_not_created	; if the shot was not created, skip
					inc hl
					inc hl
					inc hl
					inc hl
					ld (HL), MOVE_DOWNLEFT
					inc hl
					inc hl
					inc hl
					ld (hl), 2		; e->param2=2
.case5_shot8_not_created
					pop de
					pop de

					jp shot_endcase
			.shot_case0			; Basic shot
					call _AvailShoots
					ld a, l
					and a
					jp z, shot_noshoot	; no available shoots
					ld a, (_ship_x)
					add a, 20
					ld h, 0
					ld l, a
					push hl
					ld a, (_ship_y)
					ld l, a
					push hl
					call _NewShoot		;NewShoot(ship_x+20, ship_y)
					pop hl
					pop hl
					; Play sound
					ld b, FX_SINGLE_SHOOT
		                        call LOAD_FX
					jp shot_endcase
			.shot_case1			; triple shoot
					call _AvailShoots
					ld a, l 
					cp 3
					jp c, shot_noshoot	; less than 3 shoots available
					ld a, (_ship_x)
					add a, 20
					ld h, 0
					ld l, a
					push hl
					ld a, (_ship_y)
					sub 12
					jr c, case1_skip_shot1
					ld l, a
					push hl
					call _NewShoot		;NewShoot(ship_x+20, ship_y-12)
					pop hl
                        .case1_skip_shot1
					pop hl

					ld a, (_ship_x)
					add a, 20
					ld h, 0
					ld l, a
					push hl
					ld a, (_ship_y)
					add a, 12
					ld l, a
					push hl
					call _NewShoot		;NewShoot(ship_x+20, ship_y+12)
					pop hl
					pop hl

					ld a, (_ship_x)
					add a, 20
					ld h, 0
					ld l, a
					push hl
					ld a, (_ship_y)
					ld l, a
					push hl
					call _NewShoot		;NewShoot(ship_x+20, ship_y)
					pop hl
					pop hl
    	                                ; Play sound
					ld b, FX_TRIPLE_SHOOT
		                        call LOAD_FX
					jp shot_endcase
			.shot_case2		; Laser
					call _AvailShoots
					ld a, l 					
					cp 2
					jp c, shot_noshoot	; less than 2 shoots available
					ld a, (_ship_x)
					add a, 20
					ld h, 0
					ld l, a
					push hl
					ld a, (_ship_y)
					ld l, a
					push hl
					call _NewShoot		;NewShoot(ship_x+20, ship_y)
					pop hl
					pop hl
					; Play sound
					ld b, FX_LASER
		                        call LOAD_FX
					ld a, (_ship_x)
					add a, 36
					cp 208
					jr nc, case2_skip_shot2
					ld h, 0
					ld l, a
					push hl
					ld a, (_ship_y)
					ld l, a
					push hl
					call _NewShoot		;NewShoot(ship_x+36, ship_y)
					pop hl
					pop hl
			.case2_skip_shot2
					jp shot_endcase
			.shot_case3		; homing missile
					call _AvailShoots
					ld a, l
					and a
					jp z, shot_noshoot	; no available shoots
					ld a, (_ship_x)
					add a, 20
					ld h, 0
					ld l, a
					push hl
					ld a, (_ship_y)
					ld l, a
					push hl
					call _NewShoot		;NewShoot(ship_x+20, ship_y)
					pop de
					pop de			; HL holds the pointer to the new shoot
					ld de, 4
					add hl, de		; HL points to e->movement
					ld (hl), MOVE_HOMING
					; now go find the first available active enemy
					exx
					ld c, 0			; C will serve as counter for the enemies
					ld hl, _active_enemies+2 ; go for the sprnums
					ld de, 12		; sizeof (struct Entity)
			.loop_case3
					ld a, (hl)
					and a
					jr nz, found_case3
					add hl, de
					inc c 
					ld a, MAX_ENEMIES
					cp c					
					jr nz, loop_case3
					ld c, 0			; we got out of the loop without finding any active enemies
			.found_case3
					ld a, c
					exx
					inc hl
					inc hl
					inc hl
					ld (hl), a		; param2
					; Play sound
					ld b, FX_DISPARO_HOMMING
		                        call LOAD_FX
					jp shot_endcase
			.shot_case4				; bomb
					call _AvailShoots
					ld a, l
					cp 2
					jp c, shot_noshoot	; less than 2 shoots available
					; Play sound
					ld b, FX_DISPARO_HOMMING
		                        call LOAD_FX


					ld a, SHOT_BASIC
					ld (_current_weapon_sprite), a
					ld a, (_ship_x)
					add a, 20
					ld h, 0
					ld l, a
					push hl
					ld a, (_ship_y)
					ld l, a
					push hl
					call _NewShoot		;NewShoot(ship_x+20, ship_y)
					pop de
					pop de

					ld a, SHOT_BOMB
					ld (_current_weapon_sprite), a
					ld a, (_ship_x)
					add a, 10
					ld h, 0
					ld l, a
					push hl
					ld a, (_ship_y)
					add a, 10
					ld l, a
					push hl
					call _NewShoot		;NewShoot(ship_x, ship_y+20)
					pop de
					pop de
					; HL now holds the received entity
      				        ld a, h
                                        or l
      				        jr z, shot_endcase	; If the new shoot is NULL, do not touch
					ld bc, 4
					add hl, bc
					ld (hl), MOVE_DOWNRIGHT
					inc hl
					inc hl
					ld (hl), 4
					inc hl
					ld (hl), 0
					jp shot_endcase
			.shot_noshoot
				xor a
				ld (_dummy_b), a

			.shot_endcase
			#endasm

/*

			switch(current_weapon)
			{
			 case 0: // Basic shot
				 dummy_i=NewShoot(ship_x+20, ship_y);
				 break;
			 case 1: // Double shot
				 if(AvailShoots() > 1)
				 {
				 	 NewShoot(ship_x+20, ship_y-4);
					 NewShoot(ship_x+20, ship_y+4);
				 }
				 break;
			 case 2: // Triple shot
				 if (AvailShoots() > 2)
				 {
				 	 NewShoot(ship_x+20, ship_y-12);
					 NewShoot(ship_x+20, ship_y);
					 NewShoot(ship_x+20, ship_y+12);
				 }
				 break;
			 case 3: // ray
				 if (AvailShoots() > 1)
				 {
					NewShoot(ship_x+20, ship_y);
					NewShoot(ship_x+36, ship_y);
				 }
				 break;
			 case 4: // homing missile
				 if (AvailShoots())
				 {
				  current_e=NewShoot(ship_x+20, ship_y);				 
				  current_e->movement = MOVE_HOMING;				
				 
				  // Find the first available active enemy. This must be FIXME in assembler
 				  for(i=0;i<MAX_ENEMIES;i++)
				  { 
					if(active_enemies[i].sprnum)
					{
					 current_e->param2 = i;
					 break;
					}
				  }
				 }
				 break;

			 case 5: // basic + bomb

				 if (AvailShoots() > 1)
				 {
					current_weapon_sprite=SHOT_BASIC;
					NewShoot(ship_x+20, ship_y);
					current_weapon_sprite=SHOT_BOMB;
					current_e = NewShoot(ship_x, ship_y+20);
					current_e->movement = MOVE_DOWNLEFT;
					current_e->param1 = 4;					
					current_e->param2 = 1;
				 }
				 break;

			 case 6: // Megashoot
				 if (AvailShoots() > 7)
				 {
					current_weapon_sprite=SHOT_BASIC;
					NewShoot(ship_x+20, ship_y);
					current_weapon_sprite=SHOT_MEGA;
					current_e = NewShoot(ship_x+20, ship_y-12); // up right
					current_e->movement = MOVE_UPRIGHT;
					current_e = NewShoot(ship_x+20, ship_y+12); // down right - not yet implemented FIXME
					current_e->movement = MOVE_DOWNRIGHT;
					current_e->param2 = 8;
					current_e = NewShoot(ship_x, ship_y-12); // up left
					current_e->movement = MOVE_UPLEFT;
					current_e = NewShoot(ship_x, ship_y+12); 
					current_e->movement = MOVE_DOWNLEFT;
					current_e->param2 = 8;
					current_e = NewShoot(ship_x-12, ship_y); //  left
					current_e->movement = MOVE_LEFT;
					current_e = NewShoot(ship_x+12, ship_y-12); //  up - not yet implemented FIXME
					current_e->movement = MOVE_UP;
					current_e = NewShoot(ship_x+12, ship_y+12); //  down
					current_e->movement = MOVE_DOWNLEFT;
				 	current_e->param2 = 2;
				 }

				 break;


			}*/

  			if (dummy_b)
				if (( joy & (JOY_UP | JOY_DOWN)) == 0) ship0spr=6;
			if (frames_fire_pressed < 5) frames_to_shoot=5;
				else frames_to_shoot=15;
		}
	  }
	  else	// Fire is not pressed. Check here if we are about to launch the BFB (big f*cking bomb)
	  {
		if ((frames_fire_pressed > 11) && (available_superbombs))
		{
		 Do_Superbomb();
		 available_superbombs--;
		 update_superb=2;
		 #asm
		 ld b, FX_BLAST
		 call LOAD_FX
		 #endasm
		}
		frames_fire_pressed=0;
	  }
  }
  // Update ship speed if needed

  ship_x += speed_x;
  if (ship_x > 208)
  {
	if(speed_x > 0) ship_x = 208;
		else ship_x=0;
  }
  ship_y += speed_y;
  if (ship_y > 112)
  {
	if(speed_y > 0) ship_y = 112;
		else ship_y=0;
  }

  // Move map to new position
  if((map_xpos < (CurLevel_XLength - 11)) || (map_displacement < 0x04))
  {

  	map_displacement++;
	if (map_displacement > 0xb)
	{
		map_displacement = 0;
		map_xpos++;
 	}
/*	if (current_level == 2)
	{
  	map_displacement++;
	if (map_displacement > 0xb)
	{
		map_displacement = 0;
		map_xpos++;
 	}
	}	*/
  }
  else if(!final_enemy_active)  // Reached the end of the level, time to activate the final enemy!!!!
  {
	activate_final_enemy();
  }

  // Move active enemies and shoots

  #asm
	ld a, (23388)
	and $07			; keep low bits
	push af			; save current state
	ld b, 6
	di
	call setrambank		; go to ram bank 1 to manage the sprites
	ei

	ld b, MAX_ENEMIES
	ld ix, _active_enemies
	ld de, _enemy_active_shoots
        ld iy, _my_active_shoots

.moveenemy_loop
	push bc
	ld a, (ix+2)
	and a
	jp z, loop_move_shoot

	ld hl, _movement_funcs
	ld c, (ix+4)
        ld b, 0                    ; BC ==     my_active_shoots[i].movement
        add hl, bc
        add hl, bc                 ; HL ==     &movement_funcs[my_active_shoots[i].movement]
        ld c, (hl)
        inc hl
        ld b, (hl)
        ld (call_moveenemy + 1), bc
        push ix
        pop hl                     ; HL == active_enemies[i]

        push ix
        push iy
        push de
.call_moveenemy
        call 0                       ; is there any way to do this without self-modifying code?? a kind of call (de) or so
        pop de

        pop iy
        pop ix


.loop_move_shoot
	ld a, (iy+2)
	and a
	jr z, loop_move_enemyshoot

	ld hl, _movement_funcs
	ld c, (iy+4)
        ld b, 0                    ; BC ==     my_active_shoots[i].movement
        add hl, bc
        add hl, bc                 ; HL ==     &movement_funcs[my_active_shoots[i].movement]
        ld c, (hl)
        inc hl
        ld b, (hl)
        ld (call_moveshoot + 1), bc
        push iy
        pop hl                     ; HL == my_active_shoots[i]

        push ix
        push iy
        push de
.call_moveshoot
        call 0                       ; is there any way to do this without self-modifying code?? a kind of call (de) or so
        pop de
        pop iy
        pop ix

.loop_move_enemyshoot
        push ix
        ld a, d
        ld ixh, a
        ld a, e
        ld ixl, a
	ld a, (ix+2)
	and a
	jr z, loop_move_continue

	ld hl, _movement_funcs
	ld a, (ix+4)
	ld c, a
        ld b, 0                    ; BC ==     enemy_active_shoots[i].movement
        add hl, bc
        add hl, bc                 ; HL ==     &movement_funcs[enemy_active_shoots[i].movement]
        ld c, (hl)
        inc hl
        ld b, (hl)
        ld (call_moveenemyshoot + 1), bc
        push ix
        pop hl                     ; HL == enemy_active_shoots[i]

        push iy
        push de
.call_moveenemyshoot
        call 0                       ; is there any way to do this without self-modifying code?? a kind of call (de) or so
        pop de
        pop iy




.loop_move_continue
        pop ix
	ld bc, 12		; 12 = sizeof(struct Entity)
	add ix, bc		; go to the next entity
	add iy, bc
	ld hl, 12
	add hl, de
	ex de, hl
	pop bc
	dec b
	jp nz, moveenemy_loop	; continue loop
#endasm

// Move enemies with follow_* movement types, only if current_level is 6
// Actually we are executing this twice, but it is not a big issue. This just
// ensures that, if the reference enemy is located later in the active_enemies
// table, continuity is done properly, as the one following must be processed later

#asm
	ld a, (_current_level)
	cp 6
	jp nz, nolevel7
	ld b, MAX_ENEMIES
	ld ix, _active_enemies

.moveenemyfollow_loop
	push bc
        push ix
	ld a, (ix+2)
	and a
	jp z, loop_movefollow_continue

	ld hl, _movement_funcs
	ld a, (ix+4)
	cp 22			;MOVE_FOLLOW_RIGHT
	jr z, do_follow_movement
	cp 23			;	MOVE_FOLLOW_DOWN
	jr z, do_follow_movement
	cp 24			; MOVE_FOLLOW_DOWNRIGHT
	jr nz, loop_movefollow_continue

.do_follow_movement
	ld c, (ix+4)
        ld b, 0                    ; BC ==     my_active_shoots[i].movement
        add hl, bc
        add hl, bc                 ; HL ==     &movement_funcs[my_active_shoots[i].movement]
        ld c, (hl)
        inc hl
        ld b, (hl)
        ld (call_moveenemyfollow + 1), bc

        pop hl                     ; HL == active_enemies[i]
        push ix
.call_moveenemyfollow
        call 0                       ; is there any way to do this without self-modifying code?? a kind of call (de) or so
.loop_movefollow_continue
        pop ix
	ld bc, 12		; 12 = sizeof(struct Entity)
	add ix, bc		; go to the next entity
	pop bc
	dec b
	jp nz, moveenemyfollow_loop	; continue loop
.nolevel7
#endasm


  // Move power up
 #asm
	ld de, _power_up

	inc de
	inc de
	ld a, (de)
	and a
	jp z, ignore_move_powerup

	ld hl, _movement_funcs
	inc de
	inc de
	ld a, (de)
	ld c, a
        ld b, 0                    ; BC ==     my_active_shoots[i].movement
        add hl, bc
        add hl, bc                 ; HL ==     &movement_funcs[power_up.movement]
        ld c, (hl)
        inc hl
        ld b, (hl)
        ld (call_powerup + 1), bc
	ld hl, _power_up

.call_powerup
        call 0                       ; is there any way to do this without self-modifying code?? a kind of call (de) or so
	; Now, check if the power up is colliding with the ship. If so, get power up!

	ld a, (_ship_x)
	rrca
	rrca
	rrca
	and $1f
	ld (_shoot_xchar), a 		   ; shoot_xchar = ship_x / 8
	ld a, (_ship_y)
	rrca
	rrca
	rrca
	and $1f
	ld (_shoot_ychar), a 		   ; shoot_ychar = ship_y / 8

	ld hl, _power_up
	ld a, (hl)
	rrca
	rrca
	rrca
	and $1f
	ld hl, _shoot_xchar
	sub (hl)
        ld d, a     ; D =    power_up->x / 8 -   shoot_xchar
	ld hl, _power_up+1
	ld a, (hl)
	rrca
	rrca
	rrca
	and $1f
	ld hl, _shoot_ychar
    	sub (hl)
        ld e, a     ; E =    power_up->y / 8 -   shoot_ychar

	; we want to check if abs(D) is -1, 0, 1, 2 or 3, and abs(E) is 0 or 1. 
	; So we add one to E, and if the value as unsigned is less than 3, we got it
	; and add one to D, and if the value as unsigned is less than 5, we got it
	inc d
	inc e
	ld a, 4
	cp d
	jr c, ignore_move_powerup
	ld a, 2
	cp e
	jr c, ignore_move_powerup

	; Not ignoring, so... We got the power up!!!!!!! 
	; Play power up sound

        ld hl, FX_POWERUP
        push hl
        call _wyz_effect
        pop hl

        ; Increase score
	ld hl, (_score)
        ld de, 20                       ; 200 more points
	add hl, de
	ld (_score), hl			; increment score and store
	ld a, 2
	ld (_update_score), a		; We need yo update the score twice

	ld hl, _power_up + 2
	ld (hl), 0
	ld bc, 5
	add hl, bc
	ld a, (hl)		; A holds the power up + 57			
	sub 57			; get the right one				
	and a
	jr z, powerup_more_superbombs
	dec a
	jr z, powerup_more_firepower

	ld (_current_weapon), a			; This is a normal powerup, starting at 59
	ld hl, _shoot_sprites
	ld c, a
	ld b, 0
	add hl, bc
	ld a, (hl)
	ld (_current_weapon_sprite), a 		; current_weapon_sprite = shoot_sprites[current_weapon];
	ld hl, _shoot_energy
	add hl, bc
	ld a, (hl)
	ld (_current_weapon_energy), a		;  current_weapon_energy = shoot_energy[current_weapon];
	ld hl, _shoot_max_number
	add hl, bc
	ld a, (hl)
	ld (_max_shoots), a			; max_shoots=shoot_max_number[current_weapon];
	jr ignore_move_powerup
.powerup_more_firepower
	ld a, (_max_shoots)
	rlca					; max_shoots * 2
	cp 9
	jr nc, ignore_move_powerup		; if the upgraded number of shoots is greater than 8, ignore
	ld (_max_shoots), a
	jr ignore_move_powerup
.powerup_more_superbombs
	ld a, (_available_superbombs)
	inc a
	ld (_available_superbombs), a
	ld a, 2
	ld (_update_superb), a
.ignore_move_powerup

	di
	pop af			; recover RAM bank state
	ld b, a
	call setrambank		; go back to normal state
	ei
#endasm



/*
  for(i=0;i<MAX_ENEMIES;i++)
  {
	if (active_enemies[i].sprnum)	// This enemy is active

	{
		(movement_funcs[active_enemies[i].movement])(&active_enemies[i]);  // Process enemy movement
	}

	if (my_active_shoots[i].sprnum)	// This shoot is active
	{
		(movement_funcs[my_active_shoots[i].movement])(&my_active_shoots[i]);  // Process shoot movement
	}

	if (enemy_active_shoots[i].sprnum)	// This enemy shoot is active
	{
		(movement_funcs[enemy_active_shoots[i].movement])(&enemy_active_shoots[i]);  // Process enemy shoot movement
	}
  }*/
  // Activate/deactivate new enemies

// while(enemy_locations[new_enemy].x < (map_xpos+11))


#asm
	di
	ld b, 4
	call setrambank		; go to ram bank 4 to manage the enemy locations
	ei
#endasm

 while (((int)((enemy_locations[new_enemy].x - map_xpos) * 24) - (map_displacement<<1) + enemy_locations[new_enemy].x_desp) < 256)
 {
  NewEnemy(&enemy_locations[new_enemy],0);
  new_enemy++;
 }

#asm
	di
	ld b, 0
	call setrambank		; go back to normal state
	ei
#endasm

  // Process shoots
  if (frames_to_shoot) frames_to_shoot--; // Less time to wait

#asm
	ld a, (_max_shoots)
	ld b, a
	ld ix, _my_active_shoots

.checkshoots_loop
	push bc
	ld a, (ix+2)
	and a
	jr z, continue_loop_shoots        ; This shoot is active, check collisions
	push ix
	call _shoot_hits_enemy            ; HL will hold the result,
	pop ix
        ld a, 255
        cp l
        jr z, continue_loop_shoots        ; if != 255, we have found a collision

        ld a, l
        inc a
        ld bc, 12
        ld hl, _active_enemies - 12
.loop_multiply_12
        add hl, bc
        dec a
        jr nz, loop_multiply_12           ; HL is now active_enemies[dummy_n]

	; we need to substract the current weapon energy from the enemy energy
		
	ld bc, 5
	add hl, bc			; energy
	ld a, (_current_weapon_energy)
	ld c, a				
	ld a, (hl)		
	and a				; If energy is 0, ignore (not killable or already dead)
	jr z, shot_completed
	sub c
	jr c, enemy_dead
	jr z, enemy_dead		; if the energy is 0 or < 0, it is dead
	ld (hl), a			; new energy
; Play enemy hit sound
        ld b, FX_DAMAGE              
        call LOAD_FX
	jr shot_completed
.enemy_dead
	ld (hl), 0			; enemy is dead
        inc hl                               ; param1
        ld (hl), 4
	dec hl
	dec hl					; movement
        ld (hl), MOVE_EXPLOSION
	dec hl
	ld a, (hl)			  ; type
	dec hl                             ; sprnum
        ld (hl), EXPLOSION_SPR
; Play enemy explosion sound
        ld b, FX_EXPLOSION
        call LOAD_FX
; increment score according to enemy killed
	ld hl, _enemy_score
	ld e, a
	ld d, 0
	add hl, de			; DE points to the score earned by killing this enemy
	ld e, (hl)
;	ld d, 0
	ld hl, (_score)
	add hl, de	
	ld (_score), hl			; increment score and store
	ld a, 2
	ld (_update_score), a		; We need yo update the score twice
	; check if we get an extra life
	ld de, (_next_extralife)
	sbc hl, de			; if there is no carry, we get an extra life
	jr c, shot_completed
.extralife
	ld a, (_life_counter)
	inc a
	ld (_life_counter), a
	ld a, 2
	ld (_update_life), a		; extra life, and update scoreboard
	ld hl, (_next_extralife)
	ld de, 200
	add hl, de
	ld (_next_extralife), hl	; update the score to get the next extra life
.shot_completed
        xor a
        ld (ix+2), a                        ;     my_active_shoots[i].sprnum = 0;

.continue_loop_shoots
	ld bc, 12		; 12 = sizeof(struct Entity)
	add ix, bc
	pop bc
        dec b
        jp nz, checkshoots_loop
;	djnz checkshoots_loop	; continue loop

#endasm

/*
  for(i=0;i<max_shoots;i++)
  {
	if (my_active_shoots[i].sprnum)	// This shoot is active
	{
	  dummy_b = shoot_hits_enemy(&(my_active_shoots[i]));
	  if(dummy_b != 255)	// This shoot has hit an enemy
  	  {
		active_enemies[dummy_b].sprnum=EXPLOSION_SPR;	// exploding!
		active_enemies[dummy_b].movement=MOVE_EXPLOSION;
                active_enemies[dummy_b].param1=4;               // 4 frames to explode
		my_active_shoots[i].sprnum = 0;
	  }
	}
  }
*/

  // Process enemy behavior, only if the final enemy is not active

#asm
	ld a, (23388)
	and $07			; keep low bits
	push af			; save current state	
	ld b, 6
	di
	call setrambank		; go to ram bank 6 to manage the sprites
	ei

	ld a, (_final_enemy_active)
	and a
	jp nz, finalenemy_behavior
	ld b, MAX_ENEMIES
	ld ix, _active_enemies

.behavenemy_loop
	push bc
	ld a, (ix+2)
	and a
	jr z, loop_behav_continue	; continue if sprnum = 0
	; skip explosion detection if this is a power up or an asteroid
	ld a, (ix+3)
	cp 8				; ENEMY_POWERUP
	jr z, behav_skip_expl
	cp 15
	jr z, behav_skip_expl		; ENEMY_ASTEROID
	ld a, (ix+4)
	cp MOVE_EXPLOSION
	jr z, loop_behav_continue	; and also if the enemy is exploding
.behav_skip_expl
	ld hl, _behavior_funcs
	ld c, (ix+10)
        ld b, 0                    ; BC ==     my_active_shoots[i].behavior
        add hl, bc
        add hl, bc                 ; HL ==     &behavior_funcs[my_active_enemies[i].behavior]
        ld c, (hl)
        inc hl
        ld b, (hl)
        ld (call_behav + 1), bc
        push ix
        pop hl                     ; HL == active_enemies[i]

        push ix
.call_behav
        call 0                       ; is there any way to do this without self-modifying code?? a kind of call (de) or so
        pop ix

.loop_behav_continue
	ld bc, 12		; 12 = sizeof(struct Entity)
	add ix, bc		; go to the next entity
	pop bc
	dec b
	jp nz, behavenemy_loop	; continue loop
	jp behav_complete

  // Process final enemy behavior, when it is active
.finalenemy_behavior
	ld a, (_current_level)
	add a, a
	ld c, a
	ld b, 0
	ld hl, _fenemy_behavior_funcs
	add hl, bc		; HL ==     &fenemy_behavior_funcs[current_level]
	ld c, (hl)
	inc hl
	ld b, (hl)
        ld (call_fenemy_behav + 1), bc
	ld hl, _active_enemies	; Pass the active_enemies array, so we have some variables to play with
.call_fenemy_behav
	call 0			 ; is there any way to do this without self-modifying code?? a kind of call (de) or so
	; check if we have beaten the final enemy
	ld a, (_fenemy_defeat)
	cp 10
	jr c, behav_complete
	cp 110
	jr nc, end_behav

	ld de, 1
	ld hl, (_score)
	add hl, de
	ld (_score), hl			; increment score and store
        ; Play effect
        ld hl, FX_SCORE
        push hl
        call _wyz_effect
        pop hl


	ld a, 2
	ld (_update_score), a		; We need yo update the score twice
	; check if we get an extra life
	ld de, (_next_extralife)
	sbc hl, de			; if there is no carry, we get an extra life
	jr c, behav_complete
.endlevel_extralife
	ld a, (_life_counter)
	inc a
	ld (_life_counter), a
	ld a, 2
	ld (_update_life), a		; extra life, and update scoreboard
	ld hl, (_next_extralife)
	ld de, 200
	add hl, de
	ld (_next_extralife), hl	; update the score to get the next extra life
.end_behav
	; we won!
	ld a, (_current_level)
	inc a
	ld (_current_level), a
	ld a, 7
	ld (_mayday), a		; end current level
	xor a
	ld (_respawn_xpos), a	; next level has to start at X=0
	
.behav_complete
	di
	pop af			; recover RAM bank state
	ld b, a
	call setrambank		; go back to normal state
	ei
#endasm

  if(fenemy_defeat == 1)
  {
 	wyz_stop_music();
        #asm
        ld b, FX_EXPLOSION
        call LOAD_FX
        #endasm
  }

  // Check collisions with enemies

  if(mayday==1)
  {
	ship0spr = MAINSHIP_EXPLOSION;
	life_counter--;
	mayday++;
	if (map_xpos >= level_checkpoints[(current_level <<2)+3]) respawn_xpos = level_checkpoints[(current_level <<2)+3];
	 else if (map_xpos >= level_checkpoints[(current_level <<2)+2]) respawn_xpos = level_checkpoints[(current_level <<2)+2];
	 else if (map_xpos >= level_checkpoints[(current_level <<2)+1]) respawn_xpos = level_checkpoints[(current_level <<2)+1];
	 else respawn_xpos = level_checkpoints[(current_level <<2)];
	wyz_stop_music();
        #asm
        ld b, FX_EXPLOSION
        call LOAD_FX
        #endasm
  }
  else if(mayday)
  {
	ship0spr+=2;
	mayday++;
  }
 }
}


void init_level(void)
{

 wyz_load_music(level_music[current_level]);
#asm
 ld a, (_sound_selection)
 ld (SOUND_SFX), a
#endasm
 load_level(current_level);
 // Load specific sprites for this level
 LoadSprBlock(22,levelsprites[current_level],28);	// The sprite block for each level will always be placed starting on sprite number 22, with a length of 28 sprites

 if(current_level == 6) LoadSprBlock(0,193,10);		// For the last level, load the GENESIS sprites

 #asm
	call InitSprCacheList	; initialize sprite cache list
 #endasm

 // init everything
 ship_x=0;
 map_xpos=respawn_xpos;
 map_displacement=0;
 ship_y=64;
 speed_x=speed_y=frames_to_shoot=frames_fire_pressed=0;
 available_superbombs=1;
 final_enemy_active=0;
 fenemy_defeat=0;
 fenemy_activation_counter=0;

 level_color=levelcolors[current_level];
 load_background();
 clean_screen();

 mayday=0; // not dying... yet
 current_weapon = 0;
 current_weapon_sprite = shoot_sprites[current_weapon];
 current_weapon_energy = shoot_energy[current_weapon];
 max_shoots=shoot_max_number[current_weapon];


 ship0spr=0;
 update_score=0;
 update_superb=0;
 update_life=0;
//for (i=0;i<MAX_ENEMIES;i++) active_enemies[i].sprnum=my_active_shoots[i].sprnum=enemy_active_shoots[i].sprnum=0;
#asm
	ld de, _active_enemies+1
 	ld hl, _active_enemies
	ld (hl), 0
	ld bc, 95
	ldir
	ld de, _my_active_shoots
 	ld hl, _active_enemies
	ld bc, 96
	ldir
	ld de, _enemy_active_shoots
 	ld hl, _active_enemies
	ld bc, 96
	ldir
#endasm

 power_up.sprnum=0;
 frameskip_counter=0;
 // Activate enemies
 new_enemy=0;

#asm
	di	
	ld b, 4
	call setrambank		; go to ram bank 4 to manage the enemy locations
#endasm

 while((map_xpos+10) > enemy_locations[new_enemy].x) new_enemy++;

#asm
	ld b, 0
	call setrambank		; go back to normal state
	ei
#endasm
}

#asm
.cleanattr
	ld hl, 16384+6144
	ld (hl), 0
	ld de, 16384+6145
	ld bc, 767
	ldir			; clean screen quickly
	ret
#endasm

unsigned char gameover(void)
{
#asm
	di
	
	call cleanattr

	ld b, 3			; Static images in RAM3
	call setrambank		; la ponemos en $c000 - $ffff	
	ld hl, GAMEOVER_SCR
	ld de, 16384
	call depack
	ld b, 0
	call setrambank		
	ei
#endasm
	// Print credits counter	
#asm
	ld b, 152
	ld c, 184
	ld a, (_credit_counter)
	call PrintNumber_4000
#endasm


	if(credit_counter)
	{
		// Print "Press C to Continue"
#asm
	ld hl, 23207
	ld b, 19
.say_pressc
	ld (hl), $43
	inc hl
	djnz say_pressc
#endasm
	}
	else
	{
		// Print "Press Fire"
#asm
	ld hl, 23179
	ld b, 10
.say_pressfire
	ld (hl), $43
	inc hl
	djnz say_pressfire
#endasm
	}


	wyz_load_music(MUSIC_GAMEOVER);

	if(credit_counter)
	{
	#asm
		ld a, 150			; will wait for 15 seconds
	.loop_wait_credit
		halt
		halt
		halt
		halt
		halt				; this is 1/10 of a second
		push af
		ld e, a
		ld d, 10
		call Div8		; A = number /10 (first digit), D = remainder (second digit)	
		ld b, 120
		ld c, 8
		call PrintLarge_2_4000
		ld BC, KEY_C
		call GET_KEY_STATE
		and a
		jr nz, end_check_continue
		ld a, (_credit_counter)
		dec a
		ld (_credit_counter), a
		call _wyz_stop_music
		; Play sound
		ld b, FX_START_GAME 
                call LOAD_FX
		ld b, 50
	.delay_continue
		halt
		djnz delay_continue
		pop af
		ld hl, 0
		ld (_score), hl
		ret				; return 0, meaning that we chose to continue
	.end_check_continue
		pop af
		dec a
		jr nz, loop_wait_credit
	#endasm		
	}
	else
	{
		while ((joy & JOY_FIRE)==0) 
		{

		}	// Wait until fire is pressed
		while (joy & JOY_FIRE) {}	// Wait until fire is released
	}
	wyz_stop_music();
	return 1;
}



void happyend(void)
{
 if (current_screen == 5)
 {
 	current_screen=7;
#asm
	di
	call switchscreen
	ei
#endasm
 }

#asm
	di
	
	call cleanattr

	ld b, 3			; Static images in RAM3
	call setrambank		; la ponemos en $c000 - $ffff	
	ld hl, GAME_END_SCR
	ld de, 16384
	call depack

	ld hl, GAME_END_ATTR
	ld de, $A000		; use $a000 as the decompression point, as we do not need the map here...
	call depack

	ld b, 0
	call setrambank		
	ei
#endasm

	if (inertia_cheat) wyz_load_music(MUSIC_HAPPYEND_CHEATER);
		else	wyz_load_music(MUSIC_HAPPYEND_OK);		

// Fade for the message
#asm
	ld c, 16			; loop Y 16 times
	ld hl, 16384+6144+128		; attribute for char X=0, Y=4
.fademessage_loopY
	push hl
	ld a, 5				; loop 5 colors
.fademessage_loopcolor
	ld b, 16			; loop X 16 times
	halt
	halt
	halt
	halt
	halt
	halt
	push af
	push hl
.fademessage_loopX
	ld a, (hl)
	cp 5
	jp z, fademessage_equal
	inc (hl)
.fademessage_equal
	inc hl
	djnz fademessage_loopX
	pop hl
	pop af
	dec a
	jp nz, fademessage_loopcolor

	pop hl
	ld de, 32
	add hl, de	
	dec c
	jp nz, fademessage_loopY
#endasm

// Fade for the egg
#asm
	ld a, 7				; loop 7 colors

.fadeegg_outerloop
	halt
	halt
	halt
	halt
	halt
	halt
	halt
	halt
	push af
	ld c, 15			; loop Y 15 times
	ld hl, 16384+6144+128+17	; attribute for char X=17, Y=4
	ld de, $a000+128+17		; attribute for char X=17, Y=4 in the buffer

.fadeegg_loopY
	ld b, 15			; loop X 15 times
	push hl
	push de
.fadeegg_loopX
	ld a, (de)
	inc de
	cp (hl)
	jp z, fadeegg_equal
	inc (hl)
.fadeegg_equal
	inc hl
	djnz fadeegg_loopX
	pop de
	pop hl
	push bc
	ld bc, 32
	add hl, bc
	ex de, hl
	add hl, bc
	ex de, hl			; add 32 to both counters
	pop bc
	dec c
	jp nz, fadeegg_loopY
	pop af
	dec a
	jp nz, fadeegg_outerloop
#endasm

// Wait for 2 seconds, then fade out message and fade in alien
#asm
	ld b, 100
.loopwait
	halt
	djnz loopwait

; Now fade out message, all together
	ld a, 5				; loop 5 colors

.fadeout_outerloop
	halt
	halt
	halt
	halt
	halt
	halt
	halt
	halt
	push af
	ld c, 16			; loop Y 16 times
	ld hl, 16384+6144+128		; attribute for char X=0, Y=4

.fadeout_loopY
	ld b, 16			; loop X 16 times
	push hl
.fadeout_loopX
	ld a, (hl)
	and a 
	jp z, fadeout_equal
	dec (hl)
.fadeout_equal
	inc hl
	djnz fadeout_loopX
	pop hl
	ld de, 32
	add hl, de			; add 32 to the counters
	dec c
	jp nz, fadeout_loopY
	pop af
	dec a
	jp nz, fadeout_outerloop

; Wait for 2 more seconds
	ld b, 100
.loopwait2
	halt
	djnz loopwait2

; Decompress and fade in alien
	di
	ld b, 3			; Static images in RAM3
	call setrambank		; la ponemos en $c000 - $ffff	
	ei

	ld hl, ALIEN_END_SCR
	ld de, $A000		; use $a000 as the decompression buffer, as we do not need the map here...
	call depack

	ld hl, ALIEN_END_ATTR
	ld de, $B800		; use $B800 as the decompression point, as we do not need the tiles here...
	call depack

	di
	ld b, 0
	call setrambank		
	ei

; Copy the alien

	ld a, 192
	ld de, 16384
	ld hl, $A000
.alien_copy_loop
	ld bc, 18
	ldir
	ld bc, 14
	add hl, bc
	ex de, hl
	add hl, bc
	ex de, hl
	dec a
	jr nz, alien_copy_loop

; Copy alien attributes

	ld a, 24
	ld de, 16384+6144
	ld hl, $B800
.alien_copy_attr_loop
	ld bc, 18
	ldir
	ld bc, 14
	add hl, bc
	ex de, hl
	add hl, bc
	ex de, hl
	dec a
	jr nz, alien_copy_attr_loop


#endasm



	if(inertia_cheat)
	{
#asm
	ld hl, 23186
	ld b, 12
.you_cheater1
	ld (hl), $47
	inc hl
	djnz you_cheater1
	ld hl, 23186+32
	ld b, 12
.you_cheater2
	ld (hl), $47
	inc hl
	djnz you_cheater2
	ld hl, 23186+64

	ld b, 12
.you_cheater3
	ld (hl), $47
	inc hl
	djnz you_cheater3
	ld hl, 23186+96
	ld b, 12
.you_cheater4
	ld (hl), $47
	inc hl
	djnz you_cheater4
#endasm		
	}



	while ((joy & JOY_FIRE)==0) {}	// Wait until fire is pressed
	while (joy & JOY_FIRE) {}	// Wait until fire is released

	wyz_stop_music();
}



unsigned char blink_startx[5]={14,6,12,15,19};
unsigned char blink_starty[5]={2,11,10,10,11};
unsigned char blink_width[5]={3,6,3,4,7};
unsigned char blink_height[5]={8,8,12,12,8};
unsigned char blink_color;

#asm
.PaintShipFull
	ld ix, _blink_startx
	ld iy, _blink_starty
	ld de, _blink_width
	exx
	ld de, _blink_height
	exx
.loop_changecolour
	push af


	ld hl, 16384+6144
	ld a, (iy+0)
	ld c, a
	ld b, 0
	xor a
	rl c
	rl b
	rl c
	rl b
	rl c
	rl b
	rl c
	rl b
	rl c
	rl b		; multiply Y * 32
	add hl, bc
	ld a, (ix+0)
	ld c, a
	ld b, 0
	add hl, bc	; HL = first position to change

	exx
	ld a, (de)
	exx
	ld b, a		; B= height

	push hl
.innerloopY_changecolour
	ld a, (de)
	ld c, a		; C = width
.innerloopX_changecolour
	ld a, (_blink_color)
	ld (hl), a	; as a test
	inc hl
	dec c
	jr nz, innerloopX_changecolour
	pop hl
	push bc
	ld bc, 32
	add hl, bc
	pop bc
	push hl
	djnz innerloopY_changecolour	
	pop hl	

	pop af
	inc ix
	inc iy
	inc de
	exx
	inc de
	exx
	dec a
	jp nz, loop_changecolour
	ret

.PaintShip
	ld ix, _blink_startx
	ld iy, _blink_starty
	ld de, _blink_width
	exx
	ld de, _blink_height
	exx
	dec a
	jr z, loop_changecolour2
.loop_increment
	inc ix
	inc iy
	inc de
	exx
	inc de
	exx
	dec a
	jr nz, loop_increment


.loop_changecolour2
	ld hl, 16384+6144
	ld a, (iy+0)
	ld c, a
	ld b, 0
	xor a
	rl c
	rl b
	rl c
	rl b
	rl c
	rl b
	rl c
	rl b
	rl c
	rl b		; multiply Y * 32
	add hl, bc
	ld a, (ix+0)
	ld c, a
	ld b, 0
	add hl, bc	; HL = first position to change
	
	exx
	ld a, (de)
	exx
	ld b, a		; B= height

	push hl
.innerloopY_changecolour2
	ld a, (de)
	ld c, a		; C = width
.innerloopX_changecolour2
	ld a, (_blink_color)
	ld (hl), a	; as a test
	inc hl
	dec c
	jr nz, innerloopX_changecolour2
	pop hl
	push bc
	ld bc, 32
	add hl, bc
	pop bc
	push hl
	djnz innerloopY_changecolour2
	pop hl	
	ret
#endasm



void show_genesis_pieces(void)
{
 	 if (current_screen == 5)
	 {
	 	current_screen=7;
#asm
		di
		call switchscreen
		ei
#endasm
	 }

#asm
	halt
	di

	call cleanattr

	ld b, 3			; Static images in RAM3
	call setrambank		; la ponemos en $c000 - $ffff
	ld hl, SHIP_PIECES_SCR
	ld de, 16384
	call depack
	ld b, 0
	call setrambank
	ei
#endasm
	
	
	// here we should do the changing colours effect!!
#asm
	; Play SFX 
	call ASSEMBLE_EFFECT

	; first, if previous_level == 5, we display the genesis assembled message
	ld a, (_previous_level)
	cp 5
	jr nz, startpaint

	ld hl, 22758
	ld b, 7
.say_genesis
	ld (hl), $C4
	inc hl
	djnz say_genesis
	ld hl, 22769
	ld b, 9
.say_assembled
	ld (hl), $C4
	inc hl
	djnz say_assembled

.startpaint
	ld a, 5
	ld (_blink_color), a
	ld a, (_previous_level)
	call PaintShipFull


	ld b, 5
.blinkloop
	push bc

	ld a, 1
	ld (_blink_color), a
	ld a, (_previous_level)

	call PaintShip
		
	ld b, 50
.sillyloop
	halt
	djnz sillyloop

	ld a, 5
	ld (_blink_color), a
	ld a, (_previous_level)
	call PaintShip

	ld b, 50
.sillyloop2
	halt
	djnz sillyloop2

	pop bc
	djnz blinkloop
#endasm
             wyz_stop_music();

//	while ((joy & JOY_FIRE)==0) {}	// Wait until fire is pressed
//	while (joy & JOY_FIRE) {}	// Wait until fire is released	


}


void main()
{


#asm
   ld a, r
   and $7f
   ld r,a	; clear the highest bit of the R register. It will be used to flag for a screen switch
   ld a, 0x8a
   ld hl, 0x8000
   ld de, _gameISR
   call SetIM2
#endasm

 current_screen = 7;
 score=0;
 credit_counter=3;		// 3 credits



 for(;;)
 {

	wyz_load_music(MUSIC_MAINMENU);
	#asm
		call cleanattr
		di
		ld b, 6
		call setrambank		; go to ram bank 6 for the menu
		ei
		ld de, (_score)		; get the score, to see if we have a high score!

		call MAINMENU
		

		ld a, (SELECTED_JOYSTICK)
		ld (_joystick_type),a    ; store selected joystick
		ld a, (INERTIA_CHEAT)
		ld (_inertia_cheat),a	 ; store cheat status
		ld a, (SOUND_SELECTION)
		ld (_sound_selection), a ; store sound and/or sfx selection
		ld hl, (HIGH_SCORE)

		ld (_hiscore), hl	; store high score

		di
		ld b, 0
		call setrambank		; go back to ram bank 0 for the menu
		ei
		call _wyz_stop_music
		call cleanattr
	#endasm

	#asm
		call GenStarfield	; generate starfield
	#endasm

	 previous_level=current_level=0;
	 respawn_xpos=0;
	 score=0;
	 next_extralife=200;		

	 // Load the common sprites, used everywhere
	 LoadSprBlock(0, 0, 22);
	 LoadSprBlock(50,50,14);

	 end_game=0;
	 while(!end_game)
	 {
		 life_counter=5;		// 5 lives to start with
		 while(life_counter != 255)
		 {
			if(current_level == 7)	// We won!!!
			{
				happyend();
				life_counter=255;
				end_game=1;
			}
			else
			{
				if(previous_level != current_level)
				 {
				  if ((current_level > 1) && (current_level < 7))    show_genesis_pieces();
				  previous_level=current_level;
				 }
			 	init_level();
				gameloop();
			#asm
				ld b, 25
			.waitloop_endgame
				halt
				djnz waitloop_endgame
			#endasm
				wyz_stop_music();

/*			// Very simple effect to clean the screen, left to right
			#asm
				di
				ld a, (_current_screen)
				xor 2
				ld b, a
				call setrambank		
				ei
				ld hl, 49152+6144	; start of atribute zone
				ld a, 32
			.outercleanloop
				halt
				ld b, 16
				ld de, 32
				push hl
			.innercleanloop
				ld (hl), 0
				add hl, de
				djnz innercleanloop
				pop hl
				inc hl
				dec a
				jr nz, outercleanloop
				di
				ld b, 0
				call setrambank		
				ei
			#endasm*/
			// Simple fade out effect
			#asm
				di
				ld a, (_current_screen)
				xor 2
				ld b, a
				call setrambank		
				ei
				ld c, 7
			.outercleanloop
				halt
				halt
				ld hl, 49152+6144	; start of atribute zone
				ld b,0
				ld e,2
			.innercleanloop1
				ld a, (hl)
				and $f8
				ld d, a
				ld a, (hl)
				and $7
				jr z, continueinner1
				dec a
				or d
				ld (hl), a
			.continueinner1
				inc hl
				djnz innercleanloop1
				dec e
				jr nz, innercleanloop1
				dec c
				jr nz, outercleanloop


				di
				ld b, 0
				call setrambank		
				ei
			#endasm

			}
		 }

		 if (current_screen == 5)
		 {
		 	current_screen=7;
		#asm
			di
			call switchscreen
			ei
		#endasm
		 }

		 // Display the game over screen, with its music and all stuff. 
		 if (!end_game)
		 {
		 end_game=gameover();
		#asm
			call cleanattr
		#endasm
		 }
	}
 }
}


// Here is the aplib depack routine, for now

#asm
; aPPack decompressor
; original source by dwedit
; very slightly adapted by utopian
; optimized by Metalbrain

;hl = source
;de = dest

.depack		ld	ixl,128
.apbranch1	ldi
.aploop0	ld	ixh,1		;LWM = 0
.aploop		call 	ap_getbit
		jr 	nc,apbranch1
		call 	ap_getbit
		jr 	nc,apbranch2
		ld 	b,0
		call 	ap_getbit
		jr 	nc,apbranch3
		ld	c,16		;get an offset
.apget4bits	call 	ap_getbit
		rl 	c
		jr	nc,apget4bits
		jr 	nz,apbranch4
		ld 	a,b
.apwritebyte	ld 	(de),a		;write a 0
		inc 	de
		jr	aploop0
.apbranch4	and	a
		ex 	de,hl 		;write a previous byte (1-15 away from dest)
		sbc 	hl,bc
		ld 	a,(hl)
		add	hl,bc
		ex 	de,hl
		jr	apwritebyte
.apbranch3	ld 	c,(hl)		;use 7 bit offset, length = 2 or 3
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
.apbranch2	call 	ap_getgamma	;use a gamma code * 256 for offset, another gamma code for length
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
.apskip2	ld 	hl,127
		sbc 	hl,de
		jr 	c,apskip3
		inc 	bc
		inc 	bc
.apskip3	pop 	hl		;bc = len, de = offs, hl=junk
		push 	hl
		or 	a
.ap_finishup	sbc 	hl,de
		pop 	de		;hl=dest-offs, bc=len, de = dest
.ap_finishup2	ldir
		pop 	hl
		ld	ixh,b
		jr 	aploop

.ap_r0_gamma	call 	ap_getgamma		;and a new gamma code for length
		push 	hl
		push 	de
		ex	de,hl

		ld	d,iyh
		ld	e,iyl
		jr 	ap_finishup


.ap_getbit	ld	a,ixl
		add	a,a
		ld	ixl,a
		ret	nz
		ld	a,(hl)
		inc	hl
		rla
		ld	ixl,a
		ret

.ap_getgamma	ld 	bc,1
.ap_getgammaloop call 	ap_getbit
		rl 	c
		rl 	b
		call 	ap_getbit
		jr 	c,ap_getgammaloop
		ret

#endasm
