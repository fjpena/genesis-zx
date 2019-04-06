org 49152

level1:
INCBIN	"level1.map"
enemies1:               ; the enemy structure has just one byte detailing the number of enemies
                        ; and the locations in struct Enemy format
INCLUDE "level1_enemies.asm"


level2:
INCBIN "level2.map"
enemies2:               ; the enemy structure has just one byte detailing the number of enemies
                        ; and the locations in struct Enemy format
INCLUDE "level2_enemies.asm"

level3:
INCBIN "level3.map"
enemies3:               ; the enemy structure has just one byte detailing the number of enemies
                        ; and the locations in struct Enemy format
INCLUDE "level3_enemies.asm"


level4:
INCBIN "level4.map"
enemies4:               ; the enemy structure has just one byte detailing the number of enemies
INCLUDE "level4_enemies.asm"

level5:
INCBIN "level5.map"
enemies5:           
INCLUDE "level5_enemies.asm"

level6:
INCBIN "level6.map"
enemies6:               ; the enemy structure has just one byte detailing the number of enemies
INCLUDE "level6_enemies.asm"

level7:
INCBIN "level7.map"
enemies7:               ; the enemy structure has just one byte detailing the number of enemies
                        ; and the locations in struct Enemy format
INCLUDE "level7_enemies.asm"


; The final enemy structure is almost the same as the enemy one... The initial byte marks the number of sprites in the enemy
; The movement parameters for the first sprite specify the movement of the whole enemy
finalenemy1:
	db 4
	db 120, 0, 48,  FINAL1_UL,    MOVE_WAVE,     26, 0, 16 	; Energy = 20, move ping-pong 2 pixels at a time
	db 136, 0, 48,  FINAL1_UR,    MOVE_NONE,     0, 0, 2  	; 
	db 120, 0, 64,  FINAL1_DL,    MOVE_NONE,     0, 0, 2  	; 
	db 136, 0, 64,  FINAL1_DR,    MOVE_NONE,     0, 0, 2 	; 
finalenemy2:
	db 7
	db 128, 0, 48,  FINAL2_UL,    MOVE_WAVE_NOGONELEFT,     35, 2, 16 	; Energy = 20, move ping-pong 2 pixels at a time
	db 144, 0, 48,  FINAL2_UR,    MOVE_NONE,     0, 0, 2  	; 
	db 128, 0, 64,  FINAL2_DL,    MOVE_NONE,     0, 0, 2  	; 
	db 144, 0, 64,  FINAL2_DR,    MOVE_NONE,     0, 0, 2 	; 
	db 118, 0, 42,	FINAL2_SHIELD1, MOVE_NONE,   0, 0, 2
	db 118, 0, 58,	FINAL2_SHIELD2, MOVE_NONE,   0, 0, 2
	db 118, 0, 74,	FINAL2_SHIELD3, MOVE_NONE,   0, 0, 2
finalenemy3:
	db 4
	db 170, 0, 48,  FINAL3_UL,    MOVE_KAMIKAZE_NOANIM,   25, 0, 0 	; Energy = 20, move ping-pong 2 pixels at a time
	db 186, 0, 48,  FINAL3_UR,    MOVE_NONE,     0, 0, 2  	; 
	db 170, 0, 64,  FINAL3_DL,    MOVE_NONE,     0, 0, 2  	; 
	db 186, 0, 64,  FINAL3_DR,    MOVE_NONE,     0, 0, 2 	; 
finalenemy4:
	db 6
	db 132, 0, 48,  FINAL4_UP,    MOVE_PINGPONG_FINAL4,     24, 0, 4 	; Energy = 20, move ping-pong 2 pixels at a time
	db 164, 0, 48,  FINAL4_UP,    MOVE_NONE,     24, 0, 2  	; 
	db 132, 0, 64,  FINAL4_DL,    MOVE_NONE,     0, 0, 2  	; 
	db 148, 0, 64,  FINAL4_DM,    MOVE_NONE,     0, 0, 2 	; 
	db 164, 0, 64,  FINAL4_DM,    MOVE_NONE,     0, 0, 2  	; 
	db 180, 0, 64,  FINAL4_DR,    MOVE_NONE,     0, 0, 2 	; 
finalenemy5:
	db 4
	db 165, 0, 28,  FINAL5_UL,    MOVE_NONE,   35, 0, 0
	db 181, 0, 28,  FINAL5_UR,    MOVE_NONE,     0, 0, 2  	; 
	db 165, 0, 44,  FINAL5_DL,    MOVE_NONE,     0, 0, 2  	; 
	db 181, 0, 44,  FINAL5_DR,    MOVE_NONE,     0, 0, 2 	; 
finalenemy6:
	db 4
	db 165-32, 0, 28,  FINAL6_UL,    MOVE_NONE,   50, 0, 0
	db 181-32, 0, 28,  FINAL6_UR,    MOVE_NONE,     0, 0, 2  	; 
	db 165-32, 0, 44,  FINAL6_DL,    MOVE_NONE,     0, 0, 2  	; 
	db 181-32, 0, 44,  FINAL6_DR,    MOVE_NONE,     0, 0, 2 	; 
finalenemy7:
	db 4
	db 180, 0, 54,  FINAL7_UL,    MOVE_NONE,    50, 0, 0
	db 196, 0, 54,  FINAL7_UR,    MOVE_NONE,     0, 0, 2  	; 
	db 180, 0, 70,  FINAL7_DL,    MOVE_NONE,     0, 0, 2  	; 
	db 196, 0, 70,  FINAL7_DR,    MOVE_NONE,     0, 0, 2 	; 
; Movement definitions (copy from movement.h)

MOVE_NONE EQU 0
MOVE_LEFT EQU 1
MOVE_RIGHT EQU  2
MOVE_PINGPONG EQU 3
MOVE_EXPLOSION EQU 4
MOVE_FURBY  EQU 5		
MOVE_KAMIKAZE EQU 6
MOVE_WAVE EQU 7
MOVE_LEFTANIM EQU 8
MOVE_UPLEFT EQU 9
MOVE_UPRIGHT EQU 10
MOVE_DOWNLEFT EQU 11
MOVE_LEFT_EXPIRE EQU 12
MOVE_PINGPONG_FINAL4 EQU 13
MOVE_HOMING EQU 14
MOVE_DOWNRIGHT EQU 15
MOVE_UP EQU 16
MOVE_TARGET EQU 17
MOVE_KAMIKAZE_NOANIM EQU 18
MOVE_WAVE_NOGONELEFT  EQU 19
MOVE_MISSILE	EQU	20
MOVE_KAMIKAZE_2 EQU	21
MOVE_FOLLOW_RIGHT EQU 22
MOVE_FOLLOW_DOWN EQU 23
MOVE_FOLLOW_DOWNRIGHT EQU 24
MOVE_LEFT_ACCEL	EQU 25
MOVE_RIGHT_ACCEL	EQU 26

; Enemy definitions. Each label is the sprite number where they will be loaded

ENEMY_EYE		EQU 0
ENEMY_CANNON_DOWN_LEFT	EQU 1
ENEMY_CANNON_DOWN_RIGHT EQU 2
ENEMY_CANNON_UP		EQU 3
ENEMY_1			EQU 4
ENEMY_KAMIKAZE		EQU 5
ENEMY_FURBY		EQU 6
ENEMY_FINAL4_EYE	EQU 7
ENEMY_POWERUP		EQU 8

ENEMY_SHIP1		EQU 9
ENEMY_SHIP2		EQU 10
ENEMY_SHIP3		EQU 11
ENEMY_SHIP4		EQU 12
ENEMY_TURRET		EQU 13

ENEMY_FINAL1_SHOOT	EQU 14

ENEMY_ASTEROID		EQU 15
ENEMY_TRASH1		EQU 16
ENEMY_TRASH2		EQU 17
ENEMY_TRASH3		EQU 18
ENEMY_TRASH4		EQU 19
ENEMY_TRASH5		EQU 20

ENEMY_SALTARIN		EQU 21
ENEMY_CASCO		EQU 22
ENEMY_EGG		EQU 23
ENEMY_UGLYGUY		EQU 24
ENEMY_1_BIS		EQU 25

ENEMY_SHIP2_BIS		EQU 26
ENEMY_SHIP3_BIS		EQU 27
ENEMY_SALTARIN_BIS	EQU 28
ENEMY_CASCO_BIS		EQU 29
ENEMY_EGG_BIS		EQU 30
ENEMY_UGLYGUY_BIS	EQU 31

ENEMY_SHIP1_LEVEL7	EQU	32
ENEMY_SHIP2_LEVEL7	EQU	33
ENEMY_SHIP3_LEVEL7	EQU	34
ENEMY_SHIP4_LEVEL7	EQU	35
ENEMY_TURRET_LEVEL7	EQU	36
ENEMY_PACOSHIP_1	EQU	37
ENEMY_PACOSHIP_2	EQU	38
ENEMY_FINAL1_LEVEL7_UL	EQU	39
ENEMY_FINAL1_LEVEL7_UR	EQU	40
ENEMY_FINAL1_LEVEL7_DL	EQU	41
ENEMY_FINAL1_LEVEL7_DR	EQU	42

; Final enemy definitions
FINAL1_UL	EQU 0
FINAL1_UR	EQU 1
FINAL1_DL	EQU 2
FINAL1_DR	EQU 3

FINAL2_UL	EQU 0
FINAL2_UR	EQU 1
FINAL2_DL	EQU 2
FINAL2_DR	EQU 3
FINAL2_SHIELD1	EQU 4
FINAL2_SHIELD2	EQU 5
FINAL2_SHIELD3	EQU 6

FINAL3_UL	EQU 0
FINAL3_UR	EQU 1
FINAL3_DL	EQU 2
FINAL3_DR	EQU 3

FINAL4_UP	EQU 0
FINAL4_DL	EQU 1
FINAL4_DM	EQU 2
FINAL4_DR	EQU 3

FINAL5_UL	EQU 0
FINAL5_UR	EQU 1
FINAL5_DL	EQU 2
FINAL5_DR	EQU 3

FINAL6_UL	EQU 0
FINAL6_UR	EQU 1
FINAL6_DL	EQU 2
FINAL6_DR	EQU 3
FINAL7_UL	EQU 0
FINAL7_UR	EQU 1
FINAL7_DL	EQU 2
FINAL7_DR	EQU 3
