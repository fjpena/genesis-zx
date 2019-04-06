#ifndef CONSTANTS_H
#define CONSTANTS_H

#include "sprdefs.h"

#define MAX_ENEMIES 8
unsigned int level_address[]={49152,0xC65E,0xCD82,0xD31C,0xDADC,0xE3A3,0xEC43};	// Level addresses
unsigned int enemy_address[]={0xC31D,0xCB29,0xCF7B,0xD7A3,0xDFEA,0xE8A2,0xF04B};        // Enemy addresses
unsigned int finalenemy_address[]={0xF3CC,0xF3ED,0xF426,0xF447,0xF478,0xF499,0xF4BA};  // Addresses of final enemy definitions

// Enemy and specific sprites for each level
unsigned int levelsprites[]={69,22,100,22,131,129,203};
// Final enemy sprites
unsigned int finalspr[]={76,93,113,64,159,175,216};
// Final enemy sprite count
unsigned int finalspr_count[]={17,7,16,5,16,18,16};	
// Colors for each level
unsigned char levelcolors[]={5,6,7,0x44,0x46,0x45,5};
// Level checkpoints (up to 4 per level, first is the beginning)
unsigned char level_checkpoints[]={ 0, 60, 120, 180,		// Level 1
				    0, 40, 84, 145,		// Level 2
				    0, 40, 83, 135,		// Level 3
				    0, 30, 78, 126,		// Level 4
				    0, 53, 100, 153,		// Level 5
				    0, 50, 100, 170,		// Level 6
				    0, 60, 140, 230};		// Level 7

// The enemy_sprites and behavior_types arrays have one entry per enemy type...

// Enemy sprites
unsigned char enemy_sprites[]={	ENEMY_EYE,
				ENEMY_CANNON_DOWN_LEFT,
				ENEMY_CANNON_DOWN_RIGHT,
				ENEMY_CANNON_UP,
				ENEMY_1,
				ENEMY_KAMIKAZE,
				ENEMY_FURBY,
				ENEMY_FINAL4_EYE,
				ENEMY_POWERUP,
				ENEMY_SHIP1,
				ENEMY_SHIP2,
				ENEMY_SHIP3,
				ENEMY_SHIP4,
				ENEMY_TURRET,
				ENEMY_FINAL1_SHOOT,
				ENEMY_ASTEROID,
				ENEMY_TRASH1,
				ENEMY_TRASH2,
				ENEMY_TRASH3,
				ENEMY_TRASH4,
				ENEMY_TRASH5,
				ENEMY_SALTARIN,
				ENEMY_CASCO,
				ENEMY_EGG,
				ENEMY_UGLYGUY,
				ENEMY_1_BIS,
				ENEMY_SHIP2_BIS,
				ENEMY_SHIP3_BIS,
				ENEMY_SALTARIN_BIS,
				ENEMY_CASCO_BIS,
				ENEMY_EGG_BIS,
				ENEMY_UGLYGUY_BIS,
				ENEMY_SHIP1_LEVEL7,
				ENEMY_SHIP2_LEVEL7,
				ENEMY_SHIP3_LEVEL7,
				ENEMY_SHIP4_LEVEL7,
				ENEMY_TURRET_LEVEL7,
				ENEMY_PACOSHIP_1,
				ENEMY_PACOSHIP_2,
				ENEMY_FINAL1_LEVEL7_UL,
				ENEMY_FINAL1_LEVEL7_UR,
				ENEMY_FINAL1_LEVEL7_DL,
				ENEMY_FINAL1_LEVEL7_DR		};

unsigned char enemy_score[]={	1,2,2,3,2,4,3,10,2,1,
				1,2,3,1,1,1,1,1,1,1,
				1,3,3,4,5,3,2,2,3,3,
				4,5,2,2,3,3,4,6,5,10,
				10,10,10};

// Final enemy sprites and behaviours
unsigned char finalenemy_sprites[]={FINALE_SPR1,FINALE_SPR2,FINALE_SPR3,FINALE_SPR4,FINALE_SPR5,FINALE_SPR6,FINALE_SPR7};

// Shoot sprites
unsigned char shoot_sprites[]={SHOT_BASIC,SHOT_BASIC,SHOT_LASER,SHOT_HOMING,SHOT_BOMB,SHOT_MEGA};
unsigned char shoot_energy[]={1,1,3,3,2,4};
unsigned char shoot_max_number[]={2,4,2,2,4,8};
unsigned char shoot_speed[]={8,8,6,6,8,4};

// Behaviors are directly associated to the enemy sprite with the same index

#define BEHAV_DO_NOTHING 0
#define BEHAV_SHOOT_LEFT 1
#define BEHAV_SHOOT_TARGET 2
#define BEHAV_SHOOT_TARGET_LEFT 3
#define BEHAV_SHOOT_TARGET_RIGHT 4
#define BEHAV_POWERUP 	5
#define BEHAV_TURRET 	6
#define BEHAV_SHOOT_LEFT_Y 7
#define BEHAV_SHOOT_LEFT_WAIT 8
#define BEHAV_ASTEROID 	9
#define BEHAV_SALTARIN 	10
#define BEHAV_CASCO 	11
#define BEHAV_EGG	12
#define BEHAV_UGLYGUY	13
#define BEHAV_SALTARIN_BIS 	14
#define BEHAV_CASCO_BIS 	15
#define BEHAV_FOLLOW	16
#define BEHAV_FINAL1_L7	17

unsigned char behavior_types[]=	{BEHAV_DO_NOTHING,		//ENEMY_EYE
				BEHAV_SHOOT_TARGET_LEFT,	//ENEMY_CANNON_DOWN_LEFT
				BEHAV_SHOOT_TARGET_RIGHT,	//ENEMY_CANNON_DOWN_RIGHT
				BEHAV_SHOOT_TARGET,		//ENEMY_CANNON_UP
				BEHAV_SHOOT_LEFT,		//ENEMY_1
				BEHAV_DO_NOTHING,		//ENEMY_KAMIKAZE
				BEHAV_DO_NOTHING,		//ENEMY_FURBY
				BEHAV_DO_NOTHING,		//ENEMY_FINAL4_EYE
				BEHAV_POWERUP,			//ENEMY_POWERUP
				BEHAV_DO_NOTHING,		//ENEMY_SHIP1
				BEHAV_SHOOT_LEFT_Y,		//ENEMY_SHIP2
				BEHAV_DO_NOTHING,		//ENEMY_SHIP3
				BEHAV_SHOOT_LEFT_WAIT,		//ENEMY_SHIP4
				BEHAV_TURRET,			//ENEMY_TURRET
				BEHAV_DO_NOTHING,		//ENEMY_FINAL1_SHOOT
				BEHAV_ASTEROID,			//ENEMY_ASTEROID
				BEHAV_DO_NOTHING,		//ENEMY_TRASH1
				BEHAV_DO_NOTHING,		//ENEMY_TRASH2
				BEHAV_DO_NOTHING,		//ENEMY_TRASH3
				BEHAV_DO_NOTHING,		//ENEMY_TRASH4
				BEHAV_DO_NOTHING,		//ENEMY_TRASH5
				BEHAV_SALTARIN,			//ENEMY_SALTARIN
				BEHAV_CASCO,			//ENEMY_CASCO
				BEHAV_EGG,			//ENEMY_EGG
				BEHAV_UGLYGUY,			//ENEMY_UGLYGUY
				BEHAV_SHOOT_LEFT_WAIT,		//ENEMY_1_BIS
				BEHAV_SHOOT_LEFT_Y,		//ENEMY_SHIP2_BIS
				BEHAV_DO_NOTHING,		//ENEMY_SHIP3_BIS
				BEHAV_SALTARIN_BIS,		//ENEMY_SALTARIN_BIS
				BEHAV_CASCO_BIS,		//ENEMY_CASCO_BIS
				BEHAV_EGG,			//ENEMY_EGG_BIS
				BEHAV_UGLYGUY,			//ENEMY_UGLYGUY_BIS
				BEHAV_DO_NOTHING,		//ENEMY_SHIP1_LEVEL7
				BEHAV_SHOOT_LEFT_Y,		//ENEMY_SHIP2_LEVEL7
				BEHAV_DO_NOTHING,		//ENEMY_SHIP3_LEVEL7
				BEHAV_SHOOT_LEFT_WAIT,		//ENEMY_SHIP4_LEVEL7
				BEHAV_TURRET,			//ENEMY_TURRET_LEVEL7
				BEHAV_DO_NOTHING,		//ENEMY_PACOSHIP_1
				BEHAV_FOLLOW,			//ENEMY_PACOSHIP_2
				BEHAV_FINAL1_L7,		//ENEMY_FINAL1_LEVEL7_UL
				BEHAV_FOLLOW,			//ENEMY_FINAL1_LEVEL7_UR
				BEHAV_FOLLOW,			//ENEMY_FINAL1_LEVEL7_DL
				BEHAV_FOLLOW,			//ENEMY_FINAL1_LEVEL7_DR
				};


// Music themes for each level and final enemy
unsigned char level_music[]={4,5,6,7,8,9,10};
unsigned char finalenemy_music[]={11,12,13,14,11,12,13};
// Effects channel for each music, A:1, B:2, C:3
//unsigned char fxchannel_music[]={3,1,1,1,3,2,2,2,3,3,3,3,2,2,2};
unsigned char fxchannel_music[]=  {1,1,1,1,1,2,1,1,1,3,1,1,1,1,1};

#define MUSIC_MAINMENU		0
#define MUSIC_GAMEOVER  	1
#define MUSIC_HAPPYEND_OK 	2
#define MUSIC_HAPPYEND_CHEATER  3

// WYZ player addresses

#define CARGA_CANCION 	0xC000		// LOAD_SONG in the wyzplayer sym file
#define WYZ_PLAY	0xC009		// It is INICIO in the wyzplayer sym file
#define STOP_PLAYER	0xC116		// PLAYER_OFF

#define LOAD_FX         0xE073          // INICIA_EFECTO
#define PLAY_FX         0xE083          // REPRODUCE_EFECTO
#define STOP_FX         0xE0D7          // FIN_EFECTO
#define FX_CHANNEL      0xE072          // CANAL_EFECTOS
#define ASSEMBLE_EFFECT 0xE0DD		// ASSEMBLE_EFFECT

#define SOUND_SFX	0xE44C

// FX definitions

#define FX_EXPLOSION    0               // Ship explosion                         (ok ship, ok enemies)
#define FX_BLAST        1               // Secondary shoot (blast)                (ok)
#define FX_HIT_CAPSULE  2               // Hit power-up capsule                   (ok)
#define FX_DAMAGE       3               // Hit enemy, but not kill it             (ok)
#define FX_DOUBLE_SHOOT 4               // Double shoot                           (ok)
#define FX_MENU_INERTIA 5               // Change inertia on main menu            (ok)
#define FX_POWERUP      6               // Get power up                           (ok)
#define FX_SINGLE_SHOOT 7               // Simple shoot                           (ok)
#define FX_TRIPLE_SHOOT 8               // Triple shoot                           (ok)
#define FX_START_GAME   9               // Start game                              (ok)
#define FX_ORGANIC_EXPL 10              // Explosion of organic creatures
#define FX_LASER	11		// Laser
#define FX_SCORE	12		// Increment score
#define FX_DISPARO_MULTI	13	// Multidirectional shoot
#define FX_DISPARO_HOMMING	14	// Homing missile


// Menu addresses

#define MAINMENU	49152
#define SELECTED_JOYSTICK 0xC393
#define INERTIA_CHEAT	0xC394
#define HIGH_SCORE	0xC187
#define SOUND_SELECTION 0xCB77

// Screen addresses

#define GAMEOVER_SCR	0xC2FB
#define SHIP_PIECES_SCR 0xCE28
#define GAME_END_SCR	0xDB50
#define GAME_END_ATTR	0xE2D2
#define ALIEN_END_SCR	0xE35B
#define ALIEN_END_ATTR	0xEC63


unsigned char number_font[80] = {
  0,127, 65, 73, 65,127,  0,  0,
  0, 24,  8,  8,  8,  8,  0,  0,
  0,127,  1,127, 64,127,  0,  0,
  0,127,  1,127,  1,127,  0,  0,
  0, 65, 65,127,  1,  1,  0,  0,
  0,127, 64,127,  1,127,  0,  0,
  0,127, 64,127, 65,127,  0,  0,
  0,127,  1,  1,  1,  1,  0,  0,
  0,127, 65,127, 65,127,  0,  0,
  0,127, 65,127,  1,127,  0,  0};

#endif
