#ifndef ENGINE_H
#define ENGINE_H

// Function definitions...is there a way to automate this?

#asm
DEFC get_joystick = $86CB
DEFC Ship_Collision = $8615
DEFC CreaTablaTiles = $879F
DEFC setrambank = $82ee
DEFC DrawMap = $8101
DEFC drawsprite = $830B
DEFC drawsprite_ship = $8400
DEFC switchscreen = $82fd
DEFC InitSprCacheList = $85B5
DEFC SetIM2 = $887A
DEFC Display_starfield = $899E
DEFC GenStarfield = $8985
DEFC MoveStarfield = $8964
DEFC GET_KEY_STATE = $86C4
#endasm

// Joystick definitions

#define JOY_KEMPSTON	0
#define JOY_SINCLAIR1	1
#define JOY_SINCLAIR2	2
#define JOY_KEYS	3

#define JOY_UP		0x1
#define JOY_DOWN	0x2
#define JOY_LEFT	0x4
#define JOY_RIGHT	0x8
#define JOY_FIRE	0x10

// Key definitions

#define KEY_CAPS 	0xfe01
#define KEY_Z 		0xfe02 	
#define KEY_X 		0xfe04
#define KEY_C 		0xfe08
#define KEY_V 		0xfe10
#define KEY_A 		0xfd01
#define KEY_S 		0xfd02
#define KEY_D 		0xfd04
#define KEY_F 		0xfd08
#define KEY_G 		0xfd10
#define KEY_Q 		0xfb01
#define KEY_W 		0xfb02
#define KEY_E 		0xfb04
#define KEY_R 		0xfb08
#define KEY_T 		0xfb10
#define KEY_1 		0xf701
#define KEY_2 		0xf702
#define KEY_3 		0xf704
#define KEY_4 		0xf708
#define KEY_5 		0xf710
#define KEY_0 		0xef01
#define KEY_9 		0xef02
#define KEY_8 		0xef04
#define KEY_7 		0xef08
#define KEY_6 		0xef10
#define KEY_P 		0xdf01
#define KEY_O 		0xdf02
#define KEY_I 		0xdf04
#define KEY_U 		0xdf08
#define KEY_Y 		0xdf10
#define KEY_ENTER 	0xbf01
#define KEY_L 		0xbf02
#define KEY_K 		0xbf04
#define KEY_J 		0xbf08
#define KEY_H 		0xbf10
#define KEY_SPACE 	0x7f01
#define KEY_SS 		0x7f02
#define KEY_M 		0x7f04
#define KEY_N 		0x7f08
#define KEY_B 		0x7f10

#endif
