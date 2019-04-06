
extern unsigned char enemy_sprites[];
extern struct Entity active_enemies[];
extern unsigned char ship_y;


// Definition for the movement functions
#define move_downleft	0xE2B1
#define move_downright	0xE2CC
#define move_explosion	0xE0AB
#define move_find_enemy_to_follow 0xE480
#define move_follow_down 0xE4E1
#define move_follow_downright 0xE506
#define move_follow_right 0xE4BC
#define move_furby	0xE0C1
#define move_homing	0xE1A6
#define move_kamikaze	0xE152
#define move_kamikaze_2	0xE43D
#define move_kamikaze_noanim 0xE37B
#define move_left	0xE000
#define move_left_accel	0xE52D
#define move_left_expire 0xE014
#define move_leftanim	0xE218
#define move_missile	0xE3D4
#define move_none	0xE560
#define move_pingpong	0xE043
#define move_pingpong_final4 0xE077
#define move_right	0xE02C
#define move_right_accel 0xE545
#define move_target	0xE2FB
#define move_up		0xE2EA
#define move_upleft	0xE278
#define move_upright	0xE290
#define move_wave	0xE1E4
#define move_wave_nogoneleft 0xE3A8

void __FASTCALL__ *movement_funcs[]={move_none,move_left,move_right,move_pingpong,move_explosion,move_furby,move_kamikaze,move_wave,move_leftanim,move_upleft,move_upright,
				    move_downleft, move_left_expire,move_pingpong_final4,move_homing,move_downright,move_up,move_target,move_kamikaze_noanim,
				    move_wave_nogoneleft,move_missile,move_kamikaze_2,move_follow_right,move_follow_down,move_follow_downright,move_left_accel,move_right_accel};



