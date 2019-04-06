#include "structs.h"
#include "movement.h"
#include "sprdefs.h"


extern struct Entity *NewEnemyShoot(unsigned char, unsigned char, unsigned char, unsigned char);
extern struct Entity *NewEnemy(struct Enemy *, unsigned char);
extern unsigned char fenemy_defeat;
extern struct Entity power_up;
extern unsigned char ship_x;
extern unsigned char ship_y;

#define behav_asteroid	0xF2AB
#define behav_casco	0xF80A
#define behav_casco_bis	0xF859
#define behav_egg	0xF8A8
#define behav_fenemy1	0xF2C0
#define behav_fenemy2	0xF46B
#define behav_fenemy3	0xF643
#define behav_fenemy4	0xF371
#define behav_fenemy5	0xF8D5
#define behav_fenemy6	0xF9E7
#define behav_fenemy7	0xFC50
#define behav_final1_l7	0xFC0B
#define behav_follow	0xFBDB
#define behav_none	0xF077
#define behav_powerup	0xF1EA
#define behav_saltarin	0xF754
#define behav_saltarin_bis 0xF7AF
#define behav_shoot_left 0xF078
#define behav_shoot_left_Y 0xF09F
#define behav_shoot_left_wait 0xF0DA
#define behav_shoot_target 0xF196
#define behav_shoot_target_left 0xF102
#define behav_shoot_target_right 0xF14C
#define behav_turret	0xF21C
#define behav_uglyguy	0xF8C0




void __FASTCALL__ *behavior_funcs[]={	behav_none,
					behav_shoot_left,
					behav_shoot_target,
					behav_shoot_target_left,
					behav_shoot_target_right,
					behav_powerup,
					behav_turret,
					behav_shoot_left_Y,
					behav_shoot_left_wait,
					behav_asteroid,
					behav_saltarin,
					behav_casco,
					behav_egg,
					behav_uglyguy,
					behav_saltarin_bis,
					behav_casco_bis,
					behav_follow,
					behav_final1_l7};
void __FASTCALL__  *fenemy_behavior_funcs[]={behav_fenemy1,behav_fenemy2,behav_fenemy3,behav_fenemy4,behav_fenemy5,behav_fenemy6,behav_fenemy7};
