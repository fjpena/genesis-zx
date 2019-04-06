#ifndef STRUCTS_H
#define STRUCTS_H

struct Entity
{
	unsigned char x;
	unsigned char y;
	unsigned char sprnum;	// Number of the entity sprite: if 0, this entity is not active
	unsigned char type;	// Entity type
	unsigned char movement;	// movement type, from one of the predefined ones
	unsigned char energy;
	unsigned char param1;
	unsigned char param2;	// Two parameters, to store some information that is entity-specific
	char param3; 		// These 3rd and 4th parameters will be used by movement functions, as counters for example
	char param4; 		// These 3rd and 4th parameters will be used by movement functions, as counters for example
	unsigned char behavior; // Entity behavior (for enemy shoots)
	unsigned char behav_param;  // Parameter for behavior control
};

// This is a clone of the Entity structure, just for the enemy_locations array. 
// It adds a parameter for displacement within tile!
struct Enemy
{
	unsigned char x;        // Tile in X
	unsigned char x_desp;   // Displacement in tile (0-23)
	unsigned char y;        // Y position
	unsigned char enemy_type;	// Type of the enemy
	unsigned char movement;	// movement type, from one of the predefined ones
	unsigned char energy;
	unsigned char param1;
	unsigned char param2;	// Two parameters, to store some information that is entity-specific
};

#endif
