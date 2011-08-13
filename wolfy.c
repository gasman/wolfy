#include "basic/basic.h"

#include "usetable.h"

#define RESX 96
#define RESY 68
#define RESY_B 9

#define TEX_HEIGHT 64
#define TEX_WIDTH 64

#include "wall.h" /* texture stored column-wise: pixel column 0 stored top-to-bottom, then pix column 1 etc */

/*
static int maze[] = {
	0b1111111111111111,
	0b1000000000000001,
	0b1011101111101101,
	0b1010000000000101,
	0b1010111101110101,
	0b1010100000010101,
	0b1010100000010101,
	0b1010100000010101,
	0b1010000000010101,
	0b1010101000010101,
	0b1000100000000101,
	0b1010111000110101,
	0b1010000000000101,
	0b1010111100111101,
	0b1000000000000001,
	0b1111111111111111
};
*/
static int maze[] = {
	0b1010101010101010,
	0b1010101010101010,
	0b1010101010101010,
	0b1010101010101010,
	
	0b1010101010101010,
	0b1010101010101010,
	0b1010101010101010,
	0b0000000000000000,

	0b1010101010101010,
	0b1010101010101010,
	0b1010101010101010,
	0b1010101010101010,

	0b1010101010101010,
	0b1010101010101010,
	0b1010101010101010,
	0b1010101010101010,
};
#define IS_WALL(x,y) ( (x)<0 || (x) > 0x0f || (y)<0 || (y)>0x0f || ( maze[y] & (1 << (x)) ) )

//#define IS_WALL(x,y) ( (x) < 0 || (x) > 0x000fffff || (y) < 0 || (y) > 0x000fffff)

/* maths operations on 16.16 fixpoint values */
#define FPFRAC(x) ( (x) & 0xffff )
#define FPMUL(a,b) ( ( (a)/0x100 ) * ( (b)/0x100 ) )
#define FPDIV(a,b) ( ( (a) * 0x100 / (b) ) * 0x100 )

/* floating point versions, for sanity checking */
//#define FPDIV(a,b) ((int)(((((float)(a))/65536) / (((float)(b))/65536)) * 65536))
//#define FPMUL(a,b) ((int)(((((float)(a))/65536) * (((float)(b))/65536)) * 65536))

/* straight line distance given x/y deltas (i.e. pythagoras) */
#define FPDISTANCE(x,y) ( iSqrt( FPMUL(x,x) + FPMUL(y,y) ) * 256 )

static int player_x = 0x00048000; /* fixpoint 16.16 */
static int player_y = 0x00048000;
static int player_angle = 0x2000; /* scaled 0..32767 as per o_sin */

static int o_sin(int x);
static int o_cos(int x);
static void render(void);
static long iSqrt(long value);

void ram(void)
{
	int button;
	
	while(1){
		render();
		lcdRefresh();
		delayms(23);
		while((button=getInputRaw())==BTN_NONE)
			delayms(23);
		switch (button) {
			case BTN_ENTER:
				return;
			case BTN_UP:
				player_x += o_sin(player_angle);
				player_y -= o_cos(player_angle);
				break;
			case BTN_DOWN:
				player_x -= o_sin(player_angle);
				player_y += o_cos(player_angle);
				break;
			case BTN_LEFT:
				player_angle = (player_angle - 256) & 0x7fff;
				break;
			case BTN_RIGHT:
				player_angle = (player_angle + 256) & 0x7fff;
				break;
		}
	}
}

static void render(void)
{
	int x, y, bits_until_write, wall_y, strip_height;
	uint8_t *col_buf, *buf, *wall_line;
	uint8_t b, wall_byte, bit;
	int distance, tex_step, tex_pos; /* fixpoint 16.16 */
	int tex_pos_int;
	
	int sin_pa, cos_pa; /* sin/cos of player_angle, fixpoint 16.16 */
	int vx, vy; /* view ray vector, fixpoint 16.16 */
	
	int xstep, ystep; /* amount to increment x position by for every unit travelled in y, and vice versa */
	int sample_x, sample_y; /* position counter when sampling for wall positions */

	int yd_vert, xd_vert; /* y and x distance to the nearest vertical wall, fixpoint 16.16 */
	int tex_vert; /* x position within texture at which we hit the wall, fixpoint 16.16 */
	int tex_vert_pix; /* tex_vert in pixel coordinates (0 to TEX_WIDTH) */
	int dist_vert; /* distance to vertical wall */

	int yd_horz, xd_horz; /* y and x distance to the nearest horizontal wall, fixpoint 16.16 */
	int tex_horz; /* x position within texture at which we hit the wall, fixpoint 16.16 */
	int tex_horz_pix; /* tex_horz in pixel coordinates (0 to TEX_WIDTH) */
	int dist_horz; /* distance to horizontal wall */

	int i;

	sin_pa = o_sin(player_angle) << 4;
	cos_pa = o_cos(player_angle) << 4;

	col_buf = lcdBuffer + (RESX * RESY_B) - 1; /* pointer to the top of the current column in lcdBuffer */

	for (x = 0; x < RESX; x++) {
		/* calculate view ray vector */
		vx = sin_pa + cos_pa * x / RESX - cos_pa/2;
		vy = -cos_pa + sin_pa * x / RESX - sin_pa/2;
		
		/* find where this crosses a vertical (integer x) */
		if (vx > 0x0010) { /* looking to the right */
			ystep = FPDIV(vy, vx);
			
			/* find point of intersection with a vertical grid line at floor(player_x) + 1 */
			sample_y = player_y + FPMUL(ystep, 0x10000-FPFRAC(player_x));
			for (i = (player_x >> 16) + 1; i < 16; i++) {
				if (IS_WALL(i, sample_y >> 16)) break;
				sample_y += ystep;
			}

			xd_vert = (i<<16) - player_x;
			yd_vert = FPMUL(xd_vert, ystep);
			dist_vert = FPDISTANCE(xd_vert, yd_vert);

			tex_vert = FPFRAC(player_y + yd_vert);
			tex_vert_pix = (tex_vert * TEX_WIDTH) >> 16;
		} else if (vx < -0x0010) {
			ystep = FPDIV(vy, -vx);

			/* find point of intersection with a vertical grid line at floor(player_x) */
			sample_y = player_y + FPMUL(ystep, FPFRAC(player_x));
			for (i = (player_x >> 16); i >= 0; i--) {
				if (IS_WALL(i, sample_y >> 16)) break;
				sample_y += ystep;
			}

			xd_vert = player_x - (i<<16);
			yd_vert = FPMUL(xd_vert, ystep);
			dist_vert = FPDISTANCE(xd_vert, yd_vert);

			tex_vert = FPFRAC(player_y + yd_vert);
			tex_vert_pix = TEX_WIDTH - 1 - (tex_vert * TEX_WIDTH >> 16);
		} else {
			/* view vector is vertical (or close enough) - crossing point is at infinity */
			dist_vert = 0x8000000;
			tex_vert_pix = 0;
		}
		
		/* find where this crosses a horizontal (integer y) */
		if (vy > 0x0010) {
			yd_horz = 0x10000 - FPFRAC(player_y);
			xd_horz = FPMUL(yd_horz, FPDIV(vx, vy));
			dist_horz = FPDISTANCE(xd_horz, yd_horz);

			tex_horz = FPFRAC(player_x + xd_horz);
			tex_horz_pix = TEX_WIDTH - 1 - (tex_horz * TEX_WIDTH >> 16);
		} else if (vy < -0x0010) {
			yd_horz = FPFRAC(player_y);
			xd_horz = FPMUL(-yd_horz, FPDIV(vx, vy));
			dist_horz = FPDISTANCE(xd_horz, yd_horz);

			tex_horz = FPFRAC(player_x + xd_horz);
			tex_horz_pix = tex_horz * TEX_WIDTH >> 16;
		} else {
			/* view vector is horizontal (or close enough) - crossing point is at infinity */
			dist_horz = 0x8000000;
			tex_horz_pix = 0;
		}
		
		//if (dist_vert < dist_horz) {
			distance = dist_vert;
			wall_line = wall + (tex_vert_pix * 8); /* pointer to the texture column to use in this screen column */
		//} else {
		//	distance = dist_horz;
		//	wall_line = wall + (tex_horz_pix * 8);
		//}

		tex_pos = (0x10000-distance) * TEX_HEIGHT/2; /* current texture y coordinate */
		tex_step = distance * TEX_HEIGHT / RESY; /* increment to add to tex_pos per screen pixel */
		
		buf = col_buf;
		b = 0; /* byte to write to lcdBuffer will be assembled here */
		bits_until_write = 8 - (RESY & 0x07); /* number of bits to add to b before we have a full byte to write to lcdBuffer */
		/* (NB less than 8 for the top line, because Y resolution is not a multiple of 8) */

		for (y = 0; y < RESY; y++) {
			b <<= 1; /* make room for new bit to add to the bottom of b */
			
			tex_pos_int = tex_pos >> 16;
			if (tex_pos_int < 0) {
				bit = 0; /* above top of texture => ceiling */
			} else if (tex_pos_int >= TEX_HEIGHT) {
				bit = 1; /* below bottom of texture => floor */
			} else {
				wall_byte = wall_line[tex_pos_int / 8]; /* read texture byte */
				bit = (wall_byte >> (tex_pos_int & 0x07)) & 0x01; /* extract required bit from texture */
			}
			tex_pos += tex_step;
			b |= bit;
			bits_until_write--;
			if (bits_until_write == 0) {
				*buf = b; /* write to buf */
				buf -= RESX; /* advance buf to the byte below */
				b = 0;
				bits_until_write = 8;
			}
		}
		col_buf--;
	}
	//fprintf(stderr, "player at (%08x, %08x), angle %d\n", player_x, player_y, player_angle);
}

//----------------------------------------------
//
// Integer square root. Take the square root of an integer.
//

#define step(shift) \
    if((0x40000000l >> shift) + root <= value)          \
    {                                                   \
        value -= (0x40000000l >> shift) + root;         \
        root = (root >> 1) | (0x40000000l >> shift);    \
    }                                                   \
    else                                                \
    {                                                   \
        root = root >> 1;                               \
    }

static long iSqrt(long value) {
    long root = 0;

    step( 0);
    step( 2);
    step( 4);
    step( 6);
    step( 8);
    step(10);
    step(12);
    step(14);
    step(16);
    step(18);
    step(20);
    step(22);
    step(24);
    step(26);
    step(28);
    step(30);

    // round to the nearest integer, cuts max error in half

    if(root < value)
    {
        ++root;
    }

    return root;
}

/* sin/cos functions nicked from rockets.c and o.c:
	takes an angle in range 0..32767 (for one revolution), returns sin/cos scaled to -4096..4096 */
static int o_sin(int x)
{
#define qN 13
#define qA 12
#define qP 15
#define qR (2*qN-qP)
#define qS (qN+qP+1-qA)

    x= x<<(30-qN);          // shift to full s32 range (Q13->Q30)

    if( (x^(x<<1)) < 0)     // test for quadrant 1 or 2
        x= (1<<31) - x;

    x= x>>(30-qN);

    return (x * ( (3<<qP) - (x*x>>qR) ) >> qS );
}

static inline int o_cos(int x)
{
  return o_sin(x + 8192);
}

