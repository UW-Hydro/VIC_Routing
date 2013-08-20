/***** Model Constants *****/

/*************************************************************/
/* Change if needed                                          */
/*************************************************************/
#define MAXROWS 300
#define MAXCOLS 300
#define MAXYEARS 107
/*************************************************************/
/* No changes after here                                     */
/*************************************************************/
#define DAYS   MAXYEARS*366
/*************************************************************/
/* No changes after here                                     */
/*************************************************************/
#define KE     12              //number of steps in uh_file 
#define LE     48              //impulse response function. why 48?   
#define DELTA_T 3600.0
#define UH_DAY 96              //max days to outlet 
#define TMAX   UH_DAY*24
#define MAX_CELLS_IN_BASIN 5000   
#define MAXSTRING 512
#define NODATA -9999
#define PI 4.0*atan(1.0)       //pi!
#define EPS 1e-6               //precision
#define EARTHRADIUS 6371.229  //radius of earth in kilometers
#define SORT 0
#define RHO 1000;
#define CONV_M3S_CM 86400.      /* Conversion from m3/s to m3/day */
#define min(a,b) (a < b) ? a : b
#define max(a,b) (a > b) ? a : b
#define GE 9.81               //acceleration due to gravity, m/s2
#define EFF 0.85              //efficiency of power generating system
#define HUGENUMBER 1e10;    
#define MINNUMBER 1e-6;
/*************************************************************/
/* TYPE DEFINITIONS, GLOBALS, ETC.                           */
/*************************************************************/
typedef enum {double_f, int_f, float_f, long_f, short_f} FORMAT_SPECIFIER;

typedef struct {
  int flag;
  int routflag; //already routed cell: routflag=1. Info in station file.
  int ntypes;
  int direction;
  int routed;  //1:routed,2:flow exist
  int resloc; //0:no reservoir,1:reservoir
  int tocol;
  int torow;
  float fraction;
  float velocity;
  float xmask;
  float diffusion;
} ARC;

typedef struct {
  int id;
  int col;
  int row;
  int type; /* 1: Regular, 2: Dam, 3: Irrigated part of cell */
  float area;
  float demand;
  char name[5];
} LIST;

typedef struct {
  int year;
  int month;
  int day;
} TIME;
