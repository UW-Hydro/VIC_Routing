/*      PROGRAM rout, C-version   

     Routing algorithm developed by D. Lohmann.
     Modified to allow more flexible array dimensions and
     the removal of harcoded file names.  
     Rewritten from FORTRAN to C 2003, AWW/IH.
      
     MAXROWS and MAXCOLS should be larger than the grid
     MAXYEARS should equal at least run length yrs+1   
     i: row from bottom (starts at 1)
     j: col from bottom (starts at 1)
     [][]: [row][col]
     Indexing (arrays): Starts at 1 (i.e. 0 is left unused)

     X.D.Z
     Oct-Nov., 2006: 
         add the part to read a seperate spinup data; change the control file format;
         add the factor of 35.315 (factor of m3 to cft, which was in the old fortran version, but missed in the new c version);
         rescale the unit in monthly output from cft per month back to cft per day;
     Dec 2007:
         change the order in SearchCatchment to be the same as in the fortran version, so that it can use the old uh_s files;
     Feb 2007:
         change the format of uh_s file;
         ignore the negative runoff and baseflow in the fluxes files;
     March 2007:
         the input vic data can now start from any day in a month (the old version must start in the first day);
*/

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <float.h>
#include <math.h>
#include <string.h>

/*************************************************************/
/* Change if needed                                          */
/*************************************************************/
#define MAXROWS  480
#define MAXCOLS  480
#define MAXYEARS 100
#define MAXSTNS  100
/*************************************************************/
/* No changes after here                                     */
/*************************************************************/
#define DAYS   MAXYEARS*366
#define KE     12              //number of steps in uh_file 
#define LE     48              //?? impulse response function. 
                               //number tken from f-version,
                               //but I haven't figured out why 
                               //it is 48.........   
#define DELTA_T 3600.0
#define UH_DAY  96             //max days to outlet 
#define TMAX    UH_DAY*24
#define MAX_CELLS_IN_BASIN 5000   
#define MAXSTRING 512
#define NODATA -9999
#define PI 4.0*atan(1.0)       //pi!
#define EPS 1e-6               //precision
#define EARTHRADIUS 6371.229;  //radius of earth in kilometers

#define MIDSTRING   100
/*************************************************************/
/* TYPE DEFINITIONS, GLOBALS, ETC.                           */
/*************************************************************/
typedef struct {
  int id;
  int ntypes;
  int direction;
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
  float area;
  char name[20];
} LIST;

typedef struct {
  int col;
  int row;
  float lon;
  float lat;
  float fraction;
} CATCH;

typedef struct {
  int year;
  int month;
  int day;
} DATE;

int DaysInMonth[13] = { 0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

/*************************************************************/
int  CalculateDaysInYear(DATE *);
int  CalculateNumberDays(DATE *, DATE *);
void FindRowsCols(char *, int *, int *, float *, float *, float *); 
int  IsLeapYear(int);
void MakeConvolution(int,int,int,CATCH *,ARC **,
         float *,float *, float *,float **,float,
         char *,char *,int,float *,long);
void MakeGridUH(ARC **,CATCH *,LIST *,
         int,int,float,float,float,float **,float ***,float **,float **,float **);
void MakeUH(float ***UH, ARC **, int, int);
void ReadDiffusion(char *, ARC **, int, int); 
void ReadDirection(char *, ARC **, int, int, int *); 
void ReadFraction(char *, ARC **, int, int); 
char ReadGridUH(ARC **,CATCH *,LIST *,int *,float **,char *);
void ReadUHBox(char *, float **, int);
void ReadStartStopDate(char *, DATE *, DATE *, char *);
void ReadStation(char *,LIST *,char uhstring[MAXSTNS][MIDSTRING],int *);
void ReadVelocity(char *, ARC **, int, int); 
void ReadXmask(char *, ARC **, int, int); 
void SearchCatchment(ARC **,CATCH *,int,int,int,int,int *);
void WriteData(float *,char *,char *,DATE *,int,int);
/*************************************************************/
/* Start of ROUT                                             */
/*************************************************************/
int main (int argc, char *argv[])
{   
  FILE *fp;

  ARC **BASIN=NULL; //Grid input information is stored here,
                    //like direction,fraction,velocity of 
                    //each cell in the grid. Row(i) is row from
                    //the bottom, col(j) is column from the left.
                    //Numbering starts at 1 for both i and j.
  LIST *STATION=NULL;    //Holds information about station locations and names.
                    //Area is included in the list, although the value
                    //isn't used in the routing program.
  CATCH *CATCHMENT=NULL;
  DATE spinup_start, spinup_stop, inp_start, inp_stop, out_start, out_stop; 
                    // start and stop date of spinpup, input flux data, and output route data;

  char filename[MAXSTRING];
  char spinuppath[MAXSTRING];
  char inpath[MAXSTRING];
  char outpath[MAXSTRING];
  char name[MAXSTRING];
  char uhstring[MAXSTNS][MIDSTRING]; //name of uhfile (or "NONE")
  char dummy[MAXSTRING];
  char none[5]="NONE";

  float xllcorner; //x-coordinate, lower left corner of grid
  float yllcorner; //y-coordinate, lower left corner of grid
  float size;      //size of grid cell, in degrees
  float value;
  float factor_sum;
  float ***UH;     //impulse response function UH[row][col][1-48]
  float **UH_BOX;  //unit hydrograph[1-number_of_cells][1-12]. 
  float **UH_DAILY;
  float **UH_S;
  float **FR;
  float *BASEFLOW;  
  float *RUNOFF;
  float *FLOW;         // note: data in BASEFLOW,RUNOFF,FLOW starts from subscript 1;

  int i,j;
  int nrows,ncols;     //number of rows/columns in basin (read from direction file)
  int active_cells;    //total number of active cells in grid
  int number_of_cells; //number of cells upstream a station location
  int number_of_stations; //number of stations 
  int decimal_places;  //decimal places in VIC output 
  int ndays,spinup_days,spinup_first_date;   //number of days and months in spinup and VIC simulations 
                       //what about skipping whatever is 
                       //before/after the period of interest?
  int day0, day1;      //the first and last day of output 

  char MISSING_STN;    // 1 if failed to read uh_s file (will skip this station), 0 otherwise
  
  /***********************************************************/

  if(argc != 2){
      printf("USAGE: %s <infile>\n", argv[0]);
   exit(0);
  }
  if((fp = fopen(argv[1], "r")) == NULL) {
   printf("Cannot open %s\n",argv[1]);
   exit(1);
  }

  /* Find number of rows and cols in grid of interest */
  fgets(dummy, MAXSTRING, fp);
  fscanf(fp, "%*s %s",filename);
  FindRowsCols(filename,&nrows,&ncols,&xllcorner,&yllcorner,&size);

  /* Allocate memory for BASIN, UH, FR */
  BASIN=calloc(nrows+1,sizeof(ARC));
  for(i=0;i<=nrows;i++) 
      BASIN[i]=calloc(ncols+1,sizeof(ARC));
  UH = (float***)calloc(nrows+1,sizeof(float**));
  for(i=0;i<=nrows;i++) {
    UH[i]=(float**)calloc(ncols+1,sizeof(float*));
    for(j=0;j<=ncols;j++) 
      UH[i][j]=(float*)calloc(LE+1,sizeof(float*));      
  }  
  FR = (float**)calloc(TMAX+1,sizeof(float*));
  for(i=0;i<=TMAX;i++) 
     FR[i]=(float*)calloc(2,sizeof(float));

  /* Read direction file */
  printf("Direction file: %s\n",filename);
  ReadDirection(filename,BASIN,nrows,ncols,&active_cells);
  printf("Active cells in basin: %d\n",active_cells);

  /* Allocate memory for CATCHMENT, UH_BOX, UH_S, UH_DAILY, STATION */
  CATCHMENT = calloc(active_cells+1,sizeof(CATCH));
  UH_BOX=(float**)calloc(active_cells+1,sizeof(float*));
  for(i=0;i<=active_cells;i++) 
      UH_BOX[i]=(float*)calloc(KE+1,sizeof(float*));      
  UH_DAILY=(float**)calloc(active_cells+1,sizeof(float*));
  for(i=0;i<=active_cells;i++) 
      UH_DAILY[i]=(float*)calloc(UH_DAY+1,sizeof(float));      
  UH_S=(float**)calloc(active_cells+1,sizeof(float*));
  for(i=0;i<=active_cells;i++) 
      UH_S[i]=(float*)calloc(KE+UH_DAY+1,sizeof(float));      
  STATION=calloc(MAXSTNS,sizeof(LIST));

  /* Read velocity file if any */
  fscanf(fp, "%*s %s",filename);
  value=atof(filename);
  if(value<EPS) ReadVelocity(filename,BASIN,nrows,ncols);
  else {
    printf("Velocity: %.2f\n",value);
    for(i=1;i<=nrows;i++) 
      for(j=1;j<=ncols;j++) 
	BASIN[i][j].velocity=value;
  }

  /* Read diffusion file */
  fscanf(fp, "%*s %s",filename);
  value=atof(filename);
  if(value<EPS) ReadDiffusion(filename,BASIN,nrows,ncols);
  else {
    printf("Diffusion: %.2f\n",value);
    for(i=1;i<=nrows;i++)
      for(j=1;j<=ncols;j++) 
        BASIN[i][j].diffusion=value;
  }

  /* Read xmask file */
  fscanf(fp, "%*s %s",filename);
  value=atof(filename);
  if(value<EPS) ReadXmask(filename,BASIN,nrows,ncols);
  else {
    printf("Xmask: %.2f\n",value);
    for(i=1;i<=nrows;i++)
      for(j=1;j<=ncols;j++) 
        BASIN[i][j].xmask=value;
  }


  /* Read fraction file */
  fscanf(fp, "%*s %s ",filename);
  printf("Fraction file: %s\n",filename);
  ReadFraction(filename,BASIN,nrows,ncols);

  /* Read station file */
  fscanf(fp, "%*s %s",filename);
  printf("Station file: %s\n",filename);
  ReadStation(filename,STATION,uhstring,&number_of_stations);

  /* Read input precision of VIC filenames*/
  fscanf(fp, "%*s %d",&decimal_places);
  printf("Decimal_places: %d\n",decimal_places);	  

  /* Read spinup path of VIC filenames, and start and end year/month */
  fscanf(fp, "%*s %s ",spinuppath);
  fgets(dummy, MAXSTRING, fp);
  if (strcmp(spinuppath,none) !=0 ) {
    ReadStartStopDate(dummy, &spinup_start, &spinup_stop, "spinup");
    spinup_days = CalculateNumberDays(&spinup_start, &spinup_stop);
    spinup_first_date = spinup_start.year*10000L+spinup_start.month*100+spinup_start.day;
    printf("Spinup path: %s\n", spinuppath);
    printf("      start: %d %d %d; end: %d %d %d; Ndays: %d.\n",
	   spinup_start.year,spinup_start.month,spinup_start.day,
           spinup_stop.year, spinup_stop.month, spinup_stop.day, spinup_days);
  }
  else {
    spinup_days = 0;
    spinup_first_date = 0;
  }

  /* Read input path of VIC filenames, and start and end year/month */
  fscanf(fp, "%*s %s ",inpath);
  fgets(dummy, MAXSTRING, fp);
  ReadStartStopDate(dummy, &inp_start, &inp_stop, "input flux");
  ndays = CalculateNumberDays(&inp_start,&inp_stop);
  printf("VIC    path: %s\n", inpath);
  printf("      start: %d %d %d; end: %d %d %d; Ndays: %d.\n",
         inp_start.year,inp_start.month,inp_start.day,inp_stop.year,inp_stop.month,inp_stop.day,ndays);  
  ndays += spinup_days;    // ndays: the total days of data for MakeConvolution (spinup + input)

  /* Read output path, and start and end year/month */
  fscanf(fp, "%*s %s ",outpath);
  fgets(dummy, MAXSTRING, fp);
  ReadStartStopDate(dummy, &out_start, &out_stop, "output route");
  printf("Output path: %s\n", outpath);
  printf("      start: %d %d %d; end: %d %d %d.\n",
         out_start.year,out_start.month,out_start.day,out_stop.year,out_stop.month,out_stop.day);  

  /* Check spinup, input and output period */
  if (spinup_days > 0) {
    day0 = CalculateNumberDays(&spinup_start, &out_start);  // the first day for WriteData();
    if (day0 < 0) {
      printf("\nOutput starting date must not be earlier than spinup.\n");
      printf("Spinup starts: %4d %2d %2d,\n", spinup_start.year, spinup_start.month, spinup_start.day);
      printf("Output starts: %4d %2d %2d.\n", out_start.year, out_start.month, out_start.day);
      printf("Change these dates and restart.\n\n");
      exit(-1);
    }
  }
  else {
    day0 = CalculateNumberDays(&inp_start, &out_start); // the first day for WriteData();
    if (day0 < 0) {
      printf("\nWhen no spinup is used, output starting date must not be earlier than input.\n");
      printf("Input  starts: %4d %2d %2d,\n", inp_start.year, inp_start.month, inp_start.day);
      printf("Output starts: %4d %2d %2d.\n", out_start.year, out_start.month, out_start.day);
      printf("Change these dates and restart.\n\n");
      exit(-1);
    }
    memcpy((void *)&spinup_start, (void *)&inp_start, sizeof(DATE)); // use input start day for spinup (used to calculate day1)
  }

  day1 = CalculateNumberDays(&spinup_start, &out_stop);  // day1: the last day for WriteData()
  if (day1 > ndays) {
    printf("\nOutput stopping date must not be later than input.\n");
//    printf("Input  stops: %4d %2d,\n", stop_year, stop_month);
    printf("Output stops: %4d %2d %2d.\n", out_stop.year, out_stop.month, out_stop.day);
    printf("Change these dates and restart.\n\n");
    exit(-1);
  }

/* Allocate memory for BASEFLOW, RUNOFF and FLOW */
  RUNOFF=(float*)calloc(ndays + 1,sizeof(float));
  BASEFLOW=(float*)calloc(ndays + 1,sizeof(float));
  FLOW=(float*)calloc(ndays + 1,sizeof(float));

  /* Read uh-file */
  fscanf(fp, "%*s %s",filename);
  fclose(fp);

  /* Make impulse response function (UH) */
  printf("Making impulse response function........\n");
  MakeUH(UH,BASIN,nrows,ncols);

  /* Loop over required output stations, rout fluxes and write them out */  
  for(i=1;i<=number_of_stations;i++) {
     if(STATION[i].id==1) {
        strcpy(name,STATION[i].name);
        printf("\n\nStation %s. Location row %d col %d\n", name, STATION[i].row,STATION[i].col);
        if(strcmp(uhstring[i],none)!=0) {
          printf("Read grid UH_S\n");
          MISSING_STN = ReadGridUH(BASIN,CATCHMENT,STATION,&number_of_cells,UH_S,uhstring[i]);
        }
        else {
          printf("\n\nSearching catchment... Location: row %d col %d\n",
                 STATION[i].row,STATION[i].col);
          SearchCatchment(BASIN,CATCHMENT,STATION[i].row,STATION[i].col,
                          nrows,ncols,&number_of_cells);

          /* Read grid UH_BOX. */
          printf("Read grid UH_BOX\n");
          ReadUHBox(filename,UH_BOX,number_of_cells); // ReadUHBOX is the old ReadGridUH

          /* Making grid UH_S*/
          printf("Make grid UH_S\n");
          MakeGridUH(BASIN,CATCHMENT,STATION,number_of_cells,i,
                     xllcorner,yllcorner,size,UH_DAILY,UH,FR,UH_BOX,UH_S);
          MISSING_STN = 0;
	}

        if (MISSING_STN == 1) continue;  // no uh_s information, skip station

        /* Making convolution */
        printf("Make convolution...\n");
        MakeConvolution(number_of_cells,ndays,spinup_days,CATCHMENT,BASIN,BASEFLOW,RUNOFF,FLOW,
                        UH_S,size,inpath,spinuppath,decimal_places,&factor_sum,spinup_first_date);

        /* Writing data */
        printf("Writing data...\n");
        WriteData(FLOW,name,outpath,&out_start,day0,day1);
        printf("finish processing station: %s\n", name);
      }
   }
   
  /* Free memory */
  for(i=1;i<=nrows;i++) 
    free(BASIN[i]);
  for(i=1;i<=TMAX;i++) 
    free(FR[i]);
  free(BASEFLOW);
  free(RUNOFF);
  free(FLOW);
  free(STATION);
  free(CATCHMENT);
  
  printf("route program finished.\n");
  return(1);
}
/***************************************************/
/* CalculateDaysInYear                             */
/***************************************************/
int CalculateDaysInYear(DATE *date)
{
  int mn, days;

  days = date->day;
  for (mn = 1; mn < date->month; mn ++) days += DaysInMonth[mn];
  if (IsLeapYear(date->year) && (date->month > 2)) days ++;
  return(days);
}
/****************************************************/
/* CalculateNumberDays                              */
/****************************************************/
int CalculateNumberDays(DATE *start_date,            
                        DATE *stop_date)
{
  int year, days;

  days = CalculateDaysInYear(stop_date) - CalculateDaysInYear(start_date) + 1;
  for (year = start_date->year; year < stop_date->year; year ++) {
    if (IsLeapYear(year)) days += 366;
    else                  days += 365; 
  }
  return(days);
}

/**********************************************/
/* FindRowsCols - finds rows,cols,xllcorner,
   yllcorner,size                             */
/**********************************************/
void FindRowsCols(char *filename, 
		  int *nrows,
		  int *ncols,
		  float *xllcorner,
		  float *yllcorner,
		  float *size)
{
  FILE *fp;
  char dummy[25];
  int i,j,missing;

  if((fp = fopen(filename, "r")) == NULL) {
    printf("Cannot open %s\n",filename);
    exit(1);
  }

  fscanf(fp,"%s %d", dummy, &(*ncols)); 
  fscanf(fp,"%s %d", dummy, &(*nrows)); 
  fscanf(fp,"%s %f", dummy, &(*xllcorner));
  fscanf(fp,"%s %f", dummy, &(*yllcorner));
  fscanf(fp,"%s %f", dummy, &(*size)); 
  fscanf(fp,"%s %d", dummy, &missing); 

  fclose(fp);

  if((*nrows)>MAXROWS || (*ncols)>MAXCOLS){
   printf("Incorrect dimensions: Reset nrow and ncol in main to %d, %d\n",
     (*nrows), (*ncols));
   exit(0);
   }
}
/*************************************/
/* IsLeapYear                        */
/*************************************/
int IsLeapYear(int year)
{
  if ((year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)) return 1;
  else return 0;
}  
/**************************************/
/* MakeConvolution                    */ 
/**************************************/
void MakeConvolution(int number_of_cells,
		     int ndays,
		     int spinup_days,
		     CATCH *CATCHMENT,
		     ARC **BASIN,
		     float *BASEFLOW,
		     float *RUNOFF,
		     float *FLOW,
		     float **UH_S,
		     float size,
		     char *inpath,
		     char *spinuppath,
		     int decimal_places,
		     float *factor_sum,
                     long spinup_first_date)
{
  FILE *fp;
  int  i,j,k,n; //ii,jj;
  int  yr,mn,dy;
  long date;
  double factor;
  char infile[500];
  char LATLON[50];
  char fmtstr[17];
  char tmpdat[MAXSTRING];
  float lat, lon;
  double area,area_sum;
  double radius;
  double storage;
  float k_const;
  float dummy;
  char  MISSING_CELL;

  k_const=1.0;
  area_sum=(*factor_sum)=0.0;

  for(i=1;i<=ndays;i++) 
    FLOW[i] = 0.0;
  
  for(n=1;n<=number_of_cells;n++) { //the gridcell loop
    storage=0.0; MISSING_CELL = 0;
    for(i=1;i<=ndays;i++) {
      RUNOFF[i]=0.0;
      BASEFLOW[i]=0.0;
    }
    
    lon = CATCHMENT[n].lon;
    lat = CATCHMENT[n].lat;
    
    /* Give area of box in square kilometers */
    radius = (double)EARTHRADIUS;
    area = radius*radius*fabs(size)*PI/180*            
           fabs(sin((lat-size/2.0)*PI/180)-sin((lat+size/2.0)*PI/180));         // note: area: unit in km2
    area_sum += area;
    factor = CATCHMENT[n].fraction*area/86.4;
//    factor = BASIN[ii][jj].fraction*area/86.4;  //conversion factor for
                                                //mm/day to m3/s
                                                //&mult. by cell fract
    factor *= 35.315;                           // 1 m3 = 35.315 ft3						
    (*factor_sum) += factor;

    /* Make spinup filename */
    if(spinup_days > 0) {
      strcpy(infile,spinuppath);
      sprintf(fmtstr,"%%.%if_%%.%if",decimal_places,decimal_places);
      sprintf(LATLON,fmtstr,lat,lon);
      strcat(infile,LATLON);

      if((fp = fopen(infile,"r"))==NULL) { 
        printf("Cannot open spinup file %s; skip the cell.\n",infile);
        MISSING_CELL = 1;
      }
      else {
      /* Read spinup: <year> <month> <day> <p> <et> <runoff> <baseflow>*/
        do {
          fgets(tmpdat,MAXSTRING,fp); 
          sscanf(tmpdat,"%d %d %d",&yr, &mn, &dy);
          date = yr * 10000L + mn * 100 + dy;
        } while (date < spinup_first_date);

        if (date != spinup_first_date) {
          printf("Spinup data %s does not start from %ld; skip the cell.\n", infile, spinup_first_date);
          MISSING_CELL = 1;
        }
        else {      
          sscanf(tmpdat,"%*d %*d %*d %*f %*f %f %f",&RUNOFF[1],&BASEFLOW[1]);
          for (i = 2; i <= spinup_days; i ++) {
            fgets(tmpdat,MAXSTRING,fp);
            sscanf(tmpdat,"%*d %*d %*d %*f %*f %f %f",&RUNOFF[i],&BASEFLOW[i]);
          }
        }
        fclose(fp);
      }
    }

    if (MISSING_CELL == 1) continue;

    /* Make vic filename */
    strcpy(infile,inpath);
    sprintf(fmtstr,"%%.%if_%%.%if",decimal_places,decimal_places);
    sprintf(LATLON,fmtstr,lat,lon);
    strcat(infile,LATLON);
    printf("File %d of %d: %s\n",n,number_of_cells,infile);

    if((fp = fopen(infile,"r"))==NULL) { 
      printf("Cannot open flux file %s, skip the cell.\n",infile);
      MISSING_CELL = 1;
    }
    else {
      /* Read VIC model output: <year> <month> <day> <p> <et> <runoff> <baseflow> */
      for(i = (spinup_days + 1);i<=ndays;i++) {
        fgets(tmpdat,MAXSTRING,fp); 
        sscanf(tmpdat,"%*d %*d %*d %*f %*f %f %f",&RUNOFF[i],&BASEFLOW[i]);
      }
      fclose(fp);
    }

    if (MISSING_CELL == 1) continue;
    for(i=1;i<=ndays;i++) {
      RUNOFF[i] = RUNOFF[i]*factor;
      BASEFLOW[i] = BASEFLOW[i]*factor;
      if (RUNOFF[i] < 0.) RUNOFF[i] = 0.; // avoid negative runoff and baseflow. added by X.D.Z
      if (BASEFLOW[i] < 0.) BASEFLOW[i] = 0.;
    }
     
    for(i=1;i<=ndays;i++) {
      for(j=1;j<KE+UH_DAY;j++) {
        if((i-j+1)>=1) {
          FLOW[i] += UH_S[n][j]*(BASEFLOW[i-j+1]+RUNOFF[i-j+1]);
        }
      }
    }
  }  
}
/************************************************************************/
/* MakeGridUH                                                           */
/************************************************************************/
void MakeGridUH(ARC **BASIN,
		CATCH *CATCHMENT,
		LIST *STATION,
		int number_of_cells,
		int station_number,
                float xllcorner,
                float yllcorner,
                float size,
		float **UH_DAILY,
		float ***UH,
		float **FR,
		float **UH_BOX,
		float **UH_S)
{
  FILE *fp;
  float sum;
  int i,j,k,l,n,t,u,ii,jj,tt;
  char name[10];

  printf("Making UH_S grid.... It takes a while...\n");
  strcpy(name,STATION[station_number].name);
  strcat(name,".uh_s2");
  if((fp = fopen(name, "w")) == NULL) {
    printf("Cannot open %s\n",name);
    exit(1);
  }
  else printf("File opened for writing: %s\n",name);

  fprintf(fp, "%d\n", number_of_cells);
  for(n=1;n<=number_of_cells;n++) {
    printf("grid cell %d out of %d\t",n,number_of_cells);
    i=CATCHMENT[n].row;
    j=CATCHMENT[n].col;
    CATCHMENT[n].lat=yllcorner + i*size - size/2.0;
    CATCHMENT[n].lon=xllcorner + j*size - size/2.0;
    CATCHMENT[n].fraction=BASIN[i][j].fraction;
    printf("n=%d i=%d j=%d\n",n,i,j);
    fprintf(fp, "%.4f %.4f %f %d %d\n", CATCHMENT[n].lon, CATCHMENT[n].lat, CATCHMENT[n].fraction, j, i);
    for(k=1;k<=UH_DAY;k++) UH_DAILY[n][k] = 0.0;
    for(t=1;t<=24;t++) {
      FR[t][0]=1.0/24.;
      FR[t][1]=0.0;
    }
    for(t=25;t<=TMAX;t++) {
      FR[t][0]=0.0;
      FR[t][1]=0.0;
    }

    while (i!=STATION[station_number].row || j!=STATION[station_number].col) { 
      for(t=1;t<=TMAX;t++) {
        for(l=1;l<=LE;l++) {
          if((t-l)>0) 
            FR[t][1]=FR[t][1]+FR[t-l][0]*UH[i][j][l];
        }
      }
      for(t=1;t<=TMAX;t++) {
        FR[t][0]=FR[t][1];
        FR[t][1]=0.0;
      }
      ii=BASIN[i][j].torow;
      jj=BASIN[i][j].tocol;
      i=ii;
      j=jj;
    }

    for(t=1;t<=TMAX;t++) {
      tt=(t+23)/24;
      UH_DAILY[n][tt] += FR[t][0];
    }

    for(k=0;k<KE+UH_DAY;k++) UH_S[n][k]=0.;
    
    for(k=1;k<=KE;k++) {
      for(u=1;u<=UH_DAY;u++) 
        UH_S[n][k+u-1] = UH_S[n][k+u-1]+UH_BOX[n][k]*UH_DAILY[n][u];
    }	
    sum=0.0;
    for(k=1;k<KE+UH_DAY;k++) sum+=UH_S[n][k];
    for(k=1;k<KE+UH_DAY;k++) UH_S[n][k]=UH_S[n][k]/sum;
    
    for(k=1;k<KE+UH_DAY;k++) {
      if(UH_S[n][k]>0) fprintf(fp,"%g ",UH_S[n][k]);
      else fprintf(fp,"%.1f ",UH_S[n][k]);
    }
    fprintf(fp,"\n");
  }
  fclose(fp);
}

/****************************************************
MakeUH:
 Calculate impulse response function for grid cells
 using equation (15) from Lohmann, et al. (1996)  
 Tellus article
*****************************************************/
void MakeUH(float ***UH,
	    ARC **BASIN,
	    int nrows,
	    int ncols)
{
  int i,j,k;
  float time,exponent;
  float green; // Green's function h(x,t)
  float conv_integral; // Convolution integral Q(x,t)

  for(i=1;i<=nrows;i++) {
    for(j=1;j<=ncols;j++) {
      time=0.0;
      for(k=1;k<=LE;k++) {
	time=time+DELTA_T;
	if(BASIN[i][j].velocity>0.0) {
	  exponent=((BASIN[i][j].velocity*time-BASIN[i][j].xmask)*
		    (BASIN[i][j].velocity*time-BASIN[i][j].xmask))/
	    (4.0*BASIN[i][j].diffusion*time);
	  if(exponent>69.0)  //where does this number come from???? 
	    green=0.0;
	  else 
	    green=1.0/(2.0*sqrt(PI*BASIN[i][j].diffusion)) * 
	      BASIN[i][j].xmask/pow(time,1.5) * 
	      exp(-exponent);  // eq. 15 in Tellus article
	}
	else
	  green=0.0;
	UH[i][j][k]=green;
      }
    }
  }

  for(i=1;i<=nrows;i++) {
    for(j=1;j<=ncols;j++) {
      conv_integral = 0.0;
      for(k=1;k<=LE;k++) 
	conv_integral += UH[i][j][k];
      if (conv_integral > 0.0) {
	for(k=1;k<=LE;k++) 
	  UH[i][j][k] = UH[i][j][k]/conv_integral; //why? normalizing?
      }
    }
  }
  
}
/*********************************/
/* Reads the diffusion file      */
/*********************************/
void ReadDiffusion(char *filename, 
		  ARC **BASIN,
		  int nrows,
		  int ncols)
{
  FILE *fp;
  char dummy[25];
  int i,j,nodata;

  if((fp = fopen(filename, "r")) == NULL) {
    printf("Cannot open %s\n",filename);
    exit(1);
  }
  else printf("File opened %s\n",filename);

  for(i=0;i<6;i++)
    fscanf(fp,"%s %s", dummy, dummy); 

  for(i=nrows;i>=1;i--) { //kept bounds
    for(j=1;j<=ncols;j++) { 
      fscanf(fp,"%f",&BASIN[i][j].diffusion); 
      if(BASIN[i][j].diffusion<0.) BASIN[i][j].diffusion=0.; 
    }
  }
  fclose(fp);
}
/*********************************/
/* Reads the flow direction file */
/*********************************/
void ReadDirection(char *filename, 
		   ARC **BASIN,
		   int nrows,
		   int ncols,
		   int *active_cells)
{
  FILE *fp;
  char dummy[25];
  int i,j,missing;

  if((fp = fopen(filename, "r")) == NULL) {
    printf("Cannot open %s\n",filename);
    exit(1);
  }

  for(i=0;i<5;i++)
    fscanf(fp,"%*s %*s"); 
  fscanf(fp,"%*s %d", &missing); 

  (*active_cells)=0;

  for(i=nrows;i>=1;i--) { // kept bounds 
    for(j=1;j<=ncols;j++) { 
      fscanf(fp,"%d",&BASIN[i][j].direction); 
      if(BASIN[i][j].direction>missing) 
      	(*active_cells)++;
    }
  }

  fclose(fp);

  for(i=1;i<=nrows;i++) {
    for(j=1;j<=ncols;j++) {
      switch (BASIN[i][j].direction) {
        case 1:
          BASIN[i][j].tocol=j;
          BASIN[i][j].torow=i+1;
          break;
        case 2:
          BASIN[i][j].tocol=j+1;
          BASIN[i][j].torow=i+1;
          break;
        case 3:
          BASIN[i][j].tocol=j+1;
          BASIN[i][j].torow=i;
          break;
        case 4:
          BASIN[i][j].tocol=j+1;
          BASIN[i][j].torow=i-1;
          break;
        case 5:
          BASIN[i][j].tocol=j;
          BASIN[i][j].torow=i-1;
          break;
        case 6:
          BASIN[i][j].tocol=j-1;
          BASIN[i][j].torow=i-1;
          break;
        case 7:
          BASIN[i][j].tocol=j-1;
          BASIN[i][j].torow=i;
          break;
        case 8:
          BASIN[i][j].tocol=j-1;
          BASIN[i][j].torow=i+1;
          break;
        default:
	  BASIN[i][j].tocol=0;
	  BASIN[i][j].torow=0;
      }
    }
  }
}

/*********************************/
/* Reads the fraction file       */
/*********************************/
void ReadFraction(char *filename, 
		  ARC **BASIN,
		  int nrows,
		  int ncols)

{
  FILE *fp;
  char dummy[25];
  int i,j;
  float fvalue;
  int ivalue;

  if((fp = fopen(filename, "r")) == NULL) {
    printf("Cannot open %s\n",filename);
    exit(1);
  }

  for(i=0;i<6;i++)
  fscanf(fp,"%s %s", dummy, dummy); 

  for(i=nrows;i>=1;i--) { 
    for(j=1;j<=ncols;j++) { 
      fscanf(fp,"%f",&BASIN[i][j].fraction); 
    }
  }
  fclose(fp);
}

/************************************************************************/
/* ReadGridUH                                                           */
/************************************************************************/
char ReadGridUH(ARC **BASIN,
		CATCH *CATCHMENT,
		LIST *STATION,
		int *number_of_cells,
//		int station_number,
		float **UH_S,
		char *uh_string)
{
  FILE *fp;
  int n,k;

  printf("Reading UH_S grid from file\n");
  if((fp = fopen(uh_string, "r")) == NULL) {
    printf("Cannot open %s. skip station.\n",uh_string);
    return(1);
  }
  else printf("File opened for reading: %s\n",uh_string);

  fscanf(fp, "%d ", number_of_cells);
  for(n=1;n<=*number_of_cells;n++) {
    fscanf(fp, "%f %f %f %*d %*d", &CATCHMENT[n].lon, &CATCHMENT[n].lat, &CATCHMENT[n].fraction);
    for(k=1;k<KE+UH_DAY;k++) fscanf(fp,"%f ",&UH_S[n][k]);
  }
  return(0);
}

/*****************************************/
/* Reads the uh_box (the old ReadGridUH) */
/*****************************************/
void ReadUHBox(char *filename, 
		float **UH_BOX,
		int number_of_cells)

{
  FILE *fp;
  int i,j;

  for(i=1;i<=number_of_cells;i++) { 
    if((fp = fopen(filename, "r")) == NULL) {
      printf("Cannot open %s\n",filename);
      exit(1);
    }
    for(j=1;j<=12;j++) 
      fscanf(fp,"%*f %f",&UH_BOX[i][j]); 
    
    fclose(fp);
  }

}
/*************************************************************************/
/* Read the Start and Stop Date (of spinup, input flux, and output route */
/*************************************************************************/
void ReadStartStopDate(char *data, DATE *start_date, DATE *stop_date, char *msg)
{
  int n, t1, t2, t3, t4;  // temporally variables
  n = sscanf(data, "%*s %d %d %d %d %d %d",
	     &(start_date->year), &(start_date->month), &t1, &t2 ,&t3, &t4);
  switch (n) {
    case 6:
      start_date->day = t1;
      stop_date->year = t2; stop_date->month = t3; stop_date->day = t4;
      break;
    case 4:
      start_date->day = 1;
      stop_date->year = t1; stop_date->month = t2;
      stop_date->day = DaysInMonth[t2];
      if ((t2 == 2) && IsLeapYear(t1)) stop_date->day ++;
      break;
    default:
      printf("%s dates format error.\n", msg);
      printf("should be either: <start yr> <month> <day> <stop yr> <month> <day>\n");
      printf("              or: <start yr> <month> <stop yr> <month>\n");
      printf("Change the dates and restart.\n");
      exit(-1);  
  }
}
/*********************************/
/* Reads the station file        */
/*********************************/
void ReadStation(char *filename, 
		 LIST *STATION,
		 char uhstring[MAXSTNS][MIDSTRING],
		 int *number_of_stations)
{
  FILE *fp;
  int i,j;

  if((fp = fopen(filename, "r")) == NULL) {
    printf("Cannot open %s\n",filename);
    exit(1);
  }

  i=1;
  while(!feof(fp)) {
    fscanf(fp,"%d %s %d %d %f ",
	   &STATION[i].id,STATION[i].name,&STATION[i].col,
	   &STATION[i].row,&STATION[i].area); 
    fscanf(fp,"%s",uhstring[i]);
    i+=1;	     
  }

  (*number_of_stations)=i-1;
  fclose(fp);
}
/*********************************/
/* Reads the velocity file       */
/*********************************/
void ReadVelocity(char *filename, 
		  ARC **BASIN,
		  int nrows,
		  int ncols)
{
  FILE *fp;
  char dummy[25];
  int i,j,nodata;

  if((fp = fopen(filename, "r")) == NULL) {
    printf("Cannot open %s\n",filename);
    exit(1);
  }
  else printf("File opened %s\n",filename);

  for(i=0;i<6;i++)
    fscanf(fp,"%s %s", dummy, dummy); 

  for(i=nrows;i>=1;i--) { //kept bounds
    for(j=1;j<=ncols;j++) { 
      fscanf(fp,"%f",&BASIN[i][j].velocity); 
      if(BASIN[i][j].velocity<0.) BASIN[i][j].velocity=0.; 
    }
  }
  fclose(fp);
}
/*********************************/
/* Reads the xmask file          */
/*********************************/
void ReadXmask(char *filename, 
	       ARC **BASIN,
	       int nrows,
	       int ncols)
{
  FILE *fp;
  char dummy[25];
  int i,j,nodata;

  if((fp = fopen(filename, "r")) == NULL) {
    printf("Cannot open %s\n",filename);
    exit(1);
  }
  else printf("File opened %s\n",filename);

  for(i=0;i<6;i++)
    fscanf(fp,"%s %s", dummy, dummy); 

  for(i=nrows;i>=1;i--) { //kept bounds
    for(j=1;j<=ncols;j++) { 
      fscanf(fp,"%f",&BASIN[i][j].xmask); 
      if(BASIN[i][j].xmask<0.) BASIN[i][j].xmask=0.; 
    }
  }
  fclose(fp);
}
/*********************************************/
/* SearchCatchment                           */
/* Purpose: Find number of cells upstream    */
/*          current gage location, and their */
/*          row and col number               */ 
/*********************************************/
void SearchCatchment(ARC **BASIN,
		     CATCH *CATCHMENT,
		     int row,
		     int col,
		     int nrows,
		     int ncols,
		     int *number_of_cells)
{
  int i,j,ii,jj,iii,jjj;
  int count; char op;

  count = 0;
  for(j=ncols;j>=1;j--) {
    for(i=1;i<=nrows;i++) {
      ii=i;
      jj=j;
      op = 0;
      do {
        if (ii==row && jj==col) { 
          count++; op = 1;
          CATCHMENT[count].col=j;
          CATCHMENT[count].row=i;
        } 
        else if (BASIN[ii][jj].tocol!=0 && BASIN[ii][jj].torow!=0) { 
          iii = BASIN[ii][jj].torow;         
          jjj = BASIN[ii][jj].tocol;         
          ii  = iii;                  
          jj  = jjj;                  
          if (ii>nrows || ii<1 || jj>ncols || jj<1) op = -1;
        }                                
        else {
          op = -1;
        }
      } while (op == 0);
    }
  }

  (*number_of_cells)=count;
  printf("Upstream grid cells from present station: %d\n", (*number_of_cells));
}
/****************************************************************/
void WriteData(float *FLOW,
	       char *stn_name,
	       char *outpath,
	       DATE *date,
	       int day0,
               int day1)
// day0 and day1: the first and last day to be written (from the FLOW)
{
  FILE *fp, *fp_m;
  char filename[MIDSTRING];
  float monthly_mean, yearly_mean[13];
  int i,j,k,nmonths;
  int yr,mn,day,days_cur_mon;
  int days_counts[13]; // count days of data in multiple years for the month
  int cnt; // count days in current month (in case of not a full first/last month)

  sprintf(filename,"%s%.5s.day",outpath,stn_name);
  if((fp = fopen(filename, "wt")) == NULL) {
    printf("Cannot open %s\n",filename);
    exit(-1);
  }
  sprintf(filename,"%s%.5s.month",outpath,stn_name);
  if((fp_m = fopen(filename, "wt")) == NULL) {
    printf("Cannot open %s\n",filename);
    exit(-1);
  }

  yr = date->year; mn = date->month; day = date->day;
  days_cur_mon = DaysInMonth[mn];
  if ((mn == 2) && (IsLeapYear(yr))) days_cur_mon ++; 
    
  /* Write daily data and monthly values */
  memset((void *)days_counts, 0, sizeof(days_counts));
  memset((void *)yearly_mean, 0, sizeof(yearly_mean));
  monthly_mean = 0; cnt = 0;
  for(i = day0; i <= day1; i ++) {
    fprintf (stderr, "%4d %2d %2d %f, %d %d %d\n", yr, mn, day, FLOW[i], i, day1, day0); ///
    fprintf(fp,"%4d %2d %2d %f\n", yr, mn, day, FLOW[i]);
    monthly_mean += FLOW[i];
	      
    day ++; cnt ++;
    /* update year, month, day at the end of each month, and write monthly flow*/	      
    if (day > days_cur_mon) {
      yearly_mean[mn] += monthly_mean;
      days_counts[mn] += days_cur_mon;
      
      monthly_mean /= cnt;
      fprintf(fp_m,"%4d %2d %f\n", yr, mn, monthly_mean);
      
      day = 1; mn ++; 
      cnt = 0; monthly_mean = 0;
      if (mn > 12) {mn = 1; yr ++; }
      days_cur_mon = DaysInMonth[mn];
      if ((mn == 2) && (IsLeapYear(yr))) days_cur_mon ++;          
    }
  }
  fclose(fp); fclose(fp_m);
  
  if ((day1 - day0) < 730) return;  // less than 2 years of route data. do not write .year files
  sprintf(filename,"%s%.5s.year",outpath,stn_name);
  if((fp = fopen(filename, "w")) == NULL) {
    printf("Cannot open %s\n",filename);
    exit(-1);
  }
  for (mn = 1; mn < 13; mn ++) fprintf(fp, "%2d %f\n", mn, yearly_mean[mn] / days_counts[mn]);
  fclose(fp);    
}
