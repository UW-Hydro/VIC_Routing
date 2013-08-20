/*      PROGRAM rout, C-version   

     Routing algorithm developed by D. Lohmann.
     Rewritten from FORTRAN to C 2003, AWW/IH.

     New features, 2003 (IH):
      Upstream routed areas are not rerouted
      Dams/reservoirs included (based on power or streamflow demand)

     MAXROWS and MAXCOLS should be larger than the grid
     MAXYEARS should equal at least run length yrs+1   
     i: row from bottom (starts at 1)
     j: col from left (starts at 1)
     [][]: [row][col]
     Indexing (arrays): Starts at 1 (i.e. 0 is left unused)

make

*/

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <float.h>
#include <math.h>
#include <string.h>
#include "rout.h"
/*************************************************************/
/* Start of ROUT                                             */
/*************************************************************/
int main (int argc, char *argv[]) {   

  FILE *fp;

  ARC **BASIN=NULL; //Grid input information is stored here,
                    //like direction,fraction,velocity of 
                    //each cell in the grid. Row(i) is row from
                    //the bottom, col(j) is column from the left.
                    //Numbering starts at 1 for both i and j.
  LIST *STATION=NULL;  //Information about station locations and names.
                    //Area is included in the list, although the value
                    //isn't used in the routing program.
  TIME *DATE=NULL;

  char *filename;
  char *filename_reservoirs;
  char *inpath;
  char *fluxpath;
  char *demandpath;
  char *outpath;
  char *workpath;
  char *naturalpath;
  char *dummy;
  char *name;
  char uhstring[20][20]; //name of uhfile (or "NONE")

  float xllcorner; //x-coordinate, lower left corner of grid
  float yllcorner; //y-coordinate, lower left corner of grid
  float size;      //size of grid cell, in degrees
  float lat;
  float lon;
  float value;
  float test;
  float factor_sum;
  float ***UH;     /* Impulse response function UH[row][col][48] */
                   /* Based on Lohmann's Tellus article          */
		   /* Depends on velocity, diffusion and size    */  
  float **UH_BOX;  /* Unit hydrograph[number_of_cells][12]       */
                   /* Normally the 12 numbers are equal to       */
                   /* the ones in the unit hydrograph file       */ 
  float **UH_DAILY; /* UH_DAILY[number_of_cells][uh_day]         */
  float **UH_S;    /* .uh_s grid, UH_S[numberofcells][ke+uh_day] */
  float **FR;
  double *BASEFLOW;  
  double *RUNOFF;
  double *FLOW;
  float *R_FLOW;
  float *STORAGE;
  float *LEVEL;
  float *PowerProd; /* Numbers from moscem, unit MW */
  float **RESEVAPDATA;
  float **WATER_DEMAND; /* Water demand (irrigation) for reservoirs */

  int **CATCHMENT; /*A list with row and col number for all cells upstream 
                     station location (includes station location). 
                     CATCHMENT[cellnumber][row(0)/col(1)/routed(2)]
                     The order of the list is arbitrary. 
                     CATCHMENT[cellnumber][2]=0 means routing as normal
                     CATCHMENT[cellnumber][2]=1 means this cell is routed before, in current routing
                     CATCHMENT[cellnumber][2]=2 means flow already exist for that
                     cell (routed some time previously). */
  int i,j;
  int nr;
  int nrows,ncols;     //number of rows/columns in basin 
                       //(read from direction file)
  int active_cells;    //total number of active cells in grid
  int number_of_cells; //number of cells upstream a station location
  int upstream_cells;			   
  int number_of_stations; //number of stations 
  int decimal_places;  //decimal places in VIC output 
  int start_year,start_month; //start of VIC simulation
  int stop_year,stop_month;
  int first_year,first_month; //start of output to be written
  int last_year,last_month;
  int ndays,nmonths;   //number of days and months to be routed 
  int skip;            //number of days to skip in VIC simulations
  int irr_rout;        //irrigation type routing    
  int res_rout;        //reservoir routing should be included	   
  int irow,icol;			   
  int missing;
  int demand;
  int basin_number;    //basin number...  
  int nbytes=54; //nbytes pr day in flux files used in ReadDataForReservoirEvaporation. bad programming....should not be hardcoded!!! Tian changed it to 54 Apr 2013
  /***********************************************************/

  if(argc != 2){
   printf("USAGE:  rout <infile>\n");
   exit(0);
  }
  if((fp = fopen(argv[1], "r")) == NULL) {
   printf("Cannot open %s\n",argv[1]);
   exit(1);
  }

  /* Allocate memory for input parameters/names */
  filename = (char*)calloc(100,sizeof(char));
  filename_reservoirs = (char*)calloc(100,sizeof(char));
  fluxpath = (char*)calloc(100,sizeof(char));
  inpath = (char*)calloc(100,sizeof(char));
  demandpath = (char*)calloc(150,sizeof(char));
  outpath = (char*)calloc(100,sizeof(char));
  workpath = (char*)calloc(100,sizeof(char));
  naturalpath = (char*)calloc(100,sizeof(char));
  dummy = (char*)calloc(100,sizeof(char));
  name = (char*)calloc(100,sizeof(char));

  /* Find basin number  */
  fgets(dummy, MAXSTRING, fp);
  fscanf(fp, "%*s %d",&basin_number);

  /* Find number of rows and cols in grid of interest */
  fscanf(fp, "%*s %s",filename);
  FindRowsCols(filename,&nrows,&ncols,&xllcorner,
               &yllcorner,&size,&missing);

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
  ReadDirection(filename,BASIN,nrows,ncols,&active_cells,missing);
  printf("Active cells in basin: %d\n",active_cells);

  /* Allocate memory for CATCHMENT, UH_BOX, UH_S, 
                         UH_DAILY, STATION */
  CATCHMENT = (int**)calloc(active_cells+1,sizeof(int*));
  for(i=0;i<=active_cells+1;i++) 
    CATCHMENT[i]=(int*)calloc(4,sizeof(int));
  UH_BOX=(float**)calloc(active_cells+1,sizeof(float*));
  for(i=0;i<=active_cells+1;i++) 
      UH_BOX[i]=(float*)calloc(KE+1,sizeof(float*));      
  UH_DAILY=(float**)calloc(active_cells+1,sizeof(float*));
  for(i=0;i<=active_cells+1;i++) 
      UH_DAILY[i]=(float*)calloc(UH_DAY+1,sizeof(float));      
  UH_S=(float**)calloc(active_cells+1,sizeof(float*));
  for(i=0;i<=active_cells+1;i++) 
      UH_S[i]=(float*)calloc(KE+UH_DAY+1,sizeof(float));      
  STATION=calloc(20,sizeof(LIST));
  DATE=calloc(DAYS,sizeof(TIME));

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
  //if(value<EPS) 
  ReadXmask(filename,BASIN,nrows,ncols);
  /*else {
    printf("Xmask: %.2f\n",value);
    for(i=1;i<=nrows;i++)
      for(j=1;j<=ncols;j++) 
        BASIN[i][j].xmask=value;
	}*/

  /* Read fraction file */
  fscanf(fp, "%*s %s ",filename);
  value= atof(filename);
  //if(value<EPS) 
  ReadFraction(filename,BASIN,nrows,ncols);
  /*else {
    printf("Fraction: %.2f filename: %s %f\n",value,filename);
    for(i=1;i<=nrows;i++)
      for(j=1;j<=ncols;j++) 
        BASIN[i][j].fraction=value;
	}*/

  /* Read routing information 
     irr_rout=0: Nonirrigated part of cell, or entire cell
     irr_rout=1: Irrigated part of cell */
  fscanf(fp, "%*s %d",&irr_rout); 

  /* Read information on previously routed grid cells */
  fscanf(fp, "%*s %s",filename);
  value=atof(filename);
  if(value<EPS) {
    ReadRouted(filename,BASIN,nrows,ncols);
  }
  else {
    for(i=1;i<=nrows;i++)
      for(j=1;j<=ncols;j++) 
        BASIN[i][j].routed=0;
    printf("Routed cells: %s not found, setting values to zero\n",
	   filename);    
  }

  /* Include reservoir routing? 1: yes, 0: no */ 
  fscanf(fp, "%*s %d",&res_rout); 

  /* Read reservoir file name */
  fscanf(fp, "%*s %s",filename_reservoirs);
  printf("Reservoir file: %s\n",filename_reservoirs);
  value=atof(filename_reservoirs);
  if(value<EPS && res_rout==1) ReadReservoirs(filename_reservoirs,nrows,ncols,BASIN);
  else {
    printf("Reservoirs not taken into account\n");
    for(i=1;i<=nrows;i++)
      for(j=1;j<=ncols;j++) 
        BASIN[i][j].resloc=0;
  }

  /* Read station file */
  fscanf(fp, "%*s %s",filename);
  printf("Station file: %s\n",filename);
  ReadStation(filename,BASIN,STATION,nrows,ncols,
	      uhstring,&number_of_stations);
  printf("Station file: %s %d\n",filename,number_of_stations);
  /* Read input paths and precision of VIC filenames, and output path */
  fscanf(fp, "%*s %s",fluxpath); 
  fscanf(fp, "%*s %s",inpath);
  fscanf(fp, "%*s %d",&decimal_places);
  fscanf(fp, "%*s %s",demandpath); //filepath to water demands, reservoirs
  fscanf(fp, "%*s %d",&demand);   //0/1: do not/do take demands into account
  fscanf(fp, "%*s %s",outpath);
  fscanf(fp, "%*s %s",workpath);
  fscanf(fp, "%*s %s",naturalpath); //filepath to routing results, naturalized simulations

  printf("Input file path: %s\n",inpath);
  printf("Decimal places: %d\n",decimal_places);
  printf("Demand path: %s\n",demandpath);
  printf("Demand: %d\n",demand);
  printf("Output file path: %s\n",outpath);
  printf("Working files path: %s\n",workpath);
  printf("Naturalized simulations: %s\n",naturalpath);

  /* Read start and end year/month from VIC simulation */
  fscanf(fp, "%*s %d %d %d %d",
	 &start_year,&start_month,&stop_year,&stop_month);

  /* Read start and end year/month for writing output */
  fscanf(fp, "%*s %d %d %d %d",
	 &first_year,&first_month,&last_year,&last_month);
  printf("Start input: %d %d  Start output: %d %d  \n",
	 start_year,start_month,first_year,first_month);  

  /* Calculate number of days & months to be routed, 
     and number of days to skip when reading VIC simulation 
     results */
  CalculateNumberDaysMonths(start_year,start_month,
                            first_year,first_month,
                            last_year,last_month, 
                            &skip,&ndays,&nmonths);
  printf("Output Start: %d %d  End: %d %d  Skip: %d Ndays: %d Nmonths:%d\n",
	 first_year,first_month,last_year,last_month,skip,ndays,nmonths);  

  /* Allocate memory for BASEFLOW, RUNOFF and OUTPUT arrays */
  RUNOFF=(double*)calloc(ndays+1,sizeof(double));
  BASEFLOW=(double*)calloc(ndays+1,sizeof(double));
  FLOW=(double*)calloc(ndays+1,sizeof(double));
  R_FLOW=(float*)calloc(ndays+1,sizeof(float));
  STORAGE=(float*)calloc(ndays+1,sizeof(float));
  LEVEL=(float*)calloc(ndays+1,sizeof(float));
  PowerProd=(float*)calloc(ndays+1,sizeof(float));
  RESEVAPDATA=(float**)calloc(ndays+1,sizeof(float*));
  for(i=0;i<ndays+1;i++) 
    RESEVAPDATA[i]=(float*)calloc(5,sizeof(float));   
  WATER_DEMAND=(float**)calloc(stop_year-start_year+1,sizeof(float*));
  for(i=0;i<stop_year-start_year+1;i++) 
    WATER_DEMAND[i]=(float*)calloc(13,sizeof(float));   

  /* Read name of uh-file */
  fscanf(fp, "%*s %s",filename);
  fclose(fp);

  /* Make impulse response function (UH). */
  /* Based on Lohmann's Tellus article    */
  printf("Making impulse response function.....UH[row][col][48]\n");
  MakeUH(UH,BASIN,nrows,ncols);

  /* Loop over required output stations, 
     rout fluxes and write to output files */  
  for(nr=1;nr<=number_of_stations;nr++) {
    for(j=1;j<=ndays;j++) {
	R_FLOW[j]=0.;
    }
    if(STATION[nr].id==1) {
        printf("\n\nSearching catchment..Location: row %d col %d\n",
               STATION[nr].row,STATION[nr].col);
        SearchCatchment(BASIN,CATCHMENT,STATION[nr].row,
                        STATION[nr].col,nrows,ncols,
                        STATION[nr].type,&number_of_cells,
			&upstream_cells);

        /* Read grid UH, UH_BOX[number_of_cells][12] */
        printf("Read grid UH_BOX...%s\n",filename);
        ReadGridUH(filename,UH_BOX,number_of_cells,
                   CATCHMENT);

        /* Make .uh_s-file if it doesn't exist, 
			     UH_S[numberofcells][ke+uh_day] */
        printf("Make grid UH_S...\n");
        MakeGridUH_S(BASIN,CATCHMENT,STATION,number_of_cells,nr,
	           UH_DAILY,UH,FR,UH_BOX,UH_S,uhstring[nr]);
 
        /* Make convolution. This is where the VIC fluxes are read. */
        printf("Make convolution...\n");
        MakeConvolution(number_of_cells,skip,ndays,CATCHMENT,
                        BASIN,BASEFLOW,RUNOFF,FLOW,
  		        UH_S,STATION,nr,
			xllcorner,yllcorner,size,
		        inpath,outpath,workpath,decimal_places,DATE,
			&factor_sum,first_year,first_month,irr_rout,
			missing);

	/* Find lat and lon of station */
	lat=yllcorner + STATION[nr].row*size - size/2.0;
        lon=xllcorner + STATION[nr].col*size - size/2.0;

	/* Reservoir routing if needed */
	if(!res_rout && !irr_rout) STATION[nr].type=1;
	if(STATION[nr].type==2) {
	  printf("Reservoir routing.....\n");
	  ReadDataForReservoirEvaporation(fluxpath,RESEVAPDATA,
	  				  lat,lon,skip,nbytes,ndays,
	  				  decimal_places); 
	  /* NB! Above function assumes daily time step!!! must be customized for your use!!! Or made flexible.... */
	  ReservoirRouting(filename_reservoirs,demandpath,
			   FLOW,R_FLOW,STORAGE,LEVEL,PowerProd,
			   WATER_DEMAND,RESEVAPDATA,DATE,STATION[nr].name,
	  		   STATION[nr].row,STATION[nr].col,
			   start_year,stop_year,ndays,demand,
			   basin_number,naturalpath);
	}

        /* Write data */
	printf("Writing data...\n");
        strcpy(name,STATION[nr].name);
        WriteData(FLOW,R_FLOW,STORAGE,LEVEL,PowerProd,
		  name,outpath,ndays,nmonths,
		  DATE,factor_sum,
                  first_year,first_month,last_year,
		  last_month,start_year,lat,lon,STATION[nr].type,
		  decimal_places);
	if(irr_rout==1) {
	  for(i=3;i<=upstream_cells;i++) {
	    irow=CATCHMENT[i][0];
	    icol=CATCHMENT[i][1];
	    BASIN[irow][icol].routed=1;	
	  }
	}
    }
  }

  /* Make new 'routed' file */
  MakeRoutedFile(BASIN,CATCHMENT,nrows,ncols,number_of_cells); 
 
  /* Make new direction file */
  MakeDirectionFile(BASIN,CATCHMENT,nrows,ncols,number_of_cells,
		    xllcorner,yllcorner,size,missing); 

  /* Free memory */
  for(i=0;i<=nrows;i++) {
    free(BASIN[i]);
    free(UH[i]);
   }
  free(BASIN);
  free(UH);

  /*  for(i=0;i<=active_cells+1;i++) 
    free(CATCHMENT[i]);
  free(CATCHMENT); 

  free(DATE);
  free(STATION);
  free(RUNOFF);
  free(BASEFLOW);
  free(FLOW);
  free(R_FLOW);
  free(STORAGE);
  free(LEVEL);*/

  return 0;
}
