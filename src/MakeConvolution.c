#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "rout.h"
/**************************************/
/* MakeConvolution                    */ 
/**************************************/
void MakeConvolution(int number_of_cells,
                     int skip,
		     int ndays,
		     int **CATCHMENT,
		     ARC **BASIN,
		     double *BASEFLOW,
		     double *RUNOFF,
		     double *FLOW,
		     float **UH_S,
		     LIST *STATION,
		     int nr,
		     float xllcorner,
		     float yllcorner,
		     float size,
		     char *inpath,
		     char *outpath,
		     char *workpath,
		     int decimal_places,
		     TIME *DATE,
		     float *factor_sum,
		     int first_year,
		     int first_month,
		     int irr_rout,
		     int missing)
{
  FILE *fp;
  int i,j,n,ii,jj;
  int row,col;
  double factor;
  char infile[500];
  char LATLON[50];
  char fmtstr[17];
  char leftover[MAXSTRING];
  double area,area_sum;
  double radius;
  float k_const;
  float dummy;
  float lat,lon;

  k_const=1.0;
  area_sum=(*factor_sum)=0.0;

  for(i=1;i<=ndays;i++) 
    FLOW[i] = 0.0;
  
  for(n=1;n<=number_of_cells;n++) { //the gridcell loop
    for(i=1;i<=ndays;i++) {
      RUNOFF[i]=0.0;
      BASEFLOW[i]=0.0;
    }
    
    ii = CATCHMENT[n][0];  //row from bottom 
    jj = CATCHMENT[n][1]; //col from left
    lat=yllcorner + ii*size - size/2.0;
    lon=xllcorner + jj*size - size/2.0;
    //printf("Makeconv cellnumber%d row%d col%d %f %f\n",n,ii,jj,lat,lon);
    
    /* Give area of box in square kilometers */
    radius = (double)EARTHRADIUS;
    area = radius*radius*fabs(size)*PI/180*            
      fabs(sin((lat-size/2.0)*PI/180)-     
	   sin((lat+size/2.0)*PI/180));
    //if(CATCHMENT[n][2]!=2) 
    area_sum += area;
    printf("area_sum=%f area=%f routed %d\n",area_sum,area,BASIN[ii][jj].routed);

    /* Find conversion factor for mm/day to m3/s
       and multiply by cell fraction */
    factor = BASIN[ii][jj].fraction*area/86.4;  
    (*factor_sum) += factor;

    if(BASIN[ii][jj].routed==0 || 
       (irr_rout==1 && CATCHMENT[n][2]==0)) {
      /* Make vic filename */
      strcpy(infile,inpath);
      sprintf(fmtstr,"%%.%if_%%.%if",
	      decimal_places,decimal_places);
      sprintf(LATLON,fmtstr,lat,lon);
      strcat(infile,LATLON);
      printf("File %d of %d: %s\n",n,number_of_cells,infile);

      if((fp = fopen(infile,"r"))==NULL) { 
	printf("Cannot open file, inserting zeroes...%s \n",infile);
	for(i=1;i<=ndays;i++) {
	  DATE[i].year=0;
	  DATE[i].month=0;
	  DATE[i].day=0;
	  RUNOFF[i]=0.;
	  BASEFLOW[i]=0.;
	}
	//printf("Cannot open file, exiting...%s \n",infile);
	//exit(0);
      }
      else {
	/* Read VIC model output: 
	   <year> <month> <day> <p> <et> <runoff> <baseflow>*/
	for(i=1;i<=skip;i++) 
	  fgets(leftover,MAXSTRING,fp); 
	for(i=1;i<=ndays;i++) {
	  fscanf(fp,"%d %d %d %lf %lf ",
		 &DATE[i].year,&DATE[i].month,&DATE[i].day,
		 &RUNOFF[i],&BASEFLOW[i]);
	  /* Check to be sure dates in VIC file start at same time 
	     specified in input file */
	  if(i == 1) {
	    if(DATE[i].year!=first_year || DATE[i].month!=first_month) {
	      printf("VIC output file does not match specified\n");
	      printf("period in input file.%d %d %d %d\n",DATE[i].year,first_year,DATE[i].month,first_month);
	      exit(2);
	    }
	  }
	}
	fclose(fp);
      }
    }
    else {
      if(BASIN[ii][jj].routed==2) {
	/* Flow already routed at this location, read from file */
        printf("MakeConvolution, flow already routed, read from file\n");
	strcpy(infile,workpath);
	sprintf(fmtstr,"streamflow_%%.%if_%%.%if",decimal_places,decimal_places);
	sprintf(LATLON,fmtstr,lat,lon);
	strcat(infile,LATLON);
	printf("File %d of %d: %s\n",n,number_of_cells,infile);
	
	if((fp = fopen(infile,"r"))==NULL) { 
	  printf("Cannot open file %s \n",infile);
	  exit(0);
	}
	else {
	  /* Read routed output (m3/s): 
	     <year> <month> <day> <total runoff> */
	  for(i=1;i<=ndays;i++) {
	    fscanf(fp,"%d %d %d %lf ",
		   &DATE[i].year,&DATE[i].month,&DATE[i].day,
		   &RUNOFF[i]);
	    BASEFLOW[i]=0;
	    /* Check to be sure dates in routed file start at same time 
	       as specified */
	    if(i == 1) {
	      if(DATE[i].year!=first_year || DATE[i].month!=first_month) {
		printf("Routed output file does not match specified\n");
		printf("period.\n");
		exit(2);
	      }
	    }
	  }
	  fclose(fp);
	}	 
      }
    }

    if(BASIN[ii][jj].routed!=1) {
      if(BASIN[ii][jj].routed==0 || CATCHMENT[n][2]==0) {
	for(i=1;i<=ndays;i++) {
	  RUNOFF[i] = RUNOFF[i]*factor;
	  BASEFLOW[i] = BASEFLOW[i]*factor;
	}
      }
      for(i=1;i<=ndays;i++) {
	for(j=1;j<KE+UH_DAY;j++) {
	  if((i-j+1)>=1) {
	    FLOW[i] += UH_S[n][j]*(BASEFLOW[i-j+1]+RUNOFF[i-j+1]);
	  }
	}
      }
    }

    row=CATCHMENT[n][0];
    col=CATCHMENT[n][1];
    if(row==STATION[nr].row && col==STATION[nr].col) 
       BASIN[row][col].routed=2;
    else {
      BASIN[row][col].direction=missing;
      BASIN[row][col].routed=1;
    }
  } /* End gridcell loop */
}
