#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <float.h>
#include <math.h>
#include <string.h>
#include "rout.h"
/*********************************************/
/* SearchCatchment                           */
/* Purpose: Find number of cells upstream    */
/*          current gage location, and their */
/*          row and col number               */ 
/*********************************************/
void SearchCatchment(ARC **BASIN,
		     int **CATCHMENT,
		     int row,
		     int col,
		     int nrows,
		     int ncols,
		     int type,
		     int *number_of_cells,
		     int *upstream_cells)
{
  int i,j,k;
  int ii,jj,iii,jjj;
  int count;
  int missing=-9;
  int rank;
 
  count = 0;
  (*upstream_cells)=0;

  for(i=1;i<=nrows;i++) {
    for(j=1;j<=ncols;j++) {
      ii=i;
      jj=j;
    loop:
      if(ii>nrows || ii<1 || jj>ncols || jj<1) {
	  //printf("Outside basin %d %d %d %d\t",ii,jj,nrows,ncols);
      }
      else {
	  if(ii==row && jj==col) { 
	    count+=1;
	    CATCHMENT[count][0]=i;
	    CATCHMENT[count][1]=j;
	    CATCHMENT[count][2]=BASIN[i][j].routed; 
	  }
	  else { 
	    /*check if the current ii,jj cell routes down
	      to the subbasin outlet point, following the
	      flow direction from each cell;
	      if you get here no_of_cells increment and you 
	      try another cell*/ 
	    if(BASIN[ii][jj].tocol!=0 &&    
	       BASIN[ii][jj].torow!=0) { 
	      iii = BASIN[ii][jj].torow;         
	      jjj = BASIN[ii][jj].tocol;         
	      ii  = iii;                  
	      jj  = jjj;                  
	      goto loop;
	    }
	  }
      }
    }
  }

 (*number_of_cells)=count;
  printf("Upstream grid cells from present station: %d type %d\n", 
	 (*number_of_cells),type);

  if(type==3) { /* Irrigated part of cell */
    (*upstream_cells)=(*number_of_cells);
    (*number_of_cells)=2;
    CATCHMENT[1][0]=row;
    CATCHMENT[1][1]=col;
    CATCHMENT[1][2]=0;  
    CATCHMENT[2][0]=row;
    CATCHMENT[2][1]=col;
    CATCHMENT[2][2]=2;    
    printf("Irrigated part of cell\n");    
  }

  if(SORT) { /*Sort Catchment*/
    printf("Sorting Catchment......\n");
    count=0;
    do {
      for(i=1;i<=nrows;i++) {
	for(j=1;j<=ncols;j++) {
	  for(k=1;k<=(*number_of_cells);k++) {
	    if(CATCHMENT[k][0]==i && CATCHMENT[k][1]==j) {
	      if(BASIN[i][j].flag!=missing) {
		if(BASIN[i-1][j].direction==1) BASIN[i][j].flag=1;
		if(BASIN[i-1][j+1].direction==8) BASIN[i][j].flag=1;
		if(BASIN[i][j+1].direction==7) BASIN[i][j].flag=1;
		if(BASIN[i+1][j+1].direction==6) BASIN[i][j].flag=1;
		if(BASIN[i+1][j].direction==5) BASIN[i][j].flag=1;
		if(BASIN[i+1][j-1].direction==4) BASIN[i][j].flag=1;
		if(BASIN[i][j-1].direction==3) BASIN[i][j].flag=1;
		if(BASIN[i-1][j-1].direction==2) BASIN[i][j].flag=1;
		if(BASIN[i][j].flag==0) {
		  count++;
		  CATCHMENT[k][3]=count;
		  printf("rank: %d %d %d\n",i,j,CATCHMENT[k][3]);
		}
	      }
	    }
	  }
	  //printf("%d %d %d %d %d\n",i,j,count,nrows,ncols);
	}
      }
      for(i=1;i<=nrows;i++) { // Reset cells
	 for(j=1;j<=ncols;j++) {
	   if(BASIN[i][j].flag==0) {
	     BASIN[i][j].flag=missing;
	     BASIN[i][j].direction=missing;
	   }
	   if(BASIN[i][j].flag!=missing) BASIN[i][j].flag=0;
	 }
      }
      //if(count<250) printf("%d %d %d %d %d\n",i,j,count,nrows,ncols);
    }
    while(count<(*number_of_cells));

    for(i=1;i<(*number_of_cells);i++) {
      for(j=(*number_of_cells-1);j>=i;j--) {
	if(CATCHMENT[j-1][3]>CATCHMENT[i][3]) {
	  row=CATCHMENT[j-1][0];
	  col=CATCHMENT[j-1][1];
	  rank=CATCHMENT[j-1][3];
	  CATCHMENT[j-1][0]=CATCHMENT[j][0];
	  CATCHMENT[j-1][1]=CATCHMENT[j][1];
	  CATCHMENT[j-1][3]=CATCHMENT[j][3];
	  CATCHMENT[j][0]=row;
	  CATCHMENT[j][1]=col;	
	  CATCHMENT[j][3]=rank;	
	}
      }
    }

  } /* end if(SORT) */

}
