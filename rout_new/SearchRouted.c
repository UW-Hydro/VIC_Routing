#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <float.h>
#include <math.h>
#include <string.h>
#include "rout.h"
/***********************************************/
/* SearchRouted                                */
/* Purpose: Find already routed cells in basin */
/***********************************************/
void SearchRouted(ARC **BASIN,
		  int row,
		  int col,
		  int nrows,
		  int ncols)
{
  int i,j;
  int ii,jj,iii,jjj;
  int count;

  count = 0;

  for(i=1;i<=nrows;i++) {
    for(j=1;j<=ncols;j++) {
      ii=i;
      jj=j;
    loop:
      if(ii>nrows || ii<1 || jj>ncols || jj<1) {
	printf("Outside basin\t");
      }
      else {
	  if(ii==row && jj==col) { 
	    count+=1;
	    BASIN[i][j].routed=1;
	    BASIN[row][col].routed=2;
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
  printf("Grid cells already routed: %d\n",count);
}
