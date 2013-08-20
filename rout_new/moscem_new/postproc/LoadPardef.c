/*------------------------------------------------------
   Read in the default parameter set

   Yuqiong Liu, March 2003
-------------------------------------------------------*/

#include <stdio.h>
#include "constant.h"
#include "datatype.h"
#include "utility.h"

void LoadPardef(Moscem *moscem, Files *files, double *xpar) 
{
 
    int i, tmp1;
    FILE *fp1;
    char str[MAX_LINE_LEN];
    double tmp2;

    fp1 = Fopen(files->ParFile, "r");

    fgets(str, MAX_LINE_LEN, fp1);

    for (i=0;i<moscem->nPars; i++)  {
       fscanf(fp1, "%d%s%lf%lf%lf%d", &tmp1, str, &xpar[i], &tmp2, &tmp2, &tmp1);
       fgets(str,  MAX_LINE_LEN, fp1);
    }

    fclose (fp1);
}
