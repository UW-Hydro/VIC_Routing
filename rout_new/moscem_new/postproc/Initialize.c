/* =========================== Initialize.c ====================
	Intitialize parameters, read input/output file names 

        Yuqiong Liu, March 2003
================================================================ */

#include <stdio.h>
#include "constant.h"
#include "datatype.h"
#include "utility.h"

void Initialize(Moscem* moscem, Files* files) {

     FILE *fp;
     char str[MAX_LINE_LEN];
     int i;

     moscem->nPars          = NPAR;
     moscem->nInputs        = NINPUT;
     moscem->nFluxes        = NFLUX;
     moscem->nTSteps_Inputs = NTSTEP1;
     moscem->nTSteps_Fluxes = NTSTEP2;

     fp = Fopen("data/postproc.in","r");
     fscanf(fp,"%s%d", str, &moscem->iMod);
     fscanf(fp,"%s%d", str, &moscem->nParIdx);
     fscanf(fp,"%s%d", str, &moscem->nSamples);
     
     moscem->ObjMax = DoubleVector(moscem->nFluxes);
     fscanf(fp,"%s", str);
     for (i=0;i<moscem->nFluxes;i++) fscanf(fp,"%lf", &(moscem->ObjMax[i]) );

     fscanf(fp,"%s%s", str, files->InputFile);
     fscanf(fp,"%s%s", str, files->ValFile);
     fscanf(fp,"%s%s", str, files->ParFile);
     fscanf(fp,"%s%s", str, files->ParOptFile);
     fscanf(fp,"%s%s", str, files->ObjFile);
     fscanf(fp,"%s%s", str, files->TsOutFile);
     fscanf(fp,"%s%s", str, files->ObjOutFile);
     fscanf(fp,"%s%s", str, files->TrdoffFile);

     fclose(fp);
}

