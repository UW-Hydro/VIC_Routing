/* =========================== MoscemInit.c ====================
	Intitialize MOSCEM parameters 
	Input: none
	Output: moscem - MOSCEM parameters

        Yuqiong Liu, March  2003
================================================================ */

#include "constant.h"
#include "datatype.h"
#include "utility.h"
#include "moscem.h"

void MoscemInit(Moscem* moscem, Files* files) {

     FILE *fp;
     char str[MAX_LINE_LEN];

     moscem->nPars          = NPAR;
     moscem->nInputs        = NINPUT;
     moscem->nFluxes        = NFLUX;
     moscem->nTSteps_Inputs = NTSTEP1;
     moscem->nTSteps_Fluxes = NTSTEP2;

     fp = Fopen("data/moscem.in","r");
     fscanf(fp,"%s%d", str, &moscem->nOptPar);
     fscanf(fp,"%s%d", str, &moscem->nOptObj);
#if MOD_ALGO == MULTI
     if (moscem->nOptObj <= 1) 
        PrintError("For multi-objective applications, no. of optimizing fluxes must be no less than 2.");
#endif
     fscanf(fp,"%s%d", str, &moscem->nSamples);
     fscanf(fp,"%s%d", str, &moscem->nComplex);
     fscanf(fp,"%s%d", str, &moscem->nMaxDraw);

     fscanf(fp,"%s%s", str, files->ParControlFile);
     fscanf(fp,"%s%s", str, files->ObjControlFile);
     fscanf(fp,"%s%s", str, files->InputFile);
     fscanf(fp,"%s%s", str, files->ValFile);
     fscanf(fp,"%s%s", str, files->ResCarFile);
     fscanf(fp,"%s%s", str, files->ObjOutFile);
     fscanf(fp,"%s%s", str, files->ParOutFile);
     fscanf(fp,"%s%s", str, files->CvgOutFile);
     fscanf(fp,"%s%s", str, files->RoutOutFile);

     fclose(fp);
}
