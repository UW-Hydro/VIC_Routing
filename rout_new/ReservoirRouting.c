/* FLOW og R_FLOW are in m3s-1, but all calculations are in m3 and m3day-1 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "rout.h"


void ReservoirRouting(char *filename,
                      char *demandpath,
		      double *FLOW,
		      float *R_FLOW,
		      float *STORAGE,
		      float *LEVEL,
		      float *PowerProd,
                      float **WATER_DEMAND,
		      float **RESEVAPDATA,
		      TIME *DATE,
		      char *name,
		      int row,
		      int col,
		      int start_year,
		      int stop_year,
		      int ndays,
		      int demand,
		      int basin_number,
		      char *naturalpath)
{
  FILE *fp,*fin,*fmin,*fres,*fmoscem;

  char resname[25];
  char purpose[7];
  char fmtstr[17];
  char file_name[100];
  char *purpose1,*purpose2,*purpose3;
  char *purptest;
  const char *rescarname = "rescarfile.txt";
  const char *inname = "infile.txt";
  const char *outputname = "output/output.day";
  
  double oldstorage,oldflow;
  double temp_storage;
  double release,surplus_storage,spill;
  double possible_flow;
  double *res_evap; /* m/day */
  double K_E;

  float dummy;
  float capacity; /* input capacity must be given in m3 */
  float *max_release;
  float min_head=0;
  float *power_demand;  /* Input demand must be given in MW */
  float *water_demand; 
  float *outflow; /* m3 (daily) */
  float *minflow; /* m3 (daily) */
  float *avail_water;  /* m3 */
  float *waterdemanddaily; /*(m3 daily)*/
  float *storage;  /* Read in Mm3, multiply by 1000, use in m3 */
  float *flow_accumulated;  /* m3 (daily) */
  float *res_evap_accumulated;  /* m3 (daily) */
  float storage_optimal_start_hyd_year = 0.8;
  float storage_optimal_start_hyd_year_flood = 0.8;
  float storage_min_start_hyd_year = 0.6;
  float storage_fraction;
  float power_prod;
  float head;
  float mean_inflow; /* m3 (daily) */
  float month_inflow[13]; /* m3 month-1 */
  float year_mean_inflow;
  float this_year_inflow;
  float mean_flood;
  float this_year_demand,dummy_demand;
  float year_mean_water_demand;
  float fraction,factor;
  float q7_10; /* m3s-1 */
  float end_storage,start_storage;
  float available;
  float tot_res_evap=0;
  float inst_cap; /* in MW */
  float dummy_inst_cap;
  float surf_area;  /* input surface area is given in 1000 m2 */
  float latitude;
  float longitude;

  int i; /* i runs from 0 to ndays */
  int j; /* j runs from 0 to 365+leapyear */
  int k; /* k also runs from 0 to 365+leapyear */
  int m; /* m runs from 1 to 12 */
  int yy,year,month,day;
  int height;
  int irow,icol,type,start_month,end_month;
  int DaysInMonth[13] = { 0,31,28,31,30,31,30,31,31,30,31,30,31 };
  int oldmonth,newmonth;
  int test,leapyear;
  int purp_irr;
  int purp_flood;
  int purp_hydro;
  int moscem;

  moscem=1;
	
  if((fp = fopen(filename, "r")) == NULL) {
    printf("Cannot open %s (ReservoirRouting)\n",filename);
    exit(1);
  }
  else printf("File opened, reservoir routing information: %s %s\n",filename,name);

  year=0;

  /* Allocate memory */
  res_evap=(double*)calloc(ndays+1,sizeof(double));
  outflow=(float*)calloc(ndays+1,sizeof(float));
  minflow=(float*)calloc(ndays+1,sizeof(float));
  storage=(float*)calloc(ndays+1,sizeof(float));
  max_release=(float*)calloc(ndays+1,sizeof(float));
  avail_water=(float*)calloc(ndays+1,sizeof(float));
  flow_accumulated=(float*)calloc(ndays+1,sizeof(float));
  power_demand=(float*)calloc(13,sizeof(float));
  water_demand=(float*)calloc(13,sizeof(float));
  res_evap_accumulated=(float*)calloc(367,sizeof(float));
  waterdemanddaily=(float*)calloc(367,sizeof(float));


  /* Find reservoir located in current cell */
  //  while(fscanf(fp,"%d %d %f %f %*d %s %*d %f %f %*f %f %*d %*d %d %s ",
  //	       &icol,&irow,&longitude,&latitude,resname,&capacity,&surf_area,&inst_cap,&height,purpose)!=EOF) {
  while(fscanf(fp,"%d %d %f %f %*d %s %*d %f %f %*f %f %*d %*d %d ",
  	       &icol,&irow,&longitude,&latitude,resname,&capacity,&surf_area,&inst_cap,&height)!=EOF) {

    fgets(purpose,7,fp);

    capacity*=1000.; //go from Mm3 to m3
    surf_area*=1000.; //go from 1000m2 to m2. 
    
    if(irow==row && icol==col) { /* Reservoir found */

      //printf("ReservoirRouting row%d col%d name:%s capacity%f (m3) inst_cap%f height%d surfarea:%f (m2)\n",
      //	     row,col,resname,capacity,inst_cap,height,surf_area);

      if(surf_area == 0) surf_area=1e6*exp(log(capacity/(1e6*9.208))/1.114); /* Takeuchi, 1997 */
      if(capacity/(surf_area*height) > 2 ) surf_area=1e6*exp(log(capacity/(1e6*9.208))/1.114); /* Some ICOLD areas seem to be unrealistically small. Possible unit errors */

      printf("ReservoirRouting row%d col%d name:%s capacity%f (m3) inst_cap%f height%d surfarea:%f (m2)\n",
	     row,col,resname,capacity,inst_cap,height,surf_area);

      /* Initialize reservoir and waterdemands */
      for(i=1;i<=ndays;i++) max_release[i]=0.;
      for(i=0;i<stop_year-start_year+1;i++) 
	for(j=0;j<13;j++) WATER_DEMAND[i][j]=0.;

      purp_irr=purp_flood=purp_hydro=0;
      purpose1 = strchr(purpose,'I');
      purpose2 = strchr(purpose,'H');
      purpose3 = strchr(purpose,'C');
      if(purpose1 != NULL ) purp_irr=1;
      if(purpose2 != NULL ) purp_hydro=1;
      if(purpose3 != NULL ) purp_flood=1;
      purp_flood=0; //because in this version you do not allow outflow larger than mean annual flood anyway.
      //printf("\tpurp_irr=%d purp_flood=%d purp_hydro=%d\n",purp_irr,purp_flood,purp_hydro);

      storage[0]=storage_optimal_start_hyd_year*capacity; //m3
      if(purp_flood==1) storage_optimal_start_hyd_year=storage_optimal_start_hyd_year_flood; 
      storage_fraction=storage_optimal_start_hyd_year;
      oldstorage=storage[0];

      printf("\tCapacity %.1f Storage at first time step: %.1f m3 fraction %.2f purpose:%s\n",capacity,storage[0],storage[0]/capacity,purpose);
      printf("\tStartYear %d Stopeyar %d ndays %d\n",start_year,stop_year,ndays);
     
      /* Read water demands if appropriate. [nyears][month]. year starts at 0, month goes from 1 to 12
	 Only happens when you have irrigation = TRUE or FREE and purp_irr==1
	 Numbers in file you read are in m3s-1, but are recalculated to m3day-1 */
      if(demand && purp_irr==1) ReadWaterDemand(demandpath,resname,WATER_DEMAND,start_year,stop_year,ndays);

      /* Calculate mean inflow for the entire simulation period 
         (annual and monthly). Calculated based on current routing simulation */
      CalculateMeanInflow(FLOW,DATE,ndays,&mean_inflow,flow_accumulated,month_inflow);

      printf("ReservoirRouting.c Mean annual inflow:%f (m3day-1), or %f (m3/s) which is %.2f times the storage capacity\n", 
	     mean_inflow,mean_inflow/CONV_M3S_CM,mean_inflow*365/capacity);

      /* If instcap is not given, or outside "reasonable" limits: Set to mean_inflow * height..... hm. ok assumption???? */
      printf("ReservoirRouting.c instcap:%f with mean inflow:%f\n",inst_cap,mean_inflow/CONV_M3S_CM*height*EFF*GE/1000.);
      dummy_inst_cap=mean_inflow/CONV_M3S_CM*height*EFF*GE/1000.; //MW
      //if(inst_cap<0.1 || dummy_inst_cap/inst_cap>4 || dummy_inst_cap/inst_cap<0.25) //if commented out: alway use instcap=meaninflow*height 
      inst_cap=mean_inflow/CONV_M3S_CM*height*EFF*GE/1000.; //MW 

      /* Find time of year when reservoir filling is likely to be at its
	 highest and lowest. Highest defines start of operational year. 
         In addition, the mean annual flood is calculated 
	 Based on calculated naturalized flow 
      */
      FindStartOfOperationalYear(&mean_flood,mean_inflow,
				 ndays,name,naturalpath,
				 &start_month,&end_month,
				 latitude,longitude);
  
      /* Calc 7Q10 (m3day-1), based on simulated daily naturalized flow for entire simulation period.
	 This value is set as minimum water release from reservoir
      */
      q7_10=Find7Q10(ndays,stop_year-start_year,name,basin_number,naturalpath,latitude,longitude);
      printf("ReservoirRouting.c Start of operational year: %d end dry period:%d\n",
	     start_month,end_month); 
      printf("ReservoirRouting.c mean_flood:%f (m3day-1) or %f (m3/s) q710:%f (m3/day) or %f (m3/s) start:%d stop:%d\n",
	     mean_flood,mean_flood/CONV_M3S_CM,q7_10,q7_10/CONV_M3S_CM,start_year,stop_year); 
      
      /* Calculate inflow and demand and outflow.
	 One operational year at a time. 
	 i.e. start at beginning of operational year.
	 Reservoir evaporation taken into account in optimization scheme.
	 i: #day in the entire simulatio period (1-ndays)
	 j: #day in current year (operational year) (0-365/366)
	 k: month-counter
      */ 

      for(i=1;i<=ndays;i++) {
     	yy=DATE[i].year;
     	month=DATE[i].month;
	year_mean_inflow=0.;
	j=0;

	if(i==1) {
	  oldstorage=storage[0];
	  storage[i]=oldstorage;
	}
	else storage[i]=storage[i-1]; 

	avail_water[i]=storage[i];

	if(month!=start_month || i>(ndays-365)) { 
	  /* Not yet approached beginning of first operational year,
	     or after the last operational year.
	     Let inflow go directly to outflow. 
	  */
	  outflow[i]=FLOW[i]*CONV_M3S_CM; //m3day-1
          storage[i]=oldstorage;
	  oldstorage=storage[i];
	  head=height-(capacity-storage[i])/surf_area;
	  R_FLOW[i]=outflow[i]/CONV_M3S_CM; /*R_FLOW in m3s-1 */
	  STORAGE[i]=storage[i]; /* storage is in m3 */
	  LEVEL[i]=head;
 	} /* end if not yet at beginning of operational year */

	else { /*Start calculations for the coming operational year.
		 Calculating inflow the coming operational year 
		 At this point q7_10 is only known outflow */

	  printf("ReservoirRouting start calculating operational year:%d julianday=%d storage=%f availwater1=%f \n",yy,i,storage[i],avail_water[i]);
	  
	  if(month<=2) leapyear=IsLeapYear(yy);
	  else leapyear=IsLeapYear(yy+1);
	  
	  for(j=0;j<365+leapyear;j++) {	    /* Calculate next year's inflow, reservoir evaporation,
					       and available water each day in the coming year.
					       Reservoir evaporation calculated in VIC. 
					       Taken from naturalized VIC simulations 
					       RESEVAPDATA[i][0]=actual evaporation in cell, before reservoir (mm/day)
					       RESEVAPDATA[i][1]=airtemp (C)
					       RESEVAPDATA[i][2]=wind (m/s)
					       RESEVAPDATA[i][3]=potential lake evaporation (mm/day) */
	    //if(RESEVAPDATA[i+j][0] > RESEVAPDATA[i+j][3]) 
	      res_evap[i+j]=0.;
	      //else 
	      //res_evap[i+j]=(RESEVAPDATA[i+j][3]-RESEVAPDATA[i+j][0])/1000.;  
	                                                                        /* m/day. additional evaporation caused 
										by reservoir (mm/day) from vic simulations. 
										i.e. what is extracted is additional evap caused by
									        conversion to open water. Nothing is added/subtracted if VIC grid cell
										evapotranspiration is higher than open water evaporation. */
	    tot_res_evap+=res_evap[i+j]; // m
	    if(j>0) res_evap_accumulated[j]+=res_evap_accumulated[j-1]+res_evap[i+j]*surf_area; // m3 ok?
	    else res_evap_accumulated[j]=res_evap[i+j]*surf_area; // m3 ok?
	    year_mean_inflow+=FLOW[i+j]*CONV_M3S_CM; // FLOW in m3s-1
	    if(j==0) avail_water[i+j]=storage[i+j]+FLOW[i+j]*CONV_M3S_CM-q7_10-surf_area*res_evap[i+j]; // m3
	    else avail_water[i+j]=avail_water[i+j-1]+FLOW[i+j]*CONV_M3S_CM-q7_10-surf_area*res_evap[i+j]; // m3
	  } // end inflow and reservoir evaporation calculations coming year ......
	  
	  year_mean_inflow/=j; /*units m3day-1, i.e. mean inflow the coming year */
	  printf("ReservoirRouting.c C year %d month %d inflow %f i=%d start_month=%d totresevap (accumulated for alle years up to now, in meters):%lf\n",
		 yy,month,year_mean_inflow,i,start_month,tot_res_evap);
	  
          /* First: Set irrigation water demands = 0 m3day-1 */
	  year_mean_water_demand=0;
	  this_year_demand=0;
	  for(k=1;k<=12;k++)	    
	    water_demand[k]=0.;
	  
	  for(k=start_month;k<=12;k++) year_mean_water_demand+=(WATER_DEMAND[yy-start_year][k])*DaysInMonth[k]/(365+leapyear);
	  for(k=1;k<start_month;k++) year_mean_water_demand+=(WATER_DEMAND[yy+1-start_year][k])*DaysInMonth[k]/(365+leapyear);
	  fraction = year_mean_water_demand/(year_mean_inflow-q7_10); //All these numbers are in m3 day-1

	  /* Split possible deficit (demand - inflow - 7q10) between outflow og storage. 
	     If reservoir more than 80% full: Everything is taken from storage. 
	     If  <80%: Half of it taken from storage and half from release (i.e avialable water lower than demand).
             Minimum storage at end = 60 percent 
	  */
	  if(fraction>1) { /*Demand higher than inflow - allow storage to descrease a little */
	    storage_fraction=storage_fraction-(fraction-((fraction-1)/2-1)*year_mean_water_demand)/capacity;
	    printf("ReservoirRouting storage_fractions_more1: %f %f %f\n",
		   storage_fraction,fraction,fraction-((fraction-1)/2-1));
	    if((storage[i]/capacity)<=(storage_optimal_start_hyd_year+0.001)) {  //split deficit between storage and release
	      dummy_demand = 0.;
	      factor=fraction-((fraction-1)/2);
	      for(k=start_month;k<=12;k++) {
		water_demand[k]=WATER_DEMAND[yy-start_year][k]/factor;
		dummy_demand += WATER_DEMAND[yy-start_year][k]
		  *DaysInMonth[k]/(365+leapyear);
	      }
	      for(k=1;k<start_month;k++) {
		water_demand[k]=WATER_DEMAND[yy+1-start_year][k]/factor;
		dummy_demand += WATER_DEMAND[yy+1-start_year][k]
		  *DaysInMonth[k]/(365+leapyear); //corrected demand
	      }
	      dummy_demand *= (365+leapyear);
	      
	      /* If current storage minus next year's demand is less than 
		 0.6*magasinets storage capaciaty: Additional measures must be taken.
                 Reduce demand so that end storage=0.6. This requires
                 that you have enough water through the dry period.  */
	      if((storage[i]-dummy_demand+year_mean_inflow*CONV_M3S_CM*(365+leapyear))/capacity<0.6) {
		factor=(storage[i]-storage_min_start_hyd_year*capacity+year_mean_inflow*CONV_M3S_CM*(365+leapyear))/dummy_demand;
		for(k=1;k<=12;k++) 
		  water_demand[k] = WATER_DEMAND[yy-start_year][k]*factor;
	      }
	    }  
	    else { /*fraction still>1, i.e. demand > inflow, 
		     but storage/capacity>storage_optimal_start_hyd_year
		     Allow demanden to be released */
	      for(k=start_month;k<=12;k++) water_demand[k]=WATER_DEMAND[yy-start_year][k];
	      for(k=1;k<start_month;k++) water_demand[k]=WATER_DEMAND[yy+1-start_year][k];
	    }
	  } /* if fraction > 1 */
	  else { /*Demand less than inflow, i.e. fraction <1 
		   Allow entire demand to be released */
	    storage_fraction=storage_fraction-(fraction-((fraction-1)/2-1)*year_mean_water_demand)/capacity;
	    printf("storage_fractions %f demand %f lt inflow %f\n",
		   storage_fraction,fraction,fraction-((fraction-1)/2-1));
	    for(k=start_month;k<=12;k++) water_demand[k]=WATER_DEMAND[yy-start_year][k];
	    for(k=1;k<start_month;k++) water_demand[k]=WATER_DEMAND[yy+1-start_year][k];
	  } /* End if else fraction>1 */;
	  storage_fraction=max(storage_fraction,storage_min_start_hyd_year);
	  storage_fraction=min(storage_fraction,storage_optimal_start_hyd_year);

	  /* Compare original demand to corrected demand */
	  this_year_demand=0;
	  for(k=1;k<=12;k++) {	    
	    if(k<start_month) 
	      printf("orig water demand %d %.1f corr_demand %.1f (m3day-1)\t",
		     k,WATER_DEMAND[yy+1-start_year][k],water_demand[k]);
	    else printf("orig water demand %d %.1f corr_demand %.1f (m3day-1)\t",
			k,WATER_DEMAND[yy-start_year][k],water_demand[k]);
	    this_year_demand+=water_demand[k]*DaysInMonth[k]/(365+leapyear);
	    printf("water_demand_kcm corrected %.1f (m3day-1) year_demand %.1f (m3day-1)\n",water_demand[k],year_mean_water_demand);
	  }
	  
	  printf("this year's corrected demand (%d): %f m3s which is %f percent of inflow (%.1f m3s) this year\n",
		 yy,this_year_demand,this_year_demand*100/year_mean_inflow,year_mean_inflow);
	  printf("storage_fractions B: %f %f %f\n",
		 storage_fraction,storage_min_start_hyd_year,storage_optimal_start_hyd_year);
	  
	  for(j=0;j<366;j++) minflow[j]=q7_10;
	  
	  oldmonth=DATE[i].month;
          newmonth=i;
	  start_storage=storage[i];
	  /*****************end water demand calculations this year************************************************/
	  
	  /*****************start moscem IRR this year*****************************************************************/
	  if(purp_irr==1 && moscem==1) {
	    /* print to file for use in moscem: inflow, minoutflow, irrigation water demands, 
	       reservoir evaporation, storage, and mean_flood for each day. All numbers in m or m3day-1 */
	    system("rm -rf data/infile.txt");
	    strcpy(file_name,demandpath);
	    sprintf(fmtstr,"%s",inname);
	    strcat(file_name,fmtstr);
	    
	    if((fin = fopen("data/infile.txt", "w")) == NULL) {
	      printf("Cannot open file data/infile.txt for writing (ReservoirRouting IRR)\n");
	      exit(1);
	    }
	    else   printf("File opened for writing: data/infile.txt (ReservoirRouting IRR)\n");
	    
	    for(j=0;j<365+leapyear;j++) {
	      month=DATE[i+j].month;
	      day=DATE[i+j].day;
	      year=DATE[i+j].year;  
	      fprintf(fin,"%d %d %d %f %f %f %f %f %f\n",
		      year,month,day,FLOW[i+j]*CONV_M3S_CM,minflow[j],water_demand[month],res_evap[i+j],mean_flood,0.);
	    }
	    fclose(fin);
	    system("cp data/infile.txt data/valfile.txt");

	    system("rm -rf data/rescarfile.txt");
	    strcpy(file_name,demandpath);
	    sprintf(fmtstr,"%s",rescarname);
	    strcat(file_name,fmtstr);
	    
	    if((fres = fopen("data/rescarfile.txt", "w")) == NULL) { // Used by moscem
	      printf("Cannot open file for writing: data/rescarfile.txt (ReservoirRouting IRR)\n");
	      exit(1);
	    }
	    else printf("File opened for writing: data/rescarfile.txt\n"); 
	    fprintf(fres,"%.1f %.1f %.1f %.1f %d %d %.3f %.2f  %d\n",
		    surf_area,capacity,storage[i],storage_fraction*capacity,height,0,
		    inst_cap,mean_flood,6); //opt scheme = 6 = IRR
	    fclose(fres);

	    end_storage=storage_fraction*capacity; 
	    printf("ReservoirRouting.c start_storage %f endstorage aims at %.2f which is %.1f of capacity\n",
		   start_storage,end_storage,storage_fraction);	  
           
           printf("start to run moscem - Tian\n");

	    if(leapyear==0) system("./run_moscem_normalyear"); 
	    else  system("./run_moscem_leapyear");

	    /* read results from moscem runs. in file: year,month,day,sim_inflow,sim_outflow,power_production,spill,storage,
	       storage/max_storage,minflow (7q10),waterdemand(irrig). All numbers in m3 or m3day-1. Not all is read, though! */
	    if((fmoscem = fopen(outputname, "r")) == NULL) {  
	      printf("Cannot open file %s (ReservoirRouting IRR)\n", outputname);
	      exit(1);
	    }	else  printf("File opened for reading: %s (ReservoirRouting IRR)\n", outputname);   
	    for(j=0;j<(365+leapyear);j++) { 
	      fscanf(fmoscem,"%d %d %d %*f %f %*f %*f %f %*f %f %f\n",
		     &year,&month,&day,&outflow[j],&storage[i+j],&minflow[j],&waterdemanddaily[j]);
	      if(outflow[j]<(waterdemanddaily[j]+minflow[j])) minflow[j]=outflow[j];
	      else minflow[j]+=waterdemanddaily[j]; //update minflow for later use! i.e. increase if appropriate. 
	    }
	    fclose(fmoscem);
	  }
	  
	  /*********************end moscem IRR this year *******************************************/
	  
	  /*****************start moscem FLOOD this year**********************************************/
	  if(purp_flood==1 && moscem==1) {
	    /* print to file for use in moscem: inflow, minoutflow, irrigation water demands, 
	       reservoir evaporation, storage, and mean_flood for each day. All numbers in m or m3day-1 */
	    system("rm -rf data/infile.txt");
	    strcpy(file_name,demandpath);
	    sprintf(fmtstr,"%s",inname);
	    strcat(file_name,fmtstr);
	    
	    if((fin = fopen("data/infile.txt", "w")) == NULL) {
	      printf("Cannot open file data/infile (ReservoirRouting FLOOD)\n");
	      exit(1);
	    }
	    else   printf("File opened data/infile (ReservoirRouting FLOOD)\n");
	    
	    for(j=0;j<365+leapyear;j++) {
	      month=DATE[i+j].month;
	      day=DATE[i+j].day;
	      year=DATE[i+j].year;  
	      fprintf(fin,"%d %d %d %f %f %f %f %f %f\n",
		      year,month,day,FLOW[i+j]*CONV_M3S_CM,minflow[j],0.,res_evap[i+j],mean_flood,0.);
	      /* remember: minflow is now possibly q7_10 + irr water release!!!! after previous moscem. and hence col6=waterdemand=0. */
	    }
	    fclose(fin);
	    system("cp data/infile.txt data/valfile.txt");

	    system("rm -rf data/rescarfile.txt");
	    strcpy(file_name,demandpath);
	    sprintf(fmtstr,"%s",rescarname);
	    strcat(file_name,fmtstr);
	    
	    if((fres = fopen("data/rescarfile.txt", "w")) == NULL) { // Used by moscem
	      printf("Cannot open file for writing: data/rescarfile.txt (ReservoirRouting FLOOD)\n");
	      exit(1);
	    }
	    else printf("File opened: data/rescarfile.txt\n"); 
	    fprintf(fres,"%.1f %.1f %.1f %.1f %d %d %.3f %.2f  %d\n",
		    surf_area,capacity,storage[i],storage_fraction*capacity,height,0,
		    inst_cap,mean_flood,7); //opt scheme = 7 = FLOOD
	    fclose(fres);

	    end_storage=storage_fraction*capacity; 
	    printf("ReservoirRouting.c start_storage %f endstorage aims at %.2f which is %.1f of capacity\n",
		   start_storage,end_storage,storage_fraction);	  
  
	    if(leapyear==0) system("./run_moscem_normalyear"); 
	    else  system("./run_moscem_leapyear");

	    /* read results from moscem runs. in file: year,month,day,inflow,sim_outflow,power_prod (MW),
	       spill,storage,storage/max_storage,minoutflow,waterdemand. All numbers in m or m3day-1 */
	    if((fmoscem = fopen(outputname, "r")) == NULL) {  
	      printf("Cannot open file %s (ReservoirRouting FLOOD)\n", outputname);
	      exit(1);
	    }	else  printf("File opened: %s (ReservoirRouting FLOOD)\n", outputname);   
	    for(j=0;j<(365+leapyear);j++) { 
	      fscanf(fmoscem,"%d %d %d %*f %f %*f %*f %f %*f %f %f\n",
		     &year,&month,&day,&outflow[j],&storage[i+j],&minflow[j],&waterdemanddaily[j]);
	    }
	    fclose(fmoscem);
	  }
	  
	  /*********************end moscem FLOOD this year *******************************************/
	  
	  /*****************start moscem POW this year************************************************/
	  if(purp_hydro==1 && moscem==1) {
	    /* print to file for use in moscem: inflow, minoutflow, irrigation water demands, 
	       reservoir evaporation, storage, and mean_flood for each day. All numbers in m or m3day-1 */
	    system("rm -rf data/infile.txt");
	    strcpy(file_name,demandpath);
	    sprintf(fmtstr,"%s",inname);
	    strcat(file_name,fmtstr);
	    
	    if((fin = fopen("data/infile.txt", "w")) == NULL) {
	      printf("Cannot open file data/infile (ReservoirRouting POW))\n");
	      exit(1);
	    }
	    else   printf("File opened data/infile (ReservoirRouting POW)\n");
	    
	    for(j=0;j<365+leapyear;j++) {
	      month=DATE[i+j].month;
	      day=DATE[i+j].day;
	      year=DATE[i+j].year;  
	      fprintf(fin,"%d %d %d %f %f %f %f %f %f\n",
		      year,month,day,FLOW[i+j]*CONV_M3S_CM,minflow[j],0.,res_evap[i+j],mean_flood,0.);
	      /* minflow can now be q7_10 + irr water release!!!! after moscem IRR. Which is why you have 6th col=0 (waterdemand) */
	    }
	    fclose(fin);
	    system("cp data/infile.txt data/valfile.txt");

	    system("rm -rf data/rescarfile.txt");
	    strcpy(file_name,demandpath);
	    sprintf(fmtstr,"%s",rescarname);
	    strcat(file_name,fmtstr);
	    
	    if((fres = fopen("data/rescarfile.txt", "w")) == NULL) { // Used by moscem
	      printf("Cannot open file for writing: data/rescarfile.txt (ReservoirRouting POW)\n");
	      exit(1);
	    }
	    else printf("File opened: data/rescarfile.txt\n"); 
	    fprintf(fres,"%.1f %.1f %.1f %.1f %d %d %.3f %.2f  %d\n",
		    surf_area,capacity,storage[i],storage_fraction*capacity,height,0,
		    inst_cap,mean_flood,5); //opt scheme = 5 = POW
	    fclose(fres);

	    end_storage=storage_fraction*capacity; 
	    printf("ReservoirRouting.c start_storage %f endstorage aims at %.2f which is %.1f of capacity\n",
		   start_storage,end_storage,storage_fraction);	  
  
	    if(leapyear==0) system("./run_moscem_normalyear"); 
	    else  system("./run_moscem_leapyear");

	    /* read results from moscem runs. in file: year,month,day,inflow,sim_outflow,power_prod (MW),
	       spill,storage,storage/max_storage,minoutflow,waterdemand. All numbers in m or m3day-1 */
	    if((fmoscem = fopen(outputname, "r")) == NULL) {  
	      printf("Cannot open file %s (ReservoirRouting POW)\n", outputname);
	      exit(1);
	    }	else  printf("File opened: %s (ReservoirRouting POW)\n", outputname);   
	    for(j=0;j<(365+leapyear);j++) { 
	      fscanf(fmoscem,"%d %d %d %*f %f %f %*f %f %*f %f %f\n",
		     &year,&month,&day,&outflow[j],&PowerProd[i+j],&storage[i+j],&minflow[j],&waterdemanddaily[j]);
	    }
	    fclose(fmoscem);
	  }
	  
	  /*********************end moscem POW this year ************************************************/
	  
	  /***************** finally, for all reservoirs without purp=hydro: start moscem WAT this year*****/
	  if(purp_hydro!=1 && moscem==1) {
	    /* print to file for use in moscem: inflow, minoutflow, irrigation water demands, 
	       reservoir evaporation, storage, and mean_flood for each day. All numbers in m or m3day-1 */
	    system("rm -rf data/infile.txt");
	    strcpy(file_name,demandpath);
	    sprintf(fmtstr,"%s",inname);
	    strcat(file_name,fmtstr);
	    
	    if((fin = fopen("data/infile.txt", "w")) == NULL) {
	      printf("Cannot open file data/infile (ReservoirRouting WAT)\n");
	      exit(1);
	    }
	    else   printf("File opened data/infile (ReservoirRouting WAT)\n");
	    
	    for(j=0;j<365+leapyear;j++) {
	      month=DATE[i+j].month;
	      day=DATE[i+j].day;
	      year=DATE[i+j].year;  
	      fprintf(fin,"%d %d %d %f %f %f %f %f %f\n",
		      year,month,day,FLOW[i+j]*CONV_M3S_CM,minflow[j],0.,res_evap[i+j],mean_flood,0.);
	      /* minflow can now be q7_10 + irr water release!!!! after moscem IRR. Which is why you set 6th column=0 (waterdemand) */
	    }
	    fclose(fin);
	    system("cp data/infile.txt data/valfile.txt");

	    system("rm -rf data/rescarfile.txt");
	    strcpy(file_name,demandpath);
	    sprintf(fmtstr,"%s",rescarname);
	    strcat(file_name,fmtstr);
	    
	    if((fres = fopen("data/rescarfile.txt", "w")) == NULL) { // Used by moscem
	      printf("Cannot open file for writing: data/rescarfile.txt (ReservoirRouting WAT)\n");
	      exit(1);
	    }
	    else printf("File opened: data/rescarfile.txt\n"); 
	    fprintf(fres,"%.1f %.1f %.1f %.1f %d %d %.3f %.2f  %d\n",
		    surf_area,capacity,storage[i],storage_fraction*capacity,height,0,
		    inst_cap,mean_flood,9); //opt scheme = 9 = WAT
	    fclose(fres);

	    end_storage=storage_fraction*capacity; 
	    printf("ReservoirRouting.c start_storage %f endstorage aims at %.2f which is %.1f of capacity\n",
		   start_storage,end_storage,storage_fraction);	  
  
	    if(leapyear==0) system("./run_moscem_normalyear"); 
	    else  system("./run_moscem_leapyear");

	    /* read results from moscem runs. in file: year,month,day,inflow,sim_outflow,power_prod (MW),
	       spill,storage,storage/max_storage,minoutflow,waterdemand. All numbers in m or m3day-1 */
	    if((fmoscem = fopen(outputname, "r")) == NULL) {  
	      printf("Cannot open file for reading %s (ReservoirRouting WAT)\n", outputname);
	      exit(1);
	    }	else  printf("File opened for reading: %s (ReservoirRouting WAT)\n", outputname);   
	    for(j=0;j<(365+leapyear);j++) { 
	      fscanf(fmoscem,"%d %d %d %*f %f %*f %*f %f %*f %f %f\n",
		     &year,&month,&day,&outflow[j],&storage[i+j],&minflow[j],&waterdemanddaily[j]);
	    }
	    fclose(fmoscem);
	  }
	  
	  /*********************end moscem WAT this year *******************************************/

	  /* Finally: End results for current operational year */
	  for(j=0;j<365+leapyear;j++) { 
	    outflow[j]/=CONV_M3S_CM; //go back to units m3s-1
	    R_FLOW[i+j]=outflow[j];
	    STORAGE[i+j]=storage[i+j]; // m3
	    head=height-(capacity-storage[i+j])/surf_area;
	    LEVEL[i+j]=head;      //m 
	  }  /* end final results this operational year. j=0,365 */
	  
	  printf("\tReservoirRouting.c Storage at end of operational year %d: %f which is %.2f percent of capacity\n",
		 year,storage[i+j-1],storage[i+j-1]*100/capacity);
	  
	} /* end loop of this operational year (if month=start_month) */
	
	if(j>0) i+=j-1; /*move a year forward. i.e after start hyd year first year and before end hyd year last year */
	
      } /* end for i=0;i,ndays */
      
      printf("\tStorage at last time step in reservoir/dam %s: %f which is %.2f percent of capacity\n",
	     resname,storage[ndays-1],storage[ndays-1]*100/capacity);
      printf("\tMean inflow %f (m3/year) is %f percent of capacity\n",
	     mean_inflow*365,mean_inflow*365*100./capacity);
    }
  }
  
  /* Free memory */
  free(outflow);
  free(storage);
  free(avail_water);
  free(max_release);
  free(power_demand);
  free(water_demand);
}



