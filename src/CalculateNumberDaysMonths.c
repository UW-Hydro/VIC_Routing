#include "rout.h"

void CalculateNumberDaysMonths(int start_year,
			       int start_month,
                               int first_year,
			       int first_month,
			       int last_year,
			       int last_month,
                               int *skip,  
			       int *ndays,
			       int *nmonths)

{
  int DaysInMonth[13] = { 0,31,28,31,30,31,30,31,31,30,31,30,31 };
  int i;
  int month,year,leap_year;

  month=start_month;
  year=start_year;
  (*skip)=0;

  if(start_year<=first_year) { 
    for(i=start_month;i<12*(first_year-start_year)+first_month;i++) {
      if(month==2)
         leap_year=IsLeapYear(year); 
      else  
        leap_year=0;
      (*skip)+=DaysInMonth[month]+leap_year;
      month+=1;
      if(month>12) {
        month=1;
        year+=1;
      }
    }
  }
  else {
    printf("First day of INPUT_DATES (%d) must previous, or equal to, \
            of first day of OUTPUT_DATES (%d)\n",start_year,first_year);
    exit(0);
  }

  month=first_month;
  year=first_year;
  (*nmonths)=0;
  (*ndays)=0;

  for(i=first_month;i<=12*(last_year-first_year)+last_month;i++) {
    if(month==2)
      leap_year=IsLeapYear(year); 
    else  
      leap_year=0;
    (*ndays)+=DaysInMonth[month]+leap_year;
    (*nmonths)+=1;
    month+=1;
    if(month>12) {
      month=1;
      year+=1;
    }
  }

  if((*ndays)>DAYS) {
    printf("In rout.c reset DAYS to %d\n",(*ndays));
    exit(0);
  }
  
}
