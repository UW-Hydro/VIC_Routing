#include <string.h>       
#include <stdio.h> 
#include <math.h> 
#include <stdlib.h> 
#include "model.h"
#include "constant.h"

int DaysOfMonth(int month)
{
  int DaysInMonth[] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
  int i;

  if(NTSTEP1==366) DaysInMonth[1]=29;
  i=DaysInMonth[month-1];

  return i;
}
