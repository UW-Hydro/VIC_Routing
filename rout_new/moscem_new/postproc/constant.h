/* ======================================
some constants
Yuqiong Liu, March 2003
========================================*/

#ifndef _CONSTANT_H_
#define _CONSTANT_H_

#include <stdio.h>

#define INF  1.0e+20
#define NINF -1.0e+20
#define MAX_LINE_LEN  1000
#define MAX_FNAME_LEN  50

#ifndef MISSING_VALUE
#define MISSING_VALUE -999999.99999
#endif

#ifndef LAMBDA
#define LAMBDA 0.5
#endif

#define max(m, n) ((m) > (n)  ? (m) : (n))
#define min(m, n) ((m) < (n)  ? (m) : (n))

#endif

