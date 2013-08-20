# Rout Makefile
# Last Changed: Fri Aug 19 12:01:05 2005 by Ingjerd Haddeland <ingjerd@u.washington.edu>
# 
# compile: make

SHELL = /bin/csh

CC = gcc
#CFLAGS = -I. -g -Wall -Wno-unused
CFLAGS = -I. -g -O3 -Wall -g -Wno-unused
#CFLAGS = -I. -O3 -pg -Wall -Wno-unused
LIBRARY = -lm
#LIBRARY = -lm -lefence

HDRS = rout_def.h rout.h

OBJS =  CalculateMeanInflow.o CalculateNumberDaysMonths.o Find7Q10.o \
        FindRowsCols.o FindStartOfOperationalYear.o IsLeapYear.o \
        MakeConvolution.o MakeDirectionFile.o MakeGridUH_S.o MakeRoutedFile.o MakeUH.o \
	ReadDataForReservoirEvaporation.o ReadDiffusion.o ReadDirection.o \
        ReadFraction.o ReadGridUH.o ReadReservoirs.o ReadRouted.o ReadStation.o \
	ReadVelocity.o ReadWaterDemand.o ReadXmask.o ReservoirRouting.o \
	SearchCatchment.o SearchRouted.o WriteData.o rout.o 

SRCS = $(OBJS:%.o=%.c) 

all:
	make model

default:
	make model

clean::
	/bin/rm -f *.o core log *~

model: $(OBJS)
	$(CC) -o rout $(OBJS) $(CFLAGS) $(LIBRARY)

# -------------------------------------------------------------
# tags
# so we can find our way around
# -------------------------------------------------------------
tags:	TAGS
TAGS:	$(SRCS) $(HDRS)
	etags $(SRCS) $(HDRS)
clean::
	\rm -f TAGS	       


# -------------------------------------------------------------
# depend
# -------------------------------------------------------------
depend: .depend
.depend:	$(SRCS) $(HDRS)
	$(CC) $(CFLAGS) -MM $(SRCS) > $@

clean::
	\rm -f .depend	     

