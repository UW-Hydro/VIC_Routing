#============================================================
# GnuMakefile
# Author: Yuqiong Liu, March 2003, yqliu@hwr.arizona.edu
#===========================================================

include $(CONFIG_FILE)

TARGET	= ../../run_moscem

#----------------------  get source files    -------------------------------
#
# Automagically generate source and object lists by searching for source files
#  in the vpath
#
MOD_DIR  := $(TOP_DIR)/$(MOD_DIR)
OPT_DIR    = $(TOP_DIR)/moscem
VPATH = $(MOD_DIR):$(OPT_DIR)

# get the filepaths for all of the source files in the VPATH 
dirs = $(subst :, ,$(VPATH))
C_SRCS1 = $(foreach dir,$(dirs), $(wildcard $(dir)/*.c))
F_SRCS1 = $(foreach dir,$(dirs), $(wildcard $(dir)/*.F))

# get filenames w/o paths; remove dups by sorting
F_SRCS = $(sort $(notdir $(F_SRCS1))) 
C_SRCS = $(sort $(notdir $(C_SRCS1))) 

OBJS = $(F_SRCS:.F=.o) $(C_SRCS:.c=.o)

#----------------------   compiler settings  -----------------------------------

CC         = gcc
#FC = f77
FC         = gfortran
CFLAGS    = -g -I/usr/openwin/include 
FFLAGS     = -O4 -xlibmopt
#LINKER     = f77
LINKER = gfortran
CPP        = /usr/bin/cpp
GENDEP     = CPP

INCLUDES   = -I$(subst :, -I,$(VPATH))

ifeq ($(MOD_VER), C)
CPPFLAGS = -DMOD_VER=1
endif

ifeq ($(MOD_VER), FORTRAN)
CPPFLAGS = -DMOD_VER=2
endif

CPPFLAGS := $(CPPFLAGS) -DNINPUT=$(NINPUT) -DNFLUX=$(NFLUX) -DNTSTEP1=$(NTSTEP1) \
   -DNTSTEP2=$(NTSTEP2) -DNPAR=$(NPAR) -DIDUM=$(IDUM) $(INCLUDES)

FPPFLAGS   = $(CPPFLAGS)

ifeq ($(MOD_ALGO), MULTI)
CPPFLAGS := $(CPPFLAGS) -DMOD_ALGO=1
endif

ifeq ($(MOD_ALGO), SINGL)
CPPFLAGS := $(CPPFLAGS) -DMOD_ALGO=0
endif

ifeq ($(OBJ_FUNC), RMSE)
CPPFLAGS := $(CPPFLAGS) -DOBJ_FUNC=1
endif

ifeq ($(OBJ_FUNC), STD)
CPPFLAGS := $(CPPFLAGS) -DOBJ_FUNC=2
endif

ifeq ($(OBJ_FUNC), HMLE)
CPPFLAGS := $(CPPFLAGS) -DOBJ_FUNC=3 -DLAMBDA=$(LAMBDA)
endif

ifeq ($(OBJ_FUNC), NSE)
CPPFLAGS := $(CPPFLAGS) -DOBJ_FUNC=4
endif

ifeq ($(OBJ_FUNC), POW)
CPPFLAGS := $(CPPFLAGS) -DOBJ_FUNC=5
endif

ifeq ($(OBJ_FUNC), IRR)
CPPFLAGS := $(CPPFLAGS) -DOBJ_FUNC=6
endif

ifeq ($(OBJ_FUNC), FLOOD)
CPPFLAGS := $(CPPFLAGS) -DOBJ_FUNC=7
endif

ifeq ($(OBJ_FUNC), POWFLO)
CPPFLAGS := $(CPPFLAGS) -DOBJ_FUNC=8
endif

ifeq ($(OBJ_FUNC), WAT)
CPPFLAGS := $(CPPFLAGS) -DOBJ_FUNC=9
endif



ifeq (HAS_MISSING_VALUE, YES)
CPPFLAGS := $(CPPFLAGS) -DMISSING_VALUE=$(MISSING_VALUE)
endif

#-------------------------   compliation rules   ------------------------------

.SUFFIXES: .F .c 
.c.o:
	$(CC)  -c $(CFLAGS) $(CPPFLAGS)  $<
.F.o:
	$(FC)  -c $(FFLAGS) $(FPPFLAGS)  $<

#-------------------------  targets/dependencies   -------------------------------

MOSCEM: $(TARGET) 

$(TARGET): $(OBJS) 
	$(LINKER) $(OBJS) -o $(TARGET)

showsrc:
	@echo ***SRCS***  
	@echo $(C_SRCS) $(F_SRCS) 
	@echo
	@echo ***SRCS_WITH_PATH***  
	@echo $(C_SRCS1) $(F_SRCS1) 

showobj:
	@echo ***OBJS*** $(OBJS)

#-----------------   automatic dependency generation  -------------------
#
ifeq ($(GENDEP), CPP)
%.d: %.F
	@ echo "generating dependencies for $<"
	@ $(SHELL) -ec '$(CPP) -M $(CPPFLAGS) $< \
               | sed -e '\''s/\/[^ ]*\///g'\''\
                     -e '\''s/\($*\)\.o :/\1.o $@ : /g'\'' > $@'
%.d: %.c
	@ echo "generating dependencies for $<"
	@ $(SHELL) -ec '$(CPP) -M $(CPPFLAGS) $< \
              | sed -e '\''s/\($*\)\.o :/\1.o $@ : /g'\'' > $@'
endif

# include the dependencies automatically generated for each source file
# the '-' is to suppress some irrelevant warning messages

ifeq ($(MAKECMDGOALS), MOSCEM)
-include $(C_SRCS:.c=.d) $(F_SRCS:.F=.d)
endif   

