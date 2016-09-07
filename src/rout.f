      PROGRAM rout
c
c     Routing algorithm developed by D. Lohmann.
c
c     Modified to allow more flexible array dimensions and
c     the removal of harcoded file names.
c
c     Code maintained by G. O'Donnell (tempgd@hydro.washington.edu)
c     See WA Hydrology Homepage for operational details.

c     Modified 5/99 to read in the uh_s array if it has already
c     been generated in a previous run.

c     Modified 2/2001 by edm to include month and year in output
c     and also check dates in VIC output files and calculate NDAYS
c
      IMPLICIT NONE

      integer IARGC

      integer isaleap
      external isaleap

c     change dimensions here
c     nrow and ncol should be larger than the grid
c     nyr should equal run length yrs+1
      INTEGER NROW, NCOL, DAYS, NYR
      PARAMETER (NROW = 250, NCOL = 250)
      PARAMETER (NYR = 50)

c     no changes after here
      REAL    DT
      INTEGER KE, LE, TMAX, UH_DAY, PMAX
      PARAMETER (DAYS=NYR*366)
      PARAMETER (KE   = 12    )
      PARAMETER (LE   = 48    )
      PARAMETER (DT   = 3600.0)
      PARAMETER (UH_DAY = 96  )
      PARAMETER (TMAX = UH_DAY*24)
      PARAMETER (PMAX = 10000   )

      INTEGER DIREC(NCOL,NROW,2)
      REAL    VELO(NCOL,NROW), DIFF(NCOL,NROW)
      REAL    XMASK(NCOL,NROW), FRACTION(NCOL,NROW)
      REAL    UH_BOX(PMAX,KE), UHM(NCOL,NROW,LE)
      REAL    UH_S(PMAX,KE+UH_DAY-1)
      REAL    BASE(DAYS), RUNO(DAYS), FLOW(DAYS)

      INTEGER IDAY(DAYS), IMONTH(DAYS), IYEAR(DAYS)
      INTEGER MO(12*NYR),YR(12*NYR)
      INTEGER NO_OF_BOX
      INTEGER CATCHIJ(PMAX,2)
      INTEGER H(NCOL,NROW)

      INTEGER PI, PJ
      REAL    UH_DAILY(PMAX,UH_DAY)
      REAL    FR(TMAX,2)

      INTEGER NR
      INTEGER IROW, ICOL
      INTEGER LP,M,Y
      INTEGER J

      CHARACTER*80 UH_STRING         ! new, AW
      CHARACTER*21 NAME
      CHARACTER*5  NAME5
      CHARACTER*72 FILE_INPUT, FILENAME
      CHARACTER*72 INPATH, OUTPATH

      INTEGER DPREC

      REAL    AREA, FACTOR_SUM

      REAL XC, YC, SIZE

      REAL FDUM
      LOGICAL TORF
      INTEGER NDAY
      INTEGER NMONTHS

C**** variables for monthly means *****************************

      INTEGER DAYS_IN_MONTH(12)
      DATA DAYS_IN_MONTH /31,28,31,30,31,30,31,31,30,31,30,31/

      INTEGER START_YEAR, STOP_YEAR, FIRST_YEAR, LAST_YEAR
      INTEGER START_MO, STOP_MO, FIRST_MO, LAST_MO

      REAL MONTHLY(12*NYR)
      REAL MONTHLY_mm(12*NYR)
      REAL YEARLY(12)
      REAL YEARLY_mm(12)

C***********************************************************
C     OPEN NECESSARY FILES
C***********************************************************

c     INPUT FILE READ
c     process commandline args
      IF(IARGC().NE.1)THEN
         PRINT*, 'USAGE:  rout <infile>'
         STOP
      ENDIF
      CALL GETARG(1,FILE_INPUT)
      OPEN(1,FILE=FILE_INPUT,STATUS='OLD',ERR=9001)
      READ(1,'(//A)') FILENAME
      CALL READ_DIREC(DIREC,NCOL,NROW,H,XC,YC,SIZE,
     $     FILENAME,IROW,ICOL)
c     velo file
      READ(1,*)
      READ(1,*) TORF
      IF(TORF)THEN
         READ(1,'(A)') FILENAME
         CALL READ_VELO(VELO,NCOL,NROW,FILENAME,
     $        IROW,ICOL)
      ELSE
         READ(1,*) FDUM
         CALL INIT_ARRAY(VELO,NCOL,NROW,FDUM)
      ENDIF
c     diff file
      READ(1,*)
      READ(1,*)TORF
      IF(TORF)THEN
         READ(1,'(A)') FILENAME
         CALL READ_DIFF(DIFF,NCOL,NROW,FILENAME,
     $        IROW,ICOL)
      ELSE
         READ(1,*) FDUM
         CALL INIT_ARRAY(DIFF,NCOL,NROW,FDUM)
      ENDIF
c     xmask file
      READ(1,*)
      READ(1,*)TORF
      IF(TORF)THEN
         READ(1,'(A)') FILENAME
         CALL READ_XMASK(XMASK,NCOL,NROW,FILENAME,
     $        IROW,ICOL)
      ELSE
         READ(1,*) FDUM
         CALL INIT_ARRAY(XMASK,NCOL,NROW,FDUM)
      ENDIF
c     read fraction file
      READ(1,*)
      READ(1,*)TORF
      IF(TORF)THEN
         READ(1,'(A)') FILENAME
         CALL READ_FRACTION(FRACTION,NCOL,NROW,FILENAME,
     $        IROW,ICOL)
      ELSE
         READ(1,*) FDUM
         CALL INIT_ARRAY(FRACTION,NCOL,NROW,FDUM)
      ENDIF
c     station file
      READ(1,'(/A)')FILENAME
      OPEN(10,FILE=FILENAME)
c     read input path and precision of VIC filenames
      READ(1,'(/A)')INPATH
      READ(1,*)DPREC
c     output pathname
      READ(1,'(/A)')OUTPATH
c     number of days to process
      READ(1,*)
c     start and end year/month from VIC simulation
      READ(1,*) START_YEAR, START_MO, STOP_YEAR, STOP_MO
c     calculate number of days & months in simulation
      M=START_MO
      Y=START_YEAR
      NMONTHS = 0
      NDAY=0
      DO J=START_MO,12*(STOP_YEAR-START_YEAR)+STOP_MO
        IF(M.EQ.2) THEN
           LP=isaleap(Y)
        ELSE
           LP=0
        ENDIF
        NDAY = NDAY+DAYS_IN_MONTH(M)+LP
        NMONTHS = NMONTHS + 1
        MO(NMONTHS) = M
        YR(NMONTHS) = Y
        M = M + 1
        IF (M .GT. 12) THEN
            M = 1
            Y  = Y + 1
        ENDIF
      END DO
      IF(NDAY.GT.DAYS) THEN
         PRINT*, 'IN ROUT.F RESET DAYS TO ', NDAY
         STOP
      ENDIF
      PRINT*,'NDAY = ',NDAY, ' NMONTHS = ',NMONTHS
c     start and end year/month for writing output
      READ(1,*) FIRST_YEAR, FIRST_MO, LAST_YEAR, LAST_MO
c     uh file
      READ(1,'(/A)')FILENAME

C***********************************************************

      CALL MAKE_UHM(UHM,VELO,DIFF,XMASK,NCOL,NROW,LE,DT,
     $        IROW,ICOL)

C     Loop over required stations

 100  CONTINUE
      READ(10,*,END=110)
     &     NR, NAME, PI, PJ, AREA
      READ(10,'(A80)',END=110) UH_STRING   !new, AW:  uh_string
      IF (NR .EQ. 1) THEN
         WRITE(*,'(I2,2X,A,I4,I4,G12.6)')
     &        NR, NAME, PI, PJ
         PRINT*, 'Routing station: ', NAME
c     note, the arrays are flipped left to right
         PI=ICOL+1-PI
         NAME5 = NAME

      print*, 'searching catchment...'
         CALL SEARCH_CATCHMENT
     &        (PI,PJ,DIREC,NCOL,NROW,
     &        NO_OF_BOX,CATCHIJ,PMAX,IROW,ICOL)

      print*, 'reading grid_UH...'
         CALL READ_GRID_UH
     &        (UH_BOX, KE, PMAX, NO_OF_BOX, CATCHIJ,FILENAME)

      print*, 'making grid UH...'
         CALL MAKE_GRID_UH
     &        (DIREC, NO_OF_BOX, UH_DAY, TMAX, PI, PJ, LE, UH_DAILY, KE,
     &        CATCHIJ,UHM, FR, PMAX, NCOL, NROW, UH_BOX, UH_S,
     &        UH_STRING,NAME5)        !new, AW:  added uh_string

      print*, 'making convolution...'
         CALL MAKE_CONVOLUTION
     &        (NCOL, NROW, NO_OF_BOX, PMAX, DAYS,
     &        CATCHIJ, BASE, RUNO, FLOW, KE, UH_DAY, UH_S, FRACTION,
     &        FACTOR_SUM,XC,YC,SIZE,DPREC,INPATH,ICOL,NDAY,
     &        IDAY,IMONTH,IYEAR, MO, YR, NYR)

      print*, 'writing data...'
         CALL WRITE_DATA
     &        (FLOW, NDAY, NAME5, FACTOR_SUM, OUTPATH,IDAY,IMONTH,IYEAR)

         CALL WRITE_MONTH
     &     (DAYS_IN_MONTH,START_YEAR, STOP_YEAR, FIRST_YEAR,
     &     LAST_YEAR, START_MO, STOP_MO, FIRST_MO,
     &     LAST_MO,
     &     NAME5, DAYS, FLOW, FACTOR_SUM, MONTHLY, MONTHLY_mm,
     &     YEARLY,YEARLY_mm,OUTPATH,NDAY,IMONTH,IYEAR,MO,YR,NMONTHS,NYR)


      END IF
      GOTO 100
 110  CONTINUE

      STOP
 9001 WRITE(*,*) 'CANNOT OPEN: ', FILE_INPUT
      END
c     ***********************************************
c     FUNCTION  ISALEAP

      integer function isaleap( iyr )

c     return 1 if a leap yr else 0

      if( (mod(iyr,4) .eq. 0 .and. mod(iyr,100) .ne.0)
     $                       .or. mod(iyr,400) .eq. 0) then
         isaleap = 1
      else
         isaleap = 0
      endif

      end
