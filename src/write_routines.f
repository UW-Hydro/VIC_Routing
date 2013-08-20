c SUBROUTINES RELATED TO WRITING
c write_data()
c write_month()
c

      SUBROUTINE WRITE_DATA
     & (FLOW, DAYS, NAME5, FACTOR_SUM, OUTPATH,IDAY,IMONTH,IYEAR)

      IMPLICIT NONE

c     RCS ID STRING
      CHARACTER*50 RCSID
      DATA RCSID/"$Id: write_routines.f,v 1.1 2005/04/07 05:07:29 vicadmin Exp $"/

      INTEGER DAYS
      REAL    FLOW(DAYS)
      INTEGER IDAY(DAYS), IMONTH(DAYS), IYEAR(DAYS)
      INTEGER I, CLEN
      CHARACTER*5 NAME5
      REAL    FACTOR_SUM
      CHARACTER*72 OUTPATH
      
      CLEN = INDEX(OUTPATH,' ')-1

      OPEN(30, FILE = OUTPATH(1:CLEN)//NAME5//'.day')
      OPEN(31, FILE = OUTPATH(1:CLEN)//NAME5//'.day_mm')
      DO I = 1,DAYS
         WRITE(30,*) IYEAR(I),IMONTH(I),IDAY(I),FLOW(I)
         WRITE(31,*) IYEAR(I),IMONTH(I),IDAY(I),FLOW(I) / FACTOR_SUM
      END DO
      CLOSE(30)
      CLOSE(31)
      RETURN
      END

      SUBROUTINE WRITE_MONTH
     & (DAYS_IN_MONTH,START_YEAR, STOP_YEAR, FIRST_YEAR, LAST_YEAR, 
     &  START_MO, STOP_MO, FIRST_MO, LAST_MO,
     &  NAME5, DAYS, FLOW, FACTOR_SUM, MONTHLY, MONTHLY_mm, 
     &  YEARLY, YEARLY_mm,OUTPATH,NDAY,IMONTH,IYEAR,MO,YR,NMONTHS,NYR)

      IMPLICIT NONE

      INTEGER DAYS_IN_MONTH(12)
      INTEGER NYR
      INTEGER START_YEAR, STOP_YEAR, FIRST_YEAR, LAST_YEAR 
      INTEGER START_MO, STOP_MO, FIRST_MO, LAST_MO   !AWW-092700 
      INTEGER DAYS,NDAY,NMONTHS
      INTEGER IMONTH(DAYS),IYEAR(DAYS)
      INTEGER SKIPTO, STOPAT
      INTEGER OLDMO
c      INTEGER MNTH_INDX
      REAL    FLOW(DAYS)
      REAL    FACTOR_SUM
      REAL    MONTHLY(12*(STOP_YEAR-START_YEAR+1))
      REAL    MONTHLY_mm(12*(STOP_YEAR-START_YEAR+1))
      REAL    YEARLY(12)
      REAL    YEARLY_mm(12)
      CHARACTER*5  NAME5
      CHARACTER*72 OUTPATH, TMPPTH

      INTEGER I, MONTH, YEAR, DAY_IN_MONTH
      INTEGER M, MCOUNT(12)     !AWW-092700
      INTEGER MO(12*NYR),YR(12*NYR)

c     concatenate output string
      I=INDEX(OUTPATH,' ')-1
C      OUTPATH(I:I+4)=NAME5
C      I=I+4

      OPEN(40, FILE = OUTPATH(1:I)//NAME5//'.month')
      OPEN(41, FILE = OUTPATH(1:I)//NAME5//'.month_mm')
      OPEN(42, FILE = OUTPATH(1:I)//NAME5//'.year')
      OPEN(43, FILE = OUTPATH(1:I)//NAME5//'.year_mm')
      OPEN(77, FILE = OUTPATH(1:I)//NAME5//'.end_of_month')

c     iniitalize monthly averages
      DO I = 1, 12*(STOP_YEAR-START_YEAR+1)
         MONTHLY(I) = 0.0
         MONTHLY_mm(I) = 0.0
      END DO
      
c     Average flows for each month in simulation
      M=1
      OLDMO=MO(1)
      DO I = 1, NDAY
         IF(IMONTH(I).ne.OLDMO) THEN
            M=M+1
            OLDMO=IMONTH(I)
         ENDIF
         MONTHLY(M)=MONTHLY(M)+FLOW(I)/DAYS_IN_MONTH(IMONTH(I))
         MONTHLY_mm(M) = MONTHLY_mm(M) + FLOW(I)/FACTOR_SUM
      END DO

C**** writing monthly averages  ***************************************

      DO I = 1,12
         YEARLY(I) = 0.0
         YEARLY_mm(I) = 0.0
         MCOUNT(I) = 0
      END DO
c     Find months in time series to start and stop writing data
c     Note array starts at 1 regardless of actual month number
      SKIPTO = (FIRST_YEAR-START_YEAR)*12+(FIRST_MO-START_MO)+1
      STOPAT = NMONTHS-((STOP_YEAR-LAST_YEAR)*12+(STOP_MO-LAST_MO))
      DO I=SKIPTO,STOPAT
         WRITE(40,*) YR(I),MO(I), MONTHLY(I)
         WRITE(41,*) YR(I),MO(I), MONTHLY_mm(I)
         YEARLY(MOD(I-1,12)+1) = YEARLY(MOD(I-1,12)+1) + MONTHLY(I)
         YEARLY_mm(MOD(I-1,12)+1) = 
     &      YEARLY_mm(MOD(I-1,12)+1) + MONTHLY_mm(I)
         MCOUNT(MO(I)) = MCOUNT(MO(I))+1
      END DO
      DO I = 1, 12
         IF(MCOUNT(I) .GT. 0) THEN
           WRITE(42,*) I, YEARLY(I)/MCOUNT(I)
           WRITE(43,*) I, YEARLY_mm(I)/MCOUNT(I)
         ELSE
           WRITE(42,*) I, '  0'
           WRITE(43,*) I, '  0'
         END IF
      END DO

C**********************************************************************

      CLOSE(40)
      CLOSE(41)
      CLOSE(42)
      CLOSE(43)
      CLOSE(77)
      RETURN
      END
