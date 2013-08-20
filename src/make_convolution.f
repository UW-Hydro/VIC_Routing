
      SUBROUTINE MAKE_CONVOLUTION
     & (NCOL, NROW, NOB, PMAX, DAYS, CATCHIJ, 
     &  BASE, RUNO, FLOW, KE, UH_DAY, UH_S, FRACTION, FACTOR_SUM,
     &  XC, YC, SIZE, DPREC, INPATH,ICOL,NDAY,IDAY,IMONTH,IYEAR,
     &  MO, YR, NYR)

      IMPLICIT NONE

c     RCS ID STRING
      CHARACTER*50 RCSID
      DATA RCSID/"$Id: make_convolution.f,v 1.1 2005/04/07 05:07:28 vicadmin Exp $"/

      INTEGER     N, I, J, DAYS, NDAY, II, JJ  
      INTEGER     NCOL,NROW,ICOL,NOB,PMAX,KE,UH_DAY
      INTEGER     CATCHIJ(PMAX,2)
      INTEGER     NYR
      REAL        UH_S(PMAX,KE+UH_DAY-1)
      REAL        BASE(DAYS), RUNO(DAYS), FLOW(DAYS) 
      REAL        FRACTION(NCOL,NROW)

      REAL        PI, RERD, FACTOR, FACTOR_SUM

      LOGICAL TORF

      PARAMETER   (RERD  = 6371229.0)    !radius of earth in meters

      CHARACTER*20 LOC
      CHARACTER*72 INPATH

      INTEGER DPREC, CLEN

      REAL        JLOC, ILOC
      REAL        XC, YC, SIZE
      REAL        AREA, AREA_SUM

      REAL        STORAGE, K_CONST
      REAL        DUM1,DUM2
  
      INTEGER     IDAY(DAYS), IMONTH(DAYS), IYEAR(DAYS)
      INTEGER MO(12*NYR),YR(12*NYR)
      INTEGER MISS_NUM
C     MISS_NUM is the number of grid cell output files not found

      MISS_NUM=0

C     *** 0 <= K_CONST = 1.0 
C *** K_CONST smaller 1.0 makes it a simple linear storage
    
      K_CONST = 1.0

      PI = ATAN(1.0) * 4.0

      AREA_SUM   = 0.0
      FACTOR_SUM = 0.0

      DO I = 1,NDAY
         FLOW(I) = 0.0
      END DO

      DO N = 1,NOB       !is this the gridcell loop?
         STORAGE = 0.0
         DO I = 1,NDAY
            RUNO(I) = 0.0
            BASE(I) = 0.0
         END DO
         II = CATCHIJ(N,1)
         JJ = CATCHIJ(N,2)
         
c     the grid has been flipped left to right
c     find the revised cooordinates

         ILOC=XC + (ICOL-II)*SIZE + SIZE/2.0
         JLOC=YC + JJ*SIZE - SIZE/2.0

C        CONVERSIONFACTOR for mm/day to ft**3/sec         <--?????


         AREA =  RERD**2*ABS(SIZE)*PI/180*             !give area of box in 
     &        ABS(SIN((JLOC-SIZE/2.0)*PI/180)-         !square meters
     $        SIN((JLOC+SIZE/2.0)*PI/180))

         
         AREA_SUM = AREA_SUM + AREA

c         WRITE(*,*) N, ILOC, JLOC

         FACTOR = FRACTION(II,JJ)*35.315*AREA/(86400.0*1000.0)  !convert to sq.mi.
                                                                !&mult. by cell fract
         FACTOR_SUM = FACTOR_SUM + FACTOR

         
         call create_vic_names(jloc,iloc,loc,clen,dprec)

c     print*, INPATH(1:INDEX(INPATH,' ')-1)//LOC(1:CLEN)

         INQUIRE(FILE=INPATH(1:(INDEX(INPATH,' ')-1))//LOC(1:CLEN),
     $                     EXIST=TORF)

         if(torf)then
           OPEN(20,FILE=INPATH(1:(INDEX(INPATH,' ')-1))//
     $        LOC(1:CLEN),
     $        STATUS='OLD',ERR=9001)
           print*, N, ' of',NOB,": ",
     &       INPATH(1:(INDEX(INPATH,' ')-1))//LOC(1:CLEN)
c     read VIC model output: <year> <month> <day> <p> <et> <runoff> <baseflow>
           DO I = 1,NDAY
             READ(20,*,END=9001,ERR=9001) IYEAR(I), IMONTH(I), IDAY(I),
     &         DUM1, DUM2, RUNO(I), BASE(I)
c     check to be sure dates in VIC file start at same time specified
c     in input file
             if(I.eq.1) then
               if(IYEAR(I).ne.YR(1) . or. IMONTH(I).ne.MO(1)) then
                  print*, 'VIC output file does not match specified '
                  print*, 'period in input file.'
                  stop
               endif
             endif
           END DO
	 else
           print*, INPATH(1:(INDEX(INPATH,' ')-1))//LOC(1:CLEN),
     &             ' NOT FOUND, INSERTING ZEROS'
           miss_num = miss_num+1
	   do i=1,nday
             IYEAR(I)=9999
             IMONTH(I)=99
             IDAY(I)=99
	     runo(i)=0
	     base(i)=0
	   end do
	 endif

         DO I = 1,NDAY
            RUNO(I) = RUNO(I) * FACTOR
            BASE(I) = BASE(I) * FACTOR
         END DO
         DO I = 1,NDAY
            DO J = 1,KE+UH_DAY-1
               IF ((I-J+1) .GE. 1) THEN
                  FLOW(I) = FLOW(I)+UH_S(N,J)*(BASE(I-J+1)+RUNO(I-J+1))
               END IF
            END DO
         END DO
         CLOSE(20)
      END DO
      if(MISS_NUM>0) then
        print*, MISS_NUM, ' files not found, zero runoff/baseflow used'
      end if

      RETURN
 9001 WRITE(*,*) 'Error reading time-series data, ',
     $     'insufficient data or missing input file',
     $     INPATH(1:INDEX(INPATH,' ')-1)//LOC(1:CLEN)
      END
