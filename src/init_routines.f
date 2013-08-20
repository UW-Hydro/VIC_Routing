c  SUBROUTINES FOR INITIALIZATION (roughly)
c  init_array()
c  create_vic_names()
c  search_catchment()
c

      SUBROUTINE INIT_ARRAY( A, NROW, NCOL, VALUE )

C     Initialiase float array A to VALUE

      IMPLICIT NONE

c     RCS ID STRING
      CHARACTER*50 RCSID
      DATA RCSID/"$Id: init_routines.f,v 1.1 2005/04/07 05:07:28 vicadmin Exp $"/

      INTEGER NCOL, NROW
      INTEGER I, J
      REAL A(NCOL,NROW)
      REAL VALUE

      DO J=1, NROW
         DO I=1, NCOL
            A(I,J)=VALUE
         END DO
      END DO

      RETURN
      END




      SUBROUTINE CREATE_VIC_NAMES( JLOC, ILOC, EXTEN, CLEN, DPREC )


c     create string containing vic file names to be
c     appended to path given in input file

c     filenames allowed a maximum of 5 decimal places

      IMPLICIT NONE

      CHARACTER*10 JICHAR(2)
      CHARACTER*20 EXTEN
      REAL JLOC, ILOC
      INTEGER NSPACE, CLEN, CLEN_OLD, DPREC, I

      WRITE(JICHAR(1),'(F10.5)')JLOC
      WRITE(JICHAR(2),'(F10.5)')ILOC

      CLEN_OLD=1
      DO I=1,2
         NSPACE=1
 5       IF(JICHAR(I)(NSPACE:NSPACE).EQ.' ')THEN
            NSPACE=NSPACE+1
            GOTO 5
         ENDIF
         CLEN=CLEN_OLD+11-NSPACE-5+DPREC
         EXTEN(CLEN_OLD:CLEN)=JICHAR(I)(NSPACE:5+DPREC)
         IF(I.EQ.1)THEN
            EXTEN(CLEN:CLEN)='_'
         ENDIF
         CLEN_OLD=CLEN+1
      END DO

      CLEN=CLEN-1
      
      RETURN
      END





      SUBROUTINE SEARCH_CATCHMENT
     & (PI,PJ,DIREC,NCOL,NROW,NO_OF_BOX,CATCHIJ,PMAX,
     $  IROW,ICOL)

      IMPLICIT NONE

      INTEGER PI,PJ,I,J,NCOL,NROW,PMAX,ICOL,IROW
      INTEGER II, JJ, III, JJJ
      INTEGER DIREC(NCOL,NROW,2)
      INTEGER NO_OF_BOX
      INTEGER CATCHIJ(PMAX,2)

C****** CATCHMENTS ***************************************

      NO_OF_BOX = 0

      DO I = 1, ICOL
         DO J = 1, IROW
c            print*, i, j, no_of_box
            II = I
            JJ = J
 300        CONTINUE
            IF ((II .GT. ICOL) .OR. (II .LT.1) .OR. 
     &          (JJ .GT. IROW) .OR. (JJ .LT.1)) THEN
               GOTO 310
            END IF
            IF ((II .EQ. PI) .AND. (JJ .EQ. PJ)) THEN 
               NO_OF_BOX = NO_OF_BOX + 1
               CATCHIJ(NO_OF_BOX,1) = I
               CATCHIJ(NO_OF_BOX,2) = J
               GOTO 310
            ELSE 
               IF ((DIREC(II,JJ,1).NE.0) .AND.    !check if the current 
     &             (DIREC(II,JJ,2) .NE.0)) THEN   !ii,jj cell routes down
                     III = DIREC(II,JJ,1)         !to the subbasin outlet
                     JJJ = DIREC(II,JJ,2)         !point, following the 
                     II  = III                    !direction of direc(,)
                     JJ  = JJJ                    !from each cell
                     GOTO 300
               END IF   			  !if you get there, 
            END IF                                !no_of_box increments
 310        CONTINUE                              !and you try another
         END DO                                   !cell.  
      END DO

      WRITE(*,*) 'Number of grid cells upstream of present station',
     $     no_of_box

      RETURN
      END
