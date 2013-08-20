c SUBROUTINES RELATED TO READING 
c read_diff()
c read_fraction()
c read_grid_uh()
c read_velo()
c read_xmask()
c read_direc()
c

      SUBROUTINE READ_DIFF(DIFF,NCOL,NROW,FILENAME,
     $ IROW, ICOL)

c     RCS ID STRING
      CHARACTER*50 RCSID
      DATA RCSID/"$Id: read_routines.f,v 1.1 2005/04/07 05:07:28 vicadmin Exp $"/

      INTEGER NCOL,NROW,IROW,ICOL,I,J
      REAL DIFF(NCOL,NROW)
      CHARACTER*72 FILENAME

      OPEN(10, FILE = FILENAME,FORM = 'FORMATTED',
     $     STATUS='OLD',ERR=9001)

      DO I = 1,6                  !skip over header
         READ(10,*)
      END DO

      DO J = IROW,1,-1
         READ(10,*) (DIFF(I,J), I=ICOL,1,-1) 
      END DO      

      CLOSE(10)

      RETURN
 9001 WRITE(*,*)'CANNOT OPEN INPUT FILE IN READ_DIFF',
     $     FILENAME
      END



      SUBROUTINE READ_FRACTION(FRACTION,NCOL,NROW,FILENAME,
     $        IROW,ICOL)

      INTEGER NCOL,NROW,ICOL,IROW,I,J
      REAL FRACTION(NCOL,NROW)
      CHARACTER*72 FILENAME

c      PRINT*, 'HARDCODED FRACTION FILE'
      OPEN(22, FILE = FILENAME,
     &     FORM = 'FORMATTED',STATUS='OLD',ERR=9001)

      DO I = 1,6                  !skip over header
         READ(22,*)
      END DO

      DO J = IROW,1,-1
         READ(22,*) (FRACTION(I,J), I=ICOL,1,-1) 
      END DO      

      CLOSE(22)

      RETURN
 9001 WRITE(*,*) 'CANNOT OPEN INPUT FILE IN READ_FRACTION',
     $     FILENAME
      STOP
      END


      SUBROUTINE READ_GRID_UH
     &    (UH_BOX, KE, PMAX, NOB, CATCHIJ,FILENAME)

     
      IMPLICIT NONE

      INTEGER KE, PMAX, NOB
      INTEGER CATCHIJ(PMAX,2)
      REAL    UH_BOX(PMAX,KE)
      INTEGER N, K
      REAL    JUNK
      CHARACTER*72 FILENAME
      DO N = 1,NOB
         OPEN(14,FILE = FILENAME)
         DO K = 1,KE
            READ(14,*) JUNK, UH_BOX(N,K)
         END DO
         CLOSE(14)
      END DO
      RETURN
      END



      SUBROUTINE READ_VELO(VELO,NCOL,NROW,FILENAME,
     $ IROW,ICOL)

      INTEGER NCOL,NROW,IROW,ICOL,I,J
      REAL VELO(NCOL,NROW)
      CHARACTER*72 FILENAME

      OPEN(10, FILE = FILENAME,FORM = 'FORMATTED',
     $     STATUS='OLD', ERR=9001)

      DO I = 1,6                  !skip over header
         READ(10,*)
      END DO

      DO J = IROW,1,-1
         READ(10,*) (VELO(I,J), I=ICOL,1,-1) 
      END DO      

      CLOSE(10)

      RETURN
 9001 WRITE(*,*) 'CANNOT OPEN INPUT FILE IN  READ_VELO',
     $ FILENAME
      STOP
      END



      SUBROUTINE READ_XMASK(XMASK,NCOL,NROW,FILENAME,
     $        IROW,ICOL)

      INTEGER NCOL,NROW,ICOL,IROW,I,J
      REAL XMASK(NCOL,NROW)
      CHARACTER*72, FILENAME

      OPEN(10, FILE = FILENAME,FORM = 'FORMATTED',
     $     STATUS='OLD',ERR=9001)

      DO I = 1,6                  !skip over header
         READ(10,*)
      END DO

      DO J = IROW,1,-1
         READ(10,*, END=20) (XMASK(I,J), I=ICOL,1,-1) 
      END DO      
      CLOSE(10)

      RETURN
 20   WRITE(*,*) 'REACHED END OF XMASK:  LAST ROW', j
 9001 WRITE(*,*) 'CANNOT OPEN INPUT FILE IN READ_XMASK'
      STOP
      END



      SUBROUTINE READ_DIREC(DIREC,NCOL,NROW,H,XC,YC,SIZE
     $     ,FILENAME,IROW,ICOL)

c  reads the flow direction file.

      IMPLICIT NONE

      INTEGER NCOL,NROW,I,J,IROW,ICOL,IMISS
      INTEGER DIREC(NCOL,NROW,2) 
      INTEGER H(NCOL,NROW)
      REAL XC, YC, SIZE
      CHARACTER*72 FILENAME
      CHARACTER*14 CDUM 

      OPEN(10, FILE = FILENAME, FORM = 'FORMATTED',
     $     STATUS='OLD',ERR=9001)

      READ(10,*) CDUM, ICOL
      READ(10,*) CDUM, IROW
      READ(10,*) CDUM, XC
      READ(10,*) CDUM, YC
      READ(10,*) CDUM, SIZE
      READ(10,*) CDUM, IMISS
      IF(IROW.GT.NROW .OR. ICOL.GT.NCOL)THEN
         WRITE(*,*) 'Incorrect dimensions:'
         WRITE(*,*) 'Reset nrow and ncol in main to;',
     $        irow, icol
         STOP
      ENDIF
      
      DO J = IROW,1,-1
         READ(10,*) (H(I,J), I=ICOL,1,-1) 
      END DO      
      CLOSE(10)

      DO I = 1, ICOL
         DO J = 1,IROW
            IF (H(I,J) .EQ. 0) THEN
               DIREC(I,J,1) = 0
               DIREC(I,J,2) = 0
            ELSE IF (H(I,J) .EQ. 1) THEN
               DIREC(I,J,1) = I
               DIREC(I,J,2) = J+1
            ELSE IF (H(I,J) .EQ. 2) THEN
               DIREC(I,J,1) = I-1
               DIREC(I,J,2) = J+1
            ELSE IF (H(I,J) .EQ. 3) THEN
               DIREC(I,J,1) = I-1
               DIREC(I,J,2) = J
            ELSE IF (H(I,J) .EQ. 4) THEN 
               DIREC(I,J,1) = I-1
               DIREC(I,J,2) = J-1
            ELSE IF (H(I,J) .EQ. 5) THEN
               DIREC(I,J,1) = I
               DIREC(I,J,2) = J-1
            ELSE IF (H(I,J) .EQ. 6) THEN
               DIREC(I,J,1) = I+1
               DIREC(I,J,2) = J-1
            ELSE IF (H(I,J) .EQ. 7) THEN
               DIREC(I,J,1) = I+1
               DIREC(I,J,2) = J
            ELSE IF (H(I,J) .EQ. 8) THEN
               DIREC(I,J,1) = I+1
               DIREC(I,J,2) = J+1
            END IF
         END DO
      END DO
      RETURN
 9001 WRITE(*,*) 'CANNOT OPEN INPUT FILE IN READ_DIREC',
     $  FILENAME
      STOP
      END

