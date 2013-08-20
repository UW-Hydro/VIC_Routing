c  SUBROUTINES RELATED TO UNIT HYDROGRAPH
c  make_uhm()
c  make_grid_uh()
c

c     Calculate impulse response function for grid cells
c     using equation (15) from Lohmann, et al. (1996)  Tellus article
      SUBROUTINE MAKE_UHM(UH,VELO,DIFF,XMASK,NCOL,NROW,LE,DT,
     $        IROW,ICOL)

c     RCS ID STRING
      CHARACTER*50 RCSID
      DATA RCSID/"$Id: unit_hyd_routines.f,v 1.1 2005/04/07 05:07:29 vicadmin Exp $"/

      REAL    UH(NCOL,NROW,LE), INTE
      INTEGER I, J, K, LE
      REAL    T, DT, POT, VELO(NCOL,NROW), DIFF(NCOL,NROW)
      REAL    XMASK(NCOL,NROW), H, PI

      PI = 4.0*atan(1.0)

      DO I = 1, ICOL
         DO J =1, IROW
            T = 0.0
            DO K = 1,LE
               T = T + DT
               IF (VELO(I,J) .GT. 0.0) THEN
                  POT = ((VELO(I,J)*T-XMASK(I,J))**2.0)/
     &                  (4.0*DIFF(I,J)*T)
                  IF (POT .GT. 69.0) THEN
                     H = 0.0
                     ELSE
                     H = 1.0/(2.0*SQRT(PI*DIFF(I,J))) * 
     &                   XMASK(I,J)/(T**1.5) * EXP(-POT)
                  END IF
               ELSE 
                  H = 0.0
               END IF
               UH(I,J,K) = H
            END DO
         END DO
      END DO

      DO I = 1, ICOL
         DO J = 1, IROW
            INTE = 0.0
            DO K = 1,LE
               INTE = INTE + UH(I,J,K)
            END DO
            IF (INTE .GT. 0.0) THEN
               DO K = 1,LE
                  UH(I,J,K) = UH(I,J,K)/INTE
               END DO
            END IF
         END DO
      END DO
 
      RETURN
      END

c     Solve for 
      SUBROUTINE MAKE_GRID_UH
     & (DIREC, NOB, UH_DAY, TMAX, PI, PJ, LE, UH_DAILY, KE,
     &  CATCHIJ, UHM, FR, PMAX, NCOL, NROW, UH_BOX, 
     &  UH_S, UH_STRING, NAME5)

      IMPLICIT NONE

      INTEGER NOB, UH_DAY, TMAX, PMAX, KE
      INTEGER PI, PJ, LE, NCOL, NROW

      INTEGER DIREC(NCOL,NROW,2)
      REAL    UH_DAILY(PMAX,UH_DAY)
      INTEGER CATCHIJ(PMAX,2)
      REAL    UHM(NCOL,NROW,LE), FR(TMAX,2)
      REAL    UH_BOX(PMAX,KE)
      REAL    UH_S(PMAX,KE+UH_DAY-1)
      INTEGER N, I, J, K, L, T, II, JJ, TT, U
      REAL    SUM

      CHARACTER*80 UH_STRING       !new, AW
      CHARACTER*5  NAME5
         
      IF (UH_STRING(1:4) .ne. 'NONE') THEN       ! read UH_S grid, not make it
        print*, 'reading UH_S grid from file'
        open(98, file=UH_STRING, status='old')   
        DO N = 1,NOB
          READ(98, *) (UH_S(N,K), K = 1,KE+UH_DAY-1)
        END DO        

      ELSE				         ! make UH_S grid, and save it
        print*, 'making UH_S grid...it takes a while...'
        print*, 'NOTE:  your new UH_S grid file will be written in the'
        print*, '       directory you run from, and will be called'
        print*, '            '//NAME5//'.uh_s'
        print*, '       save this file and specify it in your station'
        print*, '       location file to avoid this step in the future'

        open(98, file = NAME5//'.uh_s', status='new')     

        DO N = 1, NOB
          print*, 'grid cell', N,' out of', NOB
          DO K = 1,UH_DAY
            UH_DAILY(N,K) = 0.0
          END DO
          I = CATCHIJ(N,1)
          J = CATCHIJ(N,2)

          DO K = 1,24
            FR(K,1) = 1.0 / 24.0
            FR(K,2) = 0.0
          END DO
          DO K = 25,TMAX
            FR(K,1) = 0.0
            FR(K,2) = 0.0
          END DO

 100      CONTINUE
          IF ((I .NE. PI) .OR. (J .NE. PJ)) THEN
            DO T = 1, TMAX
              DO L = 1, LE
                 IF ((T-L) .GT. 0) THEN
                   FR(T,2) = FR(T,2) + FR(T-L,1)*UHM(I,J,L) 
                 END IF
               END DO
            END DO
            II = DIREC(I,J,1)
            JJ = DIREC(I,J,2)
            I = II
            J = JJ
            DO T = 1, TMAX
               FR(T,1) = FR(T,2)
               FR(T,2) = 0.0
            END DO
          END IF
          IF ((I .NE. PI) .OR. (J .NE. PJ)) THEN
            GOTO 100
          END IF

          DO T = 1,TMAX
            TT = (T+23)/24
            UH_DAILY(N,TT) = UH_DAILY(N,TT) + FR(T,1)
          END DO
        END DO

        DO N = 1,NOB
          DO K = 1, KE+UH_DAY-1
            UH_S(N,K) = 0.0
          END DO
        END DO

        DO N = 1,NOB
          DO K = 1,KE
            DO U =1,UH_DAY
              UH_S(N,K+U-1) = UH_S(N,K+U-1) + 
     &                         UH_BOX(N,K) * UH_DAILY(N,U)
            END DO
          END DO
          SUM = 0
          DO K = 1,KE+UH_DAY-1
            SUM = SUM + UH_S(N,K)
          END DO
          DO K = 1,KE+UH_DAY-1
            UH_S(N,K) = UH_S(N,K) / SUM
          END DO

        END DO

c  write out the grid for future reference...

        DO N = 1,NOB
          WRITE(98, *) (UH_S(N,K), K = 1,KE+UH_DAY-1)
        END DO        

      END IF 
      close(98)

      RETURN
      END




