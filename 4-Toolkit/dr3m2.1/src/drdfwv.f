C
C
C
      SUBROUTINE DWM
     I              ( J, K, JTYPE, XSEG, PARAM1, PARAM2, SLOPE, DX,
     I                ALP, M, XEM, DTSX, QLAT, XA, QS, AR,
     M                XQ )
C
C     + + + PURPOSE + + +
C     Subroutine  DWM  solves for the unknowm flow discharge XQ(J)
C     by the variable-parameter Muskingum-Cunge diffusion wave
C     method. (see ASCE J. of Hydr. Div., Vol. 104, no. HY12,
C     December 1978.)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   J, K, JTYPE
      REAL      PARAM1, PARAM2, SLOPE, DX, ALP, M, XEM, DTSX, QLAT,
     >          XA(11), QS(11), AR(11), XQ(11)
      CHARACTER*4 XSEG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     J      - spatial node number in finite difference grid
C     K      - index number of segment
C     JTYPE  - type of segment
C     XSEG   - 4-character identifier for segment
C     PARAM1 - first parameter for segment, value depends on JTYPE
C     PARAM2 - second parameter for segment, value depends on JTYPE
C     SLOPE  - slope of segment
C     DX     - routing space step for segment
C     ALP    - alpha for segment
C     M      - M for segment
C     XEM    - 1.0/M
C     DTSX   - DT in seconds divided by DX
C     QLAT   - lateral inflow for segment
C     QS     - array of discharges at  n  time level
C     AR     - array of flow areas at  n  time level
C     XQ     - array of discharges at  n+1  time level
C     XA     - array of flow areas at  n+1  time level
C
C     + + + PARAMETERS + + +
      INCLUDE 'pinout.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER ITER, NITER, K1, CNVG
      REAL ALPM, ZEM, Q0, A0, CELX, CEL, QX, QR, C, R, CC, C0,
     +     C1, C2, C3, Q, DTNEW
C
C     + + + FUNCTIONS + + +
      REAL   TPWD
C
C     + + + INTRINSICS + + +
      INTRINSIC ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL TPWD
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  NITER, K1
     >     /  15,   0 /
C
C     + + + OUTPUT FORMATS + + +
 2050 FORMAT (//, 1X, '!!!! WARNING:  Iterations exceed limit in ',
     >                'Muskingum-Cunge routing ',
     >         /, 1X, '     for segment ', A4,
     >         /, 1X, '!!!!' )
 2080 FORMAT (//, 1X, '!!!! WARNING:  Courant number exceeds 20 for ',
     >                'segment', I3,
     >         /, 1X, '     Routing computations may be inaccurate, ',
     >                'try DT <', F6.0,' seconds',
     >         /, 1X, '     Courant number =', F6.1,
     >         /, 1X, '!!!!' )
C
C     + + + END SPECIFICATIONS + + +
C
      ALPM = ALP * M
      ZEM = M - 1.
C
C     .....assign first guess at  XQ(J)  and  XA(J) from KW soln.....
      Q0 = XQ(J)
      A0 = XA(J)
C
C     .....compute sum of wave celerity's and unit-width discharges
C          at the three known nodal points in the computational box.....
      IF(ABS(ZEM) .GT. 0.1E-9) THEN
        CELX = ALPM * (AR(J-1)**ZEM + AR(J)**ZEM + XA(J-1)**ZEM)
        QX   =   QS(J-1) / TPWD(JTYPE,PARAM1,PARAM2,AR(J-1))
     >         + QS(J)   / TPWD(JTYPE,PARAM1,PARAM2,AR(J))
     >         + XQ(J-1) / TPWD(JTYPE,PARAM1,PARAM2,XA(J-1))
      ELSE
C       special case of pipe segment when M=1
        CEL  = ALP
        QX   = (QS(J-1) + QS(J) + XQ(J-1))/TPWD(JTYPE,PARAM1,PARAM2,A0)
      END IF
C
C     .....begin iteration.....
      CNVG = 0
      ITER = 0
 20   CONTINUE
        ITER = ITER + 1
        IF(ABS(ZEM) .GT. 0.1E-9) THEN
C         .....compute 4-pt average wave celerity.....
          CEL  = ( CELX + (ALPM*(A0**ZEM)) )/4.0
        END IF
C
C       .....compute 4-pt average unit-width discharge.....
        QR   = ( QX + Q0 / TPWD(JTYPE,PARAM1,PARAM2,A0) ) / 4.0
C
C       .....compute  C=Courant number  and  R=cell Reynolds number.....
        C = CEL * DTSX
        R = QR / ( SLOPE * CEL * DX )
C
C       .....compute routing coefficients.....
        CC = 1. / (  1 + C + R )
        C0 = CC * ( -1 + C + R )
        C1 = CC * ( +1 + C - R )
        C2 = CC * ( +1 - C + R )
        C3 = 2. * CC * C
C
C       .....compute next estimate of Q.....
        Q = C0*XQ(J-1) + C1*QS(J-1) + C2*QS(J) + C3*QLAT*DX
C
        IF (Q .LE. 0.0) THEN
C         converged to zero, use previous value
          Q = Q0
          CNVG = 1
        ELSE IF (ABS(Q-Q0)/Q0 .LE. 0.001) THEN
C         converged to a solution
          CNVG = 1
        ELSE
C         compute next estimate of Q
          Q0 = Q
          A0 = (Q0/ALP)**XEM
        END IF
      IF (ITER .LT. NITER  .AND.  CNVG .EQ. 0) GO TO 20
C
      IF (CNVG .EQ. 0) THEN
C       max iterations reached without converging
        WRITE (OUTFIL,2050) XSEG
        Q = Q0
      END IF
C
C     .....final estimate of unknowns.....
      XQ(J) = Q
      XA(J) = (XQ(J)/ALP)**XEM
C
      IF((C .GT. 20.0) .AND. (K .NE. K1)) THEN
C       .....courant number exceeds 20.0.....
        K1 = K
        DTNEW = 10.0 * DX / CEL
        WRITE(OUTFIL,2080) K, DTNEW, C
      END IF
C
      RETURN
      END
C
C
C
      REAL   FUNCTION   TPWD
     I                      ( JTYPE, PARAM1, PARAM2, A )
C
C     + + + PURPOSE + + +
C     Returns the value of the mean top width of a segment corresponding
C     to the flow area  A  for use in the diffusion wave routing method.
C     For an invalid segment type, 0.0 is returned.  The values for
C     PARAM1 and PARAM2 are dependent on JTYPE:
C
C     JTYPE     segment type            PARAM1             PARAM2
C     _____  ___________________  __________________  ________________
C       1          gutter         gutter cross slope      not used
C       2           pipe            pipe diameter         not used
C       3     triangular x-sect     one side slope    other side slope
C       5    overland, turbulent       not used           not used
C       6     overland, laminar        not used           not used
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   JTYPE
      REAL      PARAM1, PARAM2, A
C
C     + + + ARGUMENT DEFINITIONS + + +
C     JTYPE  - type of segment, see purpose for details
C     PARAM1 - segment parameter, see purpose for details
C     PARAM2 - segment parameter, see purpose for details
C     A      - flow area for which the average top width is to be
C              computed.  Used for gutter and triangular segment types
C              (JTYPE = 1 or 3).
C
C     + + + PARAMETERS + + +
      INCLUDE 'pinout.inc'
C
C     + + + LOCAL VARIABLES + + +
      REAL DEPTH, Z, DIA, T
C
C     + + + INTRINSICS + + +
      INTRINSIC SQRT
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (//, 1X, '!!!! WARNING:  Invalid segment type for the ',
     >                'Diffusion Routing Method.',
     >         /, 1X, '     Allowable ITYPEs are 1-3 and 5-6.' ,
     >         /, 1X, '     Found ITYPE =', I3,
     >         /, 1X, ' !!!! ' )
 2007 FORMAT (//, 1X, '!!!! WARNING:  Diffusion Routing Method cannot ',
     >                ' be used for a channel segment',
     >         /, 1X, '     where ITYPE = 4.',
     >         /, 1X, '!!!!' )
C
C     + + + END SPECIFICATIONS + + +
C
      IF (JTYPE .GT. 0  .AND.  JTYPE .LT. 7) THEN
C       valid segment type, compute top width
        GO TO ( 1, 2, 3, 4, 5, 6 ), JTYPE
 1      CONTINUE
C         gutter segment
          IF (A .GE. 1.E-9) THEN
            Z = PARAM1
            DEPTH = (1.4142/SQRT(Z)) * SQRT(A)
            T = 0.5*Z*DEPTH
          ELSE
C           no area
            T = 1.E-5
          END IF
          GO TO 100
 2      CONTINUE
C         pipe segment
          DIA = PARAM1
C         t = pi * diameter / 4
          T = .7854*DIA
          GO TO 100
 3      CONTINUE
C         triangular segment
          IF (A .GE. 1.E-9) THEN
            Z = PARAM1 + PARAM2
            DEPTH = (1.4142/SQRT(Z)) * SQRT(A)
            T = 0.5 * Z * DEPTH
          ELSE
C           no area
            T = 1.E-5
          END IF
          GO TO 100
 4      CONTINUE
C         segment where  ALP  and  M  are defined explicitly, invalid
          WRITE (OUTFIL,2007)
          T = 0.0
          GO TO 100
 5      CONTINUE
C         overland-flow segment (turbulent)
          T = 1.0
          GO TO 100
 6      CONTINUE
C         overland-flow segment (laminar)
          T = 1.0
          GO TO 100
 100    CONTINUE
      ELSE
C       invalid segment type
        WRITE (OUTFIL,2000) JTYPE
        T = 0.0
      END IF
C
      TPWD = T
C
      RETURN
      END
