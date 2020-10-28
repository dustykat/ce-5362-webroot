C
C
C
      SUBROUTINE   INITOP                                                 V  10
     O                       ( TRYCT )                                   KF 0389
C
C     + + + PURPOSE + + +
C     Initializes optimization routine.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TRYCT                                                    KF 0389
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TRYCT  - itteration count for set of parameters
C
C     + + + PARAMETERS + + +
      INCLUDE 'pinout.inc'                                               KF 0290
C
C     + + + COMMONS + + +
      INCLUDE 'cz1t4.inc'                                                KF 0389
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, J, K, L, LJ                                           KF 0389
      REAL      GOE, XX                                                  RSR1093
      CHARACTER FMTP*30, FMTP1(7)*4                                      RSR1093
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS                                                    KF 0389
C
C     + + + DATA INITIALIZATIONS + + +
      DATA FMTP1/'PSP ','KSAT','RGF ','BMSN','EVC ','RR  ','EAC'/        RSR1093
      DATA FMTP/'(1X,I3,3X,F12.6,3X,A4,3X,''+'') '/                      RSR1093
C  C     + + + FORMATS + + +
   13 FORMAT (   1X, 'Boundary check of parameter', I3, 3F10.3 )         KF 0389
   14 FORMAT ( '1',                                                      KF 0389
     #        //,1X, 'Initial parameter values (parameters to be ',      KF 0389
     #        /, 1X, '     optimized are marked with a +)', / )          KF 0389
   15 FORMAT (//,1X, 'Initial step size increments', / )                 KF 0389
   16 FORMAT (   1X, 6F12.6 )                                            KF 0389
   17 FORMAT (//,1X, 'The maximum number of iterations =', I5,           KF 0389
     #        //,1X, 'Initially and after each vector matrix ',          KF 0389
     #               'orthonormalization',                               KF 0389
     #        /, 1X, 'the parametric vector increment size is',          KF 0389
     #               F7.2, ' of the vector size', / )                    KF 0389
C
C     + + + END SPECIFICATIONS + + +
C
      OF=1.0E+29                                                          V 180
      DO 1 I=1,3                                                          V 190
    1 U(I)=0.0                                                            V 200
C                                                                       0184 KF
C-    - INPUT OF CARD GROUPS 12, 13, AND 14 MOVED TO INPUTN             0184 KF
C                                                                       0184 KF
      DO 4 I=1,200                                                        V 340
    4 A(I)=0.0                                                            V 350
      DO 5 I=1,14                                                         V 360
      D(I)=0.0                                                            V 370
      E(I)=0                                                              V 380
    5 F(I)=0                                                              V 390
      TRYCT=0                                                             V 400
C             CHECK IF INITIAL PARAMETER VALUE WITHIN OUTER BOUNDARY      V 410
      DO 7 I=1,EO                                                         V 420
      XX=X(I)                                                             V 430
      IF (XX.LE.G(I)) GO TO 6                                             V 440
      IF (XX.GE.H(I)) GO TO 6                                             V 450
C             STORE INITIAL PARAMETER VALUES                              V 460
      X(EO+I)=XX                                                          V 470
      GO TO 7                                                             V 480
C             IF PARAMETER VALUES NOT WITHIN BOUNDARY VALUES              V 490
C             PRINT ERROR MESSAGE                                         V 500
    6 WRITE (OUTFIL,13) I,G(I),X(I),H(I)                                DT 1089
      STOP                                                                V 520
    7 CONTINUE                                                            V 530
      DO 8 I=1,EO                                                         V 540
      L=2*EO+I                                                            V 550
      G(L)=0.0                                                            V 560
      H(L)=0.0                                                            V 570
      K=EO+I                                                              V 580
C             COMPUTE INNER BOUNDARY VALUES                               V 590
      GOE=(H(I)-G(I))*0.0001                                              V 600
      G(K)=G(I)+GOE                                                       V 610
    8 H(K)=H(I)-GOE                                                       V 620
C             ENTER A(I) = 0.0 INTO ARRAY                                 V 630
      L=FO+1                                                              V 640
      DO 9 I=1,FO                                                         V 650
      LJ=L*I                                                              V 660
      A(LJ)=1.0                                                           V 670
C             COMPUTE INITIAL STEP SIZE                                   V 680
      J=OPTNO(I)                                                          V 690
      A(I)=X(J)*EPSLN                                                     V 700
    9 CONTINUE                                                            V 710
C             DESCRIBE INFILTRATION PARAMETERS                            V 720
      WRITE (OUTFIL,14)                                                 DT 1089
      J=1                                                                 V 740
      IEAC=0                                                              V 750
      DO 10 I=1,EO                                                        V 760
      FMTP(22:28)='       '                                             RSR1093
      K=I                                                                 V 790
      IF (I.GT.7) K=I-7                                                   V 800
      IF (I.NE.OPTNO(J)) GO TO 10                                         V 810
      IF (I.EQ.7) IEAC=1                                                  V 820
      FMTP(22:28)=',3X,''+'''                                           RSR1093
      J=J+1                                                               V 850
   10 WRITE (OUTFIL,FMTP) I,X(I),FMTP1(K)                               DT 1089
      IF (ABS(X(7)-1.) .GT. 0.1E-9) IEAC = 1                            1184 KF
      NO=0                                                                V 880
      IF (NK.EQ.0) RETURN                                                 V 890
      WRITE (OUTFIL,15)                                                  KF 0290
      WRITE (OUTFIL,16) (A(I),I=1,FO)                                    KF 0290
      WRITE (OUTFIL,17) NK,EPSLN                                         KF 0290
      RETURN                                                              V 930
      END                                                                 V1060-
C
C
C
      SUBROUTINE OPTIMZ                                                   W  10
     M                 ( TRYCT,                                          KF 0389
     O                   IK )                                            KF 0389
C
C     + + + PURPOSE + + +
C     Modified Rosenbrock optimization routine.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TRYCT, IK                                                KF 0389
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TRYCT  - Iteration count for set of parameters
C     IK     - ???
C
C     + + + PARAMETERS + + +
      INCLUDE 'pinout.inc'                                               KF 0290
C
C     + + + COMMONS + + +
      INCLUDE 'cz1t4.inc'                                                KF 0389
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, I2, IFO, II, IJ, J, J2, K, L, LJ, LK, M               KF 0389
      REAL      B1, B2, BD, GD, HD, SF, UU, XX                           KF 0389
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS, SQRT                                              KF 0389
C
C
C     + + + DATA INITIALIZATIONS + + +
      DATA B1/0.0/,B2/0.0/                                                W 130
C
C     + + + FORMATS + + +
   35 FORMAT (//,1X, 'At iteration number', I3,                          KF 0389
     #        /, 1X, 'Objective function =', F11.6,                      KF 0389
     #        /, 1X, 'Parameter values are', / )                         KF 0389
   37 FORMAT (   1X, 1X, 7F11.6 )                                        KF 0389
   38 FORMAT (/, 1X, 'B1 =', F9.6,                                       KF 0389
     #        /, 1X, 'B2 =', F9.6 )                                      KF 0389
   39 FORMAT (/, 1X, 'New orthonormal basis' )                           KF 0389
   40 FORMAT (   1X, 8F8.5 )                                             KF 0389
   41 FORMAT (/, 1X, 'Start of stage step size increments',              KF 0389
     #        /,(1X, 7F10.6) )                                           KF 0389
C
C     + + + END SPECIFICATIONS + + +
      UU=U(NN)                                                            W 140
C             CHECK FOR IMPROVEMENT IN OBJECTIVE FUNCTION                 W 150
      IF (UU.GT.OF) GO TO 6                                               W 160
C               NEW OBJECTIVE FUNCTION LESS THAN OLD OBJ. FUNCTION        W 170
      DO 3 I=1,FO                                                         W 180
      M=OPTNO(I)                                                          W 190
      XX=X(M)                                                             W 200
      K=EO+M                                                              W 210
      L=2*EO+M                                                            W 220
C             CHECK ON INNER LOWER BOUNDARY                               W 230
      IF (XX.GE.G(K)) GO TO 1                                             W 240
      GD=(G(K)-XX)/(G(K)-G(M))                                            W 250
      HD=UU-G(L)                                                          W 260
      GO TO 2                                                             W 270
C             CHECK ON INNER UPPER BOUNDARY                               W 280
    1 IF (XX.LE.H(K)) GO TO 3                                             W 290
      GD=(XX-H(K))/(H(M)-H(K))                                            W 300
      HD=UU-H(L)                                                          W 310
    2 UU=UU+((-2.0*GD+4.0)*GD-3.0)*GD*HD                                  W 320
      IF (UU.GT.OF) GO TO 6                                               W 330
    3 CONTINUE                                                            W 340
C             SET OF TO NEW OBJ. FCT.                                     W 350
      OF=UU                                                               W 360
      DO 4 I=1,FO                                                         W 370
C             STORE OLD PARAMETER VALUE IN LAST THIRD OF MATRIX           W 380
      M=OPTNO(I)                                                          W 390
      XX=X(M)                                                             W 400
      K=EO+M                                                              W 410
      L=2*EO+M                                                            W 420
      X(L)=XX                                                             W 430
C             CHECK ON INNER BOUNDARIES                                   W 440
      IF (XX.GT.H(K)) GO TO 4                                             W 450
      IF (XX.LT.G(K)) GO TO 4                                             W 460
C             ENTER CURRENT OBJ. FCT. IN G + H ARRAYS                     W 470
      G(L)=UU                                                             W 480
      H(L)=UU                                                             W 490
    4 CONTINUE                                                            W 500
      IF (NO.EQ.0) GO TO 5                                                W 510
C             F(I)=1 IF NEW PARAMETER VALUE IMPROVES OBJ. FCT.            W 520
      F(NO)=1                                                             W 530
      E(NO)=0                                                             W 540
C             COMPUTE CUMULATIVE STEP SIZE                                W 550
      D(NO)=D(NO)+A(NO)                                                   W 560
C             COMPUTE NEXT FORWARD STEP SIZE                              W 570
      A(NO)=3.0*A(NO)                                                     W 580
    5 WRITE (OUTFIL,35) TRYCT,U(2)                                      DT 1089
      WRITE (OUTFIL,37) (X(I),I=1,EO)                                   DT 1089
      IF (TRYCT.NE.NK) GO TO 9                                            W 620
      B3=1                                                                W 630
      RITE=1                                                              W 640
      RETURN                                                              W 650
C             IF NEW OBJ. FCT. EXCEEDS OLD OBJ. FCT.                      W 660
C             SET PARAMETER TO PREVIOUS VALUE                             W 670
    6 M=2*EO                                                              W 680
      DO 7 I=1,FO                                                         W 690
      K=OPTNO(I)                                                          W 700
      LK=K+M                                                              W 710
    7 X(K)=X(LK)                                                          W 720
      GO TO 15                                                            W 730
C             ROUTINE TO COMPUTE NEW PARAMETER VALUE                      W 740
    8 IF (TRYCT.NE.NK) GO TO 9                                            W 750
      GO TO 5                                                             W 760
    9 TRYCT=TRYCT+1                                                       W 770
      OPTION=IOUT(2)                                                      W 780
      IF (NO.EQ.FO) GO TO 10                                              W 790
      NO=NO+1                                                             W 800
      GO TO 11                                                            W 810
   10 NO=1                                                                W 820
   11 DO 14 I=1,FO                                                        W 830
      K=OPTNO(I)                                                          W 840
      IFO=FO*I+NO                                                         W 850
      XX=X(K)+A(IFO)*A(NO)                                                W 860
      IF (XX.LE.G(K).OR.XX.GE.H(K)) GO TO 12                              W 870
      X(K)=XX                                                             W 880
      GO TO 14                                                            W 890
   12 L=2*EO                                                              W 900
      IF (I.EQ.1) GO TO 15                                                W 910
      II=I-1                                                              W 920
      DO 13 IJ=1,II                                                       W 930
      I2=II+1-IJ                                                          W 940
      K=OPTNO(I2)                                                         W 950
      LK=L+K                                                              W 960
   13 X(K)=X(LK)                                                          W 970
      GO TO 15                                                            W 980
   14 CONTINUE                                                            W 990
      RETURN                                                              W1000
C             COMPUTES BACK STEP LENGTH(WHEN NEW OBJ. FCT. > OLD)         W1010
   15 IF (TRYCT.NE.NK) GO TO 16                                           W1020
      B3=1                                                                W1030
      RITE=1                                                              W1040
      RETURN                                                              W1050
C             COMPUTE NEXT BACKWARD STEP SIZE                             W1060
   16 A(NO)=-0.5*A(NO)                                                    W1070
C             E(I)=1 INDICATES PARAMETER VALUE CHANGED BY BACKWARD        W1080
C             STEP SIZE                                                   W1090
      E(NO)=E(NO)+1                                                       W1100
C             DETERMINE IF BOTH BACKWARD AND FORWARD STEP SIZE            W1110
C             ADJUSTMENTS FAILED TO IMPROVE OBJ. FCT.                     W1120
      DO 17 I=1,FO                                                        W1130
      LJ=E(I)*F(I)                                                        W1140
      IF (LJ.LE.0) GO TO 8                                                W1150
   17 CONTINUE                                                            W1160
C             VECTOR ORTHONORMALIZED WHEN IPEF.GT.0 FOR ALL I             W1170
      DO 18 I=1,FO                                                        W1180
      L=FO*(I+1)                                                          W1190
      A(L)=D(FO)*A(L)                                                     W1200
      K=FO*I                                                              W1210
      IF (FO.EQ.1) GO TO 19                                               W1220
      LJ=FO-1                                                             W1230
      DO 18 LK=1,LJ                                                       W1240
      J2=FO-LK                                                            W1250
      L=K+J2                                                              W1260
   18 A(L)=D(J2)*A(L)+A(L+1)                                              W1270
C             NORMALIZE VECTOR LENGTHS TO 1.0                             W1280
   19 BD=0.0                                                              W1290
      DO 20 I=1,FO                                                        W1300
      LJ=FO*I+1                                                           W1310
   20 BD=A(LJ)**2+BD                                                      W1320
      B1=SQRT(BD)                                                         W1330
      DO 21 I=1,FO                                                        W1340
      L=FO*I+1                                                            W1350
   21 A(L)=A(L)/B1                                                        W1360
C             RECOMPUTE STEP SIZE INCREMENT                               W1370
      SF=0.0                                                              W1380
      DO 22 I=1,FO                                                        W1390
      K=OPTNO(I)                                                          W1400
      L=FO*I+1                                                            W1410
   22 SF=SF+ABS(A(L))*X(K)                                                W1420
      A(1)=SF*EPSLN                                                       W1430
      BD=0.0                                                              W1440
      DO 23 I=1,FO                                                        W1450
      IK=FO*I+2                                                           W1460
   23 BD=A(IK)**2+BD                                                      W1470
      B2=SQRT(BD)/B1                                                      W1480
      WRITE (OUTFIL,38) B1,B2                                           DT 1089
      J=2                                                                 W1500
   24 IF (FO.LT.J) GO TO 32                                               W1510
      K=1                                                                 W1520
      BD=0.0                                                              W1530
   25 IF (K.GE.J) GO TO 28                                                W1540
      DO 26 I=1,FO                                                        W1550
      L=FO*I                                                              W1560
      LJ=L+J                                                              W1570
      LK=L+K                                                              W1580
   26 BD=BD+A(LJ)*A(LK)                                                   W1590
      DO 27 I=1,FO                                                        W1600
      L=FO*I+J                                                            W1610
      LJ=L-J+K                                                            W1620
   27 A(L)=A(L)-A(LJ)*BD                                                  W1630
      K=K+1                                                               W1640
      BD=0.0                                                              W1650
      GO TO 25                                                            W1660
   28 DO 29 I=1,FO                                                        W1670
      LJ=FO*I+J                                                           W1680
   29 BD=A(LJ)**2+BD                                                      W1690
      BD=SQRT(BD)                                                         W1700
      DO 30 I=1,FO                                                        W1710
      L=FO*I+J                                                            W1720
   30 A(L)=A(L)/BD                                                        W1730
      SF=0.0                                                              W1740
      DO 31 I=1,FO                                                        W1750
      K=OPTNO(I)                                                          W1760
      L=FO*I+J                                                            W1770
   31 SF=SF+ABS(A(L))*X(K)                                                W1780
      A(J)=SF*EPSLN                                                       W1790
      J=J+1                                                               W1800
      GO TO 24                                                            W1810
   32 WRITE (OUTFIL,39)                                                 DT 1089
      DO 33 I=1,FO                                                        W1830
      LJ=I*FO+1                                                           W1840
      LK=I*FO+FO                                                          W1850
   33 WRITE (OUTFIL,40) (A(IJ),IJ=LJ,LK)                                DT 1089
      NO=0                                                                W1870
      WRITE (OUTFIL,41) (A(I),I=1,FO)                                   DT 1089
      DO 34 I=1,FO                                                        W1890
      D(I)=0.0                                                            W1900
      F(I)=0                                                              W1910
      K=OPTNO(I)                                                          W1920
      LJ=EO+K                                                             W1930
   34 X(LJ)=X(K)                                                          W1940
      RITE=1                                                              W1950
      RETURN                                                              W1960
      END                                                                 W2050-
