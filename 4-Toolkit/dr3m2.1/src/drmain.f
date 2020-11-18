C
C
C
      PROGRAM   DR3M                                                     KF 0389
C
C     + + + PURPOSE + + +
C     A watershed model for routing storm runoff through a branched
C     system of pipes and (or) natural channels using rainfall as
C     input.  The model provieds detailed simulation of storm-runoff
C     periods selected by the user and a daily soil-moisture accounting
C     between storms.  A drainage basin is represented as a set of
C     overland-flow, channel, and reservoir segments which jointly
C     describe the drainage features of the basin.  Kinematic wave
C     theory is used for routing flows over contributing overland-
C     flow areas and through the channel network.  A set of model
C     segments can be arranged into a network that will represent many
C     complex drainage basins.  The model is intended primarily for
C     application to urban watersheds, but may have limited applications
C     to rural watersheds.
C
C     + + + PARAMETERS + + +
      INCLUDE 'dinout.inc'                                               KF 0290
      INCLUDE 'pinout.inc'                                               KF 0290
      INCLUDE 'plimt.inc'                                                KF 0389
C
C     + + + COMMONS + + +
      INCLUDE 'chead.inc'                                                KF 0389
      INCLUDE 'csgsc.inc'                                                KF 0191
      INCLUDE 'csgs1.inc'                                                KF 0191
      INCLUDE 'csgs3.inc'                                                KF 0191
      INCLUDE 'cstrm.inc'                                                KF 0389
      INCLUDE 'cc7t8.inc'                                                KF 0389
      INCLUDE 'cd1.inc'                                                  KF 0389
      INCLUDE 'cunit.inc'                                                KF 0389
      INCLUDE 'cf1a3.inc'                                                KF 0389
      INCLUDE 'cz1t4.inc'                                                KF 0389
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I, IBLANK, IFP, III, J, K, N, NS, RETC,               KF 0290
     >             DTFLW, TUFLW, MXNS, NONCH                             KF 0191
      REAL         PTEMP, YMAX, DT, DTS                                  KF 0191
      CHARACTER*64 VERSN                                                 KF 0494
C
C     + + + EXTERNALS + + +
      EXTERNAL   AM, CORR, CTCHMT, INITOP, INPUTN, OPTIMZ,               KF 0290
     #           SEQ, SIMUL, SVHYD, DRFILE, AREA                         KF 0191
C
C     + + + DATA INITIALIZATIONS + + +
      DATA IBLANK/4H    /                                                 A 380
C
C     + + + FORMATS + + +
   21 FORMAT (  '1',                                                     KF 0389
     #        /, 1X, 78('*'),                                            KF 0389
     #        /, 1X, '*', 76X, '*',                                      KF 0389
     #        /, 1X, '*', 27X, 'U.S. Geological Survey', 27X, '*',       KF 0389
     #        /, 1X, '*', 18X, 'Distributed Routing Rainfall-Runoff ',   KF 0389
     #                         'Model', 17X, '*',                        KF 0389
     #        /, 1X, '*', 26X, '      August, 1995       ', 25x, '*',    KF 0390
     #        /, 1X, '*', 76X, '*',                                      KF 0389
     #        /, 1X, 78('*') )                                           KF 0389
   22 FORMAT ('1',                                                       KF 0389
     #        //,1X, 'Segment discharges stored in file:',               KF 0389
     #        //,1X, 'storm            no. of  starting',                KF 0389
     #        /, 1X, ' no.     date    values   record ',                KF 0389
     #        /, 1X, '-----  --------  ------  --------' )               KF 0389
   23 FORMAT (   1X, I3, I6,'/',I2,'/',I2, I6, I10 )                     KF 0389
Ckf24 FORMAT (16X,50HPLOT OF MEAS.(HORIZ. AXIS) VERSUS SIM.(VERT. AXIS))  A1280
Ckf25 FORMAT (16X,7HVOLUMES)                                              A1290
Ckf26 FORMAT (16X,5HPEAKS)                                                A1300
Ckf27 FORMAT (79X,1H9)                                                   KF 1193
   27 FORMAT (//,1X, 'Note:  The option to output the simulated '        KF 1193
     $               'hydrograph is not available.',                     KF 1193
     $         /,1X, '       Use the option to output the segment '      KF 1193
     $               '       outflow (segment record)' )                 KF 1193
 2000 FORMAT (//,1X, 'Error opening files '                              KF 0290
     #        /, 1X, ' check your master file' )                         KF 0290
 2028 FORMAT ('1',                                                       KF 0389
     #        //,1X, 'Segment discharges stored in WDM file:',           KF 0389
     #        //,1X, 'storm            no. of',                          KF 0389
     #        /, 1X, ' no.     date    values',                          KF 0389
     #        /, 1X, '-----  --------  ------' )                         KF 0389
 2100 FORMAT (   1X, 'Setting up optimization' )                         KF 0290
 2102 FORMAT (   1X, 'Beginning storm simulations' )                     KF 0290
 2103 FORMAT (   1X, 'Finished storm simulations' )                      KF 0290
C
C     + + + END SPECIFICATIONS + + +
C
C     include version and unix what information                          KF 0494
      INCLUDE 'versn.inc'                                                KF 0494
C
C     open files                                                         KF 0290
      CALL DRFILE ( RETC )                                               KF 0390
      IF (RETC .NE. 0) THEN                                              KF 0290
C       problem opening files                                            KF 0290
        WRITE (IOOT,2000)                                                KF 0290
        STOP                                                             KF 0389
      END IF
C                                                                       II060183
      WRITE (OUTFIL,21)                                                 DT 1089
      B3=0                                                                A 400
      RITE=1                                                              A 410
C             SET ARRAY LIMITS                                            A 450
      IUNIT=2881                                                          A 460
      NDYS=7310                                                           A 470
      NDTS=1442                                                           A 480
C             INITIALIZE HEADER ARRAYS FOR DIRECT ACCESS FILE             A 490
      DO 11 I=22,120                                                      A 500
   11 HEAD1(I)=IBLANK                                                     A 510
      DO 12 J=1,60                                                        A 520
      HEAD2(J,1)=0                                                        A 530
      HEAD2(J,2)=0                                                        A 540
   12 HEAD3(J)=0                                                          A 550
      NRECDS=3                                                            A 560
      NSTRMS=0                                                            A 570
C             CALL PROGRAM SUBROUTINES                                    A 580
C                                                                       0184 KF
C-    - SUBROUTINE INPUTN REPLACES SUBROUTINE INPUT1                    0184 KF
C-    - AND MOST OF SUBROUTINE INPUT2.                                  0184 KF
C                                                                       0184 KF
      CALL INPUTN                                                       0184 KF
      IF (OPT.EQ.0) REWIND IFILED                                         A 600
      DO 13 I=1,NRG                                                       A 610
      IFP=IFILEP+I-1                                                      A 620
      REWIND IFP                                                          A 630
   13 CONTINUE                                                            A 640
      CALL INITOP ( TRYCT )                                              KF 0389
      CALL CTCHMT ( DT, DTS, DTFLW, TUFLW )                              KF 0191
      MXNS = MXSG                                                        KF 0191
      CALL AM ( NSEG, MXNS, ITYPE, PARAM, FRN, SLOPE, ALPADJ,            KF 0191
     O          ALPHA, EM, QMAX )                                        KF 0191
      CALL SEQ ( NSEG, MXNS, ISEG, IUP, ILAT, ITYPE, ALPHA, EM,          KF 0191
     O           NONCH, JLAT, JUP, KSEG, ITEST )                         KF 0191
      CALL AREA ( NSEG, MXNS, NPAR, NRG, JLAT, JUP, KPSET, KSEG,         KF 0191
     I            ITYPE, FLGTH, PARAM, RCOEF, RAT,                       KF 0191
     M            DA, NONCH,                                             KF 0191
     O            DIMP, DA1, DA2, DA3 )                                  KF 0191
C                                                                       0184 KF
C-    - SUBROUTINE INPUT2 NOW CONTAINED IN SUBROUTINES                  0184 KF
C-    - INPUTN AND CTCHMT.                                              0184 KF
C                                                                       0184 KF
      GO TO 15                                                            A 710
   14 CONTINUE                                                           KF 0290
      WRITE (IOOT,2100)                                                  KF 0290
      CALL OPTIMZ ( TRYCT, IK )                                          KF 0290
   15 CONTINUE                                                            A 730
      WRITE (IOOT,2102)                                                  KF 0290
      CALL SIMUL ( DT, DTS, DTFLW, TUFLW )                               KF 0191
      WRITE (IOOT,2103)                                                  KF 0290
      IF (B3.EQ.0) GO TO 14                                               A 750
C          END OF SIMULATION LOOP                                         A 760
      IF (JPUN.GE.1) WRITE (OUTFIL,27)                                   KF 1193
C          COMPUTE CORRELATION COEFFICIENT FOR PEAKS                      A 780
      IF (OPT.EQ.0) CALL CORR(NOFE,SFPK,FPK)                              A 790
C             PLOT SIM. VS. MEAS. VOLUMES                                 A 800
C             AND SIM. VS. MEAS. PEAKS                                    A 810
      PTEMP = PTIME                                                     1184 KF
      DO 19 K=1,2                                                         A 820
      YMAX=0.0                                                            A 830
      N=0                                                                 A 840
      DO 18 I=1,NOFE                                                      A 850
      IF (K.EQ.2) GO TO 16                                                A 860
      IF (SFVOL(I).LE.0.0.OR.FVOL(I).LE.0.0) GO TO 18                     A 870
      IF (NK.GT.0.AND.TESTNO(I).NE.1) GO TO 18                            A 880
      N=N+1                                                               A 890
      Q(N)=FVOL(I)                                                        A 900
      R(N)=SFVOL(I)                                                       A 910
      GO TO 17                                                            A 920
   16 IF (SFPK(I).LE.0.0.OR.FPK(I).LE.0.0) GO TO 18                       A 930
      N=N+1                                                               A 940
      Q(N)=FPK(I)                                                         A 950
      R(N)=SFPK(I)                                                        A 960
   17 IF (Q(N).GT.YMAX) YMAX=Q(N)                                         A 970
      IF (R(N).GT.YMAX) YMAX=R(N)                                         A 980
   18 CONTINUE                                                            A 990
      IF (N.LT.2) GO TO 19                                                A1000
      PTIME=100.                                                          A1010
Ckf   CALL PLT(Q,R,N,1,YMAX)                                            0289 KF
Ckf   WRITE (OUTFIL,24)                                                 DT 1089
Ckf   IF (K.EQ.1) WRITE (OUTFIL,25)                                     DT 1089
Ckf   IF (K.EQ.2) WRITE (OUTFIL,26)                                     DT 1089
   19 CONTINUE                                                            A1060
      PTIME = PTEMP                                                     1184 KF
      IF (JPERM.EQ.0 .AND. JPTG .EQ. 0) STOP                            1184 KF
      HEAD1(16)=NRECDS                                                    A1160
      HEAD1(17)=NSTRMS                                                    A1170
      IF (JPERM .EQ. 1) THEN                                             KF 0389
C             IF JPERM=1, STORE AND OUTPUT HEADER ARRAYS                  A1080
        IRECD=1                                                           A1180
C       WRITE (IFILE'IRECD) HEAD1,HEAD2,HEAD3                           DD061083
        WRITE (IFILE,REC=2)     HEAD2                                   II061083
        WRITE (IFILE,REC=3)     HEAD3                                   II061083
        WRITE (OUTFIL,22)                                               DT 1089
        NS=0                                                              A1100
        DO 20 I=1,NOFE                                                    A1110
          IF (KOUT(I).EQ.0) GO TO 20                                      A1120
          NS=NS+1                                                         A1130
          WRITE (OUTFIL,23) I,(NDATE(I,III),III=1,3),HEAD2(NS,2),       DT 1089
     *                      HEAD2(NS,1)
   20   CONTINUE                                                          A1150
      ELSE IF (JPERM .EQ. 2) THEN                                        KF 0389
C       segment Q saved to WDM file
        WRITE (OUTFIL,2028)                                             DT 1089
        NS = 0                                                           KF 0389
        DO 30 I = 1, NOFE                                                KF 0389
          IF (KOUT(I) .NE. 0) THEN                                       KF 0389
            NS = NS + 1                                                  KF 0389
            WRITE (OUTFIL,23) I, (NDATE(I,III), III = 1, 3), HEAD2(NS,2)DT 1089
          END IF                                                         KF 0389
   30   CONTINUE                                                         KF 0389
      END IF                                                             KF 0389
      IF(JPTG .GT. 0) CALL SVHYD ( OPT, NRG, PTIME, DTFLW, TUFLW )       KF 0191
      STOP                                                                A1200
      END                                                                 A1320-
C
C
C
      SUBROUTINE   SEQ                                                    X  10
     I                 ( NSEG, MXNS, ISEG, IUP, ILAT, ITYPE, ALPHA, EM,  KF 0191
     O                   NONCH, JLAT, JUP, KSEG, ITEST )                 KF 0191
C
C     + + + PURPOSE + + +
C     This subroutine sets up computational sequence.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NSEG, MXNS, JLAT(MXNS,4), JUP(MXNS,3), KSEG(NSEG),       KF 0191
     >          ITYPE(NSEG), ITEST(NSEG), NONCH                          KF 0191
      REAL      ALPHA(NSEG), EM(NSEG)                                    KF 0191
      CHARACTER*4 ISEG(NSEG), IUP(MXNS,3), ILAT(MXNS,4)                  KF 0191
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NSEG   -
C     MXNS   -
C     ISEG   -
C     IUP    -
C     ILAT   -
C     ITYPE  -
C     ALPHA  -
C     EM     -
C     NONCH  -
C     JLAT   -
C     JUP    -
C     KSEG   -
C     ITEST  -
C
C     + + + PARAMETERS + + +
      INCLUDE 'pinout.inc'                                               KF 0290
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, II, J, K, N, NIT, NN                                  KF 0191
C
C     + + + FUNCTIONS + + +
      INTEGER   ITRAN                                                    KF 0389
C
C     + + + EXTERNALS + + +
      EXTERNAL    ITRAN
C
C     + + + FORMATS + + +
   30 FORMAT ( '1',                                                      KF 0389
     #        //,1X, 'computation sequence   kinematic wave parameters', KF 0389
     #        /, 1X, '--------------------   -------------------------', KF 0389
     #        /, 1X, 'index  segment               alpha       M      ', KF 0389
     #        /, 1X, '-----  -------              ------     ------   ') KF 0389
   31 FORMAT (   1X, I4, 5X, A4, 4X, 'Missing inflow segment.' )         KF 0389
   32 FORMAT (   1X, I4, 5X, A4, 11X, F10.2, 1X, F10.3 )                 KF 0389
   33 FORMAT (//,1X, 'Error in ordering segments.', / )                  KF 0389
C
C     + + + END SPECIFICATIONS + + +
      DO 2 I=1,NSEG                                                       X 150
      ITEST(I)=0                                                          X 160
      DO 1 J=1,3                                                          X 170
      JUP(I,J)=ITRAN ( NSEG, IUP(I,J), ISEG )                            KF 0191
    1 CONTINUE                                                            X 200
      DO 2 J=1,4                                                          X 210
      JLAT(I,J)=ITRAN ( NSEG, ILAT(I,J), ISEG )                          KF 0191
    2 CONTINUE                                                            X 240
      II=0                                                                X 250
C             ORDER OVERLAND FLOW SEGMENTS FIRST                          X 260
      DO 7 I=1,NSEG                                                       X 270
      IF (ITYPE(I)-5) 3,4,3                                               X 280
    3 IF (ITYPE(I)-6) 7,4,7                                               X 290
    4 DO 6 J=1,3                                                          X 300
      IF (JUP(I,J)) 6,6,5                                                 X 310
    5 GO TO 28                                                            X 320
    6 CONTINUE                                                            X 330
      II=II+1                                                             X 340
      KSEG(II)=I                                                          X 350
      ITEST(I)=1                                                          X 360
    7 CONTINUE                                                            X 370
      NONCH=II                                                            X 380
C             CHECK EACH SEGMENT TO SEE IF IT HAS BEEN SEQUENCED          X 390
      I=1                                                                 X 400
      NIT=0                                                               X 410
    8 IF (ITEST(I)) 12,12,9                                               X 420
    9 I=I+1                                                               X 430
C             CHECK IF SEGMENT SEQUENCING IS COMPLETED AND FOR ERRORS     X 440
      IF (I-NSEG) 8,8,10                                                  X 450
   10 I=1                                                                 X 460
      NIT=NIT+1                                                           X 470
      IF (NIT-3*NSEG) 11,11,28                                            X 480
   11 IF (II-NSEG) 8,20,20                                                X 490
   12 N=0                                                                 X 500
C             CHECK SEGMENT FOR UPSTREAM SEGMENTS WHICH HAVE NOT          X 510
C             BEEN SEQUENCED YET                                          X 520
      DO 15 J=1,3                                                         X 530
      IF (JUP(I,J)) 15,15,13                                              X 540
   13 K=JUP(I,J)                                                          X 550
      IF (ITEST(K)) 14,14,15                                              X 560
   14 N=1                                                                 X 570
   15 CONTINUE                                                            X 580
C             CHECK SEGMENT FOR ANY LATERAL INFLOW SEGMENTS WHICH         X 590
C             HAVE NOT BEEN SEQUENCED YET                                 X 600
      DO 18 J=1,4                                                         X 610
      IF (JLAT(I,J)) 18,18,16                                             X 620
   16 K=JLAT(I,J)                                                         X 630
      IF (ITEST(K)) 17,17,18                                              X 640
   17 N=1                                                                 X 650
   18 CONTINUE                                                            X 660
C             IF SEGMENT HAS NO UNSEQUENCED UPSTREAM OR LATERAL INFLOW    X 670
C             SEGMENTS,SEQUENCE IT NEXT                                   X 680
      IF (N) 19,19,9                                                      X 690
   19 II=II+1                                                             X 700
      KSEG(II)=I                                                          X 710
      ITEST(I)=1                                                          X 720
      IF (II-NSEG) 9,20,20                                                X 730
C             OUTPUT COMPUTATION SEQUENCE                                 X 740
   20 N=0                                                                 X 750
      WRITE (OUTFIL,30)                                                 DT 1089
      DO 27 I=1,NSEG                                                      X 770
      K=KSEG(I)                                                           X 780
      IF (ITYPE(K).EQ.8) GO TO 21                                         X 790
      IF (ITYPE(K)-4) 21,21,26                                            X 800
C             CHECK FOR CHANNELS WITH MISSING INFLOW SEGMENT              X 810
   21 NN=0                                                                X 820
      DO 24 J=1,3                                                         X 830
      IF (JLAT(K,J)) 22,22,23                                             X 840
   22 IF (JUP(K,J)) 24,24,23                                              X 850
   23 NN=1                                                                X 860
   24 CONTINUE                                                            X 870
      IF (NN) 25,25,26                                                    X 880
   25 N=1                                                                 X 890
      WRITE (OUTFIL,31) K,ISEG(K)                                       DT 1089
      GO TO 27                                                            X 910
   26 WRITE (OUTFIL,32) K,ISEG(K),ALPHA(K),EM(K)                        DT 1089
C             CHECK FOR INPUT DATA ERROR                                  X 930
   27 CONTINUE                                                            X 940
      IF (N) 29,29,28                                                     X 950
   28 WRITE (OUTFIL,33)                                                 DT 1089
      STOP                                                                X 970
   29 CONTINUE                                                            X 980
      RETURN                                                              X1000
      END                                                                 X1070-
C
C
C
      SUBROUTINE   AREA                                                   Y  10
     I                  ( NSEG, MXNS, NPAR, NRG, JLAT, JUP, KPSET,       KF 0191
     I                    KSEG, ITYPE, FLGTH, PARAM, RCOEF, RAT,         KF 0191
     M                    DA, NONCH,                                     KF 0191
     O                    DIMP, DA1, DA2, DA3 )                          KF 0191
C
C     + + + PURPOSE + + +
C     Checks computed drainage area versus furnished drainage area.
C     Determines pervious and impervious areas covered by each rain
C     gage for each soil type.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NSEG, MXNS, NPAR, NRG, NONCH                             KF 0191
      INTEGER   JLAT(MXNS,4), JUP(MXNS,3), KPSET(NSEG), KSEG(NSEG),      KF 0191
     >          ITYPE(NSEG)                                              KF 0191
      REAL      FLGTH(NSEG), PARAM(MXNS,2), RCOEF(MXNS,3), RAT,          KF 0191
     >          DA, DIMP(9,2), DA1, DA2, DA3                             KF 0191
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NSEG
C     MXNS
C     NPAR
C     NRG
C     JLAT
C     JUP
C     KPSET
C     KSEG
C     ITYPE
C     FLGTH
C     PARAM
C     RCOEF
C     RAT
C     DA     - basin drainage area, in square miles
C     NONCH  - ???
C     DIMP   - for each raingage and each soil type, contains the
C              total effective impervious area, the pervious area,
C              and the noneffective impervious area
C     DA1
C     DA2
C     DA3
C
C     + + + PARAMETERS + + +
      INCLUDE 'pinout.inc'                                               KF 0290
      INCLUDE 'plimt.inc'                                                KF 0389
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, III, J, JK, JKK, K, KI, KJ, KJJ, KK, KL, KL1, KLL,    KF 0389
     #          KM, KM1, KMK, KMM, KMM1, KMP, KMQ, KP, KP1, KS(MXSG), L  KF 0389
      REAL      DAT, DAT1, DAT2, DT(200,6,2), DTEMP, DTEMP1, PK, RK      KF 0389
C
C     + + + FORMATS + + +
   20 FORMAT (//,1X, 'Drainage basin areas',                             KF 0389
     #        //,1X, F8.4, ' - furnished drainage area (square miles)',  KF 0389
     #        /, 1X, F8.4, ' - computed drainage area (square miles)' )  KF 0389
   21 FORMAT (   7X, '** These areas differ by more than one percent' )  KF 0389
   22 FORMAT (   1X, F8.1, ' - percent effective impervious area',       KF 0389
     #        /, 1X, F8.1, ' - percent noneffective impervious area',    KF 0389
     #        /, 1X, F8.1, ' - percent pervious area',                   KF 0389
     #        /      )                                                   KF 0290
   23 FORMAT (   1X, 'Theissen coefficients for overland flow should ',  KF 0389
     #               'sum to 1.0' )                                      KF 0389
C
C     + + + END SPECIFICATIONS + + +
      DA1=0.0                                                             Y 160
      DA2=0.0                                                             Y 170
      DA3=0.0                                                             Y 180
      DO 1 J=1,2                                                          Y 190
        DO 1 I=1,9                                                        Y 200
          DIMP(I,J)=0.0                                                   Y 210
 1    CONTINUE
      KLL=NSEG+100                                                        Y 220
      DO 4 L=1,NPAR                                                       Y 230
        DO 3 I=1,KLL                                                      Y 240
          DO 2 J=1,6                                                      Y 250
            DT(I,J,L)=0.0                                                 Y 260
 2        CONTINUE
 3      CONTINUE                                                          Y 270
 4    CONTINUE                                                            Y 280
      DAT=0.0                                                             Y 290
C             CALCULATE PERVIOUS AND IMPERVIOUS DRAINAGE AREA FROM        Y 300
C             OVERLAND FLOW SEGMENTS INTO EACH CHANNEL FOR EACH RAIN      Y 310
C             GAGE AND SOIL TYPE                                          Y 320
      DO 9 I=1,NSEG                                                       Y 330
        IF (ITYPE(I) .EQ. 5  .OR.  ITYPE(I) .EQ. 6) THEN                 KF 0191
C         turbulent (5) or laminar (6) overland flow
          DO 8 KL=1,NSEG                                                  Y 350
            DO 7 KM=1,4                                                   Y 360
              KL1=KL+NSEG                                                 Y 370
              IF (I .EQ. JLAT(KL,KM)) THEN                               KF 0191
                RK=0.0                                                    Y 390
                DO 5 KMM=1,NRG                                            Y 400
                  RK=RK+RCOEF(I,KMM)                                      Y 410
 5              CONTINUE
                IF (RK.LT.0.98.OR.RK.GT.1.02) WRITE (OUTFIL,23)          DT 1089
                DO 6 KK=1,NPAR                                            Y 430
                  PK=0.0                                                  Y 440
                  IF (KPSET(I).EQ.KK) PK=1.0                              Y 450
                  DO 6 KMM=1,3                                            Y 460
                    KMM1=KMM+3                                            Y 470
                    DTEMP=FLGTH(KL)*FLGTH(I)*RCOEF(I,KMM)*PK*PARAM(I,1)   Y 480
                    DT(KL,KMM1,KK)=DT(KL,KMM1,KK)+DTEMP*PARAM(I,2)        Y 490
                    DT(KL1,KMM1,KK)=DT(KL1,KMM1,KK)                       Y 500
     >                              +DTEMP*(1.0-PARAM(I,2))
                    DTEMP=DTEMP/RK                                        Y 510
                    DT(KL,KMM,KK)=DT(KL,KMM,KK)+DTEMP*PARAM(I,2)          Y 520
                    DT(KL1,KMM,KK)=DT(KL1,KMM,KK)+DTEMP*(1.-PARAM(I,2))   Y 530
 6              CONTINUE                                                  Y 540
              END IF                                                     KF 0191
 7          CONTINUE                                                      Y 550
 8        CONTINUE                                                        Y 560
        END IF                                                           KF 0191
 9    CONTINUE                                                            Y 570
C             AGGREGATE DRAINAGE AREA FOR EACH CHANNEL SEGMENT FOR        Y 580
C             EACH RAIN GAGE FOR EACH SOIL TYPE                           Y 590
      KM1=NSEG-NONCH                                                      Y 600
      IF (KM1 .NE. 1) THEN                                               KF 0191
        DO 10 I=1,KM1                                                     Y 620
          NONCH=NONCH+1                                                   Y 630
          KS(I)=KSEG(NONCH)                                               Y 640
 10     CONTINUE                                                          Y 650
        KM1=KM1-1                                                         Y 660
        DO 14 KI=1,KM1                                                    Y 670
          KJ=KS(KI)                                                       Y 680
          KJJ=KJ+NSEG                                                     Y 690
          KP1=KI+1                                                        Y 700
          K=KM1+1                                                         Y 710
          DO 13 I=KP1,K                                                   Y 720
            JK=KS(I)                                                      Y 730
            JKK=JK+NSEG                                                   Y 740
            DO 12 KMK=1,3                                                 Y 750
              IF (JUP(JK,KMK) .EQ. KJ) THEN                              KF 0191
                DO 11 KMM=1,NRG                                           Y 770
                  KMM1=KMM+3                                              Y 780
                  DO 11 KK=1,NPAR                                         Y 790
                    DT(JK,KMM,KK)=DT(JK,KMM,KK)+DT(KJ,KMM,KK)             Y 800
                    DT(JK,KMM1,KK)=DT(JK,KMM1,KK)+DT(KJ,KMM1,KK)          Y 810
                    DT(JKK,KMM,KK)=DT(JKK,KMM,KK)+DT(KJJ,KMM,KK)          Y 820
                    DT(JKK,KMM1,KK)=DT(JKK,KMM1,KK)+DT(KJJ,KMM1,KK)       Y 830
 11             CONTINUE                                                  Y 840
              END IF                                                     KF 0191
 12         CONTINUE                                                      Y 850
 13       CONTINUE                                                        Y 860
 14     CONTINUE                                                          Y 870
      END IF                                                             KF 0191
C             CALCULATE TOTAL DRAINAGE AREA AND TOTAL PERVIOUS AND        Y 880
C             IMPERVIOUS AREA FOR EACH RAIN GAGE AND SOIL TYPE            Y 890
      K=KSEG(NSEG)                                                        Y 900
      KP=K+NSEG                                                           Y 910
      DTEMP1=0.0                                                          Y 920
      DO 17 KMM=1,3                                                       Y 930
        KMP=KMM+3                                                         Y 940
        KMQ=KMM+6                                                         Y 950
        DO 16 KK=1,NPAR                                                   Y 960
          DAT=DAT+DT(K,KMM,KK)+DT(KP,KMM,KK)                              Y 970
          DTEMP1=DTEMP1+DT(K,KMP,KK)+DT(KP,KMP,KK)                        Y 980
          DIMP(KMM,KK)=DT(K,KMM,KK)                                       Y 990
          DIMP(KMP,KK)=DT(KP,KMM,KK)/RAT                                  Y1000
          DIMP(KMQ,KK)=DT(KP,KMM,KK)-DIMP(KMP,KK)                         Y1010
 16     CONTINUE                                                          Y1020
        DIMP(KMM,1)=DIMP(KMM,1)+DIMP(KMM,2)                               Y1030
 17   CONTINUE                                                            Y1040
      DO 18 III=1,NRG                                                     Y1050
        KMP=III+3                                                         Y1060
        KMQ=KMP+3                                                         Y1070
        DA1=DA1+DIMP(III,1)/DAT*100.                                      Y1080
        DA3=DA3+(DIMP(KMP,1)+DIMP(KMP,2))/DAT*100.                        Y1090
        DA2=DA2+(DIMP(KMQ,1)+DIMP(KMQ,2))/DAT*100.                        Y1100
 18   CONTINUE                                                            Y1110
C             CHECK COMPUTED DRAINAGE AREA WITH FURNISHED  DRAINAGE AREA  Y1120
      DAT=DAT/(5280.*5280.)                                               Y1130
      DAT1=DAT/DA                                                         Y1140
      DAT2=DA/DAT                                                         Y1150
      WRITE (OUTFIL,20) DA,DAT                                          DT 1089
      IF (DAT1 .GE. 1.01  .OR.  DAT2 .GE. 1.01) THEN                     KF 0191
C       areas differ by more than one percent
        WRITE (OUTFIL,21)                                               DT 1089
      END IF                                                             KF 0191
   19 WRITE (OUTFIL,22) DA1,DA2,DA3                                     DT 1089
      DA=DAT                                                              Y1200
      DA1=DA1*DA/100.                                                     Y1210
      DA2=DA2*DA/100.                                                     Y1220
      DA3=DA3*DA/100.                                                     Y1230
      RETURN                                                              Y1240
      END                                                                 Y1340-
C
C
C
      INTEGER   FUNCTION   ITRAN                                         AA  10
     I                           ( NSEG, SEG, SEGS )                     KF 0191
C
C     + + + PURPOSE + + +
C     Numbers lateral and upstream inflow segments to correspond to
C     the ISEG's.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     NSEG
      CHARACTER*4 SEG, SEGS(NSEG)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NSEG   -
C     SEG    -
C     SEGS   -
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I                                                        KF 0389
C
C     + + + END SPECIFICATIONS + + +
C
      I=0                                                                KF 0191
      ITRAN = 0                                                          KF 0191
 100  CONTINUE                                                           KF 0191
C       increment pointer                                                KF 0191
        I = I + 1                                                        KF 0191
        IF (SEG .EQ. SEGS(I)) THEN                                       KF 0191
C         found matching segment                                         KF 0191
          ITRAN = I                                                      KF 0191
        END IF                                                           KF 0191
      IF (I .LT. NSEG  .AND.  ITRAN .EQ. 0) GO TO 100                    KF 0191
      RETURN                                                             AA 130
      END                                                                AA 140-
C
C
C
      SUBROUTINE   AM                                                    AB  10
     I               ( NSEG, MXNS, ITYPE, PARAM, FRN, SLOPE, ALPADJ,     KF 0191
     O                 ALPHA, EM, QMAX )                                 KF 0191
C
C     + + + PURPOSE + + +
C     Computes the parameters alpha and em and the full-segment flow
C     for each segment.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NSEG, MXNS                                               KF 0191
      INTEGER   ITYPE(NSEG)                                              KF 0191
      REAL      PARAM(MXNS,2), FRN(NSEG), SLOPE(NSEG), ALPADJ,           KF 0191
     >          ALPHA(NSEG), EM(NSEG), QMAX(NSEG)                        KF 0191
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NSEG   - number of segments
C     MXNS   -
C     ITYPE  -
C     PARAM  -
C     FRN    -
C     SLOPE  -
C     ALPADJ -
C     ALPHA
C     EM     -
C     QMAX   -
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, N                                                     KF 0389
      REAL      AMAX, QFULL, SIDE                                        KF 0389
C
C     + + + INTRINSICS + + +
      INTRINSIC   SQRT                                                   KF 0389
C
C     + + + END SPECIFICATIONS + + +
      DO 100 I=1,NSEG                                                    KF 0191
        N=ITYPE(I)                                                       AB 110
        IF (N.GE.8) N=7                                                  AB 120
        GO TO (1,2,3,4,5,6,7), N                                         KF 0191
 1      CONTINUE                                                         KF 0191
C         gutter segment                                                 KF 0191
          SIDE=SQRT(PARAM(I,1))/(1.+SQRT(1.+PARAM(I,1)**2))              AB 140
          ALPHA(I)=1.18/FRN(I)*SQRT(SLOPE(I))*SIDE**(2./3.)              AB 150
          EM(I)=1.33                                                     AB 160
          QMAX(I)=100000.                                                AB 170
          GO TO 8                                                        AB 180
 2      CONTINUE                                                         KF 0191
C         pipe segment
          AMAX=3.14*PARAM(I,1)**2/4.                                     AB 190
          QFULL=1.49/FRN(I)*AMAX*(PARAM(I,1)/4.)**(2./3.)*SQRT(SLOPE(I)) AB 200
          ALPHA(I)=QFULL/AMAX                                            AB 210
          EM(I)=1.                                                       AB 220
          QMAX(I)=QFULL                                                  AB 230
          GO TO 8                                                        AB 240
 3      CONTINUE                                                         KF 0191
C         triangular cross section
          SIDE=SQRT(PARAM(I,1)+PARAM(I,2))/(SQRT(1.+PARAM(I,1)**2)       AB 250
     >         +SQRT(1.+PARAM(I,2)**2))                                  AB 260
          ALPHA(I)=1.18/FRN(I)*SQRT(SLOPE(I))*SIDE**(2./3.)              AB 270
          QMAX(I)=100000.                                                AB 280
          EM(I)=1.33                                                     AB 290
          GO TO 8                                                        AB 300
 4      CONTINUE                                                         KF 0191
C         user specified alpha and m
          ALPHA(I)=PARAM(I,1)                                            AB 310
          EM(I)=PARAM(I,2)                                               AB 320
          QMAX(I)=100000.                                                AB 330
          GO TO 8                                                        AB 340
 5      CONTINUE                                                         KF 0191
C         overland flow (turbulent)
          ALPHA(I)=1.49/FRN(I)*SQRT(SLOPE(I))                            AB 350
          QMAX(I)=100000.                                                AB 360
          EM(I)=1.67                                                     AB 370
          GO TO 8                                                        AB 380
 6      CONTINUE                                                         KF 0191
C         overland flow (laminar)
          QMAX(I)=100000.                                                AB 430
          ALPHA(I)=4.*64.4*SLOPE(I)/(FRN(I)*.0000141)                    AB 440
          EM(I)=3.                                                       AB 450
          GO TO 8                                                        KF 0191
 7      CONTINUE                                                         KF 0191
C         junction or reservoir
          QMAX(I)=100000.                                                AB 390
          ALPHA(I)=0.                                                    AB 400
          EM(I)=0.                                                       AB 410
          GO TO 8                                                        AB 420
 8      CONTINUE                                                         KF 0191
        ALPHA(I)=ALPHA(I)*ALPADJ                                         AB 460
 100  CONTINUE                                                           KF 0191
      RETURN                                                             AB 470
      END                                                                AB 480-
C
C
C
      SUBROUTINE   CORR                                                  AD  10
     I                 ( NOFE, SFPK, FPK )                               KF 0389
C
C     + + + PURPOSE + + +
C     Computes correlation coefficent for peaks.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NOFE                                                     KF 0389
      REAL      SFPK(NOFE), FPK(NOFE)                                    KF 0389
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NOFE   - number of storm events
C     SFPK   - array of maximum simulated discharges for storm events, in
C              cubic feet per second
C     FPK    - array of measured peaks for storm events, in cubic feet
C              per second
C
C     + + + PARAMETERS + + +
      INCLUDE 'pinout.inc'                                               KF 0290
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I                                                        KF 0389
      REAL      AN, COR, COV, SD1, SD2, VAR,                             KF 0389
     #          SUMX, SUMXX, SUMXY, SUMY, SUMYY, X, Y                    KF 0389
C
C     + + + INTRINSICS + + +
      INTRINSIC   SQRT                                                   KF 0389
C
C     + + + FORMATS + + +
    2 FORMAT (   1X, F8.3, ' - correlation coefficient for peaks' )      AD 300
C
C     + + + END SPECIFICATIONS + + +
      AN=0.                                                              AD  30
      SUMX=0.                                                            AD  40
      SUMY=0.                                                            AD  50
      SUMXX=0.                                                           AD  60
      SUMYY=0.                                                           AD  70
      SUMXY=0.                                                           AD  80
      DO 1 I=1,NOFE                                                      AD  90
        X=SFPK(I)                                                        AD 100
        Y=FPK(I)                                                         AD 110
        IF (X.GT.0.0001 .AND. Y.GT.0.0001) THEN                          KF 1290
C         both peaks large enough for correlation computations
          AN=AN+1.                                                       AD 130
          SUMX=SUMX+X                                                    AD 140
          SUMY=SUMY+Y                                                    AD 150
          SUMXX=SUMXX+X*X                                                AD 160
          SUMYY=SUMYY+Y*Y                                                AD 170
          SUMXY=SUMXY+X*Y                                                AD 180
        END IF
    1 CONTINUE                                                           AD 190
      IF (AN .GT. 2.999) THEN                                            KF 1290
C       enough peaks to compute correlation
        VAR=(AN*SUMXX-SUMX*SUMX)/(AN*(AN-1.))                            AD 210
        SD1=SQRT(VAR)                                                    AD 220
        VAR=(AN*SUMYY-SUMY*SUMY)/(AN*(AN-1.))                            AD 230
        SD2=SQRT(VAR)                                                    AD 240
        COV=(AN*SUMXY-SUMX*SUMY)/(AN*(AN-1.))                            AD 250
        COR=COV/(SD1*SD2)                                                AD 260
        WRITE (OUTFIL,2) COR                                            DT 1089
      END IF
      RETURN                                                             AD 280
C                                                                        AD 290
      END                                                                AD 310-
