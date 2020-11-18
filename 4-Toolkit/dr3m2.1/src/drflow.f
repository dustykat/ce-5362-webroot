C
C
C
      SUBROUTINE   FLOW                                                   D  10
     I                 ( I, DT, DTS )                                    KF 0191
C
C     + + + PURPOSE + + +
C     Computes segment outflows at T+DT
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   I                                                        KF 0389
      REAL      DT, DTS                                                  KF 0191
C
C     + + + ARGUMENT DEFINITIONS + + +
C     I      -
C     DT     -
C     DTS    -
C
C     + + + PARMAMETERS + + +
      INCLUDE 'pinout.inc'                                               KF 0290
      INCLUDE 'plimt.inc'                                                KF 0389
C
C     + + + COMMONS + + +
      INCLUDE 'csgsc.inc'                                                KF 0389
      INCLUDE 'cc3t4.inc'                                                KF 0389
      INCLUDE 'csgs3.inc'                                                KF 0389
      INCLUDE 'csgs2.inc'                                                KF 0389
      INCLUDE 'cc7t8.inc'                                                KF 0389
      INCLUDE 'ce2t3.inc'                                                KF 0389
      INCLUDE 'cunit.inc'                                                KF 0389
      INCLUDE 'cf1a3.inc'                                                KF 0389
      INCLUDE 'cstrm.inc'                                                KF 0389
C
C     + + + SAVES + + +
      INTEGER   METH, IMDE,                                              KF 0389
     >          NGRIDS, JTYPE                                            KF 0191
      REAL      IN2, QOUT2, CONST, ALP, DTSX, XEM, YEM,                  KF 0389
     >          QS(11), XA(11), XQ(11), QUP, C1FD, AFD(7),               KF 0191
     >          X(100), A(100), XL, DTS1, D1, D2, D3                     KF 0191
      CHARACTER*4 XSEG                                                   KF 0191
      SAVE      METH, IMDE, IN2, QOUT2, CONST, ALP, DTSX, XEM, YEM,      KF 0389
     >          QS, XA, XQ, QUP, C1FD, AFD, XSEG,                        KF 0191
     >          NGRIDS, X, A, XL, DTS1, JTYPE, D1, D2, D3                KF 0191
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   IP, J, N                                                 KF 0389
      REAL      AD, ALAT, B1, B2, IN1, QOUT1, THETA                      KF 0389
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS                                                    KF 0389
C
C     + + + EXTERNALS + + +
      EXTERNAL    INIT, KWFD, KWFD1, KWMOC, KWMOC1, LAT, PULS, UP, DWM   KF 1090
C
C     + + + END SPECIFICATIONS + + +
      IF (KINIT .EQ. 1) THEN                                             KF 1290
C       initialize segments at start of storm
        CALL INIT ( DTS, KIN, DELTAT, KK,                                KF 0389
     O            T, AR, Q2, QSUM, QSUML, ECOMP, KINIT, IN2, ALP, DTSX,  KF 0389
     O            XEM, YEM, METH, QS, QOUT2, CONST )                     KF 0389
        IMDE = 0                                                          D 230
        IF (METH .EQ. 1) THEN                                             1290
C         initialize for method of characteristics
          XL=FLGTH(KK)                                                   KF 0389
          JTYPE=ITYPE(KK)                                                KF 0389
          DTS1=DTS                                                        D 270
          CALL KWMOC1 ( 0.0, 0.0, ALP, YEM, XL, DTS1,                    KF 0191
     O                  NGRIDS, X, A, D1, D2, D3 )                       KF 0191
        ELSE IF (METH .EQ. 2) THEN                                       KF 1290
C         initialize for implicit finite difference method
          XSEG=ISEG(KK)                                                  KF 0389
          CALL KWFD1 ( DX(KK), ALP, DTSX, YEM, WX,                       KF 0389
     O                 C1FD, AFD )                                       KF 0191
        END IF                                                           KF 1290
      END IF
    2 CONTINUE
      T=T+DT                                                              D 330
      ICT=ICT+1                                                           D 340
      IF (ITYPE(KK).GE.7) GO TO 12                                       KF 0389
      N=NDX(KK)+1                                                        KF 0389
      CALL UP ( ISEG(KK), KK, I, DEL5, DTS, ICT, OSI, QSUM, QUP )        KF 0191
      CALL LAT ( KK, I, NRG, RAT, IK, ICT, DEL5, QSUML, QLAT )           KF 0389
      ALAT=QLAT*DTS                                                       D 390
      XQ(1)=QUP                                                           D 400
      XA(1)=(QUP/ALP)**(XEM)                                              D 410
      IF (METH.NE.1) GO TO 3                                              D 420
C                                                                         D 430
C          METHOD OF CHARACTERISTICS                                      D 440
      CALL KWMOC ( ALP, YEM, XA(1), QLAT,                                KF 0389
     I             XL, DTS1, D1, D2, D3, JTYPE,                          KF 0191
     M             NGRIDS, X, A,                                         KF 0191
     O             Q2, AD )                                              KF 0389
      GO TO 20                                                            D 460
    3 CONTINUE                                                            D 470
      B1=ALP*YEM*DTSX                                                     D 480
      B2=YEM-1.                                                           D 490
      DO 6 J=2,N                                                          D 500
      IF (ABS(YEM-1.). LT. 0.1E-9 .AND.METH.EQ.2) GO TO 5                PS 0990
C                                                                         D 520
C          EXPLICIT FINITE DIFFERENCE METHOD                              D 530
      IF (AR(J-1).LE.0..AND.QLAT.LE.0.) GO TO 4                           D 540
      IF ((ALAT*1.E3).LE.AR(J-1)) THEN                                   KF 0389
        THETA=B1*AR(J-1)**B2                                             KF 0389
      ELSE                                                               KF 0389
        THETA=ALP/(QLAT*DX(KK))*((ALAT+AR(J-1))**YEM-AR(J-1)**YEM)       KF 0389
      END IF                                                             KF 0389
      IF (THETA.LT.1) GO TO 4                                             D 580
      XQ(J)=XQ(J-1)+(ALAT+AR(J-1)-XA(J-1))/DTSX                           D 590
      XA(J)=(XQ(J)/ALP)                                                   D 600
      IF (XA(J).LT.1.E-20) XA(J)=0.0                                      D 610
      IF (ABS(YEM-1.) .GT. 0.1E-9) XA(J)=XA(J)**(XEM)                   1184 KF
      IF(METH .EQ. 0  .OR.  ABS(XA(J)) .LT. 1.E-20) GO TO 6             PES0684
      GO TO 5                                                           PES0684
    4 XA(J)=AR(J)+ALAT+DTSX*(QS(J-1)-QS(J))                               D 640
      IF (XA(J).LT.1.E-20) XA(J)=0.0                                      D 650
      XQ(J)=ALP*XA(J)                                                     D 660
      IF (ABS(YEM-1.) .GT. 0.1E-9) XQ(J)=ALP*(XA(J)**YEM)               1184 KF
      IF (METH.EQ.0.OR.ABS(XA(J)).LT.1.E-20) GO TO 6                      D 680
C                                                                         D 690
    5 CONTINUE                                                           KF 1090
      IF (METH .EQ. 2) THEN                                              PS 0990
C       implicit finite difference method                                 D 700
        CALL KWFD ( J, XSEG, ALP, C1FD, AFD, YEM, QS, QLAT, AR, T,       KF 0191
     M              XA, XQ )                                             KF 0191
      ELSE IF (METH .GE. 3) THEN                                         PS 0990
C       diffusion wave method                                            KF 1090
        CALL DWM ( J, KK, ITYPE(KK), ISEG(KK),                           KF 1090
     I             PARAM(KK,1), PARAM(KK,2), SLOPE(KK), DX(KK),          KF 1090
     I             ALP, YEM, XEM, DTSX, QLAT, XA, QS, AR,                KF 1090
     M             XQ )                                                  KF 1090
      END IF                                                             KF 1090
    6 CONTINUE                                                            D 720
C            SELECT ALPHA AND M FOR NEXT ROUTING                          D 730
      IF (ITYPE(KK).NE.4) GO TO 10                                       KF 0389
      IF (RCOEF(KK,3).LT.0.01) GO TO 10                                  KF 0389
      IF (IMDE.GT.0) GO TO 7                                              D 760
      IF (XQ(N).LT.RCOEF(KK,3)) GO TO 10                                 KF 0389
      ALP=RCOEF(KK,1)                                                    KF 0389
      YEM=RCOEF(KK,2)                                                    KF 0389
      XEM=1./YEM                                                          D 800
      IMDE=1                                                              D 810
      GO TO 8                                                             D 820
    7 IF (XQ(N).GT.RCOEF(KK,3)) GO TO 10                                 KF 0389
      ALP=ALPHA(KK)                                                      KF 0389
      YEM=EM(KK)                                                         KF 0389
      XEM=1./YEM                                                          D 860
      IMDE=0                                                              D 870
    8 DO 9 J=1,N                                                          D 880
      XA(J)=(XQ(J)/ALP)                                                   D 890
      IF (XA(J).LT.1.E-20) XA(J)=0.0                                      D 900
      IF (ABS(YEM-1.) .GT. 0.1E-9) XA(J)=XA(J)**(XEM)                   1184 KF
    9 CONTINUE                                                            D 920
   10 DO 11 J=1,N                                                         D 930
      AR(J)=XA(J)                                                         D 940
      QS(J)=XQ(J)                                                         D 950
   11 CONTINUE                                                            D 960
      Q2=XQ(N)                                                            D 970
      GO TO 20                                                            D 980
   12 CALL UP ( ISEG(KK), KK, I, DEL5, DTS, ICT, OSI, QSUM, QUP )        KF 0191
      IF (ITYPE(KK)-8) 13,14,15                                          KF 0389
   13 Q2=QUP                                                              D1010
      GO TO 20                                                            D1020
   14 CALL PULS ( KK, DELTAT, QSUM, IN2, Q2 )                            KF 0389
      GO TO 20                                                            D1040
   15 IF (ITYPE(KK)-9) 17,16,17                                          KF 0389
   16 IN1=IN2                                                             D1060
      IN2=QUP                                                             D1070
      QOUT1=QOUT2                                                         D1080
      QOUT2=CONST*((IN1+IN2)/2.-QOUT1)+QOUT1                              D1090
      Q2=QOUT2                                                            D1100
      STO(KK)=PARAM(KK,1)*Q2                                             KF 0389
      IF (STO(KK).LT.SMAX(KK)) GO TO 20                                  KF 0389
      SMAX(KK)=STO(KK)                                                   KF 0389
      GO TO 20                                                            D1140
   17 IF (ITYPE(KK)-10) 19,18,19                                         KF 0389
   18 IP=(I-IJ+DEL5)/DEL5                                                 D1160
      IF (IHYD(I1).EQ.0.OR.IP.GT.I3Q) GO TO 13                            D1170
      I4Q=I4Q+1                                                           D1180
      IF (IP.EQ.1) Q2=(DT/PTIME)*I4Q*QIH(IP)+QUP                          D1190
      IF (IP.GT.1) Q2=(DT/PTIME)*I4Q*(QIH(IP)-QIH(IP-1))+QIH(IP-1)+QUP    D1200
      GO TO 20                                                            D1210
   19 Q2=QINPT(I1)+QUP                                                    D1220
   20 CONTINUE                                                            D1230
      FLW(ICT)=Q2                                                         D1240
      IF (T.LT.ECOMP) GO TO 2                                             D1250
      RETURN                                                              D1260
      END                                                                 D1270-
C
C
C
      SUBROUTINE   KWMOC1                                                 E  10
     I                   ( AA, AB, ALPHA, M,                             KF 0389
     I                     XL, DTS1,                                     KF 0191
     O                     NGRIDS, X, A, D1, D2, D3 )                    KF 0191
C                                                                         E  20
C     + + + PURPOSE + + +
C     This routine must be called once preceding a storm for each segment
C     to define initial conditions used in the method of characteristics
C     solution for flow routing.
C                                                                         E  60
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NGRIDS                                                   KF 0191
      REAL      AA, AB, ALPHA, M,                                        KF 0389
     >          XL, DTS1, X(100), A(100), D1, D2, D3                     KF 0191
C
C     + + + ARGUMENT DEFINITIONS + + +
C     AA     -
C     AB     -
C     ALPHA  -
C     M      -
C     XL     -
C     DTS1   -
C     NGRIDS -
C     XA     -
C     D1     -
C     D2     -
C     D3     -
C
C     + + + LOCAL VARAIABLES + + +
      INTEGER   I, N                                                     KF 0389
      REAL      DA, DX                                                   KF 0389
C
C     + + + END SPECIFICATIONS + + +
C
C     ..... DEFINE CONSTANTS .....                                        E 130
      D1=ALPHA*M*DTS1                                                    KF 0389
      D2=M-1.                                                             E 150
      D3=ALPHA*DTS1                                                      KF 0389
C                                                                         E 170
C     ..... DEFINE INITIAL CONDITIONS FOR STORM .....                     E 180
      NGRIDS=6                                                            E 190
      X(1)=0.                                                             E 200
      X(NGRIDS)=XL                                                        E 210
      A(1)=AA                                                             E 220
      A(NGRIDS)=AB                                                        E 230
C                                                                         E 240
      N=NGRIDS-1                                                          E 250
      DX=XL/N                                                             E 260
      DA=(AA-AB)/N                                                        E 270
      DO 1 I=2,N                                                          E 280
      X(I)=X(I-1)+DX                                                      E 290
      A(I)=A(I-1)+DA                                                      E 300
    1 CONTINUE                                                            E 310
      RETURN                                                              E 320
      END                                                                 E 330-
C
C
C
      SUBROUTINE   KWMOC                                                  F  10
     I                  ( ALPHA, M, AC, QLAT,                            KF 0389
     I                    XL, DTS1, D1, D2, D3, JTYPE,                   KF 0191
     M                    NGRIDS, X, A,                                  KF 0191
     O                    QD, AD )                                       KF 0389
C                                                                         F  20
C     + + + PURPOSE + + +
C     This routine uses the method of characteristics for kinematic wave
C     routing.  It operates on a time step basis.  It returns the values
C     of discharge (QD) and area (AD) at the downstream end of the
C     segment after a time step of DTS (seconds).
C                                                                         F  80
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   JTYPE, NGRIDS                                            KF 0191
      REAL      ALPHA, M, AC, QLAT, QD, AD,                              KF 0389
     >          XL, DTS1, D1, D2, D3, X(100), A(100)                     KF 0191
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ALPHA  - alpha
C     M      - m
C     AC     -
C     QLAT   - lateral inflow
C     XL     -
C     DTS1   -
C     D1     -
C     D2     -
C     D3     -
C     JTYPE  -
C     NGRIDS -
C     X      -
C     A      -
C     QD     - discharge
C     AD     - area
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, II, K, L, N                                           KF 0389
      REAL      ALAT                                                     KF 0389
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL    DIMEN
C
C     + + + END SPECIFICATIONS + + +
C                                                                         F 140
      ALAT=QLAT*DTS1                                                     KF 0389
C     CHECK THAT ARRAY DIMENSIONS WILL NOT BE EXCEEDED                    F 160
      IF (NGRIDS.EQ.100) CALL DIMEN ( X, A, NGRIDS )                     KF 0389
C     IF QLAT AND A(1) EQ 0. DON'T ADD CHARACTERISTIC                     F 180
      L=1                                                                 F 190
      IF (QLAT.LE.1.E-20.AND. ABS(A(1)) .LT. 0.1E-9) L = 0              1184 KF
      NGRIDS=NGRIDS-(1-L)                                                 F 210
C                                                                         F 220
C     ..... ADVANCE CHARACTERISTICS .....                                 F 230
      N=NGRIDS+2                                                          F 240
      DO 5 K=1,NGRIDS                                                     F 250
      I=N-K                                                               F 260
      IF (ABS(M-1.) .LT. 0.1E-9) GO TO 2                                1184 KF
      IF ((ALAT*1.E3).GT.A(I-L)) GO TO 1                                  F 280
      X(I)=X(I-L)+D1*A(I-L)**D2                                           F 290
      A(I)=A(I-L)                                                         F 300
      GO TO 4                                                             F 310
    1 X(I)=X(I-L)+(ALPHA)*((ALAT+A(I-L))**M-A(I-L)**M)/(QLAT)             F 320
      GO TO 3                                                             F 330
    2 X(I)=X(I-L)+D3                                                      F 340
    3 A(I)=A(I-L)+ALAT                                                    F 350
C                                                                         F 360
C     ..... KEEP TRACK OF LAST CHARACTERISTIC THAT LEAVES SEGMENT .....   F 370
    4 IF (X(I).GE.XL) II=I                                                F 380
    5 CONTINUE                                                            F 390
C                                                                         F 400
C     ..... ASSIGN AREA AT U/S BOUNDARY .....                             F 410
      A(1)=AC                                                             F 420
C                                                                         F 430
C     ..... INTERPOLATE FOR AREA AT D/S BOUNDARY AND ASSIGN NGRIDS .....  F 440
      AD=A(II)-(A(II)-A(II-1))*((X(II)-XL)/(X(II)-X(II-1)))               F 450
      QD=ALPHA*AD**M                                                      F 460
      NGRIDS=II                                                           F 470
      X(NGRIDS)=XL                                                        F 480
      A(NGRIDS)=AD                                                        F 490
      IF (JTYPE.EQ.5.OR.JTYPE.EQ.6.OR.NGRIDS.LE.2) GO TO 11              KF 0389
      IF(ABS(M-1.) .LT. 0.1E-9) GO TO 11                                1184 KF
C                                                                         F 510
C     ..... TAKE CARE OF SHOCKS .....                                     F 520
      I=NGRIDS-1                                                          F 530
    6 IF (X(I).LE.X(I-1)) GO TO 7                                         F 540
      GO TO 10                                                            F 550
    7 K=I-1                                                               F 560
      IF (ABS(A(K)) .LT. 0.1E-9) GO TO 8                                1184 KF
      X(K)=(X(I)+X(K))/2.                                                 F 580
      A(K)=(A(I)+A(K))/2.                                                 F 590
    8 NGRIDS=NGRIDS-1                                                     F 600
      DO 9 K=I,NGRIDS                                                     F 610
      X(K)=X(K+1)                                                         F 620
      A(K)=A(K+1)                                                         F 630
    9 CONTINUE                                                            F 640
   10 I=I-1                                                               F 650
      IF (I.LE.2) GO TO 11                                                F 660
      GO TO 6                                                             F 670
C                                                                         F 680
   11 RETURN                                                              F 690
      END                                                                 F 700-
C
C
C
      SUBROUTINE   DIMEN                                                  G  10
     M                  ( X, A,                                          KF 0389
     O                    NGRIDS )                                       KF 0389
C                                                                         G  20
C     + + + PURPOSE + + +
C     This routine drops out characteristics for a segment if the
C     dimensions of the X and A arrays are going to be exceeded.
C                                                                         G  50
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NGRIDS                                                   KF 0389
      REAL      X(100), A(100)                                           KF 0389
C
C     + + + ARGUMENT DEFINITIONS + + +
C     X      -
C     A      -
C     NGRIDS -
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, J                                                     KF 0389
C
C     + + + END SPECIFICATIONS + + +
      J=2                                                                 G  70
      DO 1 I=4,100,2                                                      G  80
      J=J+1                                                               G  90
      X(J)=X(I)                                                           G 100
      A(J)=A(I)                                                           G 110
    1 CONTINUE                                                            G 120
      NGRIDS=51                                                           G 130
      RETURN                                                              G 140
      END                                                                 G 150-
C
C
C
      SUBROUTINE   KWFD1                                                  H  10
     I                  ( DX, ALPHA, DTSX, M, WX,                        KF 0389
     O                    C1, A )                                        KF 0191
C                                                                         H  20
C     + + + PURPOSE + + +
C     Routine must be called once preceding a storm for each segment
C     where the nonlinear finite difference method is used for flow
C     routing.  Constants used in the finite difference solution are
C     defined.
C                                                                         H  70
C     + + + DUMMY ARGUMENTS + + +
      REAL      DX, ALPHA, DTSX, M, WX, C1, A(7)                         KF 0191
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DX     -
C     ALPHA  -
C     DTSX   -
C     M      -
C     WX     -
C
C     + + + LOCAL VARIABLES + + +
      REAL      A0, WT, WT1                                              KF 0389
C
C     + + + DATA INITIALIZATIONS + + +
      DATA WT/0.5/                                                        H 110
C
C     + + + END SPECIFICATIONS + + +
      WT1=1.-WT                                                           H 120
      A0=1./(DTSX*WX)                                                     H 130
      C1=WT*A0                                                            H 140
      A(1)=A0*WT1                                                        KF 0191
      A(2)=(1.-WX)/WX                                                    KF 0191
      A(3)=M-1.                                                          KF 0191
      A(4)=M-2.                                                          KF 0191
      A(5)=DX/WX                                                         KF 0191
      A(6)=ALPHA*M                                                       KF 0191
      A(7)=ALPHA*M*A(3)                                                  KF 0191
      RETURN                                                              H 220
      END                                                                 H 230-
C
C
C
      SUBROUTINE   KWFD                                                   I  10
     I                 ( J, XSEG, C0, C1, A, M, QS, QLAT, AR, T,         KF 0191
     M                   XA, XQ )                                        KF 0389
C                                                                         I  20
C     + + + PURPOSE + + +
C     This routine solves for the unknown flow area XA(J) by an
C     iterative nonlinear finite difference scheme.  Newton's second
C     order method is used to solve for the root of the nonlinear
C     equation.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   J                                                        KF 0389
      REAL      XA(11), XQ(11), C0, M, QS(11), T, QLAT, AR(11),          KF 0389
     >          C1, A(7)                                                 KF 0191
      CHARACTER*4 XSEG                                                   KF 0191
C
C     + + + ARGUMENT DEFINITIONS + + +
C     J
C     XSEG   -
C     C0     -
C     M      -
C     QS     -
C     QLAT   -
C     AR     -
C     T      -
C     XA     -
C     XQ     -
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, NITER                                                 KF 0389
      REAL      C2, FPPX, FPX, FX, H, X, X0                              KF 0389
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS                                                    KF 0389
C
C     + + + EXTERNALS + + +
      EXTERNAL    MESSGE                                                 KF 0389
C
C     + + + DATA INITIALIZATIONS + + +
      DATA NITER/15/                                                      I 160
C
C     + + + END SPECIFICATIONS + + +
      X0=XA(J)                                                            I 170
      C2=A(1)*(XA(J-1)-AR(J-1))-C1*AR(J)+A(2)*(QS(J)-QS(J-1))-XQ(J-1)    KF 0191
     >   -QLAT*A(5)                                                      KF 0191
      IF (ABS(M-1.) .LT. 0.1E-9) GO TO 4                                1184 KF
      DO 1 I=1,NITER                                                      I 210
      FX=C0*X0**M+C1*X0+C2                                                I 220
      FPX=A(6)*X0**A(3)+C1                                               KF 0191
      FPPX=A(7)*X0**A(4)                                                 KF 0191
      IF (ABS(FX).LT.1.E-15) GO TO 2                                      I 250
      IF (ABS(FPX).LT.1.E-15) CALL MESSGE(1,XA(J),I,FPX,X0,J,XSEG,T)     KF 0389
      H=-FPX/FX+.5*FPPX/FPX                                               I 270
      X=X0+1./H                                                           I 280
      IF (X.LE.0.) GO TO 2                                                I 290
      IF (ABS(X-X0)/X0.LE..05) GO TO 3                                    I 300
      X0=X                                                                I 310
    1 CONTINUE                                                            I 320
      CALL MESSGE(2,XA(J),I,FX,X,J,XSEG,T)                               KF 0389
    2 X=X0                                                                I 340
    3 XA(J)=X                                                             I 350
      GO TO 5                                                             I 360
    4 XA(J)=-C2/(C0+C1)                                                   I 370
    5 XQ(J)=C0*XA(J)**M                                                   I 380
      RETURN                                                              I 390
      END                                                                 I 400-
C
C
C
      SUBROUTINE   MESSGE                                                 J  10
     I                   ( IGO, X0, I, F, X, J, ISEG, T )                KF 0389
C
C     + + + PURPOSE + + +
C     This routine is called from within Newton's method whenever the
C     number of iterations exceeds the specified limit or the first
C     derivative approaches zero.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IGO, I, J                                                KF 0389
      REAL      X0, F, X, T                                              KF 0191
      CHARACTER*4 ISEG                                                   KF 0191
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IGO    - indicator for type of message
C              1 - derivative is approaching zero
C              2 - iterations exceed limit
C     X0     - initial guess
C     I      - number of iterations
C     F      - for IGO=1, derivative
C              for IGO=2, f(A)
C     X      - last A
C     J      - grid number + 1
C     ISEG   - segment name
C     T      - time
C
C     + + + PARAMETERS + + +
      INCLUDE 'pinout.inc'                                               KF 0290
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   J1                                                       KF 0389
C
C     + + + FORMATS + + +
    4 FORMAT (//,1X, '*** Derivative apporaches zero',                   KF 0389
     #        /, 6X, 11X,A4, ' = segment',                               KF 0389
     #        /, 6X, F15.1,  ' = time',                                  KF 0389
     #        /, 6X, I15,    ' = grid numbers',                          KF 0389
     #        /, 6X, E15.8,  ' = initial guess',                         KF 0389
     #        /, 6X, I15,    ' = number of iterations',                  KF 0389
     #        /, 6X, E15.8,  ' = derivative',                            KF 0389
     #        /, 6X, E15.8,  ' = last A' )
    5 FORMAT (//,1X, '*** Iterations exceed limit',                      KF 0389
     #        /, 6X, 11X,A4, ' = segment',                               KF 0389
     #        /, 6X, F15.1,  ' = time',                                  KF 0389
     #        /, 6X, I15,    ' = grid number',                           KF 0389
     #        /, 6X, E15.8,  ' = initial guess',                         KF 0389
     #        /, 6X, I15,    ' = number of iterations',                  KF 0389
     #        /, 6X, E15.8,  ' = f(A)',                                  KF 0389
     #        /, 6X, E15.8,  ' = last A' )                               KF 0389
C
C     + + + END SPECIFICATIONS + + +
C                                                                         J  60
      J1=J-1                                                              J 110
      GO TO (1,2), IGO                                                    J 120
    1 WRITE (OUTFIL,4) ISEG,T,J1,X0,I,F,X                               DT 1089
      GO TO 3                                                             J 140
    2 WRITE (OUTFIL,5) ISEG,T,J1,X0,I,F,X                               DT 1089
    3 RETURN                                                              J 160
C                                                                         J 170
      END                                                                 J 260-
C
C
C
      SUBROUTINE   LAT                                                    K  10
     I                ( K, I, NRG, RAT, IK, ICT, DEL5,                   KF 0389
     M                  QSUML,                                           KF 0389
     O                  QLAT )                                           KF 0389
C
C     + + + PURPOSE + + +
C     Computes lateral inflow from overland flow segments or from
C     rainfall.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   K, I, NRG, IK, ICT, DEL5                                 KF 0389
      REAL      RAT, QSUML, QLAT                                         KF 0389
C
C     + + + ARGUMENT DEFINITIONS + + +
C     K      - segment index
C     I      - parameter index
C     NRG    - number of rain gages
C     RAT    - ratio of pervious area and noneffective impervious
C              area to pervious area
C     IK     -
C     ICT    - index for lateral inflow
C     DEL5   - number of CORF-minute intervals in unit-time interval
C     QSUML  - sum of lateral inflows to segment
C     QLAT   - lateral inflow
C
C     + + + PARAMETERS + + +
      INCLUDE 'plimt.inc'                                                KF 0389
C
C     + + + COMMONS + + +
      INCLUDE 'csgs3.inc'                                                KF 0389
      INCLUDE 'cuprc.inc'                                                KF 0389
      INCLUDE 'cunit.inc'                                                KF 0389
      INCLUDE 'ce1.inc'                                                  KF 0389
      INCLUDE 'cz1t4.inc'                                                KF 0389
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I2, III, IPAR                                            KF 0389
      REAL      AP, EP, QPR
C
C     + + + END SPECIFICATIONS + + +
      QLAT=0.                                                             K 200
      IF (ITYPE(K)-5) 1,3,1                                               K 210
    1 IF (ITYPE(K)-6) 2,3,2                                               K 220
    2 QPR=QSUML                                                           K 230
      QLAT=FLAT(ICT)                                                      K 240
      QSUML=QLAT                                                          K 250
      QLAT=(QLAT+QPR)/2.                                                  K 260
      RETURN                                                              K 270
C             COMPUTE LATERAL INFLOW RATE FROM RAIN                       K 280
    3 EP=0.0                                                              K 290
      AP=0.0                                                              K 300
      IPAR=I                                                              K 310
      IF (KPSET(K).EQ.2) IPAR=IPAR+1441                                   K 320
      DO 4 III=1,NRG                                                      K 330
      I2=NDAY*(III-1)+IK                                                  K 340
      EP=EP+(RCOEF(K,III)*P(IPAR,III))/CORF                               K 350
    4 AP=AP+(RCOEF(K,III)*(UPR(I2)/DEL5-IMPRET(III)))/CORF                K 360
      QLAT=PARAM(K,1)*(PARAM(K,2)*AP+((1.-PARAM(K,2))/RAT)*EP)/720.       K 370
C             THE CONSTANT 720 CONVERTS SQFT-IN/MINUTE TO CFS             K 380
      QSUML=QLAT                                                          K 390
      RETURN                                                              K 400
      END                                                                 K 410-
C
C
C
      SUBROUTINE   UP                                                     L  10
     I               ( ISEG, K, I, DEL5, DTS, ICT,                       KF 0191
     M                 OSI, QSUM,                                        KF 0389
     O                 QUP )                                             KF 0389
C
C     + + + PURPOSE + + +
C     Computes upstream inflow to segment K.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   K, I, DEL5, ICT                                          KF 0389
      REAL      DTS, QUP, OSI, QSUM                                      KF 0389
      CHARACTER*4 ISEG                                                   KF 0191
C
C     + + + ARGUMENT DEFINITIONS + + +
C     K      - segment index
C     I      -
C     DEL5   - number of CORF-minute intervals in unit-time interval
C     DTS    -
C     ICT    - counter for DT-minute intervals
C     OSI    - surcharging indicator
C     QSUM   - sum of upstream inflows to segment
C     QUP    - upstream inflow
C
C     + + + PARAMETERS + + +
      INCLUDE 'pinout.inc'                                               KF 0290
      INCLUDE 'plimt.inc'                                                KF 0389
C
C     + + + COMMONS + + +
      INCLUDE 'csgs2.inc'                                                KF 0389
      INCLUDE 'csgs3.inc'                                                KF 0389
      INCLUDE 'cunit.inc'                                                KF 0389
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   J                                                        KF 0389
      REAL      PDT, QPR                                                 KF 0389
C
C     + + + FORMATS + + +
   15 FORMAT (   1X, 'Segment ', A4, ' is surcharging at I =', I6 )      KF 0389
C
C     + + + END SPECIFICATIONS + + +
      QUP=0.                                                              L 150
      QPR=QSUM                                                            L 160
      IF (K.EQ.KSEG(1)) GO TO 1                                          KF 0389
      QUP=FUP(ICT)                                                        L 180
    1 CONTINUE                                                            L 190
      QSUM=QUP                                                            L 200
      IF (ITYPE(K).EQ.8.OR.ITYPE(K).EQ.9) GO TO 12                        L 210
      IF (QUP-QMAX(K)) 7,7,2                                              L 220
    2 IF (QPR-QMAX(K)) 3,6,6                                              L 230
    3 PDT=(QUP-QMAX(K))/(QUP-QPR)                                         L 240
      STO(K)=STO(K)-(DTS*(QMAX(K)-QPR)/2.0)*(1.0-PDT)                     L 250
      IF (STO(K)) 4,5,5                                                   L 260
    4 STO(K)=0.                                                           L 270
    5 STO(K)=STO(K)+(DTS*(QUP-QMAX(K))/2.0)*PDT                           L 280
      GO TO 13                                                            L 290
    6 STO(K)=STO(K)+(((QPR+QUP)/2.)-QMAX(K))*DTS                          L 300
      GO TO 13                                                            L 310
    7 IF (QPR-QMAX(K)) 8,8,10                                             L 320
    8 IF (STO(K)) 11,11,9                                                 L 330
    9 STO(K)=STO(K)-(QMAX(K)-((QPR+QUP)/2.))*DTS                          L 340
      IF (STO(K)) 11,13,13                                                L 350
   10 PDT=(QPR-QMAX(K))/(QPR-QUP)                                         L 360
      STO(K)=STO(K)+((QPR-QMAX(K))*PDT*DTS/2.0)-((QMAX(K)-QUP)*(1.0-PDT)  L 370
     1*DTS/2.0)                                                           L 380
      IF (STO(K)) 11,13,13                                                L 390
   11 STO(K)=0.                                                           L 400
   12 CONTINUE                                                            L 410
      RETURN                                                              L 420
   13 QUP=QMAX(K)                                                         L 430
      IF (OSI.GT.0.0) GO TO 14                                            L 440
      OSI=20.                                                             L 450
      J=I/DEL5                                                            L 460
      IF (DEL5.GT.1) J=J+1                                                L 470
      WRITE (OUTFIL,15) ISEG, J                                          KF 0191
   14 CONTINUE                                                            L 490
      RETURN                                                              L 500
C                                                                         L 510
      END                                                                 L 530-
C
C
C
      SUBROUTINE   INIT                                                   M  10
     I                 ( DTS, KIN, DELTAT, KK,                           KF 0389
     O                   T, AR, Q2, QSUM, QSUML, ECOMP, KINIT, IN2, ALP, KF 0389
     O                   DTSX, XEM, YEM, METH, QS, QOUT2, CONST )        KF 0389
C
C     + + + PURPOSE + + +
C     Initializes segment at start of storm.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   KINIT, KIN, METH, KK                                     KF 0389
      REAL      T, AR(11), QSUM, QSUML, DTS, ECOMP, IN2, ALP, Q2,        KF 0389
     #          DTSX, XEM, YEM, QS(11), DELTAT, QOUT2, CONST             KF 0389
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DTS    - time interval used in finite-difference calculations,     KF 0389
C              in seconds
C     KIN    -                                                           KF 0389
C     DELTAT - time interval for reservoir routing, in fraction of       KF 0389
C              an hour
C     KK     -                                                           KF 0389
C     T      - time in finite-difference routing                         KF 0389
C     AR     -                                                           KF 0389
C     Q2     -                                                           KF 0389
C     QSUM   - sum of upstream inflows to segment                        KF 0389
C     QSUML  - sum of lateral inflows to segment                         KF 0389
C     ECOMP  - parameter to indicate number of CORF-minutes interval     KF 0389
C              during routing
C     KINIT  -                                                           KF 0389
C     IN2    -                                                           KF 0389
C     ALP    -                                                           KF 0389
C     DTSX   -                                                           KF 0389
C     XEM    -                                                           KF 0389
C     YEM    -                                                           KF 0389
C     QS     -                                                           KF 0389
C     QOUT2  -                                                           KF 0389
C     CONST  -                                                           KF 0389
C
C     + + + PARAMETRS + + +
      INCLUDE 'pinout.inc'                                               KF 0290
      INCLUDE 'plimt.inc'                                                KF 0389
C
C     + + + COMMONS + + +
      INCLUDE 'cinit.inc'                                                KF 0389
      INCLUDE 'csgs2.inc'                                                KF 0389
      INCLUDE 'csgs3.inc'                                                KF 0389
      INCLUDE 'cz1t4.inc'                                                KF 0389
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   J, K, N                                                  KF 0389
      REAL      CRES, DDMIN                                              KF 0389
C
C     + + + EXTERNALS + + +
      EXTERNAL   TABLE
C
C     + + + FORMATS + + +
    6 FORMAT (   1X, 'Routing interval for detention reservoir', I3,     KF 0389
     #        /, 1X, 'is too large, reduce to a value less than',        KF 0389
     #               F6.3, 'hours.' )                                    KF 0389
C
C     + + + END SPECIFICATIONS + + +
      T=0.                                                                M 200
      KINIT=0                                                             M 210
      ECOMP=CORF                                                          M 220
      K=KK                                                                M 230
      QSUM=0.                                                             M 240
      QSUML=0.                                                            M 250
      SMAX(K)=0.0                                                         M 260
      IN2=0.0                                                             M 270
      IF (ITYPE(K).GE.7) GO TO 2                                          M 280
      ALP=ALPHA(K)                                                        M 290
      DTSX=DTS/DX(K)                                                      M 300
      YEM=EM(K)                                                           M 310
      XEM=1./YEM                                                          M 320
      STO(K)=0.0                                                          M 330
      N=NDX(K)+1                                                          M 340
      METH=IMETH(K)                                                       M 350
      DO 1 J=1,N                                                          M 360
      QS(J)=0.0                                                           M 370
    1 AR(J)=0.0                                                           M 380
      RETURN                                                              M 390
    2 IF (KIN.EQ.1) RETURN                                                M 400
      IF (ITYPE(K).NE.8) GO TO 3                                          M 410
      CALL TABLE(K,PARAM(K,2),S2,S,C,Q2,NDX(K))                           M 420
      S2O2(K)=PARAM(K,2)/DELTAT+Q2/2.                                     M 430
    3 STO(K)=PARAM(K,2)                                                   M 440
      IF (ITYPE(K).NE.9) GO TO 5                                          M 450
      QOUT2=0.                                                            M 460
      CRES=DELTAT/PARAM(K,1)                                              M 470
      IF (CRES.LE.2.) GO TO 4                                             M 480
      DDMIN=2.*PARAM(K,1)                                                 M 490
      WRITE (OUTFIL,6) K,DDMIN                                          DT 1089
    4 CONST=(2.*CRES)/(2.+CRES)                                           M 510
    5 RETURN                                                              M 520
C                                                                         M 530
      END                                                                 M 560-
C
C
C
      SUBROUTINE   PULS                                                  KF 0389
     I                 ( K, DELTAT, QSUM,                                KF 0389
     M                   IN2,                                            KF 0389
     O                   Q2 )                                            KF 0389
C                                                                        KF 0389
C     + + + PURPOSE + + +
C     Performs modified puls routing.
C                                                                        KF 0389
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   K                                                        KF 0389
      REAL      IN2, DELTAT, QSUM, Q2                                    KF 0389
C                                                                        KF 0389
C     + + + ARGUMENT DEFINITIONS + + +
C     K      -                                                           KF 0389
C     DELTAT -                                                           KF 0389
C     QSUM   -                                                           KF 0389
C     IN2    -                                                           KF 0389
C     Q2     -                                                           KF 0389
C                                                                        KF 0389
C     + + + PARAMETERS + + +
      INCLUDE 'plimt.inc'                                                KF 0389
C                                                                        KF 0389
C     + + + COMMONS + + +
      INCLUDE 'csgs2.inc'                                                KF 0389
      INCLUDE 'cpuls.inc'                                                KF 0389
C                                                                        KF 0389
C     + + + LOCAL VARIABLES + + +
      REAL      IN1, AVIN                                                KF 0389
C                                                                        KF 0389
C     + + + EXTERNALS + + +
      EXTERNAL  TABLE                                                    KF 0389
C                                                                        KF 0389
C     + + + END SPECIFICATIONS + + +
      IN1=IN2                                                             N  90
      IN2=QSUM                                                            N 100
      AVIN=(IN1+IN2)/2.                                                   N 110
      S2O2(K)=S2O2(K)+AVIN                                                N 120
      CALL TABLE(K,S2O2(K),WV,S1,C1,Q2,NDX(K))                            N 130
      IF (Q2.LT.0.0) Q2=0.0                                               N 140
      STO(K)=(S2O2(K)-Q2/2.)*DELTAT                                       N 150
      S2O2(K)=S2O2(K)-Q2                                                  N 160
      IF (STO(K).LT.SMAX(K)) GO TO 1                                      N 170
      SMAX(K)=STO(K)                                                      N 180
    1 CONTINUE                                                            N 190
      RETURN                                                              N 200
      END                                                                 N 210-
C
C
C
      SUBROUTINE   TABLE                                                  O  10
     I                  (K,F1,F3,S3,C3,                                  KF 0389
     O                   F2,                                             KF 0389
     I                   J )                                             KF 0389
C
C     + + + PURPOSE + + +
C     Performs linear interpolation for modified puls routing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   K, J
      REAL      F1, F3(99,10), S3(99,10), C3(99,10), F2
C
C     + + + ARGUMENT DEFINITIONS + + +
C     K      - index
C     F1     -
C     F3     -
C     S3     -
C     C3     -
C     F2     -
C     J      -
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
      DO 1 I=2,J                                                          O  50
      IF (F1.LT.F3(K,I)) GO TO 2                                          O  60
    1 CONTINUE                                                            O  70
      I=J                                                                 O  80
    2 F2=S3(K,I)*F1+C3(K,I)                                               O  90
      RETURN                                                              O 100
      END                                                                 O 110-
