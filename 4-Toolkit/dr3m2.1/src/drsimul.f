C
C
C
      SUBROUTINE   SIMUL                                                  B  10
     I                  ( DT, DTS, DTFLW, TUFLW )                        KF 0191
C
C     + + + PURPOSE + + +
C     Controls the simulation.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DTFLW, TUFLW                                             KF 0191
      REAL      DT, DTS                                                  KF 0191
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DT     -
C     DTS    -
C     DTFLW  -
C     TUFLW  -
C
C     + + + PARAMETERS + + +
      INCLUDE 'pinout.inc'                                               KF 0290
      INCLUDE 'plimt.inc'                                                KF 0389
C
C     + + + COMMONS + + +
      INCLUDE 'cstrm.inc'                                                KF 0389
      INCLUDE 'dstrm.inc'                                                KF 0290
      INCLUDE 'csgs1.inc'                                                KF 0389
      INCLUDE 'csgs2.inc'                                                KF 0389
      INCLUDE 'csgs3.inc'                                                KF 0389
      INCLUDE 'csgsc.inc'                                                KF 0389
      INCLUDE 'cc2.inc'                                                  KF 0389
      INCLUDE 'cc3t4.inc'                                                KF 0389
      INCLUDE 'cc7t8.inc'                                                KF 0389
      INCLUDE 'cd1.inc'                                                  KF 0389
      INCLUDE 'ce1.inc'                                                  KF 0389
      INCLUDE 'ce2t3.inc'                                                KF 0389
      INCLUDE 'cf1a3.inc'                                                KF 0389
      INCLUDE 'cz1t4.inc'                                                KF 0389
      INCLUDE 'cuprc.inc'                                                KF 0389
      INCLUDE 'cunit.inc'                                                KF 0389
      INCLUDE 'cdeud.inc'                                                KF 0389
      INCLUDE 'chead.inc'                                                KF 0389
C
C     + + + SAVES + + +
      INTEGER   FLAG                                                     KF 0389
      REAL      QCW                                                      KF 0389
      SAVE  FLAG, QCW                                                    KF 0389
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   CHG, FLGR, FLGW, I, I12, I2, IBEG(5), ICH, ICNT, IFP,    KF 0389
     #          III, IJK, IJKS, IK1, IKN, IPTIM, IUNIT3, IW, J, JJJ,     KF 0389
     #          K, K4DAY, K4DAYD, K4ST, K4STD, K5, KDAY, KDUM, KDY,      KF 0389
     #          KKK, KP, LJ, LK, NFD, NFD1, NKL, NO2, NP,                KF 0290
     #          NRGI, NRPSEG, NSGA, NSTRCD, W,                           KF 0389
     #          LEN5, DATE(6), BACK, DTRAN, QFLAG, TUNITS                KF 0389
      REAL      APRNT, BMS, BMSB(3,2), BMSN, BMSN2, BMST, COEF, COEF2,   KF 0389
     #          DRAIN, DRAIN2, DRN24, DRN242, EAC, ETDEL, ETW, ETW2,     KF 0389
     #          EVC, EVC2, FR, IMPSTT, INC, INC2, KDRAIN, KSAT, KSAT2,   KF 0389
     #          PAC, PLIMP, PS, PSP, PSP2, PW, PW2, QMX, QR, RGF, RGF2,  KF 0389
     #          RR, RR2, SMS, SMSB(3,2), SR, SRP, SRV,                   KF 0389
     #          U1, U2, VCOEF, ZERO                                      KF 0389
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS, ALOG, MOD                                         KF 0389
C
C     + + + EXTERNALS + + +
      EXTERNAL   ADJST, DSM, FLOW, LISTTS, PRFL, PROUT,                  KF 0389
     #           SEGSIO, STORM, COPYI, DATNXT, ZIPR, WDTGET              KF 0290
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  FLAG, BACK, DTRAN, QFLAG, TUNITS, FLGW, FLGR, LEN5, ZERO     KF 0191
     #     /   1,   -1,     0,    30,      2,    1,    2,    5,  0.0 /   KF 0389
C
C     + + + FORMATS + + +
   68 FORMAT (   1X, 'Maximum storage in detention reservoir ', A4,      KF 0389
     #               ' for storm', I3, ' was', F8.3, ' cfs-hours' )      KF 0389
   69 FORMAT (1H1)                                                        B4220
   70 FORMAT (//,1X, 'Do optimization and routing runs separately ',     KF 0389
     #               'since EAC changes are made.')                      KF 0389
   71 FORMAT (//,1X, 'Error in retrieval or input hydrograph:',          KF 0389
     #        /, 1X, '     dsn =', I6                                    KF 0389
     #        /, 1X, '    date =', I5, 2('/',I2), I3, 2(':',I2),         KF 0389
     #        /, 1X, '  number =', I10,                                  KF 0389
     #        /, 1X, '    code =', I10, // )
   72 FORMAT (//,1X, '!!!!',                                             KF 0389
     #        /, 1X, '!!!! WARNING:  Impervious retention exceeds ',     KF 0389
     #               'precipitation for', I5,2('/',I2), ' storm.',       KF 0389
     #        /, 1X, '!!!!', / )                                         KF 0389
   80 FORMAT( /, 2X, A66 )                                               KF 0389
 2000 FORMAT (   1X, '     Storm', I3, ', dated', I5, 2('-',I2) )        KF 0290
C
C     + + + END SPECIFICATIONS + + +
C             INITIALIZE                                                  B 350
      IPTIM = PTIME + .001                                              0184 KF
      U1=0.0                                                              B 360
      U2=0.0                                                              B 370
      VCOEF=26.8888889*DA*NDELS                                           B 380
      NO2=NOFE+NOFE                                                       B 390
      DO 1 I=1,NOFE                                                       B 400
      OUTVOL(I)=0.0                                                       B 410
    1 SFVOL(I)=0.0                                                        B 420
      DO 3 NRGI=1,NRG                                                     B 430
      DO 2 I=1,NO2                                                        B 440
    2 PSUM(I,NRGI)=0.0                                                    B 450
    3 CONTINUE                                                            B 460
      I1=1                                                                B 470
      SMS=0.0                                                             B 480
      BMS=0.0                                                             B 490
      CHG=1                                                               B 500
C             ESTABLISH CURRENT INFILTRATION PARAMETER VALUES             B 510
      PSP=X(1)                                                            B 520
      KSAT=X(2)*(CORF/60.)                                                B 530
      RGF=X(3)                                                            B 540
      BMSN=X(4)                                                           B 550
      EVC=X(5)                                                            B 560
      RR=X(6)                                                             B 570
      DRN24=X(2)*24.0                                                     B 580
      EAC=X(7)                                                            B 590
      PAC=1.0                                                             B 600
      IF (IEAC.EQ.1) CALL ADJST(EAC,DA1,DA2,DA3,PAC,RAT)                 KF 0389
      IF (NPAR.NE.1) THEN                                                KF 0389
        PSP2=X(8)                                                         B 630
        KSAT2=X(9)*(CORF/60.)                                             B 640
        RGF2=X(10)                                                        B 650
        BMSN2=X(11)                                                       B 660
        EVC2=X(12)                                                        B 670
        RR2=X(13)                                                         B 680
        DRN242=X(9)*24.0                                                  B 690
        DRAIN2=DRN242/NDELS                                               B 700
        COEF2=(RGF2-1.0)/BMSN2                                            B 710
      END IF                                                             KF 0389
      KDRAIN=DRN24/NDELS                                                  B 720
      COEF=(RGF-1.0)/BMSN                                                 B 730
C             INITIALIZE VARIABLES                                        B 740
      DO 6 NP=1,NPAR                                                      B 750
      DO 5 I=1,NRG                                                        B 760
      SMSB(I,NP)=0.0                                                      B 770
    5 BMSB(I,NP)=0.0                                                      B 780
    6 CONTINUE                                                            B 790
      KP=1                                                                B 800
C             BEGIN SIMULATION                                            B 810
      APRNT=(CORF+.0001)/DT                                               B 820
      IPRNT=APRNT                                                         B 830
      OSI=0.0                                                             B 840
      KINIT=0                                                             B 850
      NFD=0                                                               B 860
      NFD1=0                                                              B 870
      W=0                                                                 B 880
      DO 65 IW=1,RODYS                                                    B 890
      W=W+1                                                               B 900
      IF (W.GT.RODYS) GO TO 65                                            B 910
C             FOR GAP IN RECORD, INITIALIZE SOIL MOISTURE TO ZERO         B 920
      IF (W.EQ.INDP(KP)) THEN                                            KF 0389
        LJ=KP+1                                                           B 940
        W=INDP(LJ)+1                                                      B 950
        KP=KP+2                                                           B 960
        BMS=0.0                                                           B 970
        SMS=0.0                                                           B 980
        DO 8 NP=1,NPAR                                                    B 990
          DO 7 LJ=1,NRG                                                   B1000
            BMSB(LJ,NP)=0.0                                               B1010
            SMSB(LJ,NP)=0.0                                               B1020
    7     CONTINUE                                                       KF 0389
    8   CONTINUE                                                          B1030
      END IF                                                             KF 0389
      PW=RR*DP(W)                                                         B1050
      ETW=EVC*DE(W)                                                       B1070
      IF (NPAR.EQ.2) THEN                                                KF 0389
        PW2 = RR2 * DP(W)                                                KF 0389
        ETW2 = EVC2 * DE(W)                                              KF 0389
      END IF                                                             KF 0389
      IF (PW.LT.0.0) GO TO 45                                             B1090
C             IF FLAG=0, DO STORM COMPUTATIONS                            B1100
C             IF FLAG=1, DO DAILY MOISTURE ACCOUNTING                     B1110
      IF (FLAG.NE.0) GO TO 43                                             B1120
C             SET-UP FOR ROUTING THE GENERATED EXCESS PRECIPITATION       B1130
      NFD1=0                                                              B1140
      NFD=NFD+1                                                           B1150
      KIN=0                                                               B1160
   10 IF (I1.GT.NOFE) GO TO 42                                            B1170
C                                                                       0184 KF
      IBEG(1) = NDATE(I1,3) + 1900                                      1284 KF
      IBEG(2) = NDATE(I1,1)                                             0184 KF
      IBEG(3) = NDATE(I1,2)                                             0184 KF
      IBEG(4) = NHOUR(I1) / 100                                         0184 KF
      IBEG(5) = MOD( NHOUR(I1), 100 )                                   0184 KF
      CALL COPYI ( LEN5, IBEG, DATE )                                    KF 0389
      DATE(6) = 0                                                        KF 0389
      WRITE (IOOT,2000) I1, DATE(1), DATE(2), DATE(3)                    KF 0290
      CALL DATNXT ( IPTIM, BACK, DATE )                                  KF 0389
      IBEG(1) = IBEG(1) - 1900                                           KF 0389
      IF (IHYD(I1) .NE. 0  .AND.  B3 .NE. 0) THEN                        KF 0389
        CALL ZIPR ( IUNIT, ZERO, QIH )                                   KF 0389
C       replaced read for card group 24                                  KF 0389
        IK1 = K1(I1)                                                    0184 KF
        I3Q = K2(I1)                                                    0184 KF
        IKN = I3Q - IK1 + 1                                             0184 KF
        CALL WDTGET ( WDMFL, DSN(8), IPTIM, DATE, IKN, DTRAN, QFLAG,     KF 0389
     #                TUNITS, QIH(IK1), ICH )                            KF 0389
        IF(ICH .NE. 0) THEN                                             0184 KF
          WRITE(OUTFIL,71) DSN(8), IBEG, ICH                            DT 1089
          STOP                                                          ** STOP
        ENDIF                                                           0184 KF
        IF(OPTION .EQ. IOUT(1)) THEN                                    1284 KF
          WRITE(IL,80) STA(8)                                           1284 KF
          CALL LISTTS( IL, IBEG, IPTIM, IKN, QIH(IK1) )                 1284 KF
        ENDIF                                                           1284 KF
      END IF                                                             KF 0389
      IF (B3.EQ.0.OR.KOUT(I1).EQ.0) KINIT=0                               B1270
      IJKS=NFS(I1)                                                        B1280
      IJK=NFE(I1)                                                         B1290
      I12=I1+NOFE                                                         B1300
      IK=K1(I1)                                                           B1310
C              COMPUTE PERVIOUS RAINFALL EXCESS                           B1320
      DO 15 I=IJKS,IJK                                                    B1330
      KDY=I+1441                                                          B1340
      DO 15 NRGI=1,NRG                                                    B1350
      PSUM(I1,NRGI)=PSUM(I1,NRGI)+P(I,NRGI)                               B1360
   15 IF (NPAR.NE.1) PSUM(I12,NRGI)=PSUM(I12,NRGI)+P(KDY,NRGI)            B1370
      QMX=0.0                                                             B1380
      IF (KOUT(I1).EQ.0.OR.B3.EQ.0) GO TO 38                              B1390
      IF (KNN.GT.0.AND.IEAC.EQ.1) THEN                                   KF 0389
        WRITE (OUTFIL,70)                                               DT 1089
        STOP                                                             KF 0389
      END IF                                                             KF 0389
      NSTRMS=NSTRMS+1                                                     B1420
      HEAD2(NSTRMS,1)=NRECDS+1                                            B1430
      NSTRCD=HEAD2(NSTRMS,1)                                              B1440
      IF (JPERM.EQ.0 .AND. JPTG.EQ.0) NSTRCD = 1                        1184 KF
C               ** FLOW ROUTING **                                        B1460
      IF (IPR(NSEG).EQ.1) WRITE (OUTFIL,69)                             DT 1089
C             ROUTE EACH SEGMENT                                          B1480
      DO 31 NSGA=1,NSEG                                                   B1490
      KK=KSEG(NSGA)                                                       B1500
      KINIT=1                                                             B1510
      IF (NSGA.NE.1) THEN                                                KF 0389
        DO 16 I=1,ICT                                                     B1530
          FLAT(I)=0.0                                                     B1540
          FUP(I)=0.0                                                      B1550
   16   CONTINUE                                                         KF 0389
        DO 19 J=1,4                                                       B1560
          IF (JLAT(KK,J) .GT. 0) THEN                                    KF 0389
            JJ=JLAT(KK,J)                                                 B1580
            IRECD=NSTRCD+NRPSEG*(JJ-1)                                    B1590
            CALL SEGSIO ( FLGR, JPERM, IFILE, WDMFL, DSNS(JJ),           KF 0191
     I                    ICT, NRPSEG, IRECD, DATE, DTFLW, TUFLW,        KF 0191
     O                    FLW )                                          KF 0191
            DO 18 I=1,ICT                                                 B1610
              FLAT(I)=FLAT(I)+FLW(I)                                      B1620
   18       CONTINUE                                                     KF 0389
          END IF                                                         KF 0389
   19   CONTINUE                                                          B1630
        DO 22 J=1,3                                                      B1640
          IF (JUP(KK,J) .GT. 0) THEN                                    KF 0389
            JJ=JUP(KK,J)                                                  B1660
            IRECD=NSTRCD+NRPSEG*(JJ-1)                                    B1670
            CALL SEGSIO ( FLGR, JPERM, IFILE, WDMFL, DSNS(JJ),           KF 0191
     I                    ICT, NRPSEG, IRECD, DATE, DTFLW, TUFLW,        KF 0191
     O                    FLW )                                          KF 0191
            DO 21 I=1,ICT                                                 B1690
              FUP(I)=FUP(I)+FLW(I)                                        B1700
   21       CONTINUE                                                     KF 0389
          END IF                                                         KF 0389
   22   CONTINUE                                                          B1710
      END IF                                                             KF 0389
      IK=K1(I1)                                                           B1730
      I4Q=0                                                               B1740
      IJ=1                                                                B1750
      ICT=0                                                               B1760
      IF ((ITYPE(KK) .EQ. 5 .OR. ITYPE(KK) .EQ. 6) .AND. NFD1 .LE. 0)    KF 0389
     #  CALL ZIPR ( NRG, 0.0, IMPSTO )                                   KF 0389
C             ROUTE FOR EACH TIME STEP                                    B1820
      DO 29 I=IJKS,IJK                                                    B1830
        IF (ITYPE(KK).EQ.5.OR.ITYPE(KK).EQ.6) THEN                       KF 0389
C              CALCULATE IMPERVIOUS RETENTION                             B1850
          DO 27 NRGI=1,NRG                                                B1860
            IF (ABS(IMPSTO(NRGI)-IMP) .GE. 0.1E-9) THEN                  KF 0389
              I2=NDAY*(NRGI-1)+IK                                         B1880
              IMPSTT=IMPSTO(NRGI)+UPR(I2)/DEL5/3.                         B1890
              IF (IMPSTT.GT.IMP) IMPSTT=IMP                               B1900
              IMPRET(NRGI)=IMPSTT-IMPSTO(NRGI)                            B1910
              IMPSTO(NRGI)=IMPSTT                                         B1920
            ELSE                                                         KF 0389
              IMPRET(NRGI)=0.0                                            B1940
            END IF                                                       KF 0389
   27     CONTINUE                                                        B1950
        END IF                                                           KF 0389
C               **ROUTE OVER TIME INT. = CORF                             B1960
        CALL FLOW ( I, DT, DTS )                                         KF 0191
        ECOMP=ECOMP+CORF                                                  B1980
C             DETERMINE WHETHER OR NOT AT END OF UNIT-TIME INTERVAL       B1990
        IJ=IJ+1                                                           B2000
        IF (IJ.EQ.DEL5P) THEN                                            KF 0389
          IK=IK+1                                                         B2020
          OSI=0.0                                                         B2030
          I4Q=0                                                           B2040
          IJ=1                                                            B2050
        END IF                                                           KF 0389
   29 CONTINUE                                                            B2060
      IF (JPERM.NE.0 .OR. NSGA.NE.NSEG .OR. JPTG.NE.0) THEN              KF 0389
C       IF (NSGA.EQ.1) NRPSEG=ICT/120+1-(1-MIN0(1,MOD(ICT,120)))          B2080
        IF (NSGA .EQ. 1) NRPSEG = (ICT - 1) / 120 + 1                    KF 0389
        IRECD=NSTRCD+NRPSEG*(KK-1)                                        B2090
        CALL SEGSIO ( FLGW, JPERM, IFILE, WDMFL, DSNS(KK),               KF 0191
     I                ICT, NRPSEG, IRECD, DATE, DTFLW, TUFLW, FLW )      KF 0191
        NRECDS=NRECDS+NRPSEG                                              B2120
      END IF                                                             KF 0389
   31 CONTINUE                                                            B2200
      CALL PRFL (IJKS, IJK, I1, ND, IPRNT, CORF, DEL5, ICNT, SRV, QMX)   KF 0389
C             COMPUTE OUTLET VOLUME                                       B2210
      OUTVOL(I1)=SRV/VCOEF                                                B2220
      HEAD2(NSTRMS,2)=ICT                                                 B2230
C            WRITE OUT MAXIMUM STORAGE IN RESERVOIRS                      B2240
      IF (NO8.NE.0) THEN                                                 KF 0389
        DO 32 JJ=1,NO8                                                    B2260
          K5=IRES(JJ)                                                     B2270
          WRITE (OUTFIL,68) ISEG(K5),I1,SMAX(K5)                        DT 1089
   32   CONTINUE                                                         KF 0389
      END IF                                                             KF 0389
      IF (NO9.NE.10) THEN                                                KF 0389
        DO 34 JJJ=11,NO9                                                  B2300
          K5=IRES(JJJ)                                                    B2310
          WRITE (OUTFIL,68) ISEG(K5),I1,SMAX(K5)                        DT 1089
   34   CONTINUE                                                         KF 0389
      END IF                                                             KF 0389
Ckf   IF (IPL(I1).EQ.0) GO TO 38                                          B2340
Ckf            ** PLOT **                                                 B2350
Ckf   YMAX=QMX                                                            B2360
Ckf   IF (QMX.LT.FPK(I1)) YMAX=FPK(I1)                                    B2370
Ckf   CALL PLT(Q,R,ICNT,1,YMAX)                                          KF 0389
Ckf   IF (OPT.EQ.1) GO TO 37                                              B2390
Ckf   JJ=0                                                                B2400
Ckf   LK=K1(I1)                                                           B2410
Ckf   LJ=K2(I1)                                                           B2420
Ckf   DO 36 KQ = LK, LJ                                                 0186 KF
Ckf   JJ=JJ+1                                                             B2440
Ckf   R(JJ)=UD(KQ)                                                        B2450
Ckf36 IF (TRYCT.GT.0) R(JJ)=R(JJ)-BFL(I1)                                 B2460
Ckf   CALL PLT(Q,R,ICNT,2,YMAX)                                          KF 0389
Ckf37 CONTINUE                                                            B2480
Ckf   CALL PLT(Q,R,ICNT,3,YMAX)                                          KF 0389
   38 CONTINUE                                                            B2500
C             COPY SIMULATED STORM RUNOFF VOLUME AND PEAK                 B2510
C             FOR I-TH EVENT INTO STORAGE ARRAYS SFVOL AND SFPK.          B2520
      QCW=0.0                                                             B2530
      DO 39 LK=1,NRG                                                      B2540
      LJ=LK+3                                                             B2550
      PLIMP = POBS(I1,LK) - IMP                                          0186 KF
      IF (PLIMP .LT. 0.0) THEN                                           0186 KF
C       IMPERVIOUS RETENTION EXCEEDS PRECIPITATION                       0186 KF
        WRITE(OUTFIL,72) NDATE(I1,1), NDATE(I1,2), NDATE(I1,3)          DT 1089
        PLIMP = POBS(I1,LK)                                              0186 KF
      END IF                                                             0186 KF
      QCW=QCW+EAC*DIMP(LK,1)* (   PLIMP     ) +PAC*(DIMP(LJ,1)*PSUM(I1,L 0186 KF
     1K)+DIMP(LJ,2)*PSUM(I12,LK))                                         B2570
      IF (NFD1.NE.0) QCW=QCW+EAC*DIMP(LK,1)*IMP                          KF 0389
   39 CONTINUE                                                            B2600
      SFVOL(I1)=QCW/(5280.0*5280.0*DA)                                    B2610
      SFPK(I1)=QMX                                                        B2620
      IF (TESTNO(I1).NE.1) GO TO 40                                       B2630
      IF (ABS(SFVOL(I1)) .LT. 0.1E-9) GO TO 40                          1184 KF
      IF (ABS(FVOL(I1)) .LT. 0.1E-9) GO TO 40                           1184 KF
      U2=U2+ALOG(SFVOL(I1)/FVOL(I1))**2                                   B2660
   40 IF (ABS(QMX) .LT. 0.1E-9) GO TO 41                                1184 KF
      IF (ABS(FPK(I1)) .LT. 0.1E-9) GO TO 41                            1184 KF
      U1=U1+ALOG(QMX/FPK(I1))**2                                          B2690
   41 I1=I1+1                                                             B2700
      NFD1=NFD1+1                                                         B2710
C             IF HAVE ANALYZED ALL EVENTS OF SET OF EVENTS, GO TO 42    1184 KF
      IF (NF(NFD).EQ.NFD1) GO TO 42                                       B2730
      IJK=IJK+1                                                           B2740
      IJKS=NFS(I1)                                                        B2750
      KIN=0                                                               B2760
C             IF NEXT STORM BEGINS IMMEDIATELY AFTER LAST STORM, KIN=1    B2770
      IF (IJK.EQ.IJKS.AND.KOUT(I1-1).EQ.1) KIN=1                          B2780
      GO TO 10                                                            B2790
   42 NFD=NFD+NFD1-1                                                      B2800
      IF (W.GT.RODYS) GO TO 65                                            B2810
      FLAG=1                                                              B2820
      NFD1=0                                                              B2830
      CHG=1                                                               B2840
C             ** DAILY  ACCOUNTING **                                     B2850
   43 INC=PW-ETW                                                          B2860
      IF (NPAR.EQ.2) INC2=PW2-ETW2                                        B2870
      DO 44 III=1,NRG                                                     B2880
      CALL DSM(SMSB(III,1),BMSB(III,1),INC,DRN24,BMSN)                    B2890
      IF (NPAR.EQ.2) CALL DSM(SMSB(III,2),BMSB(III,2),INC2,DRN242,BMSN2)  B2900
   44 CONTINUE                                                            B2910
C             FINISHED WITH DAY                                           B2920
      GO TO 65                                                            B2930
C            ** DETERMINE TIME-SERIES OF RAINFALL EXCESS                  B2940
   45 FLAG=0                                                              B2950
      NFD1=NFD1+1                                                         B2960
      IF (NFD1.GT.1) GO TO 49                                             B2970
      IFP=IFILEP                                                          B2980
      IUNIT3=IUNIT*NRG                                                    B2990
      DO 46 I=1,IUNIT3                                                    B3000
   46 UPR(I)=0.0                                                          B3010
      DO 47 III=1,NRG                                                     B3020
      READ (IFP) K4ST,K4DAY,(UPR(I),I=K4ST,K4DAY)                         B3030
      IFP=IFP+1                                                           B3040
      IF (III .EQ. 1) THEN                                              0287 KF
        K4STD = K4ST                                                    0287 KF
        K4DAYD = K4DAY                                                  0287 KF
      END IF                                                            0287 KF
   47 CONTINUE                                                            B3050
      IF (OPT.GT.0) GO TO 48                                              B3060
      IF (TRYCT.GT.0.AND.B3.EQ.0) GO TO 48                                B3070
      READ (IFILED) KDUM, (UD(I),I=K4STD,K4DAYD)                        0287 KF
   48 IF (TRYCT.EQ.0)                                                    KF 0389
     #  CALL STORM ( DEL5, NOUT, NRG, DA, OPT, NDELS, NUPD, NOUP,        KF 0389
     #               I1, NOFE, NDAY )                                    KF 0389
   49 CONTINUE
      DO 64 NP=1,NPAR                                                     B3100
      DRAIN=KDRAIN                                                        B3110
      IF (NP.EQ.2) DRAIN=DRAIN2                                           B3120
      BMST=BMSN                                                           B3130
      IF (NP.EQ.2) BMST=BMSN2                                             B3140
      IF (W.GT.RODYS) GO TO 64                                            B3150
      ETDEL=PDEL*ETW                                                      B3160
      IF (NP.EQ.2) ETDEL=PDEL*ETW2                                        B3170
      IF (NP.EQ.1) KINIT=KINIT+1                                          B3180
      K=NDELS*(KINIT-1)+1                                                 B3190
      DO 63 III=1,NRG                                                     B3200
      CHG=1                                                               B3210
      KDAY=ND*(KINIT-1)+1                                                 B3220
C             COMPUTE SMS,BMS FOR AREAS FOR EACH RAIN GAGE, SOIL TYPE     B3230
      SMS=SMSB(III,NP)                                                    B3240
      BMS=BMSB(III,NP)                                                    B3250
      KKK=K+(III-1)*NOUT*NDELS                                            B3260
      K4DAY=KKK+NDELS-1                                                   B3270
      DO 62 KK=KKK,K4DAY                                                  B3280
        IF (UPR(KK).GT.0.0) THEN                                         KF 0389
          SRP=UPR(KK)/DEL5                                                B3300
          SR=SRP*RAT                                                      B3310
          IF (CHG.EQ.1) THEN                                             KF 0389
C             BEGIN COMPUTATION OF INFILTRATION                           B3330
C             REDETERMINE PS AFTER BREAK IN RAINFALL                      B3340
            PS=PSP*(RGF-COEF*BMS)                                         B3350
            IF (NP.EQ.2) PS=PSP2*(RGF2-COEF2*BMS)                         B3360
            CHG=0                                                         B3370
          END IF                                                         KF 0389
C             DEFINE CORF-MIN. RAINFALL SUPPLY RATE                       B3390
          IF (SMS.GT.0.01) THEN                                          KF 0389
C             IF SATURATED ZONE EXISTS                                    B3410
            FR=KSAT*(1.0+PS/SMS)                                          B3420
            IF (NP.EQ.2) FR=KSAT2*(1.0+PS/SMS)                            B3430
          ELSE                                                           KF 0389
C             IF NO SATURATED ZONE EXISTS                                 B3450
            FR=KSAT*(1.0+PS/SR)                                           B3460
            IF (NP.EQ.2) FR=KSAT2*(1.0+PS/SR)                             B3470
          END IF                                                         KF 0389
C             DETERMINE EXCESS PPT. IN UNIT TIME                          B3480
          DO 55 NKL=1,DEL5                                                B3490
            IF (SR.LT.FR) THEN                                           KF 0389
              QR=(SR*SR)/(2.0*FR)                                         B3510
            ELSE                                                         KF 0389
C             PONDED CONDITION                                            B3530
              QR=SR-FR/2.0                                                B3540
            END IF                                                       KF 0389
            SMS=SMS+SR-QR                                                 B3550
C             KDAY IS CORF-MIN. INTERVAL IN A DETAILED STORM              B3560
            KDY=KDAY                                                      B3570
            IF (NP.EQ.2) KDY=KDY+1441                                     B3580
            P(KDY,III)=QR                                                 B3590
            KDAY=KDAY+1                                                   B3600
C             SMS= NEW MOISTURE CONTENT OF SATURATED ZONE                 B3610
            FR=KSAT*(1.0+PS/SMS)                                          B3620
            IF (NP.EQ.2) FR=KSAT2*(1.0+PS/SMS)                            B3630
   55     CONTINUE                                                       KF 0389
C             DEPLETION OF SOIL MOISTURE BY ET DURING UNIT-TIME           B3650
C             INTERVALS OF NO PPT.                                        B3660
        ELSE                                                             KF 0389
          IF (SMS.GT.ETDEL) THEN                                         KF 0389
            SMS=SMS-ETDEL                                                 B3690
          ELSE                                                           KF 0389
            BMS=BMS+SMS-ETDEL                                             B3710
            SMS=0.0                                                       B3720
C             CHECK FOR COMPLETE SOIL DRYING                              B3730
            IF (BMS.LE.0.0) BMS=0.0                                       B3740
          END IF                                                         KF 0389
C             REDISTRIBUTION OF SOIL MOISTURE WITH FLOW FROM              B3750
C             SATURATED TO UNSATURATED ZONE                               B3760
          IF (SMS.GT.DRAIN) THEN                                         KF 0389
            SMS=SMS-DRAIN                                                 B3780
            BMS=BMS+DRAIN                                                 B3790
C             BMS= NEW SOIL MOISTURE CONTENT OF UNSATURATED ZONE          B3800
          ELSE                                                           KF 0389
            BMS=BMS+SMS                                                   B3820
            SMS=0.0                                                       B3830
          END IF                                                         KF 0389
C             DRAINAGE TO LOWER LYING ZONE                                B3840
          IF (BMS.GT.BMST) BMS=BMST                                       B3850
C             BREAK IN UNIT RAINFALL                                      B3860
          CHG=1                                                           B3870
C             NO EXCESS PRECIPITATION                                     B3880
          DO 61 NKL=1,DEL5                                                B3890
            KDY=KDAY                                                      B3900
            IF (NP.EQ.2) KDY=KDY+1441                                     B3910
            P(KDY,III)=0.0                                                B3920
            KDAY=KDAY+1                                                   B3930
   61     CONTINUE                                                       KF 0389
        END IF                                                           KF 0389
C             144 ENDS RAIN GAGE III FOR UNIT PPT. DAY.                   B3940
   62 CONTINUE                                                            B3950
C             COMPUTE SMS AND BMS FOR AREAS COVERED BY EACH RAIN GAGE     B3960
      SMSB(III,NP)=SMS                                                    B3970
      BMSB(III,NP)=BMS                                                    B3980
C             148 ENDS UNIT PPT. DAY                                      B3990
   63 CONTINUE                                                            B4000
   64 CONTINUE                                                            B4010
C             147 ENDS ALL DAYS, W= 1,RODYS                               B4020
   65 CONTINUE                                                            B4030
      IF (OPT.EQ.0.AND.TRYCT.EQ.0) REWIND IFILED                          B4040
      DO 66 III=1,NRG                                                     B4050
      IFP=IFILEP+III-1                                                    B4060
      REWIND IFP                                                          B4070
   66 CONTINUE                                                            B4080
      U(1)=U1                                                             B4090
      U(2)=U2                                                             B4100
      U(3)=U1+0.5*U2                                                      B4110
C             * RITE ROUTINE *                                            B4120
      IF (TRYCT.EQ.0) CALL PROUT(1,PAC)                                   B4130
      IF (RITE.EQ.1) THEN                                                KF 0389
        CALL PROUT(2,PAC)                                                 B4150
        RITE=0                                                            B4160
      END IF                                                             KF 0389
      RETURN                                                              B4170
C                                                                         B4180
      END                                                                 B4250-
C
C
C
      SUBROUTINE   DSM                                                    C  10
     #                (SMS,BMS,INC,DRN24,BMSN)                           KF 0389
C
C     + + + PURPOSE + + +
C     This subroutine does soil moisture accounting on days of daily
C     rainfall.  It adds daily rainfall to SMS, subtracts ET from SMS
C     or (if SMS=0) from BMS, and drain s SMS downward to BMS.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL SMS,BMS,DRN24,INC,BMSN                                         C  60
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SMS    - soil moisture storage in saturated zone (volume of
C              infiltration during period)
C     BMS    - base soil-moisture storage
C     INC    -
C     DRN24  -
C     BMSN   - maximum value of base soil-moisture storate, BMS
C
C     + + + END SPECIFICATIONS + + +
      IF (INC.GT.0.0) THEN                                               KF 0389
C             ADD EXCESS MOISTURE TO SATURATED ZONE                       C  80
        SMS=SMS+INC                                                       C  90
C             DEDUCT MOISTURE DEFICIENCY FROM SATURATED ZONE              C 110
      ELSE IF ((SMS+INC).LT.0.0) THEN                                    KF 0389
C             EVAPOTRANSPIRATION FROM UNSATURATED ZONE                    C 130
        BMS=BMS+SMS+INC                                                   C 140
        SMS=0.0                                                           C 150
C             CHECK FOR COMPLETE SOIL DRYING                              C 160
        IF (BMS.LT.0.0) BMS=0.0                                           C 170
      ELSE                                                               KF 0389
C             EVAPOTRANSPIRATION FROM SATURATED ZONE                      C 190
        SMS=SMS+INC                                                       C 200
      END IF                                                             KF 0389
C             REDISTRIBUTION OF SOIL MOISTURE WITH FLOW FROM              C 210
C             SATURATED TO UNSATURATED ZONE                               C 220
      IF (SMS.GT.DRN24) THEN                                             KF 0389
C             MOISTURE IN SATURATED ZONE ABOVE FIELD CAPACITY             C 240
        SMS=SMS-DRN24                                                     C 250
        BMS=BMS+DRN24                                                     C 260
      ELSE                                                               KF 0389
C             SATURATED ZONE COMPLETELY DEPLETED                          C 280
        BMS=BMS+SMS                                                       C 290
C             BMS= NEW MOISTURE CONTENT OF UNSATURATED ZONE               C 300
        SMS=0.0                                                           C 310
      END IF                                                             KF 0389
C             DRAINAGE TO DEEPER LYING ZONE                               C 320
      IF (BMS.GT.BMSN) BMS=BMSN                                          KF 0389
      RETURN                                                              C 340
      END                                                                 C 350-
C
C
C
      SUBROUTINE   STORM                                                  U  10
     I                  ( DEL5, NOUT, NRG, DA, OPT, NDELS, NUPD, NOUP,   KF 0389
     M                    I1, NOFE,                                      KF 0389
     O                    NDAY )                                         KF 0389
C
C     + + + PURPOSE + + +
C     Storm analysis routine.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DEL5, NOUT, NRG, OPT, NDELS, NUPD, NOUP(150), I1,        KF 0890
     #          NOFE, NDAY                                               KF 0389
      REAL      DA                                                       KF 0389
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DEL5   - number of intervals of rainfall-excess computations in
C              unit time interval (based on PTIME and CORF)
C     NOUT   - maximum number of consecutive storm days allowed by
C              dimensions of program
C     NRG    - number of rain gages
C     DA     - basin drainage area, in square miles
C     OPT    - indicator flag for reading storm values
C              0 - measured unit discharge are read
C              1 - measured unit discharge are not read
C     NDELS  - number of unit time intervals in a day
C     NUPD   - number of days of unit rainfall
C     NOUP   - array containing sequence date of for days of unit discharge
C     I1     - storm event counter/pointer
C     NOFE   - number of storm events
C     NDAY   - number od time intervals for a rain gage
C
C     + + + PARAMETERS + + +
      INCLUDE 'pinout.inc'                                               KF 0290
      INCLUDE 'plimt.inc'                                                KF 0389
C
C     + + + COMMONS + + +
      INCLUDE 'cstrm.inc'                                                KF 0389
      INCLUDE 'cuprc.inc'                                                KF 0389
      INCLUDE 'cdeud.inc'                                                KF 0389
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, I2, I4, K, KR, L, LJ, LM, NFI1, NOFT                  KF 0389
      REAL      I2CFSP, Q3, QMX, QR, SRV                                 KF 0389
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS                                                    KF 0389
C
C     + + + FORMATS + + +
   12 FORMAT (//,1X, 'Start of storm number', I3, ' has been changed ',  KF 0389
     #               'to KS =', I5, '.',                                 KF 0389
     #        /, 1X, 'This corresponds to first rainfall.  If output ',  KF 0389
     #               'from this model'                                   KF 0389
     #        /, 1X, 'serves as input to quality model, then use the ',  KF 0389
     #               'revised value ',                                   KF 0389
     #        /, 1X, 'for start of storm.' )
C
C     + + + END SPECIFICATIONS + + +
C
C             NDAY IS SET TO NUMBER OF TIME INTERVALS FOR A RAIN GAGE     U 270
      NDAY=NOUT*NDELS                                                     U 280
C             5280**2/12*60*60*24=26.888:  CONVERTS INCHES TO CFS         U 290
      I2CFSP=26.8888889*DA*NDELS                                          U 300
      NOFT=NOFE                                                           U 310
      I4=I1                                                               U 320
      NOFE=I1                                                             U 330
C             BEGIN ANALYSIS OF A SET OF EVENTS                           U 340
      I = I1 - 1                                                         KF 0191
    1 CONTINUE                                                            U 370
        I = I + 1                                                        KF 0191
      IF (NOUP(I+1) .EQ. NOUP(I)+1  .AND.  I .LT. NUPD) GO TO 1          KF 0191
      NFI1=NF(NOFE)                                                       U 380
      I1=I+1                                                              U 390
C             BEGIN ANALYSIS OF A STORM                                   U 400
C             FIND PEAK DISCHARGE                                         U 410
 3    CONTINUE
        QR=0.0                                                            U 420
        QMX=0.0                                                           U 430
        SRV=0.0                                                           U 440
        LJ=K1(NOFE)                                                       U 450
        LM=K2(NOFE)                                                       U 460
        NFI1=NFI1-1                                                       U 470
C             COMPUTE TOTAL RAINFALL FOR STORM AND                        U 480
C             REVISE START OF STORM TO COINCIDE WITH                      U 490
C              FIRST RAINFALL                                             U 500
        KR=0                                                              U 510
        DO 5 L=LJ,LM                                                      U 520
          DO 4 I=1,NRG                                                    U 530
            I2=NDAY*(I-1)+L                                               U 540
            POBS(NOFE,I)=POBS(NOFE,I)+UPR(I2)                             U 550
            IF (KR .LE. 0  .AND.  ABS(POBS(NOFE,I)) .GT. 0.1E-9) THEN    KF 0191
              KR=L                                                        U 580
              IF (KR .NE. LJ) THEN                                       KF 0191
                K1(NOFE)=KR                                               U 600
                NFS(NOFE)=(KR-1)*DEL5+1                                   U 610
                WRITE (OUTFIL,12) NOFE,KR                                DT 1089
              END IF                                                     KF 0191
            END IF                                                       KF 0191
 4        CONTINUE                                                        U 630
 5      CONTINUE                                                          U 640
        IF (OPT .NE. 1) THEN                                             KF 0191
          DO 6 K=LJ,LM                                                    U 660
            Q3=UD(K)                                                      U 670
            IF (K.EQ.LJ) QR=Q3                                            U 680
            IF (Q3 .GT. QMX) QMX = Q3                                    KF 0191
 6        CONTINUE                                                        U 710
C                 FIND RUNOFF VOLUME ABOVE BASEFLOW                       U 720
          DO 8 L=LJ,LM                                                    U 730
C                 CHECK UNIT DISCHARGE FOR VALUES LESS THAN BASEFLOW      U 740
C                 IF FOUND SET BASEFLOW TO MINIMUM UNIT DISCHARGE         U 750
            IF (UD(L) .LT. QR) QR = UD(L)                                KF 0191
            SRV=SRV+UD(L)                                                 U 780
 8        CONTINUE                                                        U 790
          SRV=SRV-QR*(LM-LJ+1)                                            U 800
          FVOL(NOFE)=SRV/I2CFSP                                           U 810
C                 FIND PEAK DISCHARGE ABOVE BASEFLOW                      U 820
          FPK(NOFE)=QMX-QR                                                U 830
          IF (ABS(QR) .GT. 0.1E-9) THEN                                  KF 0191
            DO 9 K=LJ,LM                                                    U 850
 9          UD(K)=UD(K)-QR                                                  U 860
          END IF                                                         KF 0191
        END IF                                                           KF 0191
        BFL(NOFE)=QR                                                      U 880
        NOFE=NOFE+1                                                       U 890
C             CHECK FOR MORE STORMS IN SET OF EVENTS                      U 900
      IF (NFI1.GT.0) GO TO 3                                              U 910
      NOFE=NOFT                                                           U 920
      I1=I4                                                               U 930
      RETURN                                                              U 940
C                                                                         U 950
      END                                                                 U1000-
C
C
C
      SUBROUTINE   ADJST                                                  Z  10
     I                  ( EAC, DA1, DA2, DA3,                            KF 0389
     O                    PAC, RAT )                                     KF 0389
C
C     + + + PURPOSE + + +
C     Adjusts areas for EAC.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL      EAC, DA1, DA2, DA3, PAC, RAT                             KF 0389
C
C     + + + ARGUMENT DEFINITIONS + + +
C     EAC    - adjustment factor for effective impervious area
C     DA1    - effective impervious area of watershed
C     DA2    - noneffective impervious area of watershed
C     DA3    - pervious area of watershed
C     PAC    -
C     RAT    - ratio of pervious area and noneffective impervious area
C              to pervious area
C
C     + + + PARAMETERS + + +
      INCLUDE 'pinout.inc'                                               KF 0290
C
C     + + + LOCAL VARIABLES + + +
      REAL      DA2NEW, DA3NEW, DIFF                                     KF 0389
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS                                                    KF 0389
C
C     + + + FORMATS + + +
    4 FORMAT (/, 1X, 'Upper limit on EAC is too large.' )                KF 0389
C
C     + + + END SPECIFICATIONS + + +
      DIFF=(EAC-1.0)*DA1                                                  Z  40
C     DA1NEW=DA1+DIFF                                                    KF 0389
      DA2NEW=DA2-DIFF                                                     Z  60
      DA3NEW=DA3                                                          Z  70
      IF (DA2NEW.GT.0.0) GO TO 1                                          Z  80
      DA3NEW=DA3+DA2NEW                                                   Z  90
      IF (DA3NEW.LT.0.0) GO TO 3                                          Z 100
      DA2NEW=0.0                                                          Z 110
    1 IF (ABS(DA3NEW) .LT. 0.1E-9) GO TO 2                              1184 KF
      RAT=(DA2NEW+DA3NEW)/DA3NEW                                          Z 130
    2 IF (ABS(DA3) .LT. 0.1E-9) RETURN                                  1184 KF
      PAC=DA3NEW/DA3                                                      Z 150
      RETURN                                                              Z 160
    3 WRITE (OUTFIL,4)                                                  DT 1089
      STOP                                                                Z 180
C                                                                         Z 190
      END                                                                 Z 210-
C
C
C
      SUBROUTINE   SEGSIO
     I                   ( FLAG, JPERM, IFILE, WDMFL, DSNS,              KF 0191
     I                     ICT, NRPSEG, IRECD, DATE, DTFLW, TUFLW,       KF 0191
     M                     FLW )                                         KF 0191
C
C     + + + PURPOSE + + +
C     This routine outputs/inputs segment discharges to/from the
C     direct access file.  Routine added 07/87.
C     Option to output/input to temporary file or WDM file added 3/89.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FLAG, JPERM, IFILE, WDMFL, DSNS, ICT, NRPSEG, IRECD,     KF 0191
     >          DATE(6), DTFLW, TUFLW                                    KF 0191
      REAL      FLW(1441)                                                KF 0389
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FLAG   - indicator flag for type of operation
C                1 - write discharges to file
C                2 - read discharges from file
C     JPERM  - storage method
C              0 - old format, no WDM, file not saved
C              1 - old format, no WDM, file saved
C              2 - old format, selected WDM
C              3 - WDM file only
C     IFILE  - Fortran unit number of old format file
C     WDMFL  - Fortran unit number of WDM file
C     DSNS   - data-set number in WDM file for segment flow
C              required if JPERM = 3,
C              optional if 0 <= JPERM <= 2 (use 0 if segment is not to
C              be saved in WDM.
C     ICT    - number of values to be read/written
C     NRPSEG - number of records required for the discharges
C     IRECD  - pointer to first record for current segment in old
C              format file
C     DATE   - starting date of requested data
C     DTFLW  - time step of data in TUFLW units
C     TUFLW  - time units of data
C     FLW    - array of ICT discharges
C
C     + + + PARAMETERS + + +
      INCLUDE 'pinout.inc'                                               KF 0290
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   ICT1, ICTN, N, I, DTRAN, QFLG, DTOVWR, RET
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDTPUT, WDTGET
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   DTRAN, QFLG, DTOVWR
     #     /     0,    0,      1 /
C
C     + + + FORMATS + + +
 2000 FORMAT (//,1X, 'Error in get/put of data:',
     #        /, 1X, '            dsn =', I6,
     #        /, 1X, '     start date =', I5,2('/',I2), I3,2(':',I2),
     #        /, 1X, '      no values =', I6,
     #        /, 1X, '    return code =', I6, / )
C
C     + + + END SPECIFICATIONS + + +
C
      IF (JPERM .EQ. 3) THEN
C       segment flows in WDM file, old format file not used
        RET = 0                                                          KF 0191
        IF (FLAG .EQ. 1) THEN
C         write discharge to dsn
          CALL WDTPUT ( WDMFL, DSNS, DTFLW, DATE, ICT, DTOVWR,           KF 0191
     #                  QFLG, TUFLW, FLW, RET )
        ELSE IF (FLAG .EQ. 2) THEN
C         read discharge from dsn
          CALL WDTGET ( WDMFL, DSNS, DTFLW, DATE, ICT, DTRAN,            KF 0191
     #                  QFLG, TUFLW, FLW, RET )
        END IF
        IF (RET .NE. 0) THEN
C         error in get/put of data
          WRITE (OUTFIL,2000) DSNS, DATE, ICT, RET                       KF 0191
        END IF
      ELSE                                                               KF 0191
C       old format file used for speed, selected segments to WDM         KF 0191
        IF (FLAG .EQ. 1) THEN
C         write discharges to file
          ICT1 = 1
          DO 100 N = 1, NRPSEG
            ICTN = N * 120
            IF (ICTN .GT. ICT) ICTN = ICT
            WRITE(IFILE,REC=IRECD) ( FLW(I), I = ICT1, ICTN )
            ICT1 = ICTN + 1
            IRECD = IRECD + 1
 100      CONTINUE
          IF (DSNS .GT. 0  .AND.  JPERM .EQ. 2) THEN                     KF 0191
C           write discharge to dsn
            CALL WDTPUT ( WDMFL, DSNS, DTFLW, DATE, ICT, DTOVWR,         KF 0191
     #                    QFLG, TUFLW, FLW, RET )                        KF 0191
            IF (RET .NE. 0) THEN                                         KF 0191
              WRITE (OUTFIL,2000) DSNS, DATE, ICT, RET                   KF 0191
            END IF
          END IF
        ELSE IF (FLAG .EQ. 2) THEN
C         read discharges from file
          ICT1 = 1
          DO 200 N = 1, NRPSEG
            ICTN = N * 120
            IF (ICTN .GT. ICT) ICTN = ICT
            READ(IFILE,REC=IRECD) ( FLW(I), I = ICT1, ICTN )
            ICT1 = ICTN + 1
            IRECD = IRECD + 1
 200      CONTINUE
        END IF
      END IF
C
      RETURN
      END
