C
C
C
      SUBROUTINE   PROUT                                                  S  10
     I                  ( IV, PAC )                                      KF 0389
C
C     + + + PURPOSE + + +
C     Prints out summary of simulation.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IV                                                       KF 0389
      REAL      PAC                                                      KF 0389
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IV     - flag for printing measured data
C              1 - output measured data
C              0 - do not output measured data
C     PAC    -
C
C     + + + PARAMETERS + + +
      INCLUDE 'pinout.inc'                                               KF 0290
      INCLUDE 'plimt.inc'                                                KF 0389
C
C     + + + COMMONS + + +
      INCLUDE 'cstrm.inc'                                                KF 0389
      INCLUDE 'cc7t8.inc'                                                KF 0389
      INCLUDE 'chead.inc'                                                KF 0389
      INCLUDE 'cz1t4.inc'                                                KF 0389
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, I12, III, LJ, LL, IPQ(3), IPP(3)                      KF 0389
      REAL      QCW, VR, CNVRT                                           KF 0389
      CHARACTER*80 CODE                                                  KF 0389
C
C     + + + INTRINSICS + + +
      INTRINSIC   ALOG                                                   KF 0290
C
C     + + + FORMATS + + +
   13 FORMAT ( '1',                                                      KF 0389
     #        /, 1X, 'End of run--results of last successful trial.' )   KF 0389
   19 FORMAT (//,1X, 'Objective function =', F12.3,                      KF 0389
     #        //,1X, '            Final    Lower    Upper ',             KF 0389
     #        /, 1X, 'Parameter   value    value    boune ',             KF 0389
     #        /, 1X, '---------  -------  -------  -------' )            KF 0389
   20 FORMAT (   1X, I5, 4X, 3F9.4 )                                     KF 0389
   21 FORMAT (/, 1X, 'New value for RAT is', F6.3 )                      KF 0389
   25 FORMAT (//,1X, F8.3, ' - objective function for peaks.' )          KF 0389
 2000 FORMAT ( A80 )                                                     KF 0389
 2001 FORMAT ( '1',                                                      KF 0389
     #        //,1X, 'Summary of measured data',                         KF 0389
     #        //,1X, '    storm                   ',                     KF 0389
     #               '  direct       peak     baseflow',                 KF 0389
     #        /, 1X, '-------------   rainfall    ',                     KF 0389
     #               '  runoff    discharge    assumed',                 KF 0389
     #        /, 1X, ' no    date     (inches)    ',                     KF 0389
     #               ' (inches)     (cfs)       (cfs) ',                 KF 0389
     #        /, 1X, '---  --------   --------    ',                     KF 0389
     #               ' --------   ---------   --------' )                KF 0389
 2002 FORMAT ( '1',                                                      KF 0389
     #        //,1X, 'Summary of measured data',                         KF 0389
     #        //,1X, '    storm        rainfall (inches)  ',             KF 0389
     #               '  direct       peak     baseflow',                 KF 0389
     #        /, 1X, '-------------    -----------------  ',             KF 0389
     #               '  runoff    discharge    assumed',                 KF 0389
     #        /, 1X, ' no    date       gage 1   gage 2   ',             KF 0389
     #               ' (inches)     (cfs)       (cfs) ',                 KF 0389
     #        /, 1X, '---  --------     ------   ------   ',             KF 0389
     #               ' --------   ---------   --------' )                KF 0389
 2003 FORMAT ( '1',                                                      KF 0389
     #        //,1X, 'Summary of measured data',                         KF 0389
     #        //,1X, '    storm             rainfall (inches)      ',    KF 0389
     #               '  direct       peak     baseflow',                 KF 0389
     #        /, 1X, '-------------     ------------------------   ',    KF 0389
     #               '  runoff    discharge    assumed',                 KF 0389
     #        /, 1X, ' no    date       gage 1   gage 2   gage 3   ',    KF 0389
     #               ' (inches)     (cfs)       (cfs) ',                 KF 0389
     #        /, 1X, '---  --------     ------   ------   ------   ',    KF 0389
     #               ' --------   ---------   --------' )                KF 0389
 2004 FORMAT ( I4, 2X, I2,'/',I2,'/',I2, 2X, 3F9.3, 37(' ') )            KF 0389
 2005 FORMAT ( F9.3 )                                                    KF 0389
 2006 FORMAT ( F9.2, F11.3 )                                             KF 0389
 2020 FORMAT ( '1',                                                      KF 0389
     #        //,1X, 'Summary of simulated data:',                       KF 0389
     #        //,1X, '             simulated rainfall   runoff ',        KF 0389
     #               '(inches)                               ',          KF 0389
     #        /, 1X, '               excess (inches)  ---------',        KF 0389
     #               '---------                     contri- ',           KF 0389
     #        /, 1X, '   storm     ------------------          ',        KF 0389
     #               'simulated     peak (cfs)     bution to',           KF 0389
     #        /, 1X, '------------  pervious          measured ',        KF 0389
     #               '  volume  ------------------ objective',           KF 0389
     #        /, 1X, ' no   date      area     total   direct  ',        KF 0389
     #               'at outlet measured simulated  function',           KF 0389
     #        /, 1X, '--- --------  --------  ------  -------- ',        KF 0389
     #               '--------- -------- --------- ----------' )         KF 0389
 2021 FORMAT ( I4, 1X, I2,'/',I2,'/',I2, F10.3, F8.3, 49(' ') )          KF 0389
 2022 FORMAT ( 2F10.3, F9.2, F10.2, F10.3 )                              KF 0389
 2023 FORMAT ( F10.2 )                                                   KF 0389
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   IPQ,      IPP                                               KF 0389
     #     / 29,38,47, 41,50,59 /                                        KF 0389
C
C     + + + END SPECIFICATIONS + + +
      IF (IV.EQ.1) THEN                                                  KF 0389
C       output measured data
        IF (NRG .EQ. 1) THEN                                             KF 0389
C         one rain gage                                                  KF 0389
          WRITE (OUTFIL,2001)                                           DT 1089
        ELSE IF (NRG .EQ. 2) THEN                                        KF 0389
C         two rain gages                                                 KF 0389
          WRITE (OUTFIL,2002)                                           DT 1089
        ELSE
C         three rain gages
          WRITE (OUTFIL,2003)                                           DT 1089
        END IF                                                           KF 0389
        LL=0                                                              S 210
        DO 2 I=1,NOFE                                                     S 230
          WRITE (CODE,2004) I, (NDATE(I,III), III = 1, 3),               KF 0389
     #                      (POBS(I,III), III = 1, NRG)                  KF 0389
          IF (FVOL(I) .GT. 0.0) WRITE (CODE(IPQ(NRG):80),2005) FVOL(I)   KF 0389
          IF (FPK(I) .GT. 0.0)                                           KF 0389
     #      WRITE (CODE(IPP(NRG):80),2006) FPK(I), BFL(I)                KF 0389
          WRITE (OUTFIL,2000) CODE                                      DT 1089
          IF (KOUT(I) .EQ. 1) THEN                                       KF 0389
            LL = LL + 1                                                  KF 0389
            HEAD3(LL)=NDATE(I,3)*10000+NDATE(I,1)*100+NDATE(I,2)         KF 0389
          END IF                                                         KF 0389
    2   CONTINUE                                                          S 330
      ELSE                                                               KF 0389
        IF (KNN.NE.0) THEN                                               KF 0389
          IF (B3.EQ.1) THEN                                              KF 0389
C           results of last trial                                        KF 0389
            WRITE (OUTFIL,13)                                           DT 1089
            WRITE (OUTFIL,19) U(NN)                                     DT 1089
            WRITE (OUTFIL,20) (I, X(I), G(I), H(I), I = 1, EO)          DT 1089
            IF (IEAC.EQ.1) WRITE (OUTFIL,21) RAT                        DT 1089
          END IF                                                         KF 0389
        END IF                                                           KF 0389
        CNVRT = PAC / (5280. * 5280. * DA)                               KF 0389
        WRITE (OUTFIL,2020)                                             DT 1089
        DO 11 I=1,NOFE                                                    S 460
          I12=I+NOFE                                                      S 470
          QCW=0.0                                                         S 480
          DO 6 III=1,NRG                                                  S 490
            LJ=III+3                                                      S 500
            QCW=QCW+DIMP(LJ,1)*PSUM(I,III)+DIMP(LJ,2)*PSUM(I12,III)       S 510
    6     CONTINUE                                                       KF 0389
          QCW=QCW * CNVRT                                                KF 0389
          IF (SFVOL(I).LT.QCW) SFVOL(I)=QCW                               S 530
          WRITE (CODE,2021) I, (NDATE(I,III), III = 1, 3), QCW, SFVOL(I) KF 0389
          IF (TESTNO(I) .EQ. 1) THEN                                     KF 0389
            VR = 0.0                                                     KF 0389
            IF (FVOL(I) .GT. 0.0  .AND.  SFVOL(I) .GT. 0.0)              KF 0389
     #        VR = ALOG( FVOL(I) / SFVOL(I) ) ** 2                       KF 0389
            WRITE (CODE(32:80),2022) FVOL(I), OUTVOL(I), FPK(I),         KF 0389
     #                               SFPK(I), VR                         KF 0389
          ELSE IF (FVOL(I) .GT. 0) THEN                                  KF 0389
            WRITE (CODE(32:80),2022) FVOL(I), OUTVOL(I), FPK(I), SFPK(I) KF 0389
          ELSE IF (SFPK(I) .GT. 0.0) THEN                                KF 0389
            WRITE (CODE(61:80),2023) SFPK(I)                             KF 0389
          END IF                                                         KF 0389
          WRITE(OUTFIL,2000) CODE                                       DT 1089
   11   CONTINUE                                                          S 690
        IF (U(1).GT.0.0) WRITE (OUTFIL,25) U(1)                         DT 1089
      END IF                                                             KF 0389
      RETURN                                                              S 710
      END                                                                 S 960-
C
C
C
      SUBROUTINE   PRFL                                                   T  10
     I                 ( IJKS, IJK, I1, ND, IPRNT, CORF, DEL5,           KF 0389
     O                   ICNT,                                           KF 0389
     O                   SRV, QMX )                                      KF 0389
C
C     + + + PURPOSE + + +
C     Outputs detailed simulated data and sets up data for plotting.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IJKS, IJK, I1, ND, IPRNT, ICNT, DEL5                     KF 0389
      REAL      CORF, SRV, QMX                                           KF 0389
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IJKS   - CORF-minute interval within sequence of days at which
C              a storm starts
C     IJK    - CORF-minute interval within sequence of days at which
C              a storm ends
C     I1     - storm event counter/pointer
C     ND     - number of CORF-minute intervals in a day
C     IPRNT  - number of DT's in CORF minutes
C     CORF   - time interval of rainfall-excess computations, in
C              minutes, assigned as follows:
C              1.O if PTIME < 5.0 minutes
C              5.0 if 5.0 minutes <= PTIME < 15.0 minutes
C              15. if PTIME >= 15 minutes
C     DEL5   - number of CORF-minute intervals in unit-time interval
C     ICNT   - counter for PTIME-minuted intervals
C     SRV    -
C     QMX    - peak flow during a storm
C
C     + + + PARAMETERS + + +
      INCLUDE 'pinout.inc'                                               KF 0290
      INCLUDE 'plimt.inc'                                                KF 0389
C
C     + + + COMMONS + + +
      INCLUDE 'csgs1.inc'                                                KF 0389
      INCLUDE 'csgs2.inc'                                                KF 0389
      INCLUDE 'csgs3.inc'                                                KF 0389
      INCLUDE 'csgsc.inc'                                                KF 0389
      INCLUDE 'cf1a3.inc'                                                KF 0389
      INCLUDE 'cunit.inc'                                                KF 0389
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, I5, IE, IHR(5), IJJ, IRV, IV, IX, J, K, MOUT(5),      KF 0389
     #          NL, NR, TOTLNS, TOTPTS, NCOLS                            KF 0389
      REAL      RT(5), TMN(5), TOUT(5)                                   KF 0389
C
C     + + + INTRINSICS + + +
      INTRINSIC   AMOD, INT                                              KF 0389
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  NCOLS / 4 /                                                  KF 0389
C
C     + + + FORMATS + + +
 2000 FORMAT (//,1X, ' storm number', I3,                                KF 0389
     #        /, 1X, ' segment ', A4,                                    KF 0389
     #        //,    4( '     time     flow  ' ),                        KF 0389
     #        /,     4( '     hours     cfs  ' ),                        KF 0389
     #        /,     4( '    ------  --------' ) )                       KF 0389
 2001 FORMAT (       4( I6, ':', F3.0, F10.3 ) )                         KF 0389
C
C     + + + END SPECIFICATIONS + + +
C
      K=KK                                                                T 190
      IF (IPR(K).NE.1) GO TO 1                                          1184 KF
    1 IJJ=0                                                               T 240
      I5=0                                                                T 250
      ICT=0                                                               T 260
      ICNT=0                                                              T 270
      SRV=0.                                                              T 280
      QMX=0.                                                              T 290
      DO 6 I=IJKS,IJK                                                     T 300
      IJJ=IJJ+1                                                           T 310
      ICT=ICT+IPRNT                                                       T 320
      IF (IJJ.NE.DEL5) GO TO 6                                            T 330
      IJJ=0                                                               T 340
      I5=I5+1                                                             T 350
      RT(I5)=FLW(ICT)                                                     T 360
      MOUT(I5)=I/ND                                                       T 370
      IRV=MOUT(I5)*ND                                                     T 380
      TOUT(I5)=((I-IRV)*CORF)/60.                                         T 390
      IF (IPR(K).NE.1) GO TO 2                                          1184 KF
      IHR(I5)=INT(TOUT(I5))                                               T 410
      TMN(I5)=AMOD(TOUT(I5),1.)*60.                                       T 420
    2 IF (I5.LT.5.AND.I.LT.IJK) GO TO 6                                   T 430
      IF (IPR(K).NE.1) GO TO 3                                          1184 KF
C     WRITE (OUTFIL,2001) (IHR(IV),TMN(IV),RT(IV),IV=1,I5)              DT 1089
    3 IF (K.NE.KSEG(NSEG)) GO TO 5                                        T 460
C          FIND OUTLET PEAK AND VOLUME OF RUNOFF                          T 470
C          SET UP FOR PLOTTING OUTLET HYDROGRAPH                          T 480
      DO 4 J=1,I5                                                         T 490
      ICNT=ICNT+1                                                         T 500
      R(ICNT)=RT(J)                                                       T 510
      SRV=SRV+RT(J)                                                       T 520
      IF (RT(J).GT.QMX) QMX=RT(J)                                         T 530
      IF (IPL(I1).EQ.0) GO TO 4                                           T 540
      Q(ICNT)=TOUT(J)+MOUT(J)*24.                                         T 550
    4 CONTINUE                                                            T 560
    5 I5=0                                                                T 570
    6 CONTINUE                                                            T 580
C                                                                       II061583
C  WRITE TABLE - DOWN COLUMNS INSTEAD OF ACROSS ROWS                    II061583
C                                                                       II061583
      K = KK                                                            II061583
      IX = 0                                                            II061583
      WRITE (OUTFIL,2000) I1, ISEG(K)                                   DT 1089
      TOTPTS = (IJK - (IJKS-1))/DEL5                                    II061583
      TOTLNS = TOTPTS / NCOLS                                            KF 0389
      IF (TOTLNS*NCOLS.LT.TOTPTS) TOTLNS = TOTLNS + 1                    KF 0389
      DO 108 NL = 1,TOTLNS                                              II061583
        DO 107 NR = 1, NCOLS                                             KF 0389
          IX = ((NR-1)*TOTLNS + NL)*IPRNT  * DEL5                       PS042384
          I = (IJKS-1) + ((NR-1)*TOTLNS + NL)*DEL5                      II061583
          IF (I.GT.IJK) GO TO 108                                       II061583
          IE = NR                                                       PS042384
          RT(NR) = FLW(IX)                                              II061583
          MOUT(NR) = I/ND                                               II061583
          IRV = MOUT(NR)*ND                                             II061583
          TOUT(NR) = ((I-IRV)*CORF)/60.0                                II061583
          IHR(NR) = INT(TOUT(NR))                                       II061583
          TMN(NR) = AMOD(TOUT(NR),1.)*60.                               II061583
107     CONTINUE                                                        II061583
108   WRITE(OUTFIL,2001) (IHR(IV),TMN(IV),RT(IV),IV=1,IE)               DT 1089
      RETURN                                                              T 590
      END                                                                 T 650-
C
C
C
      SUBROUTINE   SVHYD
     I                  ( OPT, NRG, PTIME, DTFLW, TUFLW )                KF 0191
C
C     + + + PURPOSE + + +
C     Generates a PLTGEN format output file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   OPT, NRG, DTFLW, TUFLW                                   KF 0191
      REAL      PTIME                                                    KF 0389
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OPT    - Indicator flag for observed discharge
C              0 - observed discharge available
C              1 - observed discharge not available
C     NRG    - number of raingages
C     PTIME  - time step for unit data, in minutes
C     DTFLW  -
C     TUFLW  -
C
C     + + + PARAMETERS + + +
      INCLUDE 'pinout.inc'                                               KF 0290
      INCLUDE 'plimt.inc'                                                KF 0389
C
C     + + + COMMONS + + +
      INCLUDE 'cstrm.inc'                                                KF 0389
      INCLUDE 'csgs2.inc'                                                KF 0389
      INCLUDE 'chead.inc'                                                KF 0389
      INCLUDE 'cf1a3.inc'                                                KF 0389
C
      COMMON /FKSP  / BUFF
      REAL      BUFF(4,2881)                                             KF 0389
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   BACK, DATE(6), DATEG(6), FLGR, I, IFP, IPT, IPTS,        RSR1093
     #          ISKP, J, JBQ, JFLAG, K, K4E, K4S, KDUM, KDUM1, KDUM2,    KF 0389
     #          KP, KPT, KREC, KST, N, NC, NFNEW, NG, NN, NOFE, NRPSEG,  KF 0389
     #          NSEGS, NV, PDT, PKTIM(5), XX, YY, KEEP(10),              DT 0889
     #          LEN5, LEN6                                               KF 0389
      REAL      ARRAY(10,2881), BQ, DARRAY(2881), PEAKS(10)              KF 0389
      LOGICAL   PPEAK, PUNIT                                             KF 0389
      CHARACTER*4 PEAK, UNIT                                             DT 0889
C
C     + + + EXTERNALS + + +
      EXTERNAL    COPYI, DATNXT, SEGSIO, ZIPR                            KF 0389
C
C     + + + DATA INITIALIZATIONS + + +
      DATA    PEAK,   UNIT,  XX, YY, BACK, LEN5, LEN6                    KF 0389
     #     / 'PEAK', 'UNIT', 50, 51,   -1,    5,    6 /                  KF 0389
C
C     + + + FORMATS + + +
 2001 FORMAT ( '1',                                                      KF 0389
     #        //,1X, 'Processed for the PLTGEN files(s):',               KF 0389
     #        //,1X, '   Curve      PLTGEN',                             KF 0389
     #        /, 1X, 'description   column',                             KF 0389
     #        /, 1X, '------------  ------' )                            KF 0389
 2002 FORMAT (   1X, 'rain gage', I2, I7 )                               KF 0389
 2003 FORMAT (   1X, 'obsv base Q', I7 )                                 KF 0389
 2004 FORMAT (   1X, 'segment ', A4, I6 )                                KF 0389
 2005 FORMAT (   1X, 'segment ', A4, ' not used.' )
 2101 FORMAT(  A4, T6, 'DR3M GENERATED ', A4, ' VALUES PLOT FILE',
     *      /, A4, T6, 'TIME INTERVAL:', I5, ' MINS',
     *             T40,'LAST MONTH IN PRINTOUT YEAR:  9',
     *      /, A4, T6, 'NO. OF CURVES PLOTTED:',
     *             T30,'POINT VALUED:  0',
     *             T49,'MEAN VALUED:', I3,
     *             T67,'TOTAL', I3,
     *      /, A4, T6, 'LABEL FLAG:  0',
     *             T30,'PIVL:    1',
     *             T50,'IDELT:', I5 )
 2102 FORMAT(  A4, T6, 'PLOT TITLE:   ', 10A4,
     *      /, A4, T6, 'Y-AXIS LABEL: ', A4, ' VALUES',
     *      /, A4, T6, 'SCALE INFO:',
     *             T19,'YMIN:     0.0  ',
     *      /, A4, T19,'YMAX:   100.0  ',
     *      /, A4, T19,'TIME   10.           INTERVALS/INCH',
     *      /, A4, T6, 'DATA FOR EACH CURVE ',
     *             T26,'(POINT-VALUED FIRST, THEN MEAN-VALUED):',
     *      /, A4, T6, 'LABEL',
     *             T30,'LINTYP     INTEQ    COLCOD      TRAN   TRANCOD')
 2103 FORMAT(  A4, T6, 'PRECIP, GAGE', I4, T62, 'SUM ' )
 2104 FORMAT(  A4, T6, 'OBSV Q - BASE Q ', T62, 'AVER' )                1284 KF
 2105 FORMAT(  A4, T6, 'SEGMENT ', A4,     T62, 'AVER' )
 2106 FORMAT(  A4 )
 2107 FORMAT(  A4, T6, 'TIME SERIES (PT-VALUED, THEN MEAN-VALUED):',
     *      /, A4,
     *      /, A4, T6, 'DATE/TIME                      VALUES',
     *      /, A4 )
 2108 FORMAT(  A4, I6, 4I3,     10( 2X, G12.5 ) )
C
C     + + + END SPECIFICATIONS + + +
C
C
      PUNIT = .FALSE.
      FLGR = 2                                                          0787 KF
      PPEAK = .FALSE.
      IF(JPTG .EQ. 1  .OR.  JPTG .EQ. 3) PUNIT = .TRUE.
      IF(JPTG .EQ. 2  .OR.  JPTG .EQ. 3) PPEAK = .TRUE.
C
      NOFE = HEAD1(17)
      NSEGS = HEAD1(18)
      PDT = PTIME + .001
      ISKP = ( 60 * PDT ) / HEAD1(19)                                    0186 KF
      DATE(6) = 0
C                                                                        0186 KF
C     REWIND FILES                                                       0186 KF
      IF (OPT .EQ. 0) REWIND IFILED                                      0186 KF
      IFP = IFILEP -1                                                    0186 KF
      DO 90 N = 1, NRG                                                   0186 KF
        IFP = IFP + 1                                                    0186 KF
        REWIND IFP                                                       0186 KF
   90 CONTINUE                                                           0186 KF
C
      WRITE(OUTFIL,2001)                                                DT 1089
      NC = NRG
      DO 100 N = 1, NRG
C       RAIN GAGES
        KEEP(N) = 0
        WRITE(OUTFIL,2002) N, N                                         DT 1089
  100 CONTINUE
      IF (OPT .EQ. 0) THEN
C       OBSERVED DISCHARGE
        NC = NC + 1
        KEEP(NC) = -1
        JBQ = NC                                                        1284 KF
        WRITE(OUTFIL,2003) NC                                           DT 1089
      END IF
C
C     - SIMULATED FLOWS
C
      JFLAG = NC + 1
      DO 150 N = 1, NSEGS
        IPT = NSEGS - N + 1
        IF (IPR(IPT) .EQ. 2) THEN
          IF (NC .LT. 10) THEN
            NC = NC + 1
            KEEP(NC) = IPT
            WRITE(OUTFIL,2004) HEAD1(IPT+21), NC                        DT 1089
          ELSE
            WRITE(OUTFIL,2005) HEAD1(IPT)                               DT 1089
          END IF
        END IF
  150 CONTINUE
C
C-    - WRITE HEADING ON PLTGEN FILE(S).
C
      IF ( PUNIT ) THEN
        WRITE(XX,2101) UNIT, UNIT, UNIT, PDT, UNIT, NC, NC, UNIT, PDT
        WRITE(XX,2102) UNIT, ( HEAD1(I), I = 3, 12 ), (UNIT, J = 1, 7)
      END IF
      IF ( PPEAK ) THEN
        WRITE(YY,2101) PEAK, PEAK, PEAK, PDT, PEAK, NC, NC, PEAK, PDT
        WRITE(YY,2102) PEAK, ( HEAD1(I), I = 3, 12 ), (PEAK, J = 1, 7)
      END IF
      DO 200 N = 1, NC
        IF (KEEP(N) .LE. 0) THEN
          IF (N .LE. NRG) THEN
C           RAIN GAGE
            IF( PUNIT ) WRITE(XX,2103) UNIT, N
            IF( PPEAK ) WRITE(YY,2103) PEAK, N
          ELSE
C           OBSERVED FLOW
            IF( PUNIT ) WRITE(XX,2104) UNIT
            IF( PPEAK ) WRITE(YY,2104) PEAK
          END IF
        ELSE
C         PREDICTED SEGMENT FLOW
          IPT = KEEP(N) + 21
          IF( PUNIT ) WRITE(XX,2105) UNIT, HEAD1(IPT)
          IF( PPEAK ) WRITE(YY,2105) PEAK, HEAD1(IPT)
        END IF
  200 CONTINUE
      NN = 10 - NC
      IF ( PUNIT ) THEN
        WRITE(XX,2106) ( UNIT, N = 1, NN )
        WRITE(XX,2107) ( UNIT, N = 1, 4 )
      END IF
      IF ( PPEAK ) THEN
        WRITE(YY,2106) ( PEAK, N = 1, NN )
        WRITE(YY,2107) ( PEAK, N = 1, 4 )
      END IF
C
C-    - PROCESS FLOW EVENTS
C
      KREC = 3 - NSEGS
      NFNEW = 0
      DO 390 N = 1, NOFE
         BQ = BFL(N)                                                    1284 KF
         KREC = HEAD2(N,1)
         NV = HEAD2(N,2)
C        NRPSEG = NV / 120  +  1  -  (1 - MIN0( 1, MOD(NV,120) ) )      0389 KF
         NRPSEG = (NV - 1) / 120 + 1                                    0389 KF
C        IPTS = NV / 120 + 1                                            0389 KF
         IPTS = NRPSEG                                                  0389 KF
         KST = ISKP                                                     0186 KF
         DATE(1) = NDATE(N,3) + 1900
         DATE(2) = NDATE(N,1)
         DATE(3) = NDATE(N,2)
         DATE(4) = NHOUR(N) / 100
         DATE(5) = NHOUR(N) - DATE(4) * 100
         CALL COPYI ( LEN6, DATE, DATEG )                                KF 0389
         CALL DATNXT ( PDT, BACK, DATEG )                                KF 0389
         CALL ZIPR( NC, 0.0, PEAKS )
         IF (NFNEW .EQ. 0) THEN                                         0186 KF
C          FILL BUFFER WITH OBSERVED DATA                               0186 KF
           IFP = IFILEP - 1                                             0186 KF
           DO 310 NG = 1, NRG
             IFP = IFP + 1                                              0186 KF
             IF (NG .EQ. 1) THEN                                        0287 KF
               READ(IFP) K4S, K4E, ( BUFF(NG,K), K = K4S, K4E )         0186 KF
               IF (OPT .EQ. 0)                                          0287 KF
     *           READ(IFILED) KDUM, ( BUFF(NRG+1,K), K = K4S, K4E )     0287 KF
             ELSE                                                       0287 KF
               READ(IFP) KDUM1, KDUM2, ( BUFF(NG,K), K = K4S, K4E )     0287 KF
             END IF                                                     0287 KF
  310      CONTINUE                                                     0186 KF
           NFNEW = NF(N)                                                0186 KF
         END IF                                                         0186 KF
         KPT = K1(N)                                                    0486 KF
         NFNEW = NFNEW - 1                                              0186 KF
         DO 320 J = 1, NC
            IF (KEEP(J) .EQ. 0  .OR.  KEEP(J) .EQ. -1) THEN             0186 KF
C             GET OBSERVED DATA                                         0186 KF
              KP = KPT                                                  0186 KF
              DO 315 K = KST, NV, ISKP                                  0186 KF
                ARRAY(J,K) = BUFF(J,KP)                                 0186 KF
                KP = KP + 1                                             0186 KF
  315         CONTINUE                                                  0186 KF
            ELSE IF (KEEP(J) .GT. 0) THEN                               0186 KF
C             READ SIMULATED FLOW
              IRECD = KREC + KEEP(J) * IPTS - IPTS                       KF 0389
              ICT = NV                                                   KF 0389
              JJ = KEEP(J)                                               KF 0389
              CALL SEGSIO ( FLGR, JPERM, IFILE, WDMFL, DSNS(JJ),         KF 0191
     I                      ICT, NRPSEG, IRECD, DATEG, DTFLW, TUFLW,     KF 0191
     O                      DARRAY )                                     KF 0191
              DO 318 K = 1, NV                                          0787 KF
                ARRAY(J,K) = DARRAY(K)                                  0787 KF
 318          CONTINUE                                                  0787 KF
            END IF
  320       CONTINUE
         DO 370 K = KST, NV, ISKP                                       0186 KF
            IF(OPT .EQ. 0) ARRAY(JBQ,K) = ARRAY(JBQ,K) - BQ             1284 KF
            IF( PUNIT )
     *        WRITE(XX,2108) UNIT, ( DATE(J), J = 1, 5 ),               0486 KF
     *                       ( ARRAY(J,K), J = 1, NC )                  0486 KF
            IF ( PPEAK ) THEN
              DO 350 J = 1, NC
                IF (J .LE. NRG) THEN
                  IF (ARRAY(J,K) .GT. 0.0)
     *              PEAKS(J) = PEAKS(J) + ARRAY(J,K)
                ELSE
                  IF (ARRAY(J,K) .GT. PEAKS(J)) THEN
                    PEAKS(J) = ARRAY(J,K)
                    IF (J .EQ. JFLAG) CALL COPYI ( LEN5, DATE, PKTIM )   KF 0389
                  END IF
                END IF
  350         CONTINUE
            END IF
            CALL DATNXT( PDT, 1, DATE )
  370     CONTINUE
         IF( PPEAK )
     *     WRITE(YY,2108) PEAK, PKTIM, ( PEAKS(J), J = 1, NC )
  390    CONTINUE
      RETURN
      END
