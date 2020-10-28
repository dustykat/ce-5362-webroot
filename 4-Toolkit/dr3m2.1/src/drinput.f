C
C
C
      SUBROUTINE   CHKIRR
     I                   ( IBEG, IEND, NDAYS, IRR,
     M                     DP )
C
C     + + + PURPOSE + + +
C     Accounts for irregation (for example, lawn watering) in the
C     daily water balance.  If a daily rainfall is less than the
C     daily irregation rate, the daily rainfall is reset equal to
C     the irregation rate.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IBEG(5), IEND(5), NDAYS                                  KF 0389
      REAL      IRR(12), DP(1)                                           KF 0389
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IBEG   - start date (yr,mo,dy,hr,mn)
C     IEND   - end date (yr,mo,dy,hr,mn)
C     NDAYS  - number of days
C     IRR    - Array of monthly irregation rates, in inches per week
C              starting in January
C     DP     - array of daily precipitation
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   D1, DAY, DN, IP, IY, LEAP, M1, MN, MP, CLND(12)          KF 0389
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD                                                    KF 0389
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  CLND
     #    / 31,28,31,30,31,30,31,31,30,31,30,31/
C
C     + + + END SPECIFICATIONS + + +
C
      IY = IBEG(1)
      M1 = IBEG(2)
      MN = 12
      D1 = IBEG(3)
      IP = 0
  100 CONTINUE
         LEAP = 0
         IF(MOD(IY+1900,4) .EQ. 0) LEAP = LEAP + 1
         IF(IP+365+LEAP .GT. NDAYS) MN = IEND(2)
         DO 150 MP = M1, MN
            DN = CLND(MP)
            IF(IP+DN .GT. NDAYS) DN = IEND(3)
            DO 125 DAY = D1, DN
               IP = IP + 1
               IF(DP(IP) .LT. IRR(MP)) DP(IP) = IRR(MP)
  125          CONTINUE
            D1 = 1
  150       CONTINUE
         M1 = 1
         IY = IY + 1
         IF(IY .LT. IEND(1)) GO TO 100
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   JULIAN
     I                           ( IDATE )
C
C     + + + PURPOSE + + +
C     Computes the Julian day of the given date.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IDATE(5)                                                 KF 0389
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IDATE  - date (yr,mo,dy,hr,mn)
C
C     + + + SAVES + + +
      INTEGER   YN(99)                                                   KF 0389
      LOGICAL   FIRST                                                    KF 0389
      SAVE      FIRST, YN
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   CLND(12), I, LEAP
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD                                                    KF 0389
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   FIRST, CLND
     #     /.TRUE., 1,32,60,91,121,152,182,213,244,274,305,335/          KF 0389
C
C     + + + END SPECIFICATIONS + + +
C
      IF( FIRST )
     *  THEN
          YN(1) = 0
          DO 10 I = 2, 99
             YN(I) = YN(I-1) + 365
             IF(MOD(I-1,4) .EQ. 0) YN(I) = YN(I) + 1
   10        CONTINUE
          FIRST = .FALSE.
        ENDIF
C
      LEAP = 0
      IF(MOD(IDATE(1),4) .EQ. 0)
     *  THEN
          IF(IDATE(2) .GT. 2) LEAP = 1
        ENDIF
      JULIAN = YN(IDATE(1)) + CLND(IDATE(2)) - 1 + IDATE(3) + LEAP
C
      RETURN
      END
C
C
C
      SUBROUTINE   LISTTS
     I                   ( IL, JBEG, JDT, NV, X )
C
C     + + + PURPOSE + + +
C     Output observed time-series data to a file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  JBEG(5), JDT, IL, NV                                      KF 0389
      REAL     X(NV)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IL     - Fortran unit number of output file
C     JBEG   - time of first observation (yr,mo,dy,hr,mn)
C     JDT    - time step of data, in minutes
C     NV     - number of values to print
C     X      - array of NV observations
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   DATE(6), I, N, NDT, NP1, NP2, NPS                        KF 0389
C
C     + + + EXTERNALS + + +
      EXTERNAL  DATNXT                                                   KF 0389
C
C     + + + FORMATS + + +
    1 FORMAT ( /, 2X, '   date  hour', / )
    3 FORMAT (    2X, I3, 2I2, I3, I2, 2X, 12F7.2 )
    4 FORMAT ( // )
C
C     + + + END SPECIFICATIONS + + +
C
      DO 100 I = 1, 5
         DATE(I) = JBEG(I)
  100    CONTINUE
      DATE(6) = 0
C
      IF(JDT .EQ. 1440)
     *  THEN
C
C-        - DAILY VALUES
C
          NPS = 10
          NDT = 1440 * NPS
          WRITE(IL,1)
          DATE(4) = 24
        ELSE
C
C-        - UNIT VALUES
C
          NPS = 12
          NDT = JDT * NPS
          WRITE(IL,1)
        ENDIF
C
      DO 200 NP1 = 1, NV, NPS
         NP2 = NP1 + NPS - 1
         IF(NP2 .GT. NV) NP2 = NV
         WRITE(IL,3) (DATE(I), I = 1, 5), ( X(N), N = NP1, NP2 )
         CALL DATNXT( NDT, 1, DATE )
  200    CONTINUE
      WRITE(IL,4)
C
      RETURN
      END
C
C
C
      SUBROUTINE   INPUTN
C
C     + + + PURPOSE + + +
C     This routine is used to read the model input parameters.  It is a
C     combination of old INPUT1, most of old INPUT2, and some of INITOP.
C
C     + + + PARAMETERS + + +
      INCLUDE 'pinout.inc'                                               KF 0290
      INCLUDE 'plimt.inc'                                                KF 0389
C
C     + + + COMMONS + + +
      INCLUDE 'csgs1.inc'                                                KF 0389
      INCLUDE 'cstrm.inc'                                                KF 0389
      INCLUDE 'cc2.inc'                                                  KF 0389
      INCLUDE 'cc3t4.inc'                                                KF 0389
      INCLUDE 'cc7t8.inc'                                                KF 0389
      INCLUDE 'cz1t4.inc'                                                KF 0389
      INCLUDE 'cf1a3.inc'                                                KF 0389
      INCLUDE 'cd1.inc'                                                  KF 0389
      INCLUDE 'chead.inc'                                                KF 0389
      INCLUDE 'cuprc.inc'                                                KF 0389
      INCLUDE 'cdeud.inc'                                                KF 0389
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   BACK, CODE, DATE(6), DATERF, DATERL, DTRAN,              KF 0389
     #          I, I2, IBEG(6), ICH, ID1440, IDATE, IEND(6), IFNSH,      KF 0389
     #          IFOP1, IFP, II, IOPT(13), IPTIM, ISTRT, J, J2,           KF 0290
     #          JBEG(6), JD1, JD2, JEND(6), JINH, JOPT, K, K4DAY, K4ST,  KF 0389
     #          KE, KN, KNT, KP, KPT, KS, KST, L, L2, LD1, LD2, LHOUR,   KF 0389
     #          N, NOPT1, NR, QFLG, TUNITS                               DT 1089
      REAL     IRR(12)
      CHARACTER*4 MOUT(2)                                                DT 1089
C
C     + + + FUNCTIONS + + +
      INTEGER   JULIAN                                                   KF 0389
C
C     + + + EXTERNALS + + +
      EXTERNAL  CHKIRR, COPYI, DATNXT, JULIAN, LISTTS, WDTGET            KF 0389
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  ID1440, DTRAN, QFLG, TUNITS, BACK, MOUT                      KF 0389
     *     /  1440,     0,    0,      2,   -1, 'LIST','  NO' /           KF O389
C
C     + + + FORMATS + + +
    1 FORMAT( A4, 2I1, 4I2, 6X, F8.0, F4.0, I4 )                         KF 0389
    2 FORMAT( /, 2X, 'Control Parameters:',
     *       //, 2X, 5X, A4, ' - list time series data (OPTION)',
     *        /, 2X, 5X, I4, ' - unit discharge, 0-yes, 1-no (OPT)',
     *        /, 2X, 5X, I4, ' - irregation data, 1-yes, 0-no (NOPT1)',
     *        /, 2X, 5X, I4, ' - store segment Q, 0-no,1-temp,2-wdm ',
     *                       '(JPERM)',
     *        /, 2X, 5X, I4, ' - store outflow Q, 1-yes, 0-no (JPUN)',
     *        /, 2X, 5X, I4, ' - inflow hydrograph, 1-yes, 0-no (JINH)',
     *        /, 2X, 5X, I4, ' - pltgen file, 0-no, 1-hydrographs, ',
     *                       '2-peaks, 3-both (JPTG)',
     *        /, 1X,  F10.3, ' - drainage area, in square miles (DA)',
     *        /, 1X,  F10.1, ' - time interval for unit data, in ',
     *                       'minutes (PTIME)',
     *        /, 2X, 5X, I4, ' - number of unit rain gages (NRG)' )      KF 0389
    3 FORMAT(//, 1X, 'Irregation loads in inches: ',
     *       //, 1X, '  Jan   Feb   Mar   Apr   May   Jun ',
     *               '  Jul   Aug   Sep   Oct   Nov   Dec ' ,
     *        /, 1X, 12(' -----') )
    4 FORMAT( 12F5.3 )
    5 FORMAT(    1X, 12F6.3, '  /weak' )
    6 FORMAT(    1X, 12F6.3, '  /day ' )
    7 FORMAT( A66, I5, T1, 2A4, 8X, 12A4,A2 )                            KF 0389
    8 FORMAT( A66, I5 )                                                  KF 0389
   10 FORMAT (//,1X, 'Summary of time series data:',
     #        //,1X, 'type  station         location', 44X, ' dsn',
     #        /, 1X, '----- --------------- ', 50('-'), 1X, '-----',
     #        /, 1X, 'DISCH ', A66, 1X, I5,
     #        /, 1X, 'EVAPR ', A66, 1X, I5,
     #        /, 1X, 'PRECP ', A66, 1X, I5 )
   11 FORMAT (   1X, 'UNPR',I1, 1X, A66, 1X, I5 )
   12 FORMAT (   1X, 'OTHYD ', A66, 1X, I5 )
   13 FORMAT (   1X, 'INHYD ', A66, 1X, I5 )
   14 FORMAT ( I4, 2I3, 6X, I4, 2I3, 5X, I1, I5 )                        KF 0389
   15 FORMAT(//, 1X, 'The simulation period is from ',
     *               I2,'/',I2,'/',I2, ' to ', I2,'/',I2,'/',I2, ',',
     *               I3, ' storms simulated.' )
   16 FORMAT(//, 1X, 'Period of record =',I6, '  which exceeds ',
     *               'the maximum allowable record of', I6, '.' )
   17 FORMAT (//, 1X, 'Error in retrieval of time series data:',
     #         /, 1X, '     dsn =', I6,
     #         /, 1X, '    date =', I5, 2('/',I2), I3, 2(':',I2),
     #         /, 1X, '  number =', I10,
     #         /, 1X, '    code =', I10, // )
   19 FORMAT (    1X, 30X, I2,2('/',I2), ' to ', I2,2('/',I2) )
   27 FORMAT ( '1',
     #        //,1X, 'Summary of storms to be simulated:',
     #        //,1X, '          Begins *        Ends *             ',
     #               '              Flags +',
     #        /, 1X, 'Storm  -------------  -------------  Volume  ',
     #               'Discharge     -------',
     #        /, 1X, '  no   yr mo dy hour  yr mo dy hour  inches  ',
     #               '   cfs        1 2 3 4',
     #        /, 1X, '-----  -- -- -- ----  -- -- -- ----  ------  ',
     #               '---------     - - - -' )
C  27 FORMAT(//, 2X, 'STORM   B E G I N S     E N D S      ',
C    *               'VOLUME  DISCHARGE     FLAGS *   ',
C    *               '---  D A Y ---    K1   K2  NFS  NFE ITEST  NF'
C    *        /, 2X, '       YR MO DY HOUR  YR MO DY HOUR  ',
C    *               'INCHES     CFS        1 2 3 4   ',
C    *               ' BEGIN    END                            ',
C    *        /  )
   28 FORMAT(    1X, I4, 2X, 3I3,I5, 1X, 4I3,I2, F8.2, F9.2, 6X, 4I2 )
C    *               1X, 2I8, 1X, 4I5, I6, I4 )
   29 FORMAT( I4, 3I3, I2, I5, 3I3, I2, 1X, 4I1, 7X, F7.2, F5.2 )        KF 0389
   30 FORMAT (//,1X, 'End of daily record must be as least 1 day ',
     #               'after last storm day.' )
   31 FORMAT (//,1X, 'NOTES:  * Times are at end of the time step.',
     #        //,1X, '        + Flags for storms:',
     #        /, 1X, '          1 - storm routing (KOUT)',
     #        /, 1X, '              0 - not routed, 1 - routed',
     #        /, 1X, '          2 - objective function (TESTNO)',
     #        /, 1X, '              0 - not included, 1 - included',
     #        /, 1X, '          3 - outlet discharge plot (IPL)',
     #        /, 1X, '              0 - not plotted, 1 - plotted',
     #        /, 1X, '          4 - input hydrograph (IHYD)',
     #        /, 1X, '              0 - no, 1 - yes' )
   33 FORMAT (//,1X, 'NOTE--Reset FO, was =', I2, ' now =', I2 )
   34 FORMAT (//,1X, 'NOTE--PLTGEN file is not created in ',
     #               'optimization runs.' )
   41 FORMAT( 3I4, F8.0 )
   42 FORMAT( 3F10.0, I4, 1X, 3F10.0, I4 )                               KF 0389
   44 FORMAT( /, 2X, A66 )                                               KF 0389
C
C     + + + END SPECIFICATIONS + + +
C
      IL = 11
      NDYS = 7310
      B3 = 0
      DO 50 I = 1, 2
         IOUT(I) = MOUT(I)
   50    CONTINUE
      DO 60 I = 1, NDYS
         DP(I) = 0.0
         DE(I) = 0.0
   60    CONTINUE
      DO 70 I = 1, 150
         NOUP(I) = 0
   70    CONTINUE
      IBEG(4) = 0                                                        0586KF
      IBEG(5) = 0
      IBEG(6) = 0                                                        0586KF
      JBEG(4) = 0                                                        0586KF
      JBEG(5) = 0
      JBEG(6) = 0                                                        0586KF
      IEND(4) = 24
      IEND(5) = 0
      IEND(6) = 0                                                        0586KF
      JEND(4) = 24
      JEND(5) = 0
      JEND(6) = 0                                                        0586KF
C
C-    - MODEL OPTIONS
C
      READ(INFIL,1)  OPTION, OPT, NOPT1, JPERM, JPUN, JINH, JPTG,        DT 1089
     *            DA, PTIME, NRG                                         KF 0389
      WRITE(OUTFIL,2) OPTION, OPT, NOPT1, JPERM, JPUN, JINH, JPTG,      DT 1089
     *            DA, PTIME, NRG                                         KF 0389
      JRECDS = 0
      IF(NOPT1 .EQ. 1)
     *  THEN
C
C-        - IRREGATION RATES, CONVERTS INCHES/WEEK TO INCHES/DAY
C
          WRITE(OUTFIL,3)                                               DT 1089
          READ(INFIL,4)  ( IRR(I), I = 1, 12 )                          DT 1089
          WRITE(OUTFIL,5) ( IRR(I), I = 1, 12 )                         DT 1089
          DO 100 I = 1, 12
             IRR(I) = IRR(I) / 7.
  100        CONTINUE
          WRITE(OUTFIL,6) ( IRR(I), I = 1, 12 )                         DT 1089
        ELSE
          DO 105 I = 1, 12
             IRR(I) = 0.0
  105        CONTINUE
        ENDIF
C
C-    - STATION CARDS
C
C
      READ(INFIL,7)  STA(1), DSN(1), (HEAD1(K), K = 1, 15)              DT 1089
      L2 = 3 + NRG
      READ(INFIL,8) ( STA(L), DSN(L), L = 2, L2 )                       DT 1089
      IF(JPUN .NE. 0) READ(INFIL,8) STA(7), DSN(7)                      DT 1089
      IF(JINH .NE. 0) READ(INFIL,8) STA(8), DSN(8)                      DT 1089
      WRITE(OUTFIL,10) ( STA(L), DSN(L), L = 1, 3 )                     DT 1089
      WRITE(OUTFIL,11) ( L, STA(L+3), DSN(L+3), L = 1, NRG )            DT 1089
      IF(JPUN .NE. 0) WRITE(OUTFIL,12) STA(7), DSN(7)                   DT 1089
      IF(JINH .NE. 0) WRITE(OUTFIL,13) STA(8), DSN(8)                   DT 1089
C
C-    - PERIOD OF DAILY RECORD
C
      READ(INFIL,14)  ( JBEG(I), I = 1, 3 ), ( JEND(J), J = 1, 3 ),     DT 1089
     #             CODE, NOFE                                            KF 0389
      DATERF = JULIAN( JBEG )
      DATERL = JULIAN( JEND )
      WRITE(OUTFIL,15) ( JBEG(I), I = 1, 3 ), ( JEND(J), J = 1, 3 ),    DT 1089
     #                   NOFE
      RODYS = DATERL - DATERF + 1
      IF(RODYS .GT. NDYS)
     *  THEN
          WRITE(OUTFIL,16) RODYS, NDYS                                  DT 1089
          STOP                                                          ** STOP
        ENDIF
      HEAD1(20) = JBEG(1) * 10000  +  JBEG(2) * 100  +  JBEG(3)
      HEAD1(21) = JEND(1) * 10000  +  JEND(2) * 100  +  JEND(3)
      IF(CODE .EQ. 9)
     *  THEN
C
C-        - COMPLETE, UNITERRRUPTED DAILY RECORD
C
          JBEG(1) = JBEG(1) + 1900
          CALL COPYI ( 6, JBEG, DATE )                                   0586KF
          JBEG(1) = JBEG(1) - 1900                                       0586KF
          CALL WDTGET( WDMFL, DSN(3), ID1440, DATE, RODYS,               KF 0389
     *                 DTRAN, QFLG, TUNITS, DP, ICH )                    0586KF
          IF(ICH .NE. 0)
     *      THEN
              WRITE(OUTFIL,17) DSN(3), DATE, RODYS, ICH                 DT 1089
              STOP                                                      ** STOP
            ENDIF
          CALL WDTGET( WDMFL, DSN(2), ID1440, DATE, RODYS,               KF 0389
     *                 DTRAN, QFLG, TUNITS, DE, ICH )                    0586KF
          IF(ICH .NE. 0)
     *      THEN
              WRITE(OUTFIL,17) DSN(2), DATE, RODYS, ICH                 DT 1089
              STOP                                                      ** STOP
            ENDIF
          IF(OPTION .EQ. IOUT(1))
     *      THEN
              WRITE(IL,44) STA(3)                                        KF 0389
              CALL LISTTS( IL, JBEG, 1440, RODYS, DP )
              WRITE(IL,44) STA(2)                                        KF 0389
              CALL LISTTS( IL, JBEG, 1440, RODYS, DE )
            ENDIF
          IF(NOPT1 .EQ. 1) CALL CHKIRR( JBEG, JEND, RODYS, IRR, DP )
          INDP(1) = RODYS + 1
        ELSE
C
C-        - GAP(S) IN DAILY RECORD
C
          KP = 0
  150     CONTINUE
             READ(INFIL,14)  ( IBEG(I), I = 1, 3 ),                     DT 1089
     *                    ( IEND(J), J = 1, 3 ), CODE
             ISTRT = JULIAN( IBEG )
             IFNSH = JULIAN( IEND )
             WRITE(OUTFIL,19) ( IBEG(I), I = 1, 3 ),                    DT 1089
     *                        ( IEND(J), J = 1, 3 )
             I1 = ISTRT - DATERF + 1
             I2 = IFNSH - DATERF + 1
             IK = I2 - I1 + 1
             IBEG(1) = IBEG(1) + 1900
             CALL COPYI ( 6, IBEG, DATE )                                0586KF
             IBEG(1) = IBEG(1) - 1900                                    0586KF
             CALL WDTGET( WDMFL, DSN(3), ID1440, DATE, IK,               KF 0389
     *                    DTRAN, QFLG, TUNITS, DP(I1), ICH )             0586KF
             IF(ICH .NE. 0)
     *         THEN
                 WRITE(OUTFIL,17) DSN(3), DATE, IK, ICH                 DT 1089
                 STOP                                                   ** STOP
               ENDIF
             CALL WDTGET( WDMFL, DSN(2), ID1440, DATE, IK,               KF 0389
     *                    DTRAN, QFLG, TUNITS, DE(I1), ICH )             0586KF
             IF(ICH .NE. 0)
     *         THEN
                 WRITE(OUTFIL,17) DSN(2), DATE, IK, ICH                 DT 1089
                 STOP                                                   ** STOP
               ENDIF
             IF(OPTION .EQ. IOUT(1))
     *         THEN
                 WRITE(IL,44) STA(3)                                     KF 0389
                 CALL LISTTS( IL, IBEG, 1440, IK, DP(I1) )
                 WRITE(IL,44) STA(2)                                     KF 0389
                 CALL LISTTS( IL, IBEG, 1440, IK, DE(I1) )
               ENDIF
             IF(I1 .NE. 1)
     *         THEN
                 KP = KP + 1
                 INDP(KP) = I1 - 1
               ENDIF
             IF(CODE .NE. 9)
     *         THEN
                 KP = KP + 1
                 INDP(KP) = I2 + 1
               ENDIF
             IF(NOPT1 .EQ. 1) CALL CHKIRR( IBEG, IEND, IK, IRR, DP )
             IF(CODE .NE. 9) GO TO 150
          INDP(KP+1) = I2 + 1
        ENDIF
C
C-    - PERIOD OF UNIT RECORD
C
      DO 320 I = 1, 60
         FPK(I) = 0.0
         NF(I) = 0
         FVOL(I) = 0.0
         DO 310 II = 1, 3
            POBS(I,II) = 0.0
  310       CONTINUE
  320    CONTINUE
C
      CORF = 5.0
      IF(PTIME .LT. 4.9) CORF = 1.0
      IF(PTIME .GT. 14.9) CORF = 15.
      PDEL = PTIME / 1440.
      NDELS = 1440 / PTIME
      NOUT = IUNIT / NDELS
      IF(PTIME .GE. 5.0) NOUT = NOUT / 2
      NDAY = NOUT * NDELS
      ND = 1440 / CORF
      NUPD = 0
      IPTIM = PTIME + .00001                                             0586KF
      DEL5 = IPTIM / CORF
      DEL5P = DEL5 + 1
      NSD = 0
C
      KPT = 1
      KNT = 0
      WRITE(OUTFIL,27)                                                  DT 1089
      DO 380 I = 1, NOFE
         READ(INFIL,29) (JBEG(J), J = 1, 5), (JEND(J), J = 1, 5),       DT 1089
     *               KOUT(I), TESTNO(I), IPL(I), IHYD(I),                KF 0389
     *               FVOL(I), QINPT(I)                                   KF 0389
         NDATE(I,1) = JBEG(2)
         NDATE(I,2) = JBEG(3)
         NDATE(I,3) = JBEG(1)
         NHOUR(I) = JBEG(4) * 100  +  JBEG(5)
         JD1 = JULIAN( JBEG )
         JD2 = JULIAN( JEND )
         IF(I .EQ. 1)
     *     THEN
             LD1 = JD1
             LD2 = JD1
           ENDIF
         IF(JD1 - LD2 .LE. 1)
     *     THEN
C
C-           - SAME OR NEXT DAY
C
             KNT = KNT + 1
             LD2 = JD2
             LHOUR = 60 * JEND(4)  + JEND(5)
           ELSE
C
C-           - BREAK IN UNIT RECORD
C
             DO 325 L = LD1, LD2
                NUPD = NUPD + 1
                NOUP(NUPD) = L
  325           CONTINUE
             NSD = NSD + 1
             IK = ( LHOUR + 1440 * (LD2 - LD1) ) / PTIME - K1(KPT) + 1
             IBEG(2) = NDATE(KPT,1)
             IBEG(3) = NDATE(KPT,2)
             IBEG(1) = NDATE(KPT,3) + 1900                               0586KF
             IBEG(4) = NHOUR(KPT) / 100
             IBEG(5) = NHOUR(KPT) - IBEG(4) * 100
             CALL COPYI (6, IBEG, DATE )                                 0586KF
             CALL DATNXT( IPTIM, BACK, DATE )                            0586KF
             IBEG(1) = IBEG(1) - 1900                                    0586KF
             IFP = 18
             DO 330 NR = 1, NRG
                KST = NDAY * ( NR - 1 )
                K4ST = KST + K1(KPT)
                K4DAY = K4ST + IK - 1
                N = 3 + NR
                CALL WDTGET( WDMFL, DSN(N), IPTIM, DATE, IK,             KF 0389
     *                       DTRAN, QFLG, TUNITS, UPR, ICH )             0586KF
                IF(ICH .NE. 0)
     *            THEN
                    WRITE(OUTFIL,17) DSN(N), DATE, IK, ICH              DT 1089
                    STOP                                                ** STOP
                  ENDIF
                IF(OPTION .EQ. IOUT(1))
     *            THEN
                    WRITE(IL,44) STA(N)                                  KF 0389
                    CALL LISTTS( IL, IBEG, IPTIM, IK, UPR  )
                  ENDIF
                WRITE(IFP) K4ST, K4DAY, (UPR(J), J = 1, IK)
                IFP = IFP + 1
  330           CONTINUE
             IF(OPT .EQ. 0)
     *         THEN
                 CALL WDTGET( WDMFL, DSN(1), IPTIM, DATE, IK,            KF 0389
     *                        DTRAN, QFLG, TUNITS, UD, ICH )             0586KF
                 IF(ICH .NE. 0)
     *             THEN
                     WRITE(OUTFIL,17) DSN(1), DATE, IK, ICH             DT 1089
                     STOP                                               ** STOP
                   ENDIF
                 IF(OPTION .EQ. IOUT(1))
     *             THEN
                     WRITE(IL,44) STA(1)                                 KF 0389
                     CALL LISTTS( IL, IBEG, IPTIM, IK, UD )
                   ENDIF
                 WRITE(IFILED) IK, (UD(J), J = 1, IK)
               ENDIF
             KN = KPT + KNT - 1
             DO 340 K = KPT, KN
                NF(K) = KNT
  340           CONTINUE
             KPT = KN + 1
             KNT = 1
             LD1 = JD1
             LD2 = JD2
             LHOUR = 60 * JEND(4) + JEND(5)
           ENDIF
         KS = (60*JBEG(4) + JBEG(5) + 1440*(JD1 - LD1)) / PTIME
         KE = (60*JEND(4) + JEND(5) + 1440*(JD2 - LD1)) / PTIME
         K1(I) = KS
         K2(I) = KE
         ITEST(I) = ( KE - KS + 1 )
         NFS(I) = ( KS - 1 ) * DEL5 + 1
         NFE(I) = KE * DEL5
         IF(I .EQ. NOFE)
     *     THEN
C
C-           - LAST STORM PERIOD
C
             DO 345 L = LD1, LD2
                NUPD = NUPD + 1
                NOUP(NUPD) = L
  345           CONTINUE
             NSD = NSD + 1
             IK = KE - K1(KPT) + 1
             IBEG(2) = NDATE(KPT,1)
             IBEG(3) = NDATE(KPT,2)
             IBEG(1) = NDATE(KPT,3) + 1900                               0586KF
             IBEG(4) = NHOUR(KPT) / 100
             IBEG(5) = NHOUR(KPT) - IBEG(4) * 100
             CALL COPYI ( 6, IBEG, DATE )                                0586KF
             CALL DATNXT( IPTIM, BACK, DATE )                            0586KF
             IBEG(1) = IBEG(1) - 1900                                    0586KF
             IFP = 18
             DO 350 NR = 1, NRG
                KST = NDAY * ( NR - 1 )
                K4ST = KST + K1(KPT)
                K4DAY = K4ST + IK -1
                N = NR + 3
                CALL WDTGET( WDMFL, DSN(N), IPTIM, DATE, IK,             KF 0389
     *                       DTRAN, QFLG, TUNITS, UPR, ICH )             0586KF
                IF(ICH .NE. 0)
     *            THEN
                    WRITE(OUTFIL,17) DSN(N), DATE, IK, ICH              DT 1089
                    STOP                                                ** STOP
                  ENDIF
                IF(OPTION .EQ. IOUT(1))
     *            THEN
                    WRITE(IL,44) STA(N)                                  KF 0389
                    CALL LISTTS( IL, IBEG, IPTIM, IK, UPR )
                  ENDIF
                WRITE(IFP) K4ST, K4DAY, (UPR(J), J = 1, IK)
                IFP = IFP + 1
  350           CONTINUE
             IF(OPT .EQ. 0)
     *         THEN
                 CALL WDTGET( WDMFL, DSN(1), IPTIM, DATE, IK,            KF 0389
     *                        DTRAN, QFLG, TUNITS, UD, ICH )             0586KF
                 IF(ICH .NE. 0)
     *             THEN
                     WRITE(OUTFIL,17) DSN(1), DATE, IK, ICH             DT 1089
                     STOP                                               ** STOP
                   ENDIF
                 IF(OPTION .EQ. IOUT(1))
     *             THEN
                     WRITE(IL,44) STA(1)                                 KF 0389
                     CALL LISTTS( IL, IBEG, IPTIM, IK, UD )
                   ENDIF
                 WRITE(IFILED) IK, ( UD(J), J = 1, IK )
               ENDIF
             KN = KPT + KNT - 1
             DO 360 K = KPT, KN
                NF(K) = KNT
  360           CONTINUE
           ENDIF
         WRITE(OUTFIL,28) I, NDATE(I,3), NDATE(I,1), NDATE(I,2),        DT 1089
     *                 NHOUR(I), ( JEND(J2), J2 = 1, 5 ),
     *                FVOL(I), QINPT(I), KOUT(I), TESTNO(I), IPL(I),
     *                IHYD(I)
C    *                       , JD1, JD2, K1(I), K2(I), NFS(I), NFE(I),
C    *                ITEST(I), NF(I)
  380   CONTINUE
      WRITE(OUTFIL,31)                                                  DT 1089
C
      IDATE = JEND(1) * 10000  +  JEND(2) * 100  + JEND(3)
      IF(HEAD1(21) .LE. IDATE)
     *  THEN
          WRITE(OUTFIL,30)                                              DT 1089
          STOP                                                          ** EXIT
        ENDIF
C
C-    - FLAG STORM DAYS IN DAILY PRECIP ARRAY
C
      J = 1
      I = 0
      L = NOUP(1)
      DO 410 K = DATERF, DATERL
         I = I + 1
         IF(K .EQ. L)
     *     THEN
             DP(I) = - 10.
             J = J + 1
             L = NOUP(J)
           ENDIF
  410    CONTINUE
C
C-    - SET OLD VARIABLES
C
      NDAY = NDAY * ( NRG - 1 )
C
C-    - INPUT OF CARD GROUPS 12 - 14 MOVED FROM INITOP.
C
      READ(INFIL,41) NPAR, FO, K, EPSLN                                 DT 1089
      NN = 2
      EO = 7                                                             KF 0389
      IF(NPAR .EQ. 2) EO = 13                                            KF 0389
      DO 510 I = 1, 6                                                    KF 0389
        J = I + 7                                                        KF 0389
        READ (INFIL,42) X(I), G(I), H(I), IOPT(I),                      DT 1089
     #               X(J), G(J), H(J), IOPT(J)                           KF 0389
 510  CONTINUE                                                           KF 0389
      READ (INFIL,42) X(7), G(7), H(7), IOPT(7)                         DT 1089
      JOPT = 0
      DO 515 I = 1, EO                                                   KF 0389
        IF (IOPT(I) .NE. 0) THEN                                         KF 0389
          JOPT = JOPT + 1                                                KF 0389
          OPTNO(JOPT) = I                                                KF 0380
        END IF                                                           KF 0389
 515  CONTINUE                                                           KF 0389
      IF(FO .NE. JOPT) THEN                                              0286 KF
        WRITE(OUTFIL,33) FO, JOPT                                       DT 1089
        FO = JOPT                                                        0286 KF
      END IF                                                             0286 KF
      IF (FO .NE. 0  .AND. JPTG .NE. 0) THEN                             0286 KF
        WRITE(OUTFIL,34)                                                DT 1089
        JPTG = 0                                                         0286 KF
      END IF                                                             0286 KF
      IFOP1 = FO + 1
      DO 520 I = IFOP1, EO
         OPTNO(I) = 0
  520    CONTINUE
      NK = K * FO
C
      IF(NK .EQ. 0) B3 = 1
      RETURN
      END
C
C
C
      SUBROUTINE   CTCHMT                                                 P  10
     O                   ( DT, DTS, DTFLW, TUFLW )                       KF 0191
C
C     + + + PURPOSE + + +
C     Reads in information about segmentation.
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
      INCLUDE 'csgs1.inc'                                                KF 0389
      INCLUDE 'csgs2.inc'                                                KF 0389
      INCLUDE 'csgs3.inc'                                                KF 0389
      INCLUDE 'csgsc.inc'                                                KF 0389
      INCLUDE 'cstrm.inc'                                                KF 0389
      INCLUDE 'cinit.inc'                                                KF 0389
      INCLUDE 'cuprc.inc'                                                KF 0389
      INCLUDE 'cc7t8.inc'                                                KF 0389
      INCLUDE 'cd1.inc'                                                  KF 0389
      INCLUDE 'ce2t3.inc'                                                KF 0389
      INCLUDE 'cpuls.inc'                                                KF 0389
      INCLUDE 'cf1a3.inc'                                                KF 0389
      INCLUDE 'cz1t4.inc'                                                KF 0389
      INCLUDE 'chead.inc'                                                KF 0389
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, I2, I21, ICORF, IDTS, IFLAG, II, IIM, INTVAL,         KF 0389
     #          J, JRCS, K, NRC, NRCP, NRG1,                             KF 0389
     #          CONT                                                     KF 0194
      REAL      A1, CORFS, DDMIN, DDT, QCHK, TEST, O2(MXSG,10),          KF 0389
     #          THIES                                                    KF 0194
      CHARACTER*4 CSEG                                                   KF 0389
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (O2(1,1),P(1,1))                                        P 270
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS, INT, MIN0, MOD                                    KF 0389
C
C     + + + EXTERNALS + + +
      EXTERNAL   SEGDSN                                                  KF 0389
C
C     + + + FORMATS + + +
   17 FORMAT (//,1X, 'Error in segment data for type 15 or 16.' )
   18 FORMAT (//,1X, 'Error in outflow-storage data.' )
   19 FORMAT (3F10.0)                                                     P1190
   23 FORMAT (//,1X, 'Routing interval for detention reservoir ', A4,    KF 1192
     #               ' is too large',
     #        /, 1X, 'reduce it to a value less than', F6.3 )
   24 FORMAT (   1X, 5X, '    Reservoir segment ', A4 )
   25 FORMAT (   1X, 5X, 'Outflow     Storage     S2/DT+O2/2',
     #        /, 1X, 5X, '-------     -------     ----------',
     #        /,(1X, F9.2, 4X, F9.2, 4X, F9.2 ) )
   28 FORMAT (//, ' *** Maximum number of segments exceeded ',           KF 0389
     #         /, '          maximum =', I5,                             KF 0389
     #         /, '            found =', I6, / )                         KF 0389
   29 FORMAT (//,1X, 'NDX for segment ', A4, ' is more than 10.' )
   30 FORMAT (//, ' *** Maximum number of Puls segments exceeded ',      KF10389
     #         /, '          maximum =   10',                            KF 0389
     #         /, '            found =', I5 )                            KF 0389
   31 FORMAT (//, ' *** Invalid DT =', I5 )                              KF 0389
   33 FORMAT (//,1X, 'Number of time steps (DT) for storm', I3,
     #               ' exceeds', I6 )
   34 FORMAT (//,1X, 'Flood plain and channel ALPHA and M must have ',
     #        /, 1X, 'same flow area for bank full discharge.',          KF 0193
     #        /, 1X, 'Change channel flow to', F10.2 )                   KF 0193
 1000 FORMAT ( I5, 4F5.0, F5.2 )                                         KF 0389
 1001 FORMAT ( A4, 1X, 7A4, 9X, I2, 3F5.0, 1X, I1, 1X, I2, 1X, I2 )      KF 0389
 1002 FORMAT ( 5X, 5F5.0 )                                               KF 0389
 1003 FORMAT ( 5X, I2, 3X, I5 )                                          KF 0389
 1004 FORMAT ( A4 )                                                      KF 0389
 2000 FORMAT (/, 1X, 'Model control:',                                   KF 0389
     #       //, 1X, I7,   ' - number of segments (NSEG)',               KF 0389
     #        /, 1X, I7,   ' - number of rain gages (NRG)',              KF 0389
     #        /, 1X, I7,   ' - number of soil types (NPAR)',             KF 0389
     #        /, 1X, F7.1, ' - routing time interval, minutes (DT)',     KF 0389
     #        /, 1X, F7.2, ' - max impervious retention, inches (IMP)',  KF 0389
     #        /, 1X, F7.3, ' - ratio (RAT)',                             KF 0389
     #        /, 1X, F7.2, ' - alpha adjustment (ALPADJ)',               KF 0389
     #        /, 1X, F7.2, ' - weighting factor (WX)' )                  KF 0389
 2001 FORMAT ( '1',                                                      KF 0389
     #        //,1X, 'Summary of segmentation and options:',             KF 0389
     #        //,1X, 'seg-    upstream                        ',         KF 0389
     #               '            soil output      ',                    KF 0389
     #        /, 1X, 'ment    segments     adjacent segments  ',         KF 0389
     #               'type method type option   dsn',                    KF 0389
     #        /, 1X, '---- -------------- ------------------- ',         KF 0389
     #               '---- ------ ---- ------ -----' )                   KF 0389
 2002 FORMAT (   1X, 8( A4,1X ), I3, I6, I6, I6, I8 )                    KF 0389
 2003 FORMAT ( '1',                                                      KF 0389
     #        //,1X, 'Description of segments:',                         KF 0389
     #        //,1X, 'seg-            length  slope   roughness ',       KF 0389
     #               '                    theissen   ',                  KF 0389
     #        /, 1x, 'ment type ndx   (feet) (ft/ft)  parameter ',       KF 0389
     #               'other parameters  coefficients ',                  KF 0389
     #        /, 1X, '---- ---- ---  ------- -------  --------- ',       KF 0389
     #               '---------------- --------------' )                 KF 0389
 2004 FORMAT (   1X, A4, I4, I5, F9.1, F8.4, E11.3, F8.3, F9.3,          KF 0389
     #           1X, 3F5.2 )                                             KF 0389
 2010 FORMAT (//,1X, 'Invalid soil type for segment.' )                  KF 0194
 2015 FORMAT (//,1X, 'Problem with rain gage(s) specified for segment.') KF 0194
 2020 FORMAT ( /,1X, 'Warning, total of thiessen coefficients is', F6.2) KF 0194
 2025 FORMAT (//,5X, '*********************************************',    KF 0194
     $         /,5X, '*                                           *',    KF 0194
     $         /,5X, '*  Critical errors occured in defining the  *',    KF 0194
     $         /,5X, '*  basin segmentation.  Correct your input  *',    KF 0194
     $         /,5X, '*  file and try again.                      *',    KF 0194
     $         /,5X, '*                                           *',    KF 0194
     $         /,5X, '*********************************************' )   KF 0194
C
C     + + + END SPECIFICATIONS + + +
      READ (INFIL,1000) NSEG, IMP, DT, RAT, ALPADJ, WX                  DT 1089
      IF (NSEG .GT. MXSG) THEN                                           KF 0389
        WRITE (OUTFIL,28) MXSG, NSEG                                    DT 1089
        STOP                                                              P 310
      END IF                                                             KF 0389
      IF (RAT.LT.1.0) RAT=1.0                                             P 320
      IF (ALPADJ.LE.0.0) ALPADJ=1.0                                       P 330
      DTS=DT*60.                                                          P 340
      IDTS=INT(DTS+.001)                                                  P 350
      HEAD1(18)=NSEG                                                      P 360
      HEAD1(19)=IDTS                                                      P 370
      WRITE (OUTFIL,2000) NSEG, NRG, NPAR, DT, IMP, RAT, ALPADJ, WX     DT 1089
C             CHECK FOR VALID DT                                          P 390
      CORFS=CORF*60.                                                      P 400
      ICORF=INT(CORFS+0.001)                                              P 410
      IF (MOD(ICORF,IDTS).NE.0) THEN                                     KF 0389
        WRITE (OUTFIL,31) DT                                            DT 1089
        STOP                                                              P 440
      END IF                                                             KF 0389
      TUFLW = 1                                                          KF 0389
      DTFLW = (DT + .00001) * 60.0                                       KF 0389
      WRITE (OUTFIL,2001)                                               DT 1089
      NO8=0                                                               P 460
      NO9=10                                                              P 470
      CONT = 0                                                           KF 0194
      DO 8 I=1,NSEG                                                       P 480
      I21=I+21                                                            P 490
      READ (INFIL,1001) CSEG, (IUP(I,J), J = 1, 3),                     DT 1089
     #             (ILAT(I,J), J = 1, 4),                               DT 1089
     #              KPSET(I), FLGTH(I), SLOPE(I), FRN(I), IMETH(I),      KF 0389
     #              NDX(I), ITYPE(I)                                     KF 0389
      READ (INFIL,1002) (PARAM(I,J), J = 1, 2), (RCOEF(I,J), J = 1, 3)  DT 1089
      READ (INFIL,1003) IPR(I), DSNS(I)                                 DT 1089
      READ (CSEG,1004) ISEG(I)                                           KF 0389
      READ (CSEG,1004) HEAD1(I21)                                        KF 0389
      IF (ITYPE(I).EQ.15.OR.ITYPE(I).EQ.16) THEN                         KF 0389
        WRITE (OUTFIL,17)                                               DT 1089
        CONT = 1                                                         KF 0194
      ELSE IF (ITYPE(I) .EQ. 5  .OR.  ITYPE(I) .EQ. 6) THEN              KF 0194
C       overland flow plane                                              KF 0194
        IF (KPSET(I) .LT. 1  .OR.  KPSET(I) .GT. NPAR) THEN              KF 0194
C         problem with soil type for this segment                        KF 0194
          WRITE (OUTFIL,2010)                                            KF 0194
        END IF                                                           KF 0194
        IF ((RCOEF(I,3) .GT. 0.0  .AND.  NRG .LT. 3)  .OR.               KF 0194
     $      (RCOEF(I,2) .GT. 0.0  .AND.  NRG .LT. 2)  .OR.               KF 0194
     $      (RCOEF(I,1)+RCOEF(I,2) .LE. 0.0 .AND. NRG .EQ. 2)  .OR.      KF 0194
     $      (RCOEF(I,1) .LE. 0.0  .AND.  NRG .EQ. 1)) THEN               KF 0194
C         invalid rain gage                                              KF 0194
          WRITE (OUTFIL,2015)                                            KF 0194
          CONT = 1                                                       KF 0194
        END IF                                                           KF 0194
        THIES = RCOEF(I,1) + RCOEF(I,2) + RCOEF(I,3)                     KF 0194
        IF (THIES .GT. 1.00001) THEN                                     KF 0194
C         warning, thiessen coefficients sum to more than 1              KF 0194
          WRITE (OUTFIL,2020) THIES                                      KF 0194
        END IF                                                           KF 0194
      END IF                                                             KF 0194
      IF (ITYPE(I).EQ.4) RCOEF(I,1)=RCOEF(I,1)*ALPADJ                     P 540
      IF (ITYPE(I).EQ.8) NO8=NO8+1                                        P 550
      IF (ITYPE(I).EQ.5.AND.KPSET(I).LT.1) KPSET(I)=1                     P 560
      IF (ITYPE(I).EQ.6.AND.KPSET(I).LT.1) KPSET(I)=1                     P 570
      IF (NO8.GT.10) THEN                                                KF 0389
        WRITE (OUTFIL,30) NO8                                           DT 1089
        STOP                                                              P 600
      END IF                                                             KF 0389
      IF (ITYPE(I).EQ.8) IRES(NO8)=I                                      P 610
      IF (ITYPE(I).EQ.9) NO9=NO9+1                                        P 620
      IF (ITYPE(I).EQ.9) IRES(NO9)=I                                      P 630
      IF (NDX(I)) 4,4,5                                                   P 640
    4 NDX(I)=10                                                           P 650
    5 DX(I)=FLGTH(I)/NDX(I)                                               P 660
      IF (NDX(I).GT.10) THEN                                             KF 0389
        WRITE (OUTFIL,29) ISEG(I)                                       DT 1089
        CONT = 1                                                         KF 0194
      END IF                                                             KF 0389
      IF (I.EQ.51) WRITE (OUTFIL,2001)                                  DT 1089
C     WRITE (OUTFIL,27) ISEG(I),(IUP(I,J),J=1,3),(ILAT(I,J),J=1,4),     DT 1089
C    0ITYPE(I),
C    1IMETH(I),IPR(I),NDX(I),FLGTH(I),SLOPE(I),FRN(I),                    P 740
C    2(PARAM(I,J),J=1,2),KPSET(I),(RCOEF(I,J),J=1,3)                      P 750
      WRITE (OUTFIL,2002) ISEG(I), (IUP(I,J), J = 1, 3),                DT 1089
     #               (ILAT(I,J), J = 1, 4),                              KF 0389
     #               ITYPE(I), IMETH(I), KPSET(I), IPR(I), DSNS(I)       KF 0389
      IF (ITYPE(I) .EQ. 4  .AND.  RCOEF(I,3) .GE. 0.01) THEN            1285KF
C       check for consistency, at bank full                              KF 0193
C       Q = alpha1 * A ** m1 = alpha2 * A ** m2                          KF 0193
        A1 = ( RCOEF(I,1) / PARAM(I,1) )                                 KF 0389
     *      ** ( 1.0 / ( PARAM(I,2) - RCOEF(I,2) ) )                    1285KF
        QCHK = ALPADJ * PARAM(I,1) * A1 ** PARAM(I,2)                    KF 0389
        IF (ABS(QCHK-RCOEF(I,3))/QCHK .GT. 0.01) THEN                   1285KF
          WRITE(OUTFIL,34) QCHK                                         DT 1089
          CONT = 1                                                       KF 0194
        END IF                                                          1285KF
      END IF                                                            1285KF
    8 CONTINUE                                                           KF 0389
      WRITE (OUTFIL,2003)                                               DT 1089
      WRITE (OUTFIL,2004) (ISEG(I), ITYPE(I), NDX(I), FLGTH(I),         DT 1089
     #                     SLOPE(I), FRN(I), (PARAM(I,J), J = 1, 2),    DT 1089
     #                    (RCOEF(I,J), J = 1, 3), I = 1, NSEG )         KF 0389
      IF (CONT .EQ. 1) THEN                                              KF 0194
C       critical errors occured, stop processing                         KF 0194
        WRITE (OUTFIL,2025)                                              KF 0194
        STOP                                                             KF 0194
      END IF                                                             KF 0194
      IF (JPERM .GE. 2) THEN                                             KF 0191
C       check that dsns exist
        CALL SEGDSN (NSEG, ISEG, DTFLW, TUFLW, NDATE(1,3), NDATE(1,1),   KF 0389
     I               DSNS, WDMFL, MESSFL,                                KF 0191
     M               JPERM )                                             KF 0191
      END IF                                                             KF 0389
      DELTAT=DT/60.                                                       P 760
      IF (NO8.EQ.0) GO TO 14                                              P 770
C             SET UP FOR MOD-PULS ROUTING                                 P 780
      DO 13 I2=1,NO8                                                      P 790
      K=IRES(I2)                                                          P 800
      DDMIN=DELTAT                                                        P 810
      J=NDX(K)                                                            P 820
      DO 9 II=1,J                                                         P 830
        READ (INFIL,19) O2(K,II), S2(K,II)                                DT 1089
        WV(K,II)=S2(K,II)/DELTAT+O2(K,II)/2.                              P 850
        TEST=WV(K,II)-O2(K,II)                                            P 860
        IF (TEST.LT.0.0) THEN                                             KF 0389
          DDT=S2(K,II)/(O2(K,II)/2.0)                                     P 880
          IF (DDT.LT.DDMIN) DDMIN=DDT                                     P 890
          WRITE (OUTFIL,23) ISEG(K),DDMIN                                KF 1192
        END IF                                                           KF 0389
    9 CONTINUE                                                            P 910
      DO 12 II=2,J                                                        P 920
      IIM=II-1                                                            P 930
      IF (O2(K,II).LE.O2(K,IIM) .OR. S2(K,II).LE.S2(K,IIM)) THEN         KF 0389
        WRITE (OUTFIL,18)                                               DT 1089
        STOP                                                              P 970
      END IF                                                             KF 0389
      S1(K,II)=(O2(K,II)-O2(K,II-1))/(WV(K,II)-WV(K,II-1))               KF 0389
      C1(K,II)=O2(K,II)-S1(K,II)*WV(K,II)                                 P 990
      S(K,II)=(O2(K,II)-O2(K,II-1))/(S2(K,II)-S2(K,II-1))                 P1000
      C(K,II)=O2(K,II)-S(K,II)*S2(K,II)                                   P1010
   12 CONTINUE                                                            P1020
      WRITE (OUTFIL,24) ISEG(K)                                         DT 1089
      WRITE (OUTFIL,25) (O2(K,II),S2(K,II),WV(K,II),II=1,J)             DT 1089
   13 CONTINUE                                                            P1050
   14 IF (NRG .LT. 3) THEN                                               KF 0389
C       SET THEISSEN COEFFICIENTS FOR UNUSED RAIN GAGES TO ZERO           P1070
        NRG1=NRG+1                                                        P1080
        DO 15 J=NRG1,3                                                    P1090
        DO 15 K=1,NSEG                                                    P1100
        IF (ITYPE(K) .NE. 4) RCOEF(K,J) = 0.0                            KF 0389
   15   CONTINUE                                                          P1130
      END IF                                                             KF 0389
C                                                                       0184 KF
C-    - TAKEN FROM OLD SUBROUTINE INPUT2                                0184 KF
C                                                                       0184 KF
      NRC = 0                                                           0184 KF
      KNN = 0                                                           0184 KF
      INTVAL = ( PTIME + .001 )  /  DT                                  0184 KF
      IFLAG = 0                                                         1185KF
      DO 50 I = 1, NOFE                                                 0184 KF
         JRCS = ITEST(I) * INTVAL                                       0184 KF
         IF (JRCS .GE. NDTS) THEN                                       1185KF
C          DT TOO SMALL FOR PTIME                                       1185KF
           WRITE(OUTFIL,33) I, NDTS                                     0390 KF
           IFLAG = IFLAG + 1                                            1185KF
         END IF                                                         1185KF
         JRCS = JRCS / 120  +  1  -  ( 1 - MIN0( 1, MOD(JRCS,120) ) )   0184 KF
         ITEST(I) = JRCS * NSEG                                         0184 KF
         IF(IPL(I) .EQ. 0  .AND.  KOUT(I) .EQ. 0) IPL(I) = 0            0184 KF
         IF(KOUT(I) .NE. 0)                                             0184 KF
     *     THEN                                                         0184 KF
             IF(JPERM .EQ. 1 .OR. JPTG .GT. 0) NRC = NRC + ITEST(I)     0184 KF
             IF(JPERM .EQ. 0  .AND.  JPTG .EQ. 0  .AND.                 1184 KF
     *          NRC .LT. ITEST(I)) NRC = ITEST(I)                       1184KF
           ENDIF                                                        0184 KF
         IF(TESTNO(I) .EQ. 1) KNN = KNN + 1                             0184 KF
   50    CONTINUE                                                       0184 KF
      IF (IFLAG .GT. 0) THEN                                            1185KF
C       BAD TIME STEP FOR STORM(S)                                      1185KF
        STOP                                                            **STOP**
      END IF                                                            1185KF
      NRCP = 0                                                          1184 KF
      IF(JPERM .EQ. 1  .OR.  JPTG .GT. 0) NRCP = 3                      1184 KF
      NRC = NRC  +  NRCP                                                1184 KF
      JRECDS = NRC                                                      0184 KF
C     WRITE(OUTFIL,32) JRECDS, ( I, ITEST(I), I = 1, NOFE )             DT 1089
      RETURN                                                              P1150
      END                                                                 P1410-
C
C
C
      SUBROUTINE   SEGDSN                                                KF 0389
     I                   ( NDSN, ISEG, TSSTEP, TUNITS, YR, MO,           KF 0389
     I                     DSNS, WDMFL, MESSFL,                          KF 0191
     M                     JPERM )                                       KF 0191
C
C     + + + PURPOSE + + +
C     This routine checks that the data sets in DSNS exist.  If they do
C     not exist, they are added to the WDM file.  New data sets have the
C     following attributes:  Compressed, mean, constant time step, with
C     month group pointers.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NDSN, TSSTEP, TUNITS, YR, MO,
     >          DSNS(NDSN), WDMFL, MESSFL, JPERM                         KF 0191
      CHARACTER*4 ISEG(NDSN)                                             KF 0191
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NDSN   - number of data sets to be processed
C     ISEG   - character array containing a 4-character descriptor
C     TSSTEP - time step of data set, in TUNITS units
C     TUNITS - time units of data set
C     YR     - base year
C     MO     - base month
C     DSNS   - array of segment data-set numbers
C              0 - segment not output to WDM file
C              >0 - data set number
C     WDMFL  - Fortran unit number of WDM file
C     MESSFL - Fortran unit number of ANNIE message file
C     JPERM  - indicator flag of method of storing segment Q
C
C     + + + PARAMETERS + + +
      INCLUDE 'pinout.inc'                                               KF 0290
      INCLUDE 'plimt.inc'
C
      INTEGER   NA
      PARAMETER ( NA = 9 )
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   N, EXIST, DSTYPE, NDN, NUP, NSASP, NDP, PSA,            RSR1093
     #          AV(NA), AI(NA), RET(NA), FLAG, I, LI, L1, L4, NSA
      CHARACTER*1 TSTYPE(4)
      CHARACTER*6 UN(6)
C
C     + + + FUNCTIONS + + +
      INTEGER   WDCKDT                                                   KF 0691
C
C     + + + INTRINSICS + + +
      INTRINSIC  MOD                                                     KF 1192
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDCKDT, WDLBAX, WDBSAI, WDBSAC                          KF 0691
C
C     + + + DATA INITIALIZATIONS + + +
C                vb-         ts-                ts-  ts- ts- ts-
C                time compfg form tgroup tunits step byr bmo type
      DATA  AI /  85,    83,  84,    34,    17,  33, 27, 28,   1 /,
     #      AV /   1,     1,   3,     5,     0,   0,  0,  0,   0 /
      DATA  LI, L1, L4, NDN, NUP, NSA, NSASP, NDP, DSTYPE
     #     / 8,  1,  4,   0,   0,  10,    10, 240,      1 /
      DATA  UN  / 'second', 'minute', 'hour  ',
     #            'day   ', 'month ', 'year  ' /
C
C     + + + FORMATS + + +
 1000 FORMAT ( 4A1 )
 2001 FORMAT ( '1',
     #        //,1X, 'Added the following data sets to the WDM file:',
     #        //,1X, ' segment                         base ',
     #        /, 1X, '---------                      -------',
     #        /, 1X, 'tstype no   dsn    time step    yr  mo',
     #        /, 1X, '------ --  -----  -----------  ---- --' )
 2002 FORMAT (   1X, 1X,4A1, I4, I7, I6, 1X, A6, I6, I3 )
 2003 FORMAT ( /,1X, '**** A dsn is required for all segments when ',    KF 1192
     >               'JPERM=3, dsn=0 for segment', I3, 1X, A4 )          KF 1192
 2004 FORMAT ( /,1X, '**** Data set number', I5, ' is not a ',           KF 0691
     >               'time series data set.' )                           KF 0691
 2005 FORMAT ( /,1X, '**** There may be a problem writing to the wdm ',  RSR1093
     $               'file at the time step ', I4, 1X, A6 )              KF 1192
C
C     + + + END SPECIFICATIONS + + +
      FLAG = 0
C     set time step and units                                            KF 1192
      IF (TUNITS .EQ. 1  .AND.  MOD(TSSTEP,60) .EQ. 0) THEN              KF 1192
C       convert second time step to minutes time step                    KF 1192
        AV(5) = 2                                                        KF 1192
        AV(6) = TSSTEP / 60                                              KF 1192
      ELSE                                                               KF 1192
C       use given time step and units                                    KF 1192
        AV(5) = TUNITS                                                   KF 1192
        AV(6) = TSSTEP                                                   KF 1192
        IF (TUNITS .EQ. 1  .AND.  TSSTEP .GT. 60) THEN                   KF 1192
C         may be problem writing to wdm file                             KF 1192
          WRITE (OUTFIL,2005) TSSTEP, UN(TUNITS)                         KF 1192
        END IF                                                           KF 1192
      END IF                                                             KF 1192
C     set base year and month                                            KF 1192
      AV(7) = YR + 1900
      AV(8) = MO
      DO 190 N = 1, NDSN
        EXIST = -1                                                       KF 0191
        IF (DSNS(N) .LE. 0  .AND.  JPERM .EQ. 3) THEN                    KF 0191
C         dsn required for all segments when JPERM = 3                   KF 0191
          WRITE (OUTFIL,2003) N, TSTYPE                                  KF 0191
          JPERM = 2                                                      KF 0191
          EXIST = -1                                                     KF 0191
        ELSE IF (DSNS(N) .GT. 0) THEN                                    KF 0191
          EXIST = WDCKDT ( WDMFL, DSNS(N) )                              KF 0691
        END IF                                                           KF 0191
        IF (EXIST .EQ. 0) THEN
C         data set does not exist, add it
          CALL WDLBAX ( WDMFL, DSNS(N), DSTYPE, NDN, NUP, NSA, NSASP,
     #                    NDP, PSA )
          DO 110 I = 1, LI
C           add integer attributes
            CALL WDBSAI ( WDMFL, DSNS(N), MESSFL, AI(I), L1, AV(I),
     O                          RET(I) )
 110      CONTINUE
C         add tstype
          READ (ISEG(N),1000) TSTYPE
          CALL WDBSAC ( WDMFL, DSNS(N), MESSFL, AI(LI+1), L4, TSTYPE,
     O                  RET(LI+1) )
          IF (FLAG .EQ. 0) THEN
C           first data set, write heading
            WRITE (OUTFIL,2001)                                         DT 1089
            FLAG = 1
          END IF
          WRITE (OUTFIL,2002) TSTYPE, N, DSNS(N), TSSTEP,               DT 1089
     #                        UN(TUNITS), AV(7), MO                      KF 1192
        ELSE IF (EXIST .NE. 1) THEN                                      KF 1293
C         data set exists, but wrong data type                           KF 0691
          WRITE (OUTFIL,2004) DSNS(N)                                    KF 1192
        END IF
 190  CONTINUE
C
      RETURN
      END
