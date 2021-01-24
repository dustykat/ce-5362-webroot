C
C
C
      SUBROUTINE   PUNCH
     M                ( LL,
     I                  NDATE,NDELS,CORF,PTIME,JPUN,I1,ICNT)
C
C     + + + PURPOSE + + +
C     Produces card image output file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   I1, ICNT, JPUN, LL, NDATE(60,3), NDELS                   KF 0389
      REAL      CORF, PTIME                                              KF 0389
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LL     -
C     NDATE  -
C     NDELS  -
C     CORF   -
C     PTIME  -
C     JPUN   -
C     I1     -
C     ICNT   -
C
C     + + + PARAMETERS + + +
      INCLUDE 'PINOUT.INC'                                               KF 0290
      INCLUDE 'PLIMT.INC'                                                KF 0389
C
C     + + + COMMONS + + +
      INCLUDE 'CC2.INC'                                                  KF 0389
      INCLUDE 'CUNIT.INC'                                                KF 0389
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, I12, ICODE, IDAYS(12), IDY, IMO, IP, IPUN,            KF 0389
     #          IYR, J, K, NCN                                           KF 0389
      REAL      X2(12)                                                   KF 0389
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD                                                    KF 0389
C
C     + + + DATA INITIALIZATIONS + + +
      DATA IDAYS/31,28,31,30,31,30,31,31,30,31,30,31/                    AC  60
C
C     + + + FORMATS + + +
    9 FORMAT ( A8,4I2,I3,12F5.1,I1)                                      KF 0389
   10 FORMAT ( A8,5I2,12F5.1,I2)                                         KF 0389
C
C     + + + END SPECIFICATIONS + + +
      NCN=120.1/PTIME                                                    AC  70
      IPUN=0                                                             AC  80
      IP=PTIME                                                           AC  90
      ICODE=2                                                            AC 100
      GO TO 2                                                            AC 110
    1 LL=LL-NDELS                                                        AC 120
    2 IF (LL.GT.NDELS) GO TO 1                                           AC 130
      IMO=NDATE(I1,1)                                                    AC 140
      IDY=NDATE(I1,2)                                                    AC 150
      IYR=NDATE(I1,3)                                                    AC 160
    3 CONTINUE                                                           AC 170
      DO 6 I=1,NCN                                                       AC 180
      IF (IPUN.EQ.ICNT) GO TO 8                                          AC 190
      I12=12*I                                                           AC 200
      IF (LL.GT.I12) GO TO 6                                             AC 210
      I12=I12-12                                                         AC 220
      DO 4 J=1,12                                                        AC 230
      X2(J)=0.0                                                          AC 240
      IF (IPUN.EQ.ICNT) GO TO 4                                          AC 250
      I12=I12+1                                                          AC 260
      IF (LL.GT.I12) GO TO 4                                             AC 270
      IPUN=IPUN+1                                                        AC 280
      X2(J)=R(IPUN)                                                      AC 290
    4 CONTINUE                                                           AC 300
      IF (CORF.GE.5.0) GO TO 5                                           AC 310
      WRITE (JPUN,9) STA(1)(1:8),IYR,IMO,IDY,IP,I,(X2(K),K=1,12),ICODE   KF 0389
      GO TO 6                                                            AC 330
    5 WRITE (JPUN,10) STA(1)(1:8),IYR,IMO,IDY,IP,I,(X2(K),K=1,12),ICODE  KF 0389
    6 CONTINUE                                                           AC 350
      LL=0                                                               AC 360
      IDY=IDY+1                                                          AC 370
      IF (IDAYS(IMO).GE.IDY) GO TO 3                                     AC 380
      IF (IMO.NE.2) GO TO 7                                              AC 390
      IF (MOD(IYR,4).NE.0) GO TO 7                                       AC 400
      IF (IDY.LE.29) GO TO 3                                             AC 410
    7 IMO=IMO+1                                                          AC 420
      IDY=1                                                              AC 430
      IF (IMO.LE.12) GO TO 3                                             AC 440
      IMO=1                                                              AC 450
      IYR=IYR+1                                                          AC 460
      GO TO 3                                                            AC 470
    8 CONTINUE                                                           AC 480
      RETURN                                                             AC 490
      END                                                                AC 530-
