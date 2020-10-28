      SUBROUTINE PLT(Q,R,ICNT,IEND,YMAX)                                00027560
C             THIS SUBROUTINE SETS UP FOR LINE PRINTER PLOTTING         00027570
      INCLUDE 'PINOUT.INC'                                               KF 0290
      DIMENSION Q(ICNT),R(ICNT)                                         00027580
      INTEGER NSCALE(5)                                                 XXXXXXXX
      INTEGER TRYCT                                                     00027590
      LOGICAL KPLOT1,KPLOT2,KPLOT                                       XXXXXXXX
      COMMON /C8/ I1,IK,TRYCT,KOUT(150),IHYD(150),PTIME,ND,OUTVOL(60)   00027600
C     LOGICAL*1 IMAGE(5200)                                             00027610
C ***** CHANGE FOR THE HARRIS *****                                     XXXXXXXX
      CHARACTER*1 IMAGE(5200),CH                                        XXXXXXXX
      CHARACTER*12 LABEL,LABELX                                         XXXXXXXX
      INTEGER FILE                                                      XXXXXXXX
      COMMON /PLOT1/ NSCALE,NHL,NSBH,NVL,NSBV                           XXXXXXXX
      COMMON /PLOTI/ IMAGE                                              XXXXXXXX
      COMMON /PLOT2/       XMAX,XMIN,     YMIN,FILE                     XXXXXXXX
      COMMON /PLOT3/ CH                                                 XXXXXXXX
      COMMON /PLOT4/ NL,LABEL,LABELX                                    XXXXXXXX
      COMMON /KP/ KPLOT1,KPLOT2,KPLOT                                   XXXXXXXX
      KPLOT1 = .FALSE.                                                  XXXXXXXX
      KPLOT2 = .FALSE.                                                  XXXXXXXX
      KPLOT = .FALSE.                                                   XXXXXXXX
      FILE = 6                                                          XXXXXXXX
      YMIN = 0.0                                                        XXXXXXXX
      NL = 11                                                           XXXXXXXX
C ***** CHANGE FOR THE HARRIS *****                                     XXXXXXXX
      GO TO (10,20,30),IEND                                             00027620
 10   IX=Q(1)                                                           00027630
      IY=Q(ICNT)                                                        00027640
      XMIN=IX                                                           00027650
      XMAX=IY+1                                                         00027660
      DIV=10.                                                           00027670
      IF(YMAX.LT.10.) DIV=0.1                                           00027680
      IF(YMAX.LT.0.1) DIV=0.01                                          00027690
      IF(YMAX.LT.0.01)  DIV=0.001                                       00027700
      AJ=YMAX/DIV                                                       00027710
      IAJ=AJ                                                            00027720
      YMAX=(IAJ+1.)*DIV                                                 00027730
      WRITE(OUTFIL,197)                                                 DT 1089
C             FOR FINAL PLOTS                                           00027750
      IF(PTIME.LT.70.) GO TO 15                                         00027760
      XMIN=0.0                                                          00027770
      XMAX=YMAX                                                         00027780
      GO TO 18                                                          00027790
15    WRITE(OUTFIL,76) I1                                               DT 1089
C18   CALL PLOT2(IMAGE,XMAX,XMIN,YMAX,0.0,6)                            00027810
 18   CALL PRPLOT(2,Q,R,ICNT,YMAX)                                      XXXXXXXX
C     CALL PLOT3(1HC,Q,R,ICNT)                                          00027820
      CH = 'C'                                                          XXXXXXXX
      CALL PRPLOT(3,Q,R,ICNT,YMAX)                                      XXXXXXXX
C     IF(PTIME.GT.70.) CALL PLOT4(2,2H  )                               00027830
      LABEL = '            '                                            XXXXXXXX
      LABELX = '            '                                           XXXXXXXX
      IF(PTIME.GT.70.) CALL PRPLOT(4,Q,R,ICNT,YMAX)                     XXXXXXXX
      RETURN                                                            00027840
C20   CALL PLOT3(1H0,Q,R,ICNT)                                          00027850
 20   CH = '0'                                                          XXXXXXXX
      CALL PRPLOT(3,Q,R,ICNT,YMAX)                                      XXXXXXXX
      RETURN                                                            00027860
 30   CONTINUE                                                          00027870
C     CALL PLOT4(11,11HFLOW IN CFS)                                     00027880
      LABEL = 'FLOW (CFS)  '                                            XXXXXXXX
      LABELX = 'TIME (HRS)  '                                           XXXXXXXX
      CALL PRPLOT(4,Q,R,ICNT,YMAX)                                      XXXXXXXX
 76   FORMAT(30X,15H** STORM NUMBER,I3)                                 00027890
 197  FORMAT(1H1)                                                       00027900
      RETURN                                                            00027910
      END                                                               00027920
C
C
C
C     SUBROUTINE PRPLOT                                                 00027930
      SUBROUTINE PRPLOT(INTRY,X,Y,N3,YMAX)                              XXXXXXXX
      INCLUDE 'PINOUT.INC'                                               KF 0290
C     IMPLICIT LOGICAL*1(W),LOGICAL*1(K)                                00027940
      LOGICAL   KABSC,KBOTGL,KNHOR,KORD,KPLOT,KPLOT1,KPLOT2             XXXXXXXX
      DIMENSION NSCALE(5), ABNOS(26), X(N3), Y(N3)                      1184 KF
C     LOGICAL*1 NOS(10)/'0','1','2','3','4','5','6','7','8','9'/        00027960
C     LOGICAL*1 IMAGE(1),CH,LABEL(1),ERR1,ERR3,ERR5                     00027970
      CHARACTER*1 IMAGE(5200),CH                                        XXXXXXXX
      CHARACTER*12 LABEL,WL,BL,LABELX                                   XXXXXXXX
C     LOGICAL*1 VC,HC,FOR1(19),FOR2(15),FOR3(19),NC,BL,HF,HF1           00027980
      CHARACTER*1 VC,HC,NC,HF,HF1                                       XXXXXXXX
C     REAL*8 FOX1(3),FOX2(2),FOX3(3)                                    00027990
C     INTEGER*2 VCR                                                     00028000
C     EQUIVALENCE (FOR1(1),FOX1(1)),(FOR2(1),FOX2(1)),(FOR3(1),FOX3(1)),00028010
C    1,(VC,VCR)                                                         00028020
      INTEGER FILE                                                      00028030
      COMMON /PLOT1/ NSCALE,NHL,NSBH,NVL,NSBV                           XXXXXXXX
      COMMON /PLOTI/ IMAGE                                              XXXXXXXX
      COMMON /PLOT2/       XMAX,XMIN,  YMIN,IFL                         XXXXXXXX
      COMMON /PLOT3/ CH                                                 XXXXXXXX
      COMMON /PLOT4/ NL,LABEL,LABELX                                    XXXXXXXX
      COMMON /KP/ KPLOT1,KPLOT2,KPLOT                                   XXXXXXXX
C     DATA HC/'-'/,NC/'+'/,BL/' '/,HF/'F'/,HF1/'.'/                     00028040
      DATA HC/'-'/,NC/'+'/,BL/'            '/                           XXXXXXXX
C     DATA FOX1/'(1XA1,F9','.2,  121','A1)     '/                       00028050
C     DATA FOX2/'(1XA1, 9','X121A1) '/                                  00028060
C     DATA FOX3/'(1H0F  .',' ,  F   ','. )     '/                       00028070
C     DATA VCR/Z4F00/                                                   00028080
      DATA VC/'!'/                                                      XXXXXXXX
C     DATA KPLOT1/.FALSE./,KPLOT2/.FALSE./                              00028090
      DATA KABSC,KORD,KBOTGL/3*.FALSE./                                 00028100
C                                                                       00028110
C     ENTRY PLOT1(NSCALE,NHL,NSBH,NVL,NSBV)                             00028120
      GO TO (101,102,103,104) INTRY                                     XXXXXXXX
  101 CONTINUE                                                          XXXXXXXX
C     IFL=FILE                                                          00028130
C     ERR1=.FALSE.                                                      00028140
C     ERR3=.FALSE.                                                      00028150
C     ERR5=.FALSE.                                                      00028160
C     KPLOT1=.TRUE.                                                     00028170
C     KPLOT2=.FALSE.                                                    00028180
C     NH=IABS(NHL)                                                      00028190
C     NSH=IABS(NSBH)                                                    00028200
C     NV=IABS(NVL)                                                      00028210
C     NSV=IABS(NSBV)                                                    00028220
C     NSCL=NSCALE(1)                                                    00028230
C     IZ = 0
C     II = NH*NSH*NV*NSV
C     IF (II.NE.IZ) GO TO 1                                             00028240
C     KPLOT=.FALSE.                                                     00028250
C     ERR1=.TRUE.                                                       00028260
C     RETURN                                                            00028270
C ----------------------------------------------------------------------XXXXXXXX
    1 KPLOT=.TRUE.                                                      00028280
C     WRITE(OUTFIL,701)                                                 DT 1089
C     IF (NV.LE.25) GO TO 2                                             00028290
C     KPLOT=.FALSE.                                                     00028300
C     ERR3=.TRUE.                                                       00028310
C     RETURN                                                            00028320
    2 CONTINUE                                                          00028330
      NVM=NV-1                                                          00028340
      NVP=NV+1                                                          00028350
      NDH=NH*NSH                                                        00028360
      NDHP=NDH+1                                                        00028370
      NDV=NV*NSV                                                        00028380
      NDVP=NDV+1                                                        00028390
      NIMG=(NDHP*NDVP)                                                  00028400
      IF (NDV.LE.120) GO TO 3                                           00028410
      KPLOT=.FALSE.                                                     00028420
C     ERR5=.TRUE.                                                       00028430
      RETURN                                                            00028440
    3 CONTINUE                                                          00028450
      IF (NSCL.EQ.0) GO TO 4                                            00028460
      FSY=10.**NSCALE(2)                                                00028470
      FSX=10.**NSCALE(4)                                                00028480
      IY=MIN0(IABS(NSCALE(3)),7)+1                                      00028490
      IX=MIN0(IABS(NSCALE(5)),9)+1                                      00028500
      GO TO 5                                                           00028510
    4 FSY=1.                                                            00028520
      FSX=1.                                                            00028530
      IY=4                                                              00028540
      IX=4                                                              00028550
    5 CONTINUE                                                          XXXXXXXX
C   5 FOR1(10)=NOS(IY)                                                  00028560
C     NA=MIN0(IX,NSV)-1                                                 00028570
C     NS=NA-MIN0(NA,120-NDV)                                            00028580
C     NB=11-NS+NA                                                       00028590
C     I1=NB/10                                                          00028600
C     I2=NB-I1*10                                                       00028610
C     FOR3(6)=NOS(I1+1)                                                 00028620
C     FOR3(7)=NOS(I2+1)                                                 00028630
C     FOR3(9)=NOS(NA+1)                                                 00028640
C     IF (NV.GT.0) GO TO 7                                              00028650
C     DO 6 J=11,18                                                      00028660
C   6 FOR3(J)=BL                                                        00028670
C     GO TO 8                                                           00028680
C   7 I1=NV/10                                                          00028690
C     I2=NV-I1*10                                                       00028700
C     FOR3(11)=NOS(I1+1)                                                00028710
C     FOR3(12)=NOS(I2+1)                                                00028720
C     FOR3(13)=HF                                                       00028730
C     I1=NSV/100                                                        00028740
C     I3=NSV-I1*100                                                     00028750
C     I2=I3/10                                                          00028760
C     I3=I3-I2*10                                                       00028770
C     FOR3(14)=NOS(I1+1)                                                00028780
C     FOR3(15)=NOS(I2+1)                                                00028790
C     FOR3(16)=NOS(I3+1)                                                00028800
C     FOR3(17)=HF1                                                      00028810
C     FOR3(18)=FOR3(9)                                                  00028820
    8 IF (KPLOT1) RETURN                                                00028830
      KPLOT1=.TRUE.                                                     00028840
C                                                                       00028850
C     ENTRY PLOT2(IMAGE,XMAX,XMIN,YMAX,YMIN,FILE)                       00028860
  102 CONTINUE                                                          XXXXXXXX
C     WRITE(OUTFIL,702)                                                 DT 1089
C     IFL=FILE                                                          00028870
      KPLOT2=.TRUE.                                                     00028880
      IF (KPLOT1) GO TO 9                                               00028890
      NSCL=0                                                            00028900
      NH=5                                                              00028910
      NSH=10                                                            00028920
      NV=10                                                             00028930
      NSV=10                                                            00028940
      GO TO 1                                                           00028950
    9 CONTINUE                                                          00028960
C     IF (KPLOT) GO TO 10                                               00028970
C     IF (ERR1) WRITE (IFL,30)                                          00028980
C     IF (ERR3) WRITE (IFL,31)                                          00028990
C     IF (ERR5) WRITE (IFL,32)                                          00029000
C     RETURN                                                            00029010
   10 YMX=YMAX                                                          00029020
      DH=(YMAX-YMIN)/FLOAT(NDH)                                         00029030
      DV=(XMAX-XMIN)/FLOAT(NDV)                                         00029040
      DO 11 I=1,NVP                                                     00029050
   11 ABNOS(I)=(XMIN+FLOAT((I-1)*NSV)*DV)*FSX                           00029060
      DO 12 I=1,NIMG                                                    00029070
   12 IMAGE(I)=BL                                                       00029080
      DO 16 I=1,NDHP                                                    00029090
      I2=I*NDVP                                                         00029100
      I1=I2-NDV                                                         00029110
C     KNHOR=MOD(I-1,NSH).NE.0                                           00029120
C     IF (KNHOR) GO TO 14                                               00029130
      IF (MOD(I-1,NSH).NE.0) GO TO 14                                   XXXXXXXX
      DO 13 J=I1,I2                                                     00029140
   13 IMAGE(J)=HC                                                       00029150
   14 CONTINUE                                                          00029160
      DO 16 J=I1,I2,NSV                                                 00029170
C     IF (KNHOR) GO TO 15                                               00029180
      IF (MOD(I-1,NSH).NE.0) GO TO 15                                   XXXXXXXX
      IMAGE(J)=NC                                                       00029190
      GO TO 16                                                          00029200
   15 IMAGE(J)=VC                                                       00029210
   16 CONTINUE                                                          00029220
      XMIN1=XMIN-DV/2.                                                  00029230
      YMIN1=YMIN-DH/2.                                                  00029240
      RETURN                                                            00029250
C                                                                       00029260
C     ENTRY PLOT3(CH,X,Y,N3)                                            00029270
 103  CONTINUE                                                          XXXXXXXX
C     WRITE(OUTFIL,703)                                                 DT 1089
C     IF (KPLOT2) GO TO 18                                              00029280
C  17 WRITE (IFL,33)                                                    00029290
C  18 CONTINUE                                                          00029300
C     IF (.NOT.KPLOT) RETURN                                            00029310
C     IF (N3.GT.0) GO TO 19                                             00029320
C     KPLOT=.FALSE.                                                     00029330
C     WRITE (IFL,34)                                                    00029340
C     RETURN                                                            00029350
   19 DO 26 I=1,N3                                                      00029360
      IF (DV) 21,20,21                                                  00029370
   20 DUM1=0                                                            00029380
      GO TO 22                                                          00029390
   21 CONTINUE                                                          00029400
      DUM1=(X(I)-XMIN1)/DV                                              00029410
   22 IF (DH) 24,23,24                                                  00029420
   23 DUM2=0                                                            00029430
      GO TO 25                                                          00029440
   24 CONTINUE                                                          00029450
      DUM2=(Y(I)-YMIN1)/DH                                              00029460
   25 CONTINUE                                                          00029470
      IF (DUM1.LT.0..OR.DUM2.LT.0.) GO TO 26                            00029480
      IF (DUM1.GE.NDVP.OR.DUM2.GE.NDHP) GO TO 26                        00029490
      NX=1+INT(DUM1)                                                    00029500
      NY=1+INT(DUM2)                                                    00029510
      J=(NDHP-NY)*NDVP+NX                                               00029520
      IMAGE(J)=CH                                                       00029530
   26 CONTINUE                                                          00029540
      RETURN                                                            00029550
C                                                                       00029560
C     ENTRY PLOT4(NL,LABEL)                                             00029570
C     ENTRY FPLOT4(NL,LABEL)                                            00029580
 104  CONTINUE                                                          XXXXXXXX
C     WRITE(OUTFIL,704)                                                 DT 1089
C     IF (.NOT.KPLOT) RETURN                                            00029590
C     IF (.NOT.KPLOT2) GO TO 17                                         00029600
C     WRITE(OUTFIL,705) NDHP                                            DT 1089
      DO 28 I=1,NDHP                                                    00029610
      IF (I.EQ.NDHP.AND.KBOTGL) GO TO 28                                00029620
      WL=BL                                                             00029630
C     IF (I.LE.NL) WL=LABEL                                             00029640
      IF (I.EQ.NDHP/2) WL = LABEL                                       XXXXXXXX
      I2=I*NDVP                                                         00029650
      I1=I2-NDV                                                         00029660
      IF (MOD(I-1,NSH).EQ.0.AND..NOT.KORD) GO TO 27                     00029670
C     WRITE (IFL,FOR2) WL,(IMAGE(J),J=I1,I2)                            00029680
      WRITE (IFL,602)  WL,(IMAGE(J),J=I1,I2)                            XXXXXXXX
      GO TO 28                                                          00029690
   27 CONTINUE                                                          00029700
      ORDNO=(YMX-FLOAT(I-1)*DH)*FSY                                     00029710
      IF (I.EQ.NDHP) ORDNO=YMIN                                         00029720
C     WRITE (IFL,FOR1) WL,ORDNO,(IMAGE(J),J=I1,I2)                      00029730
      WRITE (IFL,601)  WL,ORDNO,(IMAGE(J),J=I1,I2)                      XXXXXXXX
   28 CONTINUE                                                          00029740
      IF (KABSC) GO TO 29                                               00029750
C     WRITE (IFL,FOR3) (ABNOS(J),J=1,NVP)                               00029760
      WRITE (IFL,603)  (ABNOS(J),J=1,NVP)                               XXXXXXXX
      WRITE(IFL,604) LABELX                                             XXXXXXXX
   29 RETURN                                                            00029770
C                                                                       00029780
C     ENTRY OMIT(LSW)                                                   00029790
C     KABSC=MOD(LSW,2).EQ.1                                             00029800
C     KORD=MOD(LSW,4).GE.2                                              00029810
C     KBOTGL=LSW.GE.4                                                   00029820
C     RETURN                                                            00029830
C                                                                       00029840
  601 FORMAT(1X,A1,F9.2,121A1)                                          XXXXXXXX
  602 FORMAT(1X,A10,121A1)                                              XXXXXXXX
  603 FORMAT(1H0,1X,11F10.2)                                            XXXXXXXX
  604 FORMAT(56X,A12)                                                   XXXXXXXX
  701 FORMAT('  ENTRY POINT 1')                                         XXXXXXXX
  702 FORMAT('  ENTRY POINT 2')                                         XXXXXXXX
  703 FORMAT('  ENTRY POINT 3')                                         XXXXXXXX
  704 FORMAT('  ENTRY POINT 4')                                         XXXXXXXX
  705 FORMAT('  BEGIN LOOP, NDHP =',I5)                                 XXXXXXXX
C                                                                       00029860
   30 FORMAT (T5,'SOME PLOT1 ARG. ILLEGALLY 0')                         00029870
   31 FORMAT (T5,'NO. OF VERTICAL LINES >25')                           00029880
   32 FORMAT (T5,'WIDTH OF GRAPH >121')                                 00029890
   33 FORMAT (T5,'PLOT2 MUST BE CALLED')                                00029900
   34 FORMAT (T5,'PLOT3, ARG2 > 0')                                     00029910
      END                                                               00029920
