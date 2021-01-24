$STORAGE:2

C

C       PROGRAM DMH21

C..REVISED VERSION 10/28/1987

C

	CHARACTER IOPT*1,IFILE*15,IOFILE*15

        COMMON/BLK 1/FP(250,8),FC(250,6)

        COMMON/BLK 2/KIN(10),H(10,15,2),KOUT(10),HOUT(10,15,3)

        COMMON/BLK 3/NOSTA(10),STA(10,15,2),NODFX(50)

        COMMON/BLK 4/DMAX(250,2),TIMEX(250,2)

        COMMON/BLK 5/KINP(10),HP(10,15,2)

        COMMON/BLK 6/NODC,NCHI,NCHO,NPCHI,NPCHO,NSTA,NPSTA

        COMMON/BLK 7/DTOL,DTOLP,NFLUX,KFLUX,CHECKD,ITER

        DIMENSION NODDC(50),VEL(250,4),R(10,2),Q(4)

        DATA NR/1/,NW/2/,IW/0/

C

C       DEFINITIONS

C

C       FLOODPLAIN INFORMATION:

C

C       FP(I,J)=N,E,S,W,MANNINGS,ELEV.,INITIAL DEPTH,TEMPORARY MEMORY

C       Q(I)=FLOWRATE PER UNIT WIDTH OF FLOW

C       R(I,1)=TIME COORDINATE FOR EFFECTIVE RAINFALL INTENSITY IN HOUR

C       R(I,2)=EFFECTIVE RAINFALL INTENSITY(IN/HR)

C       KINP(I)=INFLOW NODAL POINTS

C       HP(I,J,K)=INFLOW HYDROGRAPH FOR NODE I

C       DMAX(I,J)=MAXIMUM WATER DEPTH

C       TIMEX(I,J)=TIME CORRESPONDS TO MAXIMUM WATER DEPTH

C       NODDC(I)=CRITICAL DEPTH OUTFLOW NODES

C       VEL(I,J)=N-,E-,S-,AND W-EFFLUX VELOCITIES

C

C.......OPEN INPUT AND OUTPUT FILES

        WRITE(IW,1005)

        READ(IW,1004)IFILE

        OPEN(UNIT=NR,FILE=IFILE,STATUS='OLD')

 1  	WRITE(IW,1001) 

	READ(IW,1002)IOPT

	IF(IOPT .EQ. '1') GO TO 2

	IF(IOPT .EQ. '2') GO TO 3

	GO TO 1

 2      OPEN (UNIT=NW,FILE='PRN',STATUS='NEW')

	GO TO 4

 3	WRITE(IW,1003)

	READ(IW,1004)IOFILE

	OPEN (UNIT=NW,FILE=IOFILE,STATUS='NEW')

 4      CONTINUE

C

C       DATA INPUT

C



C.......READ PROGRAM CONTROL DATA

        READ (NR,*)DTMIN,DTMAX,DTI,DTD,SIMUL,ITER,TOUT,KODE,KMODEL

        READ (NR,*)NNOD,NODC,SIDE,TOL,DTOL,DTOLP

C.......INPUT FLOODPALIN INFORMATION

        READ (NR,*)((FP(I,J),J=1,7),I=1,NNOD)

C.......READ EFFECTIVE RAINFALL INTENSITY (LINEAR FUNCTION)

        READ (NR,*)NERI

        IF(NERI.GE.1)READ (NR,*)((R(I,J),J=1,2),I=1,NERI)

C.......READ INFLOW HYDROGRAPHS (LINEAR FUNCTION)

        READ (NR,*)NFPI,NPFPI

        IF(NFPI.LT.1)GOTO 10

        DO 20 I=1,NFPI

        READ (NR,*)KINP(I),(HP(I,J,1),HP(I,J,2),J=1,NPFPI)

20 	CONTINUE

C.......READ OUTFLOW CRITICAL DEPTH NODES

10      READ (NR,*)NDC

        IF(NDC.GE.1)READ (NR,*)(NODDC(I),I=1,NDC)

C.......READ SPECIFIED OUTFLOW NODES

        READ (NR,*)NFLUX,TFOUT

        IF(NFLUX.GE.1)READ (NR,*) (NODFX(I),I=1,NFLUX)

        IF(NODC.LT.1)GOTO 30 

C.......INPUT CHANNEL INFORMATION

        DO 25 I=1,NODC

        READ (NR,*)KK,(FC(KK,J),J=1,3),FC(KK,5)

        FC(KK,4)=FP(KK,6)-FC(KK,3)

25      CONTINUE

        READ (NR,*)NCHI,NPCHI,NCHO,NPCHO,NSTA,NPSTA

        IF(NCHI.LT.1)GOTO 40 

C.......READ INFLOW HYDROGRAPHS (LINEAR FUNCTION)

        DO 50 I=1,NCHI

        READ (NR,*)KIN(I),(H(I,J,1),H(I,J,2),J=1,NPCHI)

50 	CONTINUE

40      IF(NCHO.LT.1)GOTO 60

        DO 70 I=1,NCHO

C.......READ OUTFLOW BOUNDARY CONDITION NODES

C.......  QOUT = ALPHA*(DEPTH OF WATER)**BETA

        READ (NR,*)KOUT(I),(HOUT(I,J,1),HOUT(I,J,2),

     C  HOUT(I,J,3),J=1,NPCHO)

70      CONTINUE

60      IF(NSTA.LT.1)GOTO 30

C.......READ STAGE CURVE (LINEAR FUNCTION)

        DO 80 I=1,NSTA

80      READ (NR,*) NOSTA(I),(STA(I,J,1),STA(I,J,2),J=1,NPSTA)

30      CONTINUE

C.......END OF INPUT DATA

        CLOSE(UNIT=NR)

C

C       WRITE BASIC INFORMATION TO OUTPUT FILE

C

C.......FORMATS

1001    FORMAT(1X,'PRINTOUT OPTIONS:',/5X,

     C  '1= RESULTS SENT DIRECTLY TO PRINTER',/5X,

     C  '2= RESULTS SENT TO A FILE ON DISK',/1X,

     C  'SELECT DESIRED OPTION ->',\)

1002    FORMAT(A1)

1003    FORMAT(/1X,'ENTER RESULTS FILE NAME',/1X,

     C             '(Example:  DHM21.RES)   ->',\)

1004    FORMAT(A15)

1005    FORMAT(/1X,'ENTER INPUT FILE NAME',/1X,

     C             '(Example:  DHM21.DAT)   ->',\)

2001    FORMAT(/,10X,'*** KINEMATIC ROUTING ***',/)

2002    FORMAT(/,10X,'*** DIFFUSION ROUTING ***',/)

2003    FORMAT(10X,'MIN. TIMESTEP(SEC.) = ',F5.2,/,

     C  10X,'MAX. TIMESTEP(SEC.) = ',F5.2,/,

     C  10X,'INCREASED TIMESTEP INTERVAL (SEC.) = ',F5.2,/,

     C  10X,'DECREASED TIMESTEP INTERVAL (SEC.) = ',F5.2,/,

     C  10X,'TOTAL SIMULATION(HOUR) = ',F5.2,/,

     C  10X,'UPDATE INTERVAL(TIMESTEPS) = ',I5,/,

     C  10X,'OUTPUT INTERVAL(HOUR) = ',F5.2)

2004    FORMAT(10X,'NUMBER OF NODAL POINTS FOR FLOOD PLAIN = ',I5,/,

     C  10X,'UNIFORM GRID SIDE(FEET) = ',F10.3,/,

     C  10X,'NUMBER OF NODAL POINTS FOR CHANNEL = ',I5,/

     C  10X,'RETENTION WATER DEPTH(FEET) = ',F5.4,/,

     C  10X,'TOLERANCE OF CHANGE IN WATER DEPTH(FEET) = ',F5.4,/,

     C  10X,'PERCENTAGE OF CHANGE IN WATER DEPTH = ',F5.1,' %')

2005    FORMAT(130('-'))

2006    FORMAT(//,10X,'NODAL POINT DATA ENTRY:',//,

     C  7X,'*** FLOOD PLAIN INFORMATION ***',/,

     C  10X,'NC = CENTRAL GRID NODE',/,

     C  10X,'NN,NE,NS,NW = NORTH, EAST, SOUTH, WEST NODAL POINTS',/,

     C  10X,'NBAR = NODAL POINT MANNINGS ROUGHNESS COEFFICIENT',/,

     C  12X,'(NEGATIVE SIGN INDICATES A CHANNEL PASSING THROUGH)',/,

     C  10X,'ELEV = NODAL POINT ELEVATION',/,

     C  10X,'DEPTH = INITIAL WATER DEPTH AT NODE',//)

2007    FORMAT(11X,' NC  NN  NE  NS  NW   NBAR   ELEV.  DEPTH')

2008    FORMAT(10X,5I4,1X,F6.4,2X,F6.1,1X,F5.1)

2009    FORMAT(//,10X,'NUMBER OF EFFECTIVE RAINFALL INTENSITY ',

     C  'ENTRIES = ',I2,/,4X,'LINEAR FUNCTION IN EFFECTIVE RAINFALL',

     C  ' INTENSITY (IN/HR) ON WATERSHED:',/,10X,'HOUR   INTENSITY')

2010    FORMAT(8X,F6.2,4X,F6.2)

2011    FORMAT(/,10X,'INFLOW HYDROGRAPH AT NODE #',I3,/,

     C  12X,'HOUR     CFS')

2012    FORMAT(10X,F5.1,4X,F7.0)

2013    FORMAT(//,10X,'NUMBER OF CRITICAL-DEPTH OUTFLOW NODES = ',I4,/,

     C  10X,'CRITICAL-DEPTH OUTFLOW NODE NUMBERS:')

2014    FORMAT(10X,I3,1X,I3)

2015    FORMAT(//,7X,'***CHANNEL INFORMATION***',/,

     C  10X,'NODE  NBAR  WIDTH   DEPTH   BOTTOM  INITIAL DEPTH')

2016    FORMAT(10X,I3,2X,F5.4,1X,F7.1,1X,F7.1,1X,F7.1,5X,F7.1)

2017    FORMAT(10X,'OUTFLOW IS APPROXIMATED AS THE FOLLOWING EQUATION:',

     C  /,12X,'QOUT = ALPHA*(DEPTH)**BETA')

2018    FORMAT(10X,'OUTFLOW NODE # ',I3,

     C  /,9X,'DEPTH LESS THAN',

     C  /,9X,'  OR EQUAL TO    ALPHA    BETA')

2019    FORMAT(15X,F4.1,6X,F7.3,1X,F7.3)

2020    FORMAT(/,10X,'STAGE CURVE AT NODE #',I3,/,

     C  12X,'HOUR    FEET')

2021    FORMAT(10X,F5.1,4X,F7.3)

2022    FORMAT(//,5X,'MODEL TIME(HOURS) = ',F10.2)

2023    FORMAT(11X,'EFFECTIVE RAINFALL(IN/HR) = ',F6.2,/)

2024    FORMAT(/,5X,'AVERAGE FLOW RATE FOR SPECIFIED FLOOD PLAIN ',

     C  'NODES :',/,10X,'NODE',5X,'QN',9X,'QE',9X,'QS',9X,'QW')

2025    FORMAT(10X,I4,4(2X,E9.3))

2026    FORMAT(//,5X,'MODEL TIME(HOURS) = ',F10.2,' (SECONDS) = ',E9.3,

     C  ' (TOTAL TIMESTEP NUMBER) = ',1PE9.1)

2027    FORMAT(7X,'***FLOOD PLAIN RESULTS***')

2028    FORMAT(10X,'INFLOW  RATE AT NODE ',I3,' IS EQUAL TO ',F10.2)

2029    FORMAT(/,5X,'NODE',7X,10(I3,8X))

2030    FORMAT(5X,'DEPTH',10(3X,F8.3))

2031    FORMAT(3X,'ELEVATION',F9.3,10(2X,F9.3))

2032    FORMAT(5X,'VEL-N',10(3X,F8.3))

2033    FORMAT(5X,'VEL-E',10(3X,F8.3))

2034    FORMAT(5X,'VEL-S',10(3X,F8.3))

2035    FORMAT(5X,'VEL-W',10(3X,F8.3))

2036    FORMAT(/,5X,'OUTFLOW RATE AT CRITICAL-DEPTH NODES:',

     C  /,10X,'NODE   OUTFLOW RATE(CFS)')

2037    FORMAT(10X,I4,5X,F10.2)

2038    FORMAT(//,7X,'***CHANNEL RESULTS***',/)

2039    FORMAT(10X,'OUTFLOW RATE AT NODE ',I3,' IS EQUAL TO ',F10.2)

2040    FORMAT(//,5X,'MIN. TIMESTEP(SEC.) = ',F5.2,

     C    5X,'MAX. TIMESTEP(SEC.) = ',F5.2,

     C    5X,'MEAN TIMESTEP(SEC.) = ',F5.2,//)

2041    FORMAT(130('='))

2042    FORMAT(///,10X,'MAXIMUM WATER SURFACE VALUES FOR FLOOD',

     C  ' PLAIN',/)

2043    FORMAT(5X,'TIME ',10(3X,F8.3))      

2044    FORMAT(///,10X,'MAXIMUM WATER SURFACE VALUES FOR CHANNEL',/)

2045    FORMAT(2X,'*** DEPTH OF WATER IS EITHER GREATER THAN',

     1  ' 150 OR LESS THAN 0 ***',/,2X,'*** PROGRAM STOP ***')

2046    FORMAT(2X,'*** MINIMUM TIMESTEP ',F4.1,' SEC. IS TOO LARGE!!',

     1  /,2X,' ===>  A SMALLER TIMESTEP SHOULD BE USED ***')

2047    FORMAT(2X,'MODEL TIME = ',F7.2,' HOURS')

C

        IF(KMODEL.EQ.1)WRITE(NW,2001)

        IF(KMODEL.NE.1)WRITE(NW,2002)

        WRITE(NW,2003)DTMIN,DTMAX,DTI,DTD,SIMUL,ITER,TOUT

        WRITE(NW,2004)NNOD,SIDE,NODC,TOL,DTOL,DTOLP

        WRITE(NW,2005)

        WRITE(NW,2006)

        WRITE(NW,2007)

        DO 90 I=1,NNOD

        NN=IFIX(FP(I,1))

        NE=IFIX(FP(I,2))

        NS=IFIX(FP(I,3))

        NNW=IFIX(FP(I,4))

        WRITE(NW,2008)I,NN,NE,NS,NNW,(FP(I,J),J=5,7)

90      CONTINUE

        WRITE(NW,2005)

        IF(NERI.LT.1)GOTO 100

        WRITE(NW,2009)NERI

        WRITE(NW,2010)((R(I,J),J=1,2),I=1,NERI)

        WRITE(NW,2005)

100     IF(NFPI.LT.1)GOTO 110

        DO 120 I=1,NFPI

        WRITE(NW,2011)KINP(I)

        DO 120 J=1,NPFPI

        WRITE(NW,2012)HP(I,J,1),HP(I,J,2)

120     CONTINUE

        WRITE(NW,2005)

110     IF(NDC.LT.1)GOTO 130

        WRITE(NW,2013)NDC

        WRITE(NW,2014)(NODDC(I),I=1,NDC)

        WRITE(NW,2005)

130     IF(NODC.LT.1)GOTO 140

        WRITE(NW,2015)

        DO 135 I=1,NNOD

        IF(FC(I,1).EQ.0.)GO TO 135

        WRITE(NW,2016)I,(FC(I,J),J=1,5)

135     CONTINUE

        WRITE(NW,2005)

        IF(NCHI.LT.1)GOTO 150

        DO 160 I=1,NCHI

        WRITE(NW,2011)KIN(I)

        DO 160 J=1,NPCHI

        WRITE(NW,2012)H(I,J,1),H(I,J,2)

160     CONTINUE

        WRITE(NW,2005)

150     IF(NCHO.LT.1)GOTO 170

        WRITE(NW,2017)

        DO 180 I=1,NCHO

        WRITE(NW,2018)KOUT(I)

        DO 180 J=1,NPCHO

        WRITE(NW,2019)HOUT(I,J,1),HOUT(I,J,2),HOUT(I,J,3)

180     CONTINUE

        WRITE(NW,2005)

170     IF(NSTA.LT.1)GOTO 140

        DO 190 I=1,NSTA

        WRITE(NW,2020)NOSTA(I)

        DO 190 J=1,NPSTA

        WRITE(NW,2021)STA(I,J,1),STA(I,J,2)

190     CONTINUE

        WRITE(NW,2005)

140     CONTINUE

C

C

C       MAIN PROGRAM

C

C

C.......INITIALIZE CONSTANTS

        XTERA=0.

        DSEC=DTMIN

        DT=DTMIN/3600.

        DTOLP=DTOLP*.01

        CHECKD=DTOL/DTOLP

        TTIME=0.

        QBC=0.

        QTEMP=0.

        KK=0

        TTOUT=TOUT

        TTFOUT=TFOUT

        KIT=0

        TIME=0.

        DO 200 J=1,NNOD

        DMAX(J,1)=0.

        TIMEX(J,1)=0.

        DMAX(J,2)=0.

        TIMEX(J,2)=0.

        FP(J,8)=0.

200     CONTINUE

C

C.......MAIN LOOP FOR MODEL

C

210     KKOUT=0

        TMIN=99.

        TMAX=-99.

        TMEAN=0.

C

C.......FLOODPLAIN MODEL      

C

220     KFLUX=0

        IKODE=0

        TIME=TIME+DT

230     FPMAX=0.

        XTERA=XTERA+1.

        FCMAX=0.

        IJK=0

C.......CHECK CHANNEL MODEL OPTION

        IF(ITER.EQ.0 .AND. NODC.NE.0)TTIME=DSEC

        IF(ITER.EQ.0 .AND. NODC.NE.0)GO TO 250 

C.......UPDATE TIME AND BOUNDARY CONDITION VALUES

        IF(NFPI.LT.1)GOTO 260

        DO 270 J=1,NFPI

        DO 280 I=2,NPFPI

        IF(TIME.GT.HP(J,I,1))GOTO 280

        QTEMP=HP(J,I-1,2)+(HP(J,I,2)-HP(J,I-1,2))*(TIME-HP(J,I-1,1))/

     C        (HP(J,I,1)-HP(J,I-1,1))

        GO TO 290

280     CONTINUE

        QTEMP=HP(J,NPFPI,2)

290     QBC=QTEMP

        IF(QBC.LT.0.)QBC=0.

        JJ=KINP(J)

        FP(JJ,8)=FP(JJ,8)+QBC

270     CONTINUE

C.......INCLUDE THE EFFECITIVE RAINFALL ON THE WATERSHED

260     IF(NERI.LT.1)GOTO 300

        DO 310 J=2,NERI

        IF(TIME.GT.R(J,1))GOTO 310

        RRATE=R(J-1,2)+(R(J,2)-R(J-1,2))*(TIME-R(J-1,1))/

     C        (R(J,1)-R(J-1,1))

        GO TO 320

310     CONTINUE

320     QRAIN=RRATE*SIDE*SIDE/(12.*3600.)

        DO 330 J=1,NNOD

        FP(J,8)=FP(J,8)+QRAIN

330     CONTINUE

300     IF(NFLUX.EQ.0)GOTO 340

        IF(TIME.LT.TTFOUT)GOTO 340

        TTFOUT=TTFOUT+TFOUT

        IF(ITER.EQ.0 .AND. NODC.NE.0)GO TO 340

        WRITE(NW,2005)

        WRITE(NW,2022)TIME

        IF(RRATE.NE.0.)WRITE(NW,2023)RRATE

        IJK=1

        WRITE(NW,2024)

340     CONTINUE

C.......CALCULATE FLOW VELOCITIES AND FLOWRATES

        DO 350 I=1,NNOD

        DO 360 II=1,4

        QQ=0.

        NQ=FP(I,II)

        IF(NQ.EQ.0)GOTO 360

        CALL QFP(I,NQ,SIDE,QQ,ID,VV,TOL,KMODEL)

        IF(ID.EQ.1)GOTO 370

360     Q(II)=QQ

C.......ADJUST FLOWRATES FOR DIRECTION

        Q(3)=-Q(3)

        Q(4)=-Q(4)

C.......ESTIMATE ACCUMULATION OF INFLOW

        QNET=Q(3)+Q(4)-Q(1)-Q(2)

        IF(NFLUX.EQ.0)GOTO 380

        IF(IJK.NE.1)GOTO 380

        QN=Q(1)

        QE=Q(2)

        QS=-Q(3)

        QW=-Q(4)

        DO 390 J=1,NFLUX

        IF(I.EQ.NODFX(J))WRITE(NW,2025)I,QN,QE,QS,QW

390     CONTINUE

380     FP(I,8)=QNET+FP(I,8)

350     CONTINUE

C.......ACCOUNT FOR CRITICAL-DEPTH OUTFLOW NODES

        IF(NDC.LT.1)GOTO 400

        DO 410 J=1,NDC

        WID=SIDE

        JJ=NODDC(J)

        IF(FP(JJ,5).LT.0.)WID=SIDE-FC(JJ,2)

        QOUT=5.67*(FP(JJ,7)**1.5)*WID

        IF(FP(JJ,7).LT.TOL)QOUT=0.

        FP(JJ,8)=FP(JJ,8)-QOUT

410     CONTINUE

C.......UPDATE CHANGE OF WATER DEPTH 

400     DO 420 J=1,NNOD

        AP=SIDE*SIDE

        IF(FP(J,5).GT.0.)GO TO 425

        AC=0.

        KCO=0

        DO 427 JJ=1,4

        NQ=FP(J,JJ)

        IF(NQ.EQ.0)GO TO 427

        IF(FP(NQ,5).GT.0.)GOTO 427 

        AC=AC+(FC(NQ,2)+FC(J,2))*.25*SIDE

        KCO=KCO+1

427     CONTINUE

        IF(KCO.EQ.1)AC=2.*AC

        AP=AP-AC  

425     FP(J,8)=FP(J,8)*DSEC/AP

        TEMP=ABS(FP(J,8))

        IF(TEMP.LT.DTOL)GOTO 420

        IF(FP(J,7).LT.CHECKD)FPMAX=99.

        IF(FP(J,7).LT.CHECKD)GOTO 430 

        TOLP=TEMP/FP(J,7)

        IF(TOLP.GE.DTOLP)FPMAX=99.

        IF(TOLP.GE.DTOLP)GOTO 430 

420     CONTINUE

C.......CALCULATE THE EFFLUX VELOCITIES

        IF(KODE.NE.1)GOTO 440

        DO 450 J=1,NNOD

        DO 450 II=1,4

        QQ=0.

        NQ=FP(J,II)

        IF(NQ.EQ.0)GOTO 450

        CALL QFP(J,NQ,SIDE,QQ,ID,VV,TOL,KMODEL)

        VEL(J,II)=VV

450     CONTINUE

C.......CHECK INTERFACE MODEL UPDATE REQUEST

440     KIT=KIT+1

        TTIME=TTIME+DSEC

        IF(KIT.NE.ITER .OR. NODC.LT.1)GOTO 430 

C

C.......UPDATE WATER DEPTH FOR CHANNEL

C

250     CALL FLOODC(TIME,TTIME,NNOD,SIDE,TOL,FCMAX,NW,KMODEL)

C.......UPDATE NEW TIMESTEP SIZE

430     DD=AMAX1(FPMAX,FCMAX)

        IF(DD.GT.0.)DSECP=DSEC-DTD

        IF(DD.LE.0.)DSECP=DSEC+DTI

        IF(DSECP.LT.DTMIN)DSECP=DTMIN

        IF(DSECP.GT.DTMAX)DSECP=DTMAX

        DTT=DSECP/3600.

        IF(DD.LE.DTOL)GOTO 460

        IF(DSEC.EQ.DTMIN)IKODE=1+IKODE

        IF(DSEC.NE.DTMIN)IKODE=1

        IF(IKODE.GE.3)GOTO 470

        TIME=TIME-DT+DTT

        IF(TTIME.EQ.0.)GOTO 480 

        TTIME=TTIME-DSEC

480     DO 490 J=1,NNOD

        FP(J,8)=0.

490     CONTINUE

        DT=DTT

        DSEC=DSECP

        KIT=KIT-1

        IF(KIT.LT.0)KIT=0

        GO TO 230 

C.......UPDATE DEPTH OF WATER

460     DO 500 J=1,NNOD

        FP(J,7)=FP(J,7)+FP(J,8)

        IF(FP(J,7).LT.0.)FP(J,7)=0.

        FP(J,8)=0.

        IF(NODC.LT.1)GOTO 500 

        FC(J,5)=FC(J,5)+FC(J,6)

        IF(FC(J,5).LT.0.)FC(J,5)=0.

        FC(J,6)=0.

500     CONTINUE

        IF(DSEC.GT.TMAX)TMAX=DSEC

        IF(DSEC.LT.TMIN)TMIN=DSEC

C.......INTERFACE BETWEEN FLOOD PLAIN AND CHANNEL DEPTHS

        IF(KIT.NE.ITER)GOTO 510

        IF(NODC.LT.1)GOTO 510

        IF(ITER.NE.0)CALL CHANPL(NNOD,SIDE,TOL)

        TTIME=0.

        KIT=0

C.......CHECK OUTPUT REQUEST

510     IF(TIME.LT.TTOUT)GOTO 520

C.......USE FC(I,6) AND FP(I,8) TO STORE WATER SURFACE EELEVATIOS

        DO 530 J=1,NNOD

        IF(NODC.LT.1)GOTO 540 

        FC(J,6)=FC(J,5)+FC(J,4)

        IF(ITER.EQ.0)GOTO 530 

540     FP(J,8)=FP(J,7)+FP(J,6)

530     CONTINUE

C.......UPDATE MAXIMUM WATER SURFACE VALUES

520     DO 550 J=1,NNOD

        IF(FP(J,7).LT.DMAX(J,1))GOTO 550

        DMAX(J,1)=FP(J,7)

        TIMEX(J,1)=TIME

550     CONTINUE

        IF(NODC.LT.1)GOTO 560 

        DO 570 J=1,NNOD

        IF(FC(J,5).LT.DMAX(J,2))GOTO 570

        DMAX(J,2)=FC(J,5)

        TIMEX(J,2)=TIME

570     CONTINUE

560     TMEAN=TMEAN+DSEC

        KKOUT=KKOUT+1

        DT=DTT

        DSEC=DSECP

        IF(TIME.LT.TTOUT)GOTO 220

C.......STORE FLOODPLAIN AND CHANNEL RESULTS IN OUTPUT FILE

370     WRITE(NW,2005)

        XTIME=TIME*3600.

        WRITE(IW,2047)TIME

        WRITE(NW,2026)TIME,XTIME,XTERA

        IF(RRATE.NE.0.)WRITE(NW,2023)RRATE

        IF(ITER.EQ.0 .AND. NODC.NE.0)GOTO 580

        WRITE(NW,2027)

        IF(NFPI.LT.1)GOTO 590

        DO 600 J=1,NFPI

        DO 610 I=2,NPFPI

        IF(TIME.GT.HP(J,I,1))GOTO 610

        QIN=HP(J,I-1,2)+(HP(J,I,2)-HP(J,I-1,2))*(TIME-HP(J,I-1,1))/

     C      (HP(J,I,1)-HP(J,I-1,1))

        GO TO 620

610     CONTINUE

620     WRITE(NW,2028)KINP(J),QIN

600     CONTINUE

590     KO=1

        IO=1

        JO=10

630     DO 615 II=IO,JO

        IF(FP(II,7).GT.0.)GOTO 625

615     CONTINUE

        GO TO 635 

625     WRITE(NW,2029)(J,J=IO,JO)

        WRITE(NW,2030)(FP(J,7),J=IO,JO)

        WRITE(NW,2031)(FP(J,8),J=IO,JO)

        IF(KODE.EQ.1)WRITE(NW,2032)(VEL(J,1),J=IO,JO)

        IF(KODE.EQ.1)WRITE(NW,2033)(VEL(J,2),J=IO,JO)

        IF(KODE.EQ.1)WRITE(NW,2034)(VEL(J,3),J=IO,JO)

        IF(KODE.EQ.1)WRITE(NW,2035)(VEL(J,4),J=IO,JO)

635     KO=KO+1

        IO=IO+10

        JO=10*KO

        IF(JO.LE.NNOD)GOTO 630

        IF(JO-NNOD.GE.10)GOTO 640

        JO=NNOD

        GO TO 630

640     DO 650 J=1,NNOD

650     FP(J,8)=0.

C.......OUTPUT OUTFLOW RATE AT CRITICAL-DEPTH NODES

        IF(NDC.LT.1)GOTO 580

        WRITE(NW,2036)

        WID=SIDE

        DO 660 J=1,NDC

        JJ=NODDC(J)

        IF(FP(JJ,5).LT.0.)WID=SIDE-FC(JJ,2)

        QOUT=5.67*(FP(JJ,7)**1.5)*WID

        IF(FP(JJ,7).LT.TOL)QOUT=0.

        WRITE(NW,2037)JJ,QOUT

660     CONTINUE

        WRITE(NW,2005)

580     IF(NODC.LT.1)GOTO 670

        WRITE(NW,2038)

        IF(NCHI.LT.1)GOTO 680

        DO 690 J=1,NCHI

        DO 700 I=2,NPCHI

        IF(TIME.GT.H(J,I,1))GOTO 700

        QIN=H(J,I-1,2)+(H(J,I,2)-H(J,I-1,2))*(TIME-H(J,I-1,1))/

     C      (H(J,I,1)-H(J,I-1,1))

        GO TO 710

700     CONTINUE

710     WRITE(NW,2028)KIN(J),QIN

690     CONTINUE

680     IF(NCHO.LT.1)GOTO 720

        DO 730 J=1,NCHO

        JJ=KOUT(J)

        DO 740 KJ=1,NPCHO

        IF(FC(JJ,5).GT.HOUT(J,KJ,1))GOTO 740

        QOUT=HOUT(J,KJ,2)*(FC(JJ,5)**HOUT(J,KJ,3))

        IF(FC(JJ,5).LT.TOL)QOUT=0.

        GO TO 750

740     CONTINUE

750     WRITE(NW,2039)JJ,QOUT

730     CONTINUE

720     CONTINUE

        KO=1

        IO=1

        JO=10

760     DO 770 II=IO,JO

        IF(FC(II,5).GT.0.)GOTO 780

770     CONTINUE

        GO TO 790	

780     WRITE(NW,2029)(J,J=IO,JO)

        WRITE(NW,2030)(FC(J,5),J=IO,JO)

        WRITE(NW,2031)(FC(J,6),J=IO,JO)

790     KO=KO+1

        IO=IO+10

        JO=10*KO

        IF(JO.LE.NNOD)GOTO 760

        IF(JO-NNOD.GE.10)GOTO 800

        JO=NNOD

        GO TO 760

800     DO 810 J=1,NNOD

810     FC(J,6)=0.

C

C.......END OF MAIN LOOP

C

670     IF(ID.EQ.1)GOTO 470

        TMEAN=TMEAN/FLOAT(KKOUT)

        WRITE(NW,2040)TMIN,TMAX,TMEAN

        TTOUT=TTOUT+TOUT

        IF(TIME.LT.SIMUL)GOTO 210

470     WRITE(NW,2041)

C.......OUTPUT THE MAXIMUM WATER SURFACE

        IF(ITER.EQ.0 .AND. NODC.NE.0)GOTO 820

        WRITE(NW,2042)

        KO=1

        IO=1

        JO=10

830     DO 825 II=IO,JO

        IF(DMAX(II,1).GT.0.)GO TO 835

825     CONTINUE

        GO TO 845 

835     WRITE(NW,2029)(J,J=IO,JO)

        WRITE(NW,2030)(DMAX(J,1),J=IO,JO)

        WRITE(NW,2043)(TIMEX(J,1),J=IO,JO)

845     KO=KO+1

        IO=IO+10

        JO=10*KO

        IF(JO.LE.NNOD)GOTO 830

        IF(JO-NNOD.GE.10)GOTO 840

        JO=NNOD

        GO TO 830     

840     WRITE(NW,2041)            

820     IF(NODC.LT.1)GOTO 850

        WRITE(NW,2044) 

        KO=1

        IO=1

        JO=10

860     DO 870 II=IO,JO

        IF(DMAX(II,2).GT.0.)GOTO 880

870     CONTINUE

        GO TO 890

880     WRITE(NW,2029)(J,J=IO,JO)

        WRITE(NW,2030)(DMAX(J,2),J=IO,JO)

        WRITE(NW,2043)(TIMEX(J,2),J=IO,JO)

890     KO=KO+1

        IO=IO+10

        JO=10*KO

        IF(JO.LE.NNOD)GOTO 880

        IF(JO-NNOD.GE.10)GOTO 850 

        JO=NNOD

        GO TO 880

850     WRITE(NW,2041)             

C.......END OF PROGRAM

        IF(ID.EQ.1)WRITE(NW,2045)

        IF(IKODE.GE.3)WRITE(NW,2046)DSEC

C

        STOP

        END

C

        SUBROUTINE FLOODC(TIME,TTIME,NNOD,SIDE,TOL,FCMAX,NW,KMODEL)

C

C       THIS SUBROUTINE CALCULATES THE DEPTH OF WATER FOR

C       THE CHANNEL MODEL

C

        COMMON/BLK 1/FP(250,8),FC(250,6)

        COMMON/BLK 2/KIN(10),H(10,15,2),KOUT(10),HOUT(10,15,3)

        COMMON/BLK 3/NOSTA(10),STA(10,15,2),NODFX(50)

        COMMON/BLK 4/DMAX(250,2),TIMEX(250,2)

        COMMON/BLK 6/NODC,NCHI,NCHO,NPCHI,NPCHO,NSTA,NPSTA

        COMMON/BLK 7/DTOL,DTOLP,NFLUX,KFLUX,CHECKD,ITER

        DIMENSION Q(4)

C

C       DEFINITIONS

C

C       FC(I,J)=MANNINGS,WIDTH,DEPTH,BOTTOM ELEVATION,INITIAL DEPTH,

C               TEMPORARY MEMORY

C       KIN(I)=ARRAY OF INFLOW NODE

C       H(I,J,1)=TIME COORDINATE FOR INFLOW RATE IN HOUR             

C       H(I,J,2)=INFLOW RATE(CFS)

C       KOUT(I)=ARRAY OF OUTFLOW NODE

C       HOUT(I,J)=PARAMETERS FOR OUTFLOW NODE

C       Q(I)=VOLUME OF FLOW

C       NOSTA(I)=ARRAY OF STAGE STATION

C       STA(I,J,1)=TIME COORDINATE FOR STAGE CURVE

C       STA(I,J,2)=DEPTH OF WATER IN FEET

C

C       CHANNEL MODEL  

C

C.......INITIALIZE CONSTANTS

        QBC=0.

        QTEMP=0.

        DO 30 J=1,NNOD

        FC(J,6)=0.

30      CONTINUE

C

C.......FORMATS

C

212     FORMAT(//,130('-'),/,5X,'MODEL TIME(HOUR) = ',F10.2,/)

213     FORMAT(/,5X,'AVERAGE FLOW RATE FOR SPECIFIED CHANNEL NODES :',

     C  /,10X,'NODE',5X,'QN',9X,'QE',9X,'QS',9X,'QW')

214     FORMAT(10X,I4,4(2X,E9.3))

C

        IF(KFLUX.EQ.1 .AND. ITER.EQ.0)WRITE(NW,212)TIME

        IF(KFLUX.EQ.1)WRITE(NW,213)

C

C.......MAIN LOOP FOR CHANNEL MODEL

C

C.......UPDATE TIME AND BOUNDARY CONDITION VALUES

        IF(NCHI.LT.1)GOTO 40 

        DO 50 J=1,NCHI

        DO 60 I=2,NPCHI

        IF(TIME.GT.H(J,I,1))GOTO 60 

        QTEMP=H(J,I-1,2)+(H(J,I,2)-H(J,I-1,2))*(TIME-H(J,I-1,1))/

     C        (H(J,I,1)-H(J,I-1,1))

        GO TO 70 

60      CONTINUE

70      QBC=QTEMP*TTIME

        IF(QBC.LT.0.)QBC=0.

C.......UPDATE INFLOW BOUNDARY CONDITION NODES

        JJ=KIN(J)

        FC(JJ,6)=QBC

50      CONTINUE

C.......CALCULATE FLOW VELOCITIES AND FLOWRATES

40      DO 80 I=1,NNOD

        QNET=0.

        IF(FP(I,5).GT.0.)GOTO 80  

        DO 90 II=1,4

        QQ=0.

        NQ=FP(I,II)

        IF(NQ.EQ.0)GOTO 90 

        IF(FP(NQ,5).GT.0.)GO TO 90

        CALL QFC(I,NQ,QQ,SIDE,TOL,KMODEL)

90      Q(II)=QQ

C.......ADJUST FLOWRATES FOR DIRECTION

        Q(3)=-Q(3)

        Q(4)=-Q(4)

C.......ESTIMATE ACCUMULATION OF INFLOW

        QNET=(Q(3)+Q(4)-Q(1)-Q(2))*TTIME

        IF(NFLUX.EQ.0)GOTO 80  

        IF(KFLUX.EQ.0)GOTO 80  

        DO 100 J=1,NFLUX

        IF(I.NE.NODFX(J))GOTO 100

        Q(3)=-Q(3)

        Q(4)=-Q(4)

        WRITE(NW,214)I,Q(1),Q(2),Q(3),Q(4)

100     CONTINUE

80      FC(I,6)=QNET+FC(I,6)

C.......ACCOUNT DISCHARGE AT OUTFLOW NODES

        IF(NCHO.LT.1)GOTO 110

        DO 120 J=1,NCHO

        JJ=KOUT(J)

        DO 130 K=1,NPCHO

        IF(FC(JJ,5).GT.HOUT(J,K,1))GOTO 130

        QOUT=HOUT(J,K,2)*(FC(JJ,5)**HOUT(J,K,3))*TTIME

        IF(FC(JJ,5).LT.TOL)QOUT=0.

        GO TO 140 

130     CONTINUE

140     FC(JJ,6)=FC(JJ,6)-QOUT

120     CONTINUE

C.......UPDATE THE WATER ELEVATIONS AT STAGE STATIONS

110     IF(NSTA.LT.1)GOTO 150 

        DO 160 I=1,NSTA

        NN=NOSTA(I)

        DO 170 J=2,NPSTA

        IF(TIME.GT.STA(I,J,1))GOTO 170

        DE=STA(I,J-1,2)+(STA(I,J,2)-STA(I,J-1,2))*(TIME-STA(I,J-1,1))

     C    /(STA(I,J,1)-STA(I,J-1,1))

        GO TO 180

170     CONTINUE

180     CONTINUE

        FC(NN,5)=DE-FC(NN,4)

        FC(NN,6)=0.

160     CONTINUE

C.......CHECK MAXIMUM CHANGE OF WATER DEPTH

150     DO 190 J=1,NNOD

        IF(NSTA.LT.1)GOTO 200

        DO 210 JJ=1,NSTA

        IF(J.EQ.NOSTA(JJ))GOTO 190 

210     CONTINUE

200     IF(FP(J,5).GT.0.)GOTO 190 

        A=0.

        KCO=0

        DO 220 JJ=1,4

        NQ=FP(J,JJ)

        IF(NQ.EQ.0)GO TO 220

        IF(FP(NQ,5).GT.0.)GOTO 220 

        A=A+(FC(NQ,2)+FC(J,2))*.25*SIDE

        KCO=KCO+1

220     CONTINUE

        IF(KCO.EQ.1)A=2.*A

        FC(J,6)=FC(J,6)/A     

190     CONTINUE

        DO 230 I=1,NNOD

        TEMP=ABS(FC(I,6))

        IF(TEMP.LT.DTOL)GOTO 230 

        IF(FC(I,5).LT.CHECKD)FCMAX=99.

        IF(FC(I,5).LT.CHECKD)RETURN

        TOLP=TEMP/FC(I,5)

        IF(TOLP.GE.DTOLP)FCMAX=99.

        IF(TOLP.GE.DTOLP)RETURN

230     CONTINUE

        RETURN

        END

C

        SUBROUTINE QFP(I,NQ,SIDE,QQ,ID,VEL,TOL,KMODEL)

C

C       THIS SUBROUTINE CALCULATES THE EFFLUX PER UNIT WIDTH

C       WHICH FLOWS ACROSS THE ADJACENT CONTORL VOLUMES FOR

C	FLOODPLAIN FLOW

C

        COMMON/BLK 1/FP(250,8),FC(250,6)

        VEL=0.

        ID=0

        QQ=0.

        IF(FP(I,7).LE.TOL.AND.FP(NQ,7).LE.TOL)GOTO 200

        WID=SIDE

        H=FP(I,7)+FP(I,6)

        IF(KMODEL.EQ.1)H=FP(I,6)

C.......DEPTHS ARE NONZERO

        HN=FP(NQ,7)+FP(NQ,6)

        IF(KMODEL.EQ.1)HN=FP(NQ,6)

        GRAD=(HN-H)/SIDE

        HBAR=.5*(FP(I,7)+FP(NQ,7))

        IF(HBAR.LT.0. .OR. HBAR.GT.150.)ID=1

        IF(FP(I,5).LT.0. .AND. FP(NQ,5).LT.0.)

     C  WID=SIDE-.5*(FC(I,2)+FC(NQ,2))

        IF(GRAD)150,200,170

C.......H > HN

150     IF(FP(I,7).LT.TOL)GOTO 200

        YBAR=AMIN1(HBAR,FP(I,7))

        GOTO 180

C.......HN > H

170     IF(FP(NQ,7).LT.TOL)GOTO 200

        YBAR=AMIN1(HBAR,FP(NQ,7))

180     XNBAR=.5*(ABS(FP(I,5))+ABS(FP(NQ,5)))

        AGRAD=ABS(GRAD)

        IF(AGRAD.LT..00001)GOTO 200

        XK=(1.486/XNBAR)*YBAR**0.667/SQRT(AGRAD)

        VEL=-XK*GRAD    

        QQ=VEL*YBAR*WID

200     CONTINUE

        RETURN

        END

C

        SUBROUTINE QFC(I,NQ,QQ,SIDE,TOL,KMODEL)

C

C       THIS SUBROUTINE CALCULATES VOLUME OF WATER THAT

C       FLOWS ACROSS THE ADJACENT CONTROL VOLUMES FOR

C       CHANNEL FLOW

C

        COMMON/BLK 1/FP(250,8),FC(250,6)

        QQ=0.

        IF(FC(I,5).LE.TOL.AND.FC(NQ,5).LE.TOL)GOTO 200

        DCH=.5*(FC(I,3)+FC(NQ,3))

        WID=.5*(FC(I,2)+FC(NQ,2))

        HBAR=.5*(FC(I,5)+FC(NQ,5))

        H=FC(I,4)+FC(I,5)

        IF(KMODEL.EQ.1)H=FC(I,4)

C.......DEPTHS ARE NONZERO

        HN=FC(NQ,4)+FC(NQ,5)

        IF(KMODEL.EQ.1)HN=FC(NQ,4)

        GRAD=(HN-H)/SIDE

        IF(GRAD)150,200,170

C.......H > HN

150     IF(FC(I,5).LT.TOL)GOTO 200

        YBAR=AMIN1(HBAR,FC(I,5))

        GOTO 180

C.......HN > H

170     IF(FC(NQ,5).LT.TOL)GOTO 200

        YBAR=AMIN1(HBAR,FC(NQ,5))

180     WETT=2.*YBAR+WID

        WETC=2.*DCH+WID

        WET=AMIN1(WETC,WETT)

        A=WID*HBAR

        R=A/WET

        XNBAR=.5*(FC(I,1)+FC(NQ,1))

        AGRAD=ABS(GRAD)

        IF(AGRAD.LT..00001)GOTO 200

        XK=(1.486/XNBAR)*R**0.667/SQRT(AGRAD)

        VEL=-XK*GRAD    

        QQ=VEL*WID*YBAR

200     CONTINUE

        RETURN

        END

C

        SUBROUTINE CHANPL(NNOD,SIDE,TOL)

C

C       THIS SUBROUTINE UPDATES THE WATER SUFRACE ELEVATION

C       BETWEEN THE FLOODPLAIN AND CHANNEL MODELS

C

        COMMON/BLK 1/FP(250,8),FC(250,6)

C

        DO 100 I=1,NNOD

        IF(FP(I,5).GT.0.)GOTO 100

        AC=0.

        KCO=0

        DO 200 JJ=1,4

        NQ=FP(I,JJ)

        IF(NQ.EQ.0)GO TO 200

        IF(FP(NQ,5).GT.0.)GOTO 200 

        AC=AC+(FC(NQ,2)+FC(I,2))*.25*SIDE

        KCO=KCO+1

200     CONTINUE

        IF(KCO.EQ.1)AC=2.*AC

        AP=SIDE*SIDE-AC

C.......CHECK INTERFACE BETWEEN CHANNEL AND FLOOD PLAIN

C.......A IS WATER LEVEL AT FLOOD PLAIN

C.......B IS WATER LEVEL AT CHANNEL

C.......FC(I,3) IS THE DEPTH OF CHANNEL

        A=FP(I,6)+FP(I,7)

        B=FC(I,4)+FC(I,5)

        IF(A.GT.B)GOTO 110

C.......FLOODING OF CHANNEL, B > A

        FP(I,7)=FP(I,7)+(B-A)*AC/(AP+AC)

        FC(I,5)=FP(I,7)+FC(I,3)

        GO TO 100

C.......FLOW INTO CHANNEL FROM GRID ELEMENT, A > B

110     IF(FC(I,3).LT.FC(I,5))GOTO 120

        VAL=(FC(I,3)-FC(I,5)+TOL)*AC

        VW=AP*(FP(I,7)-TOL)

C.......CASE 1 - NO FLOW INTO CHANNEL

        IF(VW.LE.0.)GOTO 100

        IF(VAL.GE.VW)GOTO 130

C.......CASE 2 - CHANNEL IS FULL AFTER FILLING

        FP(I,7)=TOL+(VW-VAL)/(AP+AC)

        FC(I,5)=FC(I,3)+FP(I,7)

        GO TO 100

C.......CASE 3 - FC(I,3) > FC(I,5)

130     FC(I,5)=FC(I,5)+VW/AC

        FP(I,7)=TOL

        GO TO 100

C.......CASE 4 - FC(I,5) > FC(I,3)

120     FC(I,5)=FC(I,5)+(A-B)*AP/(AP+AC)

        FP(I,7)=FC(I,5)-FC(I,3)

100     CONTINUE

        RETURN

        END


