C
C     This file contains a subset of the util, adwdm, and wdm
C     libraries.
C
      INTEGER   FUNCTION   CHKSTR
     I                           (LEN,NSTR,STR1,STR2)
C
C     Search thru STR2 for a match to the character array STR1.
C     Return the array location of the match, or a zero if there
C     is no match.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN,NSTR
      CHARACTER*1 STR1(LEN),STR2(LEN,NSTR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - size of character array
C     NSTR   - number of strings to be compared
C     STR1   - character array of size LEN to search for
C     STR2   - character array of size LEN,NSTR
C              to be searched for a match
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J,MAT
C
C     + + + END SPECIFICATIONS + + +
C
      CHKSTR = 0
      J = 1
C
  10  CONTINUE
        I = 1
        MAT = 1
  20    CONTINUE
          IF (STR1(I).NE.STR2(I,J)) MAT = 0
          I = I + 1
        IF (I.LE.LEN.AND.MAT.EQ.1) GO TO 20
        IF (MAT.EQ.1) CHKSTR = J
        J = J + 1
      IF (J.LE.NSTR.AND.CHKSTR.EQ.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZIPC
     I                  (LEN, ZIP,
     O                   X)
C
C     Fill the character array X of size LEN with
C     the given value ZIP.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN
      CHARACTER*1 ZIP,X(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - size of character array
C     ZIP    - character to fill array
C     X      - character array to be filled
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     L
C
C     + + + END SPECIFICATIONS + + +
C
      DO 100 L= 1, LEN
        X(L)= ZIP
 100  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   COPYI
     I                   (LEN, ZIP,
     O                    X)
C
C     + + + PURPOSE + + +
C     Copy the integer array ZIP of size LEN to
C     the integer array X.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN
      INTEGER     ZIP(LEN), X(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - size of arrays
C     ZIP    - input array of size LEN
C     X      - output array of size LEN
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     L
C
C     + + + END SPECIFICATIONS + + +
C
      DO 100 L = 1, LEN
         X(L) = ZIP(L)
  100 CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   CKDATE
     I                    (DATE1, DATE2,
     O                     FLAG)
C
C     + + + PURPOSE + + +
C     Determine the calendar order of two dates.  The dates are
C     assumed to be valid.
C     Examples:      DATE1               DATE2         FLAG
C              1980/10/1 00:00:00  1980/10/1 24:00:00   -1
C              1980/10/1 24:00:00  1980/10/1 00:00:00    1
C              1980/10/1 24:00:00  1980/10/2 00:00:00   -1
C              1980/10/2 00:00:00  1980/10/1 24:00:00    1
C              1980/10/1 24:00:00  1980/10/1 24:00:00    0
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DATE1(6), DATE2(6), FLAG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATE1  - first date
C     DATE2  - second date
C     FLAG   - flag indicating order of dates
C               1 - DATE1 follows DATE2
C               0 - DATE1 is the same date as DATE2
C              -1 - DATE2 follows DATE1
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  I
C
C     + + + END SPECIFICATIONS + + +
C
      FLAG = -99
      I = 0
C
 10   CONTINUE
        I = I + 1
C
        IF (DATE1(I) .LT. DATE2(I)) THEN
C         first date is before second date
          FLAG = -1
        ELSE
C         first date follows or equals second date
          IF (DATE1(I) .GT. DATE2(I)) THEN
C           first date follows second date
            FLAG = 1
          ELSE IF (I .EQ. 6) THEN
C           all parts of dates are equal
            FLAG = 0
          END IF
        END IF
C
      IF (I.LT.6 .AND. FLAG.EQ.-99) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   CMPTIM
     I                   ( TCODE1, TSTEP1, TCODE2, TSTEP2,
     O                     TSTEPF, TCDCMP )
C
C     + + + PURPOSE + + +
C     Compare one time unit and step to a second time unit and
C     step.  Two flags are returned.  The first flag indicates
C     compatible/incompatible time steps.  The second flag
C     indicates which time step is smaller.  Time steps are
C     considered compatible if one is an even multiple of the
C     other.  One hour and 30 minutes are compatible; one hour
C     and 90 minutes are incompatible.  Comparison of time units
C     and time steps which cross the day-month boundry are handled
C     a little different.  If the smaller time step is a day or
C     less and is compatible with 1 day and the larger time step
C     is compatible with one month, than the smaller and the
C     larger time steps are considered to be compatible.  The time
C     step of a day or less will be considered to be the smaller
C     time step.
C     EXAMPLES:  TCODE1 TSTEP1 TCODE2 TSTEP2 TSTEPF TCDCMP
C                  3      1      2      60     0      0
C                  3      1      2      90     1      1
C                  3      1      2      30     0      2
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  TCODE1, TSTEP1, TCODE2, TSTEP2, TSTEPF, TCDCMP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TCODE1 - time units code
C              1 - second    4 - day
C              2 - minute    5 - month
C              3 - hour      6 - year
C     TSTEP1 - time step, in TCODE1 units
C     TCODE2 - time units code
C              1 - second    4 - day
C              2 - minute    5 - month
C              3 - hour      6 - year
C     TSTEP2 - time step in TCODE2 units
C     TSTEPF - time step compatability flag
C              0 - compatible time steps
C              1 - incompatible time steps
C     TCDCMP - flag indicating order of time steps
C               0 - time steps are the same
C               1 - first time step is smaller
C               2 - second time step is smaller
C              -1 - time units span day-month boundry
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    TC(2), TS(2), TSX, TCX, TSFX(2), TCDX(2)
C
C     + + + EXTERNALS + + +
      EXTERNAL   CMPTM2
C
C     + + + END SPECIFICATIONS + + +
C
      TC(1) = TCODE1
      TC(2) = TCODE2
      TS(1) = TSTEP1
      TS(2) = TSTEP2
C
      IF (TC(1) .LT. 1  .OR.  TC(1) .GT. 6     .OR.
     >    TC(2) .LT. 1  .OR.  TC(2) .GT. 6     .OR.
     >    TS(1) .LT. 1  .OR.  TS(1) .GT. 1440  .OR.
     >    TS(2) .LT. 1  .OR.  TS(2) .GT. 1440) THEN
C       an invalid time units code or time step
        TSTEPF = 1
        TCDCMP = -1
      ELSE IF ((TC(1) .LE. 4  .AND.  TC(2) .GE. 5)  .OR.
     >         (TC(2) .LE. 4  .AND.  TC(1) .GE. 5)) THEN
C       special case for time units that cross day-month boundry
        TSTEPF = 1
        TCDCMP = -1
        IF (TC(1) .LE. 4) THEN
C         first time unit is day or smaller, second is month or larger
          TSX = 1
          TCX = 4
          CALL CMPTM2 ( TC(1), TS(1), TCX, TSX, TSFX(1), TCDX(1) )
          TSX = 1
          TCX = 5
          CALL CMPTM2 ( TC(2), TS(2), TCX, TSX, TSFX(2), TCDX(2) )
          IF (TSFX(1) .EQ. 0  .AND.  TSFX(2) .EQ. 0) THEN
C           times compatible with boundaries
            IF ((TCDX(1) .EQ. 0  .OR.  TCDX(1) .EQ. 1)  .AND.
     >          (TCDX(2) .EQ. 0  .OR.  TCDX(2) .EQ. 2)) THEN
C             smaller time a day or less, larger time a month or more
              TSTEPF = 0
              TCDCMP = 1
            END IF
          END IF
        ELSE
C         second time unit is day or smaller, first is month or larger
          TSX = 1
          TCX = 5
          CALL CMPTM2 ( TC(1), TS(1), TCX, TSX, TSFX(1), TCDX(1) )
          TSX = 1
          TCX = 4
          CALL CMPTM2 ( TC(2), TS(2), TCX, TSX, TSFX(2), TCDX(2) )
          IF (TSFX(1) .EQ. 0  .AND.  TSFX(2) .EQ. 0) THEN
C           times compatible with boundaries
            IF ((TCDX(1) .EQ. 0  .OR.  TCDX(1) .EQ. 2)  .AND.
     >          (TCDX(2) .EQ. 0  .OR.  TCDX(2) .EQ. 1)) THEN
C             larger time a month or more, smaller time a day or less
              TSTEPF = 0
              TCDCMP = 2
            END IF
          END IF
        END IF
      ELSE
C       valid time steps and units do not cross day-month boundry
        CALL CMPTM2 ( TC(1), TS(1), TC(2), TS(2), TSTEPF, TCDCMP )
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CMPTM2
     M                   ( TC1, TS1, TC2, TS2,
     O                     TSTEPF, TCDCMP )
C
C     + + + PURPOSE + + +
C     This routine compares one time unit and step to a second time
C     unit and step.  Two flags are returned.  The first flag
C     indicates compatible/incompatible time steps.  The second flag
C     indicates which timestep is smaller.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  TC1, TC2, TS1, TS2, TSTEPF, TCDCMP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TC1    - time units code
C              1 - second    4 - day
C              2 - minute    5 - month
C              3 - hour      6 - year
C     TS1    - time step, in TC1 units
C     TC2    - time units code, see TC1
C     TS2    - time step, in TC2 units
C     TSTEPF - time step compatability flag
C              0 - compatible time series
C              1 - incompatible time steps
C     TCDCMP - flag indicating order of time steps
C               0 - time steps are the same
C               1 - first time step is smaller
C               2 - second time step is smaller
C              -1 - time units span day-month boundry
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    CONVDN(7)
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  CONVDN / 0, 60, 60, 24, 0, 12, 100 /
C
C     + + + END SPECIFICATIONS + + +
C
      IF ((TC1 .LE. 4  .AND.  TC2 .GT. 4)  .OR.
     >    (TC1 .GT. 4  .AND.  TC2 .LE. 4)) THEN
C       time units span day-month boundry
        TSTEPF = 1
        TCDCMP = -1
      ELSE
C       acceptable time units
        IF (TC1 .NE. TC2) THEN
C         time units not same, adjust larger to agree with smaller
          IF (TC1 .LT. TC2) THEN
C           Adjust second time units to agree with first
 100        CONTINUE
              TS2 = TS2 * CONVDN(TC2)
              TC2 = TC2 - 1
            IF (TC1 .LT. TC2) GO TO 100
          ELSE
C           Adjust first time units to agree with second
 120        CONTINUE
              TS1 = TS1 * CONVDN(TC1)
              TC1 = TC1 - 1
            IF (TC2 .LT. TC1) GO TO 120
          END IF
        END IF
C
C       Time units converted, check time step
        TSTEPF = 0
        IF (TS1 .EQ. TS2) THEN
C         Same time step
          TCDCMP = 0
        ELSE IF (TS1 .LT. TS2) THEN
C         First time step smaller
          TCDCMP = 1
          IF (MOD(TS2,TS1) .NE. 0) TSTEPF = 1
        ELSE
C         Second time step smaller
          TCDCMP = 2
          IF (MOD(TS1,TS2) .NE. 0) TSTEPF = 1
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DATNXT
     I                   (INTRVL,UPBACK,
     M                    DATE)
C
C     + + + PURPOSE + + +
C     Based on the value of UPBACK, this routine adds or subtracts
C     the time interval INTRVL from the current date and time DATE.
C     The time convention has midnite as 24:00 of previous day, not
C     as 00:00 of next day.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DATE(6),UPBACK
      INTEGER*4 INTRVL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INTRVL - time step, in minutes
C     UPBACK - flag indicating direction in time to move:
C              >0  - move forward in time
C              <=0 - move back in time
C     DATE   - date to be modified
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   YEAR,MONTH,DAY,HR,MIN,SEC
      INTEGER*4 DHR,DMIN,TT,I4T24,I4T60,I4T0
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMON
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYMON
C
C     + + + END SPECIFICATIONS + + +
C
      I4T24= 24
      I4T60= 60
      I4T0 = 0
C
      YEAR = DATE(1)
      MONTH = DATE(2)
      DAY = DATE(3)
      HR = DATE(4)
      MIN = DATE(5)
      SEC = DATE(6)
C
      DMIN = MIN
      DHR = HR
C
      IF (UPBACK.GT.0) THEN
C       move forward in time
        DMIN = DMIN + INTRVL
        IF (DMIN.LT.I4T60.AND.DHR.LT.I4T24) GO TO 49
          TT = DMIN/60
          DMIN = DMIN - TT*60
          DHR = DHR + TT
          IF (DHR.LT.I4T24) GO TO 48
          IF (DHR.EQ.I4T24.AND.DMIN.EQ.I4T0) GO TO 48
            TT = DHR/24
C           special case for daily timestep to keep convention
            IF (MOD(DHR,I4T24).EQ.0.AND.DMIN.EQ.I4T0) TT = TT-1
            DHR = DHR - TT*24
            DAY = DAY + TT
 40         CONTINUE
            IF (DAY.GT.DAYMON(YEAR,MONTH)) THEN
              DAY = DAY - DAYMON(YEAR,MONTH)
              MONTH = MONTH + 1
              IF (MONTH.GT.12) THEN
                MONTH = 1
                YEAR = YEAR + 1
              END IF
              GO TO 40
            END IF
 48       CONTINUE
 49     CONTINUE
      ELSE
C       move back in time
        DMIN = DMIN - INTRVL
        IF (DMIN.LE.I4T0) THEN
          TT = DMIN/60 - 1
          DMIN = DMIN-TT*60
          DHR = DHR + TT
          IF (DMIN.EQ.I4T60) THEN
            DMIN = 0
            DHR = DHR + 1
          END IF
          IF (DHR.LE.I4T0) THEN
            TT = DHR/24 - 1
            DHR = DHR-TT*24
            DAY = DAY + TT
  70        CONTINUE
            IF (DAY.LE.0) THEN
              MONTH = MONTH - 1
              IF (MONTH.LE.0) THEN
                MONTH = 12
                YEAR = YEAR - 1
              END IF
              DAY = DAYMON(YEAR,MONTH) + DAY
              GO TO 70
            END IF
          END IF
        END IF
      END IF
C
      MIN = DMIN
      HR = DHR
      DATE(1) = YEAR
      DATE(2) = MONTH
      DATE(3) = DAY
      DATE(4) = HR
      DATE(5) = MIN
      DATE(6) = SEC
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   DAYMON
     I                           (YR,MON)
C
C     + + + PURPOSE + + +
C     Return the number of days in the given month for the given
C     year, with leap year taken into account.  For an invalid
C     month, -1 is returned.  For an invalid year and a valid month,
C     the correct number of days is returned, with February = 28.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    MON,YR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YR     - year, valid range is 1 - 2080
C     MON    - month, valid range is 1 - 12
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I4,I100,I400,NDAMON(12)
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + DATA INITIALIZATIONS + + +
      DATA NDAMON/31,28,31,30,31,30,31,31,30,31,30,31/
C
C     + + + END SPECIFICATIONS + + +
C
      I4 = 4
      I100 = 100
      I400 = 400
      IF (MON.EQ.2) THEN
        IF (YR .LE. 0  .OR.  YR .GT. 2080) THEN
C         invalid year
          DAYMON = 28
        ELSE IF (MOD(YR,I100).EQ.0) THEN
C         check whether this is a leap year on a century boundary
          IF (MOD(YR,I400).EQ.0) THEN
C           on a 400 year boundary
            DAYMON = 29
          ELSE
            DAYMON = 28
          END IF
        ELSE
          IF (MOD(YR,I4).EQ.0) THEN
C           leap year
            DAYMON = 29
          ELSE
            DAYMON = 28
          END IF
        END IF
C
      ELSE IF (MON.GE.1 .AND. MON.LE.12) THEN
C       no problem
        DAYMON = NDAMON(MON)
      ELSE
C       invalid month
        DAYMON = -1
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   NUMPTS
     I                   (DATE1,DATE2,DELT,
     O                    NPTS)
C
C     + + + PURPOSE + + +
C     Calculate the number of time steps between two dates.
C     If the second date is before the first date, a zero is
C     returned.  The first date is assumed to be at the end
C     of the first time step.  The dates are assumed to be valid.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DATE1(6),DATE2(6),DELT,NPTS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATE1  - first (start) date
C     DATE2  - second (end) date
C     DELT   - time step, in minutes
C     NPTS   - number of time steps between first and second date
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   NPD,YR,M,SYR,SMO,SDY,SHR,SMI,EYR,EMO,EDY,EHR,EMI,
     1          ERRFLG
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMON
C
C     + + + EXTERNALS + + +
      EXTERNAL   CKDATE, DAYMON
C
C     + + + END SPECIFICATIONS + + +
C
      SYR = DATE1(1)
      SMO = DATE1(2)
      SDY = DATE1(3)
      SHR = DATE1(4)
      SMI = DATE1(5)
      EYR = DATE2(1)
      EMO = DATE2(2)
      EDY = DATE2(3)
      EHR = DATE2(4)
      EMI = DATE2(5)
C
      NPTS = 0
C     number per day
      NPD = 1440/DELT
      YR = SYR
      M = SMO
C
C     if end date before start date, return npts of 0
      CALL CKDATE (DATE1,DATE2,ERRFLG)
      IF (ERRFLG.LE.0) THEN
C
        IF (SMO.LT.EMO.OR.YR.LT.EYR) THEN
C         start and stop not same month
          NPTS = NPTS + (DAYMON(SYR,SMO)+1-SDY)*NPD
C
C         middle months
 56       CONTINUE
            M = M + 1
            IF (M.GT.12) THEN
              M = 1
              YR = YR + 1
            END IF
C
            IF (M.EQ.EMO.AND.YR.GE.EYR) GO TO 58
              NPTS = NPTS + DAYMON(YR,M)*NPD
              GO TO 56
C
 58       CONTINUE
C         final month
          NPTS = NPTS + EDY*NPD
        ELSE
C         start and stop in the same month
          NPTS = NPTS + (EDY+1-SDY)*NPD
        END IF
C
C       have correct number of days
        IF (NPD.GT.1) THEN
C         for timesteps less than one day
C         first day
          NPTS = NPTS - (SHR*60 + SMI)/DELT + 1
C         last day
          NPTS = NPTS - ((23-EHR)*60 + (60-EMI))/DELT
        END IF
C
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TIMADD
     I                    (DATE1,TCODE,TSTEP,NVALS,
     O                     DATE2)
C
C     + + + PURPOSE + + +
C     Add NVALS time steps to first date to compute second date.
C     The first date is assumed to be valid.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DATE1(6),TCODE,TSTEP,DATE2(6)
      INTEGER*4 NVALS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATE1  - starting date
C     TCODE  - time units
C              1 - second          5 - month
C              2 - minute          6 - year
C              3 - hour            7 - century
C              4 - day
C     TSTEP  - time step in TCODE units
C     NVALS  - number of time steps to be added
C     DATE2  - new date
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4 CARRY,TIMTMP(6),I4ZRO,I4TMP,DPM
      INTEGER   I,STPOS,DONFG,IYR,IMO
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMON
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYMON
C
C     + + + END SPECIFICATIONS + + +
C
      I4ZRO= 0
      DO 5 I= 1,6
        TIMTMP(I)= DATE1(I)
 5    CONTINUE
C
C     figure out how much time to add and where to start
      CARRY= NVALS* TSTEP
      STPOS= TCODE
      IF (STPOS.EQ.7) THEN
C       the time units are centuries, convert to years
        STPOS= 6
        CARRY= CARRY* 100
      END IF
C
C     add the time, not changing insig. parts
      GO TO (10,20,30,40,50,60), STPOS
C
 10   CONTINUE
C       seconds
        TIMTMP(6)= TIMTMP(6)+ CARRY
        CARRY    = TIMTMP(6)/ 60
        TIMTMP(6)= TIMTMP(6)- (CARRY*60)
 20   CONTINUE
C       minutes
        TIMTMP(5)= TIMTMP(5)+ CARRY
        CARRY    = TIMTMP(5)/ 60
        TIMTMP(5)= TIMTMP(5)- (CARRY*60)
 30   CONTINUE
C       hours
        TIMTMP(4)= TIMTMP(4)+ CARRY
        CARRY    = TIMTMP(4)/ 24
        TIMTMP(4)= TIMTMP(4)- (CARRY*24)
        IF (TIMTMP(4).EQ.I4ZRO.AND.TIMTMP(5).EQ.I4ZRO.AND.
     1      TIMTMP(6).EQ.I4ZRO) THEN
C         this is the day boundry problem
          TIMTMP(4)= 24
          CARRY = CARRY- 1
        END IF
 40   CONTINUE
C       days
        TIMTMP(3)= TIMTMP(3)+ CARRY
        DONFG= 0
 45     CONTINUE
          IYR= TIMTMP(1)
          IMO= TIMTMP(2)
          DPM= DAYMON(IYR,IMO)
          IF (TIMTMP(3).GT.DPM) THEN
C           add another month
            TIMTMP(3)= TIMTMP(3)- DPM
            TIMTMP(2)= TIMTMP(2)+ 1
            I4TMP= 12
            IF (TIMTMP(2).GT.I4TMP) THEN
              TIMTMP(2)= 1
              TIMTMP(1)= TIMTMP(1)+ 1
            END IF
          ELSE IF (TIMTMP(3).LE.0) THEN
C           subtract another month
            TIMTMP(2)= TIMTMP(2)- 1
            I4TMP= 0
            IF (TIMTMP(2).EQ.I4TMP) THEN
              TIMTMP(1)= TIMTMP(1)- 1
              TIMTMP(2)= 12
            END IF
            IYR= TIMTMP(1)
            IMO= TIMTMP(2)
            TIMTMP(3)= TIMTMP(3)- DAYMON(IYR,IMO)
          ELSE
            DONFG= 1
          END IF
        IF (DONFG.EQ.0) GO TO 45
C       month and year updated here, so
        CARRY= 0
 50   CONTINUE
C       months
        TIMTMP(2)= TIMTMP(2)+ CARRY
        CARRY    = (TIMTMP(2)-1)/ 12
        TIMTMP(2)= TIMTMP(2)- (CARRY*12)
 60   CONTINUE
C       years
        TIMTMP(1)= TIMTMP(1)+ CARRY
C     end computed go to
      IF (TCODE.GE.5) THEN
C       check days/month
        IYR= TIMTMP(1)
        IMO= TIMTMP(2)
        I4TMP= DAYMON(IYR,IMO)
        IF (I4TMP.LT.TIMTMP(3)) TIMTMP(3)= I4TMP
        IF (DAYMON(DATE1(1),DATE1(2)).EQ.DATE1(3)) TIMTMP(3)= I4TMP
      END IF
C
      DO 100 I= 1,6
        DATE2(I)= TIMTMP(I)
 100  CONTINUE
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   TIMCHK
     I                            (DATE1,DATE2)
C
C     + + + PURPOSE + + +
C     Determine the calendar order of two dates.
C     The dates are assumed to be valid.
C     TIMCHK = 1 if DATE1 < DATE2
C            = 0 if DATE1 = DATE2
C            =-1 if DATE1 > DATE2
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DATE1(6),DATE2(6)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATE1  - first date
C     DATE2  - second date
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,TMSDAT(6),TMEDAT(6),LEN
C
C     + + + EXTERNALS + + +
      EXTERNAL   TIMCNV, COPYI
C
C     + + + END SPECIFICATIONS + + +
C
C     make a copy of the dates
      LEN = 6
      CALL COPYI( LEN, DATE1, TMSDAT )
      CALL COPYI( LEN, DATE2, TMEDAT )
C     convert dates to old format
      CALL TIMCNV (TMSDAT)
      CALL TIMCNV (TMEDAT)
C
C     ***************************
C     how about trying this???
C     CALL CKDATE (TMEDAT,TMSDAT,I)
C     instead of the rest of this wonderful code
C     ***************************
C
      I= 0
C     check years
      IF (TMSDAT(1).LT.TMEDAT(1)) THEN
        I= 1
      ELSE IF (TMSDAT(1).GT.TMEDAT(1)) THEN
        I= -1
      ELSE IF (TMSDAT(2).LT.TMEDAT(2)) THEN
C     checking months
        I= 1
      ELSE IF (TMSDAT(2).GT.TMEDAT(2)) THEN
        I= -1
      ELSE IF (TMSDAT(3).LT.TMEDAT(3)) THEN
C     checking days
        I= 1
      ELSE IF (TMSDAT(3).GT.TMEDAT(3)) THEN
        I= -1
      ELSE IF (TMSDAT(4).LT.TMEDAT(4)) THEN
C     checking hours
        I= 1
      ELSE IF (TMSDAT(4).GT.TMEDAT(4)) THEN
        I= -1
      ELSE IF (TMSDAT(5).LT.TMEDAT(5)) THEN
C     checking minutes
        I= 1
      ELSE IF (TMSDAT(5).GT.TMEDAT(5)) THEN
        I= -1
      ELSE IF (TMSDAT(6).LT.TMEDAT(6)) THEN
C     checking seconds
        I= 1
      ELSE IF (TMSDAT(6).GT.TMEDAT(6)) THEN
        I= -1
      END IF
C
      TIMCHK= I
C
      RETURN
      END
C
C
C
      SUBROUTINE   TIMCNV
     M                    (DATE)
C
C     + + + PURPOSE + + +
C     Convert a date that uses the midnight convention of 00:00
C     to the convention 24:00.  For example, 1982/10/01 00:00:00
C     would be converted to the date 1982/09/30 24:00:00.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DATE(6)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATE   - date being converted
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMON
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYMON
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DATE(4).EQ.0.AND.DATE(5).EQ.0.AND.DATE(6).EQ.0) THEN
C       date using new day boundry convention, convert to old
        DATE(4)= 24
        DATE(3)= DATE(3)- 1
        IF (DATE(3).EQ.0) THEN
          DATE(2)= DATE(2)- 1
          IF (DATE(2).EQ.0) THEN
            DATE(1)= DATE(1)- 1
            DATE(2)= 12
          END IF
          DATE(3)= DAYMON(DATE(1),DATE(2))
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TIMDIF
     I                    (DATE1,DATE2,TCODE,TSTEP,
     O                     NVALS)
C
C     + + + PURPOSE + + +
C     Calculate the number of time steps between two dates.  Part
C     intervals at a time step less than TCODE and TSSTEP are not
C     included.  If the second date is before the first date, or the
C     second date is the same as the first date, the number of time
C     steps will be returned as 0.  Dates are assumed to be valid.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DATE1(6),DATE2(6),TCODE,TSTEP
      INTEGER*4 NVALS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATE1  - first (starting) date
C     DATE2  - second (ending) date
C     TCODE  - time units code
C              1 - seconds     5 - months
C              2 - minutes     6 - years
C              3 - hours       7 - centuries
C              4 - days
C     TSTEP  - time step in TCODE units
C     NVALS  - number of time steps between DATE1 and DATE2
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   NDAYS,DONFG,I,NADJ,TMPSTR(6),TMPEND(6),LEN
      INTEGER*4 TM4STR(6),TM4END(6),I4TMP
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMON, TIMCHK
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYMON, TIMCNV, COPYI, TIMADD, TIMCHK
C
C     + + + END SPECIFICATIONS + + +
C
      IF (TIMCHK(DATE1,DATE2) .EQ. 1) THEN
C       end date follows start date, make temp copies of dates
        LEN = 6
        CALL COPYI ( LEN, DATE1, TMPSTR )
        CALL COPYI ( LEN, DATE2, TMPEND )
C
C       convert dates to old format
        CALL TIMCNV (TMPSTR)
        CALL TIMCNV (TMPEND)
C
C       copy dates to I*4 to preserve precision
        DO 2 I= 1,6
          TM4STR(I)= TMPSTR(I)
          TM4END(I)= TMPEND(I)
 2      CONTINUE
C
        GO TO (5,5,5,5,50,60,70), TCODE
 5      CONTINUE
C         figure out how many days
          DONFG= 0
          NDAYS= -TM4STR(3)
 8        CONTINUE
            IF (TM4STR(1).LT.TM4END(1).OR.
     1        (TM4STR(1).EQ.TM4END(1).AND.TM4STR(2).LT.TM4END(2))) THEN
              TMPSTR(1)= TM4STR(1)
              TMPSTR(2)= TM4STR(2)
              NDAYS= NDAYS+ DAYMON(TMPSTR(1),TMPSTR(2))
              TM4STR(2)= TM4STR(2)+ 1
              I4TMP= 13
              IF (TM4STR(2).EQ.I4TMP) THEN
                TM4STR(2)= 1
                TM4STR(1)= TM4STR(1)+ 1
              END IF
            ELSE
              DONFG= 1
            END IF
          IF (DONFG.EQ.0) GO TO 8
          NDAYS= NDAYS+ TM4END(3)
C
          GO TO (10,20,30,40),TCODE
 10       CONTINUE
C           seconds
            NVALS= ((((NDAYS*24)+
     1               TM4END(4)-TM4STR(4))* 60+
     2              TM4END(5)-TM4STR(5))* 60+
     3             TM4END(6)-TM4STR(6))/ TSTEP
            GO TO 45
 20       CONTINUE
C           minutes
            NVALS= (((NDAYS*24)+
     1              TM4END(4)-TM4STR(4))* 60+
     2             TM4END(5)-TM4STR(5))/ TSTEP
            GO TO 45
 30       CONTINUE
C           hours
            NVALS= ((NDAYS*24)+
     1             TM4END(4)- TM4STR(4))/ TSTEP
            GO TO 45
 40       CONTINUE
C           days
            NVALS= NDAYS/ TSTEP
 45       CONTINUE
          GO TO 90
C
 50     CONTINUE
C         months
          NVALS= ((TM4END(1)-TM4STR(1))*12+TM4END(2)-TM4STR(2))/TSTEP
          GO TO 90
C
 60     CONTINUE
C         years
          NVALS= (TM4END(1)-TM4STR(1))/ TSTEP
          GO TO 90
C
 70     CONTINUE
C         centuries
          NVALS= (TM4END(1)-TM4STR(1))/(TSTEP*100)
          GO TO 90
C
 90     CONTINUE
C
        DO 95 I= 1,6
          TMPSTR(I)= TM4STR(I)
          TMPEND(I)= TM4END(I)
 95     CONTINUE
        DONFG= 0
100     CONTINUE
          CALL TIMADD (DATE1,TCODE,TSTEP,NVALS,
     O                 TMPEND)
          NADJ= TIMCHK(DATE2,TMPEND)
          IF (NADJ.EQ.1.AND.NVALS.GE.1) THEN
C           estimate too high
            NVALS= NVALS- 1
          ELSE
C           estimate ok
            DONFG= 1
          END IF
        IF (DONFG.EQ.0) GO TO 100
      ELSE
C       end date is the same as or before start date
        NVALS = 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TIMDFX
     I                    (DATE1,DATE2,
     O                     NVALS,TCODE,TSTEP)
C
C     + + + PURPOSE + + +
C     Calculate the number of values between two dates, including
C     units and time step.  First tries at one year time step, then
C     TCODE is decreased by one and tried again until the exact
C     time difference is determined.  Dates are assumed to be valid.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DATE1(6),DATE2(6),TCODE,TSTEP
      INTEGER*4 NVALS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATE1 - first (starting) date
C     DATE2 - second (ending) date
C     NVALS  - number of values at the output TSTEP and TCODE
C     TCODE  - time units code
C              1 - seconds     5 - months
C              2 - minutes     6 - years
C              3 - hours       7 - centuries
C              4 - days
C     TSTEP  - time step in TCODE units
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   DONFG,TIMTMP(6)
C
C     + + + FUNCTIONS + + +
      INTEGER   TIMCHK
C
C     + + + EXTERNALS + + +
      EXTERNAL   TIMADD, TIMCHK, TIMDIF
C
C     + + + END SPECIFICATIONS + + +
C
      TCODE= 6
      TSTEP= 1
      DONFG= 0
C
 10   CONTINUE
        CALL TIMDIF (DATE1,DATE2,TCODE,TSTEP,
     O               NVALS)
        CALL TIMADD (DATE1,TCODE,TSTEP,NVALS,
     O               TIMTMP)
        IF (TIMCHK(DATE2,TIMTMP).EQ.0) THEN
C         we have call exact units, etc
          DONFG= 1
        ELSE
C         try again with shorter units
          TCODE= TCODE- 1
          IF (TCODE.EQ.0) WRITE (*,*) 'BAD TIMDFX',DATE1,DATE2
        END IF
C
      IF (DONFG.EQ.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   CHKINT
     I                    (IMIN,IMAX,IDEF,
     M                     IVAL,
     O                     ICHK)
C
C     + + + PURPOSE + + +
C     Check the integer IVAL against the minimum (IMIN)
C     and maximum (IMAX) values.  IVAL is set to the
C     default (IDEF) if IVAL is zero.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ICHK,IMIN,IMAX,IDEF,IVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IMIN   - minimum allowable value, -999 if there is no minimum
C     IMAX   - maximum allowable value, -999 if there is no maximum
C     IDEF   - default value, or -999 if there is no default
C     IVAL   - value to be checked
C     ICHK   - indicator flag for valid IVAL
C              0 - invalid IVAL
C              1 - valid IVAL
C
C     + + + END SPECIFICATIONS + + +
C
      ICHK = 0
      IF (IVAL.EQ.0) IVAL = IDEF
      IF (IVAL.GE.IMIN.AND.IVAL.LE.IMAX) ICHK = 1
      IF (IVAL.GE.IMIN.AND.IMAX.EQ.-999) ICHK = 1
      IF (IMIN.EQ.-999.AND.IVAL.LE.IMAX) ICHK = 1
      IF (IMIN.EQ.-999.AND.IMAX.EQ.-999) ICHK = 1
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZIPI
     I                  (LEN, ZIP,
     O                   X)
C
C     + + + PURPOSE + + +
C     Fill the integer array X of size LEN with
C     the given value ZIP.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN, ZIP
      INTEGER     X(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - size of array
C     ZIP    - value to fill array
C     X      - output array of size LEN
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     L
C
C     + + + END SPECIFICATIONS + + +
C
      DO 100 L = 1, LEN
         X(L) = ZIP
  100 CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZIPR
     I                  (LEN, ZIP,
     O                   X)
C
C     + + + PURPOSE + + +
C     Fill the real array X of size LEN with the
C     given value ZIP.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN
      REAL        ZIP, X(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - size of array
C     ZIP    - value to fill array
C     X      - output array of size LEN
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     L
C
C     + + + END SPECIFICATIONS + + +
C
      DO 100 L = 1, LEN
         X(L) = ZIP
  100 CONTINUE
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   WDRCGO
     I                           (WDMSFL,RREC)
C
C     + + + PURPOSE + + +
C     Determine the index of the user requested record within
C     the WDM in memory buffer of records.  The record is read
C     from the WDM file and pointers are updated, as required.
C     If the record is already in the buffer, the index is returned.
C     If the record is not in the buffer, the oldest record in
C     the buffer is replaced with the record read from the WDM file.
C     For a negative RREC, the index of an empty record is returned.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,RREC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of the WDM file
C     RREC   - record number to find and place in buffer
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   IND,RIND,PIND,NIND,XIND,I,CWDM,DONFG
C
C     + + + INTRINSICS + + +
      INTRINSIC IABS
C
C     + + + END SPECIFICATIONS + + +
C
      RIND = 0
      CWDM = 0
      DONFG= 0
C
 10   CONTINUE
        CWDM= CWDM+ 1
        IF (WDMOPN(CWDM).EQ.WDMSFL) THEN
C         we know about this wdm file
          DONFG= 1
        END IF
      IF (CWDM.LT.WDMCNT .AND. DONFG.EQ.0) GO TO 10
C
      IF (DONFG.EQ.1) THEN
C       working with a valid file
        IF (IABS(RREC).LE.MAXREC(CWDM)) THEN
C         record is within allowable range
          IF (RREC.GE.0) THEN
C           looking for an existing record
            IND = 0
 30         CONTINUE
C             look for desired record in currently avail records
              IND= IND+ 1
              IF (RECNO(IND).EQ.RREC .AND. WDMFUN(IND).EQ.WDMSFL) THEN
C               its already in memory
                RIND= IND
              END IF
            IF (IND.LT.CONREC.AND.RIND.EQ.0) GO TO 30
          END IF
C
          IF (RIND.EQ.0) THEN
C           record not found, allocate space for it
            RIND= FREPOS
            IF (RREC.GT.0) THEN
C             read existing record from file
              READ (WDMSFL,REC=RREC) (WIBUFF(I,RIND),I=1,512)
            ELSE
C             return an empty buffer
              DO 40 I= 1,512
                WIBUFF(I,RIND)= 0
 40           CONTINUE
            END IF
C
            FREPOS      = NXTPOS(FREPOS)
            RECNO(RIND) = IABS(RREC)
            WDMFUN(RIND)= WDMSFL
          ELSE
C           record found, update pointers to use this buffer space last
            IF (RIND.EQ.FREPOS .OR. PREPOS(FREPOS).EQ.RIND) THEN
C             pointers are ok
              IF (RIND.EQ.FREPOS) THEN
C               update frepos
                FREPOS= NXTPOS(RIND)
              END IF
            ELSE
C             forward pointer first
              NIND        = PREPOS(FREPOS)
              PIND        = PREPOS(RIND)
              XIND        = NXTPOS(RIND)
              NXTPOS(PIND)= NXTPOS(RIND)
              NXTPOS(RIND)= FREPOS
              NXTPOS(NIND)= RIND
C             now back pointers
              PREPOS(FREPOS)= RIND
              PREPOS(XIND)  = PREPOS(RIND)
              PREPOS(RIND)  = NIND
            END IF
          END IF
        END IF
      END IF
C
      IF (RIND .LT. 1 .OR. RIND .GT. CONREC) THEN
C       we have got a bad problem
        WRITE(99,*) ' IN WDRCGO: RIND,MAXREC,RREC=',RIND,MAXREC,RREC
      END IF
C
      WDRCGO= RIND
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDFLCK
     I                   (WDMSFL,
     O                    RETCOD)
C
C     + + + PURPOSE + + +
C     Check directory of WDM for major errors.  Checks version number
C     and updates old version of WDM file to current version, when required.
C     Determines the number of records in the WDM file and adds that
C     value to the common block CFBUFF.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     RETCOD - return code
C               0 - everything ok
C             -89 - WDM file is invalid
C             -88 - no room for another WDM file
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
      INCLUDE 'cdrloc.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RREC,RIND,I,J
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDRCGO, WDRCUP
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
      I     = 0
C
      IF (WDMCNT.GT.0) THEN
C       see if file already open
 10     CONTINUE
          I= I+ 1
          IF (WDMOPN(I).EQ.WDMSFL) THEN
C           already open
            I= WDMCNT+ 1
          END IF
        IF (I.LT.WDMCNT) GO TO 10
      END IF
C
      IF (I.EQ.WDMCNT) THEN
C       is there room for another WDM file?
        IF (WDMCNT.LT.MXWDM) THEN
C         another file allowed, save info about wdm file
          WDMCNT= WDMCNT+ 1
          WDMOPN(WDMCNT)= WDMSFL
          MAXREC(WDMCNT)= 1
C         check new wdm file
          RREC= 1
          RIND= WDRCGO(WDMSFL,RREC)
C
C         check first position pointer
          IF (WIBUFF(1,RIND).EQ.-998) THEN
C           first position is ok, save max number of records
            MAXREC(WDMCNT)= WIBUFF(PMXREC,RIND)
          ELSE IF (WIBUFF(1,RIND).EQ.-999) THEN
C           old version, update first record
            DO 20 I= 439,40,-1
              J= I+ 73
              WIBUFF(J,RIND)= WIBUFF(I,RIND)
 20         CONTINUE
            DO 30 I= 40,112
              WIBUFF(I,RIND)= 0
 30         CONTINUE
C           indicate now new version
            WIBUFF(1,RIND)= -998
            CALL WDRCUP (WDMSFL,RIND)
            MAXREC(WDMCNT)= WIBUFF(PMXREC,RIND)
          ELSE
C           first position incorrect, return error
            RETCOD= -89
            WDMOPN(WDMCNT)= 0
            MAXREC(WDMCNT)= 0
            WDMCNT= WDMCNT- 1
          END IF
        ELSE
C         cant have another wdm file open
          RETCOD= -88
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDFLCL
     I                   (WDMSFL,
     O                    RETCOD)
C
C     + + + PURPOSE + + +
C     Remove a WDM file from the open WDM buffer and adjust
C     buffer accordingly.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     RETCOD - return code
C                0 - everything ok
C              -87 - can't remove message WDM file from buffer
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
C
      IF (WDMCNT.GT.1) THEN
C       see if file already open
        I= 1
 10     CONTINUE
          I= I+ 1
          IF (WDMOPN(I).EQ.WDMSFL) THEN
C           its open
            IF (I.LT.WDMCNT) THEN
C             move later wdm unit numbers
              DO 20 J= I+1,WDMCNT
                WDMOPN(J-1)= WDMOPN(J)
                MAXREC(J-1)= MAXREC(J)
 20           CONTINUE
            END IF
C           close the file
            CLOSE (UNIT=WDMSFL)
C           reset the count
            WDMCNT= WDMCNT- 1
C           mark records in buffer as not usable
            DO 30 I= 1,CONREC
              IF (WDMFUN(I).EQ.WDMSFL) THEN
C               unusable
                WDMFUN(I)= 0
              END IF
 30         CONTINUE
          END IF
        IF (I.LT.WDMCNT) GO TO 10
      ELSE
C       cant close message file
        RETCOD= -87
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDRCUP
     I                   (WDMSFL,RIND)
C
C     + + + PURPOSE + + +
C     Write record index number RIND from the buffer of records
C     to the WDM file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,RIND
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     RIND   - buffer index number of record to write
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RREC,I
      INTEGER*4 IOS
C
C     + + + END SPECIFICATIONS + + +
C
      RREC= RECNO(RIND)
      WRITE (WDMSFL,REC=RREC,ERR=10,IOSTAT=IOS) (WIBUFF(I,RIND),I=1,512)
      GO TO 20
 10   CONTINUE
C       big problem writing to wdm file
        WRITE (*,*) 'WDRCUP, ERROR ON WRITE, RREC, IOS:',RREC,IOS
C       STOP
 20   CONTINUE
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   WDRCGN
     I                           (WDMSFL,PRPRRC,SCPRRC)
C
C     + + + PURPOSE + + +
C     Get the next free record from the WDM file and add it to
C     the WDM buffer of records.  Update pointers on WDM file for
C     primary and secondary records and initialize new record pointers.
C     Returns index number in WDM buffer of records.  Returns 0 if no
C     record available.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,PRPRRC,SCPRRC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     PRPRRC - primary record pointer
C     SCPRRC - secondary record pointer
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
      INCLUDE 'cdrloc.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RREC,RIND,NEWREC,PPREC,PNREC,SPREC,SNREC,FNREC
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + INTRINSICS + + +
      INTRINSIC   IABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDRCGO, WDRCUP
C
C     + + + END SPECIFICATIONS + + +
C
      WDRCGN= 0
C     get directory record
      RREC= 1
      RIND= WDRCGO(WDMSFL,RREC)
C     get next free record number
      NEWREC= WIBUFF(PFRREC,RIND)
      IF (NEWREC.NE.0) THEN
C       record is available, use it
        IF (PRPRRC.NE.0.OR.SCPRRC.NE.0) THEN
C         update previous records
          PPREC= IABS(PRPRRC)
          SPREC= SCPRRC
          IF (PRPRRC.GT.0) THEN
C           get primary previous record
            RIND= WDRCGO(WDMSFL,PPREC)
C           save old next record pointer
            PNREC= WIBUFF(2,RIND)
C           update next record pointer
            WIBUFF(2,RIND)= NEWREC
            IF (PPREC.NE.SCPRRC) THEN
C             write record
              CALL WDRCUP (WDMSFL,RIND)
            END IF
          ELSE
C           no primary previous record
            PNREC= 0
          END IF
C
          IF (SPREC.NE.0) THEN
C           get secondary previous record
            RIND= WDRCGO(WDMSFL,SPREC)
C           save old next record pointer
            SNREC= WIBUFF(4,RIND)
C           update next record pointer
            WIBUFF(4,RIND)= NEWREC
C           write record
            CALL WDRCUP(WDMSFL,RIND)
          ELSE
            SNREC= 0
          END IF
C
C         update next records
          IF (PNREC.NE.0) THEN
C           get primary previous next record
            RIND= WDRCGO(WDMSFL,PNREC)
C           update previous rec pointer
            WIBUFF(1,RIND)= NEWREC
            IF (PNREC.NE.SNREC) THEN
C             write record
              CALL WDRCUP(WDMSFL,RIND)
            END IF
          END IF
          IF (SNREC.GT.0) THEN
C           get secondary next record
            RIND= WDRCGO(WDMSFL,SNREC)
C           update previous record pointer
            WIBUFF (3,RIND)= NEWREC
C           write record
            CALL WDRCUP(WDMSFL,RIND)
          END IF
        ELSE
C         no pointers in use (directory record)
          PPREC= 0
          PNREC= 0
          SPREC= 0
          SNREC= 0
        END IF
C
C       initialize nex record
C       get next record pointer
        RIND= WDRCGO(WDMSFL,NEWREC)
        FNREC= WIBUFF(2,RIND)
C       fix pointers on new record
        WIBUFF(1,RIND)= PPREC
        WIBUFF(2,RIND)= PNREC
        WIBUFF(3,RIND)= SPREC
        WIBUFF(4,RIND)= SNREC
C       write new record
        CALL WDRCUP(WDMSFL,RIND)
C       update directory
        RREC= 1
        RIND= WDRCGO(WDMSFL,RREC)
        WIBUFF(PFRREC,RIND)= FNREC
        CALL WDRCUP(WDMSFL,RIND)
C       return index of new record
        WDRCGN= WDRCGO(WDMSFL,NEWREC)
      END IF
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   WDRCGX
     I                           (WDMSFL,PRPRRC,SCPRRC)
C
C     + + + PURPOSE + + +
C     Get the next free record from the WDM file. If no free records
C     are available, add twenty records to the WDM file.  Add free
C     record to the WDM buffer of records.  Update pointers on WDM file
C     for primary and secondary records and initialize new record pointers.
C     Returns index number in WDM buffer of records.  Returns 0 if no
C     record available.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,PRPRRC,SCPRRC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     PRPRRC - primary record pointer
C     SCPRRC - secondary record pointer
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RIND,NUMADD,FREREC
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGN
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDRCGN, WDRCAD
C
C     + + + END SPECIFICATIONS + + +
C
C     try to get new record
      RIND= WDRCGN(WDMSFL,PRPRRC,SCPRRC)
      IF (RIND.EQ.0) THEN
C       no new records available, add some
        NUMADD= 20
        CALL WDRCAD(WDMSFL,NUMADD,
     O              FREREC)
        RIND  = WDRCGN(WDMSFL,PRPRRC,SCPRRC)
      END IF
C
      WDRCGX= RIND
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDRCAD
     I                   (WDMSFL,NUMADD,
     O                    FREREC)
C
C     + + + PURPOSE + + +
C     Add NUMADD records to the WDM file.  Update directory
C     record on the WDM file (record 1).  Returns record number
C     of the first free record in the WDM file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,NUMADD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     NUMADD - number of records to add
C     FREREC - record number of first free record in the WDM file
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
      INCLUDE 'cdrloc.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RIND,RREC,FREREC,LMXREC,I
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDRCGO, WDRCUP
C
C     + + + END SPECIFICATIONS + + +
C
C     get directory
      RREC= 1
      RIND= WDRCGO(WDMSFL,RREC)
C
      LMXREC= WIBUFF(PMXREC,RIND)
      FREREC= WIBUFF(PFRREC,RIND)
C
      IF (FREREC.EQ.0) THEN
C       no current free records, ok to add more
C       update free record value
        FREREC= LMXREC+ 1
        RREC  = LMXREC
        LMXREC= LMXREC+ NUMADD
C       update in memory buffer
        I= 0
 20     CONTINUE
          I= I+ 1
          IF (WDMOPN(I).EQ.WDMSFL) THEN
C           this is it
            MAXREC(I)= LMXREC
            I= WDMCNT
          END IF
        IF (I.LT.WDMCNT) GO TO 20
C       loop to add new records
 10     CONTINUE
          RREC= RREC+ 1
          RIND= WDRCGO(WDMSFL,-RREC)
C         fix primary forward pointer
          IF (RREC.LT.LMXREC) WIBUFF(2,RIND)= RREC+ 1
C         write record
          CALL WDRCUP(WDMSFL,RIND)
          RREC= RECNO(RIND)
        IF (RREC.LT.LMXREC.AND.RREC.GT.0) GO TO 10
C
C       update directory
        RREC= 1
        RIND= WDRCGO(WDMSFL,RREC)
        WIBUFF(PMXREC,RIND)= LMXREC
        WIBUFF(PFRREC,RIND)= FREREC
        CALL WDRCUP(WDMSFL,RIND)
      END IF
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   WDDRRC
     I                           (WDMSFL,DSN,OPT)
C
C     + + + PURPOSE + + +
C     Determine WDM file directory record number for data-set
C     number DSN.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,OPT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - data-set number
C     OPT    - option flag
C              1 - add a directory record
C              2 - delete a directory record
C              other (0) - neither
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
      INCLUDE 'cdrloc.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RREC,RIND,PREC,DPT,DIND,DREC
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO,WDRCGX
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDRCGO, WDRCUP, WDRCGX
C
C     + + + END SPECIFICATIONS + + +
C
      RREC= 1
      RIND= WDRCGO(WDMSFL,RREC)
      DPT = PDIRPT+((DSN-1)/500)
      IF (DPT.GT.512) DPT= 512
      IF (OPT.EQ.2) THEN
C       delete reference to record
        WIBUFF(DPT,RIND)= 0
C       update file def record
        CALL WDRCUP(WDMSFL,RIND)
C       need a dummy value to return
        DREC= 0
      ELSE
        DREC= WIBUFF(DPT,RIND)
        IF (DREC.EQ.0.AND.OPT.EQ.1) THEN
C         add new directory record
          PREC= 0
          DIND= WDRCGX(WDMSFL,PREC,PREC)
          DREC= RECNO(DIND)
          RIND= WDRCGO(WDMSFL,RREC)
          WIBUFF(DPT,RIND)= DREC
          CALL WDRCUP(WDMSFL,RIND)
        END IF
      END IF
C
      WDDRRC= DREC
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDSCHA
     I                    (WDMSFL,DSN,DSTYP,GPFLG,
     O                     LREC,GRCNT,RETCOD)
C
C     + + + PURPOSE + + +
C     Check WDM data set existance, type and ability to update.
C     Return first record number and number of groups in data set.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,DSTYP,GPFLG,LREC,GRCNT,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - data-set number
C     DSTYP  - data-set type
C              1 - time series      6 - rastor
C              2 - table            7 - space-time
C              3 - schematic        8 - attribute
C              4 - project          9 - message
C              5 - vector
C     GPFLG  - read(1)/write(2) flag
C     LREC   - record number of first record in data set (contains label)
C     GRCNT  - number of groups in data set
C     RETCOD - return code
C                0 - data set exists and is correct DSTYP
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -84 - data set number out of range
C              -85 - trying to write to a read-only data set
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   LIND,PDAT,SAIND,POS
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO, WDSASV
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDDSCK, WDRCGO, WDSASV
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
C     does dataset exist
      CALL WDDSCK(WDMSFL,DSN,
     O            LREC,RETCOD)
      IF (RETCOD.EQ.0) THEN
C       data set exists, get label
        LIND = WDRCGO(WDMSFL,LREC)
C       check data-set type
        IF (DSTYP.NE.WIBUFF(6,LIND)) THEN
C         not expected type of data-set
          RETCOD= -82
        ELSE
C         calculate number of groups in data set
          PDAT = WIBUFF(11,LIND)
          GRCNT= WIBUFF(PDAT,LIND)
        END IF
      END IF
      IF (RETCOD.EQ.0 .AND. GPFLG.EQ.2) THEN
C       check read/write flag
        SAIND= 35
        POS  = WDSASV(SAIND,WIBUFF(1,LIND))
        IF (POS.GT.0) THEN
C         read/write flag available
          IF (WIBUFF(POS,LIND).EQ.1) THEN
C           trying to write to a read only data set
            RETCOD= -85
          END IF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   WDCKDT
     I                           (WDMSFL,DSN)
C
C     + + + PURPOSE + + +
C     Check data set for existance and type, returns:
C         0 - data set does not exist
C     or data-set type
C         1 - time series      6 - rastor
C         2 - table            7 - space-time
C         3 - schematic        8 - attribute
C         4 - project          9 - message
C         5 - vector
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - data-set number to be checked
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RIND,DSNFRC,DSTYPE,RETCOD
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDDSCK, WDRCGO
C
C     + + + END SPECIFICATIONS + + +
C
      CALL WDDSCK(WDMSFL,DSN,
     O            DSNFRC,RETCOD)
      IF (DSNFRC.GT.0) THEN
C       data set exists
        RIND  = WDRCGO(WDMSFL,DSNFRC)
        DSTYPE= WIBUFF(6,RIND)
      ELSE
C       data set does not exist
        DSTYPE= 0
      END IF
C
      WDCKDT= DSTYPE
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDDSCK
     I                   (WDMSFL,DSN,
     O                    DREC,RETCOD)
C
C     + + + PURPOSE + + +
C     Check data set for existance and return record number of
C     first record in data set (contains label)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,DREC,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - data-set number to be checked
C     DREC   - record number of first record in data set
C     RETCOD - return code
C                0 - data set exists
C              -81 - data set does not exist
C              -84 - data set number out of range
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RIND,I,OPT,DIRREC
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO, WDDRRC
C
C     + + + INTRINSICS + + +
      INTRINSIC MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDDRRC, WDRCGO
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
      IF (DSN.LT.1 .OR. DSN.GT.32000) THEN
C       dataset number out of range
        RETCOD= -84
      END IF
      IF (RETCOD.EQ.0) THEN
C       get directory record number
        OPT   = 0
        DIRREC= WDDRRC(WDMSFL,DSN,OPT)
C
        IF (DIRREC.GT.0) THEN
C         directory exists, get it
          RIND= WDRCGO(WDMSFL,DIRREC)
C         calculate offset within record for our dsn
          I= MOD(DSN,500)+ 4
          IF (I.EQ.4) I= 504
C         get record dsn begins on
          DREC= WIBUFF(I,RIND)
        ELSE
C         no directory, dsn cant exist
          DREC= 0
        END IF
C
        IF (DREC.EQ.0) THEN
C         data set does not exist
          RETCOD= -81
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDBFIN
C
C     + + + PURPOSE + + +
C     Initialize pointers and counters for WDM buffer of records.
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
      INCLUDE 'cdrloc.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize  common CFBUFF variables as required
      DO 10 I= 1, CONREC
        NXTPOS(I)= I+ 1
        PREPOS(I)= I- 1
        RECNO(I) = 0
        WDMFUN(I)= 0
 10   CONTINUE
C
      NXTPOS(CONREC)= 1
      PREPOS(1)= CONREC
      FREPOS= 1
      WDMCNT= 0
      DO 20 I= 1,MXWDM
        WDMOPN(I)= 0
        MAXREC(I)= 0
 20   CONTINUE
C
C     initialize  CDRLOC
      PFNAME= 9
      PMXREC= 29
      PFRREC= 31
      PTSNUM= 32
      PDIRPT= 113
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDCREA
     I                    (WDMSFL)
C
C     + + + PURPOSE + + +
C     Adds directory record and 19 empty records to a new WDM file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
      INCLUDE 'cdrloc.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RREC,FREREC,I,NUMADD,RIND,LMXREC,RETCOD
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDRCGO, WDRCUP, WDRCAD
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
      I= 0
 10   CONTINUE
        I= I+ 1
        IF (WDMOPN(I).EQ.WDMSFL) THEN
C         its already open, can't create it
          RETCOD= -83
          I     = WDMCNT
        END IF
      IF (I.LT.WDMCNT) GO TO 10
C
      IF (RETCOD.EQ.0) THEN
C       ok to create a new file
        WDMCNT= WDMCNT+ 1
        WDMOPN(WDMCNT)= WDMSFL
        MAXREC(WDMCNT)= 1
C
        RREC= -1
        RIND= WDRCGO(WDMSFL,RREC)
C
C       fill in values, previous record pointer
        WIBUFF(1,RIND)= -998
C       last record
        LMXREC= 1
        WIBUFF(PMXREC,RIND)= LMXREC
C       write the first record
        CALL WDRCUP(WDMSFL,RIND)
C       now fill in the other records with all zero except for pointers
        NUMADD= 19
        CALL WDRCAD (WDMSFL,NUMADD,
     O               FREREC)
      END IF
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   WDSASV (SAIND,TIBUFF)
C
C     + + + PURPOSE + + +
C     determines where values for particular search attribute
C     start within data-set label, returns 0 if attribute is
C     not present
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   SAIND
      INTEGER*4 TIBUFF(512)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SAIND  - index of particular search attribute
C     TIBUFF - buffer of search attributes
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J,PSA,POS,SAMAX
C
C     + + + END SPECIFICATIONS + + +
C
      PSA  = TIBUFF(10)
      SAMAX= TIBUFF(PSA)
      POS  = 0
      I    = 0
C     loop to look for desired attribute index number in label
 10   CONTINUE
        I= I+ 1
        J= PSA+ I* 2
        IF (TIBUFF(J).EQ.SAIND) THEN
C         attribute is present
          POS= TIBUFF(J+1)
        END IF
      IF (I.LT.SAMAX.AND.POS.EQ.0) GO TO 10
C
      WDSASV= POS
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDNXPS
     I                   (WDMSFL,RDWRFG,
     M                    DREC,DPOS,
     O                    DIND)
C
C     + + + PURPOSE + + +
C     Get the next data position on a WDM file.  If RDWRFG is 1 and
C     a new record is needed, the next data record is brought into
C     the WDM buffer of records.  If RDWRFG is 2 and a new record
C     is needed, the current record is updated and the next free
C     record is brought into the WDM buffer of records.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,RDWRFG,DREC,DPOS,DIND
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     RDWRFG - read/write flag (1- read, 2- write)
C     DREC   - record number of data on WDM file
C     DPOS   - position of data on data record (both DREC and DIND)
C     DIND   - index of record in WDM buffer of records
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    IDUM
C
C     + + + FUNCTIONS + + +
      INTEGER    WDRCGX, WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDRCUP, WDRCGX, WDRCGO
C
C     + + + END SPECIFICATIONS + + +
C
      IDUM= 0
      DIND= WDRCGO (WDMSFL,DREC)
      DPOS= DPOS+ 1
      IF (DPOS.GT.512) THEN
C       next record
        IF (RDWRFG.EQ.1) THEN
C         reading, bring next record into buffer
          DREC= WIBUFF(4,DIND)
          DIND= WDRCGO (WDMSFL,DREC)
        ELSE
C         writing, update current record, bring in new one
          CALL WDRCUP(WDMSFL,DIND)
          DIND= WDRCGX(WDMSFL,IDUM,DREC)
          DREC= RECNO(DIND)
        END IF
        DPOS= 5
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDNXDV
     I                   (WDMSFL,
     M                    DREC,DPOS,
     O                    DVAL)
C
C     + + + PURPOSE + + +
C     Move to the next data position and return the
C     integer equivalent of the data value.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DREC,DPOS,DVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     DREC   - record number of data on WDM file
C     DPOS   - position of data on data record (both DREC and DIND)
C     DVAL   - data value on WDM file
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    DIND,RDWRFG
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDNXPS
C
C     + + + END SPECIFICATIONS + + +
C
      RDWRFG= 1
C     move to next data position
      CALL WDNXPS (WDMSFL,RDWRFG,
     M             DREC,DPOS,
     O             DIND)
C
C     get the data value
      DVAL= WIBUFF(DPOS,DIND)
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDPRPS
     I                   (WDMSFL,
     M                    DREC,DPOS,
     O                    DIND)
C
C     + + + PURPOSE + + +
C     Get the previous data position on a WDM file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DREC,DPOS,DIND
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     DREC   - record number of data on WDM file
C     DPOS   - position of data on data record (both DREC and DIND)
C     DIND   - index of record in WDM buffer of records
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + FUNCTIONS + + +
      INTEGER    WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDRCGO
C
C     + + + END SPECIFICATIONS + + +
C
      DIND= WDRCGO (WDMSFL,DREC)
      DPOS= DPOS- 1
      IF (DPOS.LT.5) THEN
C       bring previous record into buffer
        DREC= WIBUFF(3,DIND)
        DIND= WDRCGO (WDMSFL,DREC)
        DPOS= 512
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WMSGTE
     I                   (WDMSFL,TLEN,LLEN,
     M                    DREC,DPOS,GLEN,MLEN,
     O                    OLEN,OBUFF,CONT)
C
C     + + + PURPOSE + + +
C     Get one record of text off WDM file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDMSFL,TLEN,LLEN,DREC,DPOS,GLEN,MLEN,OLEN,CONT
      CHARACTER*1 OBUFF(LLEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     TLEN   - total length of text (may be more than one record)
C     LLEN   - maximum size of record to get
C     DREC   - record number of data on WDM file
C     DPOS   - position of data on data record
C     GLEN   - counter to keep track of when to read off WDM file
C              should be initialized to 0 for first call
C     MLEN   - number of characters retrieved so far (must be <= TLEN)
C              should be initialized to 0 for first call
C     OLEN   - actual size of record retreived
C     OBUFF  - array of size LLEN containing OLEN characters retrieved
C     CONT   - indicator flag for text
C              0 - no more text available
C              1 - more text available
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     DONFG,RDWRFG,DIND,INULL,ITMP
      CHARACTER*1 BLNK,CTMP
      CHARACTER*4 CTXT4
C
C     + + + FUNCTIONS + + +
      INTEGER     WDRCGO
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD, ICHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDRCGO, ZIPC, WDNXPS
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (A4)
C
C     + + + END SPECIFICATIONS + + +
C
      BLNK  = ' '
      INULL = 0
      OLEN  = 0
      CONT  = 1
      DONFG = 0
      RDWRFG= 1
      CALL ZIPC (LLEN,BLNK,OBUFF)
      DIND  = WDRCGO(WDMSFL,DREC)
      WRITE (CTXT4,2000) WIBUFF(DPOS,DIND)
C
 10   CONTINUE
C       read until null character
        IF (MOD(GLEN,4).EQ.0) THEN
C         read off WDM file (next position if already read some)
          CALL WDNXPS (WDMSFL,RDWRFG,
     M                 DREC,DPOS,
     O                 DIND)
C         read more text
          WRITE (CTXT4,2000) WIBUFF(DPOS,DIND)
          GLEN= 0
        END IF
        GLEN= GLEN+ 1
        MLEN= MLEN+ 1
        CTMP= CTXT4(GLEN:GLEN)
        ITMP= MOD(ICHAR(CTMP),128)
        IF (ITMP.EQ.INULL) THEN
C         record terminator
          DONFG= 1
        ELSE IF (OLEN.LT.LLEN) THEN
C         save character in output buffer
          OLEN= OLEN+ 1
          OBUFF(OLEN)= CTMP
        END IF
        IF (MLEN.GE.TLEN) THEN
C         end of group text
          CONT = 0
          CTXT4= ' '
          DONFG= 1
        END IF
      IF (DONFG.EQ.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   WMSSKB
     I                    (WDMSFL,TLEN,
     M                     DREC,DPOS)
C
C     + + + PURPOSE + + +
C     Position DREC and DPOS at the end of the current data block.
C     DREC and DPOS are assumed to be input as the start of the block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,TLEN,DREC,DPOS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     TLEN   - total number of characters to skip
C     DREC   - record number on WDM file
C     DPOS   - position on record DREC
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,HLEN,RDWRFG,LIND
C
C     + + + INTRINSICS + + +
      INTRINSIC  MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDNXPS
C
C     + + + END SPECIFICATIONS + + +
C
      RDWRFG= 1
      HLEN  = TLEN/4
      IF (MOD(TLEN,4).GT.0) HLEN= HLEN+ 1
      DO 100 I= 1,HLEN
        CALL WDNXPS (WDMSFL,RDWRFG,
     M               DREC,DPOS,
     O               LIND)
 100  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDSASP
     I                    (SAIND,SALEN,SATYP,
     M                     TIBUFF,
     O                     PSAVAL,RETCOD)
C
C     + + + PURPOSE + + +
C     adds space for search attribute on a dsn label if not present on it
C
C     + + + HISTORY + + +
C     08/30/94  kmf  added code for case where no space for index (-103)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   SAIND,SALEN,SATYP,PSAVAL,RETCOD
      INTEGER*4 TIBUFF(512)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SAIND  - index number of search attribute to look for
C     SALEN  - length of search attribute
C     SATYP  - type of search attribute
C     TIBUFF - array containing data-set label
C     PSAVAL - pointer to search attribute information if available
C     RETCOD - flag indicating status of search attribute locate
C                 0 - attribute space added
C              -102 - attribute already on label
C              -103 - no space for this attribute
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   PSA,SACNT,SANMX,SASMX,SASTR,TSALEN
C
C     + + + FUNCTIONS + + +
      INTEGER   WDSASV
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDSASV
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
      TSALEN= SALEN
      IF (SATYP.EQ.3) THEN
C       character attribute, adjust size from characters to words
        TSALEN= TSALEN/4
      END IF
C
      PSAVAL= WDSASV (SAIND,TIBUFF)
      IF (PSAVAL.GT.0) THEN
C       attribute already on label
        RETCOD= -102
      ELSE
C       not there yet, try to add it
C       get pointer to search attribute start
        PSA  = TIBUFF(10)
C       current number of search attributes
        SACNT= TIBUFF(PSA)+ 1
C       max number of search attributes allowed
        SANMX= (TIBUFF(PSA+1)- PSA- 2)/2
C       last space to put search attributes in
        SASMX= TIBUFF(11)- 1
        IF (SACNT.LE.SANMX) THEN
C         space available for index, now check for space for value
          SASTR = PSA+ (SACNT*2)
          PSAVAL= TIBUFF(SASTR-1)
          IF (SACNT.GT.1) THEN
C           look for free space after last attribute
 10         CONTINUE
              PSAVAL= PSAVAL+ 1
            IF (TIBUFF(PSAVAL).NE.-999) GO TO 10
          END IF
C         check to see that enough space available
          IF (PSAVAL+TSALEN.GT.SASMX) THEN
C           oops, it wont fit
            PSAVAL= 0
            RETCOD= -103
          ELSE
C           update the label values
            TIBUFF(PSA)    = SACNT
            TIBUFF(SASTR)  = SAIND
            TIBUFF(SASTR+1)= PSAVAL
          END IF
        ELSE
C         no space for index for attribute
          RETCOD = -103
        END IF
      END IF
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   WDDTFG
     I                            (DREC,TIBUFF)
C
C     + + + PURPOSE + + +
C     determines if data is present in a WDMS data set,
C       returns 1 for yes, 0 for no
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DREC
      INTEGER*4 TIBUFF(512)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DREC   - label record number
C     TIBUFF - array containing data-set label information
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   PREC,POFF,DATFLG,PDATV
      INTEGER*4 TDFREE,PDAT
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDPTSP
C
C     + + + END SPECIFICATIONS + + +
C
      PDAT  = TIBUFF(11)
      PDATV = TIBUFF(12)
      TDFREE= TIBUFF(PDAT+1)
      CALL WDPTSP (TDFREE,
     O             PREC,POFF)
      IF (PREC.EQ.DREC.AND.POFF.EQ.PDATV) THEN
C       no data present on dataset
        DATFLG= 0
      ELSE
C       free space not first possible space, data is present
        DATFLG= 1
      END IF
C
      WDDTFG= DATFLG
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDFDUP
     I                    (WDMSFL,DSN,DSFREC)
C
C     + + + PURPOSE + + +
C     updates a WDMS file directory record,
C     adds value if DSFREC> 0, deletes if DSFREC<=0
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,DSFREC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - data-set number
C     DSFREC - data-set first record number, <0 for delete
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   DIND,DREC,OPT,I,DELFLG
C
C     + + + FUNCTIONS + + +
      INTEGER   WDDRRC,WDRCGO,WDRCDL
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDDRRC, WDRCDL, WDRCGO, WDRCUP
C
C     + + + END SPECIFICATIONS + + +
C
      OPT = 1
      DREC= WDDRRC(WDMSFL,DSN,OPT)
      DIND= WDRCGO(WDMSFL,DREC)
      I   = MOD(DSN,500)+ 4
      IF (I.EQ.4) THEN
C       last position, not first
        I= 504
      END IF
      DELFLG= 0
      IF (DSFREC.GT.0) THEN
C       add this data set
        WIBUFF(  I,DIND)= DSFREC
        WIBUFF(512,DIND)= WIBUFF(512,DIND)+ 1
      ELSE
C       delete this data set
        WIBUFF(  I,DIND)= 0
        WIBUFF(512,DIND)= WIBUFF(512,DIND)- 1
        IF (WIBUFF(512,DIND).EQ.0) THEN
C         delete directory record, no more data sets in it
          I= WDRCDL(WDMSFL,RECNO(DIND))
C         update file def record, delete the reference to this record
          OPT= 2
          I= WDDRRC(WDMSFL,DSN,OPT)
          DELFLG= 1
        END IF
      END IF
      IF (DELFLG.EQ.0) THEN
C       update the changed directory record
        CALL WDRCUP(WDMSFL,DIND)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDFCUP
     I                    (WDMSFL,DSTYPE,DSN,OPT)
C
C     + + + PURPOSE + + +
C     updates file defintions record data set counters and
C     pointers, also prev and next record pointers in dsn recs are
C     updated.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL, DSTYPE, DSN, OPT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSTYPE - type of DSN
C     DSN    - data set number
C     OPT    - option, 1=add, 2= delete DSN
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
      INCLUDE 'cdrloc.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RIND,RREC,PCNT,PFDSN,OFDSN,NXDSN,PRDSN,CDSN,RETCOD
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDDSCK, WDRCGO, WDRCUP
C
C     + + + END SPECIFICATIONS + + +
C
C     bring file definition record into memory
      RREC = 1
      RIND = WDRCGO(WDMSFL,RREC)
C     calculate pointers within file definition record
      PCNT = PTSNUM + (DSTYPE-1)* 2
      PFDSN= PCNT+ 1
C     save old first dsn
      OFDSN= WIBUFF(PFDSN,RIND)
C
      IF (OPT.EQ.1) THEN
C       add a dsn
C       update first dsn
        WIBUFF(PFDSN,RIND)= DSN
C       update count of data sets
        WIBUFF(PCNT,RIND) = WIBUFF(PCNT,RIND)+ 1
C       write out updated file def record
        CALL WDRCUP(WDMSFL,RIND)
C       update pointers in old first dsn if it exists
        IF (OFDSN.GT.0) THEN
C         it does exist
          CALL WDDSCK(WDMSFL,OFDSN,
     O                RREC,RETCOD)
          RIND= WDRCGO(WDMSFL,RREC)
          WIBUFF(1,RIND)= DSN
          CALL WDRCUP(WDMSFL,RIND)
        END IF
C       update pointers in new first dsn
        CALL WDDSCK(WDMSFL,DSN,
     O              RREC,RETCOD)
        RIND= WDRCGO(WDMSFL,RREC)
        WIBUFF(2,RIND)= OFDSN
        CALL WDRCUP(WDMSFL,RIND)
      ELSE
C       delete a dsn
        CDSN= OFDSN
 10     CONTINUE
          CALL WDDSCK(WDMSFL,CDSN,
     O                RREC,RETCOD)
          RIND = WDRCGO(WDMSFL,RREC)
          NXDSN= WIBUFF(2,RIND)
          IF (CDSN.EQ.DSN) THEN
C           this is the data set to delete
            PRDSN= WIBUFF(1,RIND)
            IF (DSN.EQ.OFDSN) THEN
C             update old first dsn, it is being deleted
              OFDSN= NXDSN
            END IF
            IF (NXDSN.GT.0) THEN
C             update back pointer in next record
              CALL WDDSCK(WDMSFL,NXDSN,
     O                    RREC,RETCOD)
              RIND= WDRCGO(WDMSFL,RREC)
              WIBUFF(1,RIND)= PRDSN
              CALL WDRCUP(WDMSFL,RIND)
            END IF
            IF (PRDSN.GT.0) THEN
C             update forward pointer in prev record
              CALL WDDSCK(WDMSFL,PRDSN,
     O                    RREC,RETCOD)
              RIND= WDRCGO(WDMSFL,RREC)
              WIBUFF(2,RIND)= NXDSN
              CALL WDRCUP(WDMSFL,RIND)
            END IF
          ELSE
C           have not found the right dsn yet, get ready to try next one
            CDSN= NXDSN
          END IF
        IF (CDSN.NE.DSN.AND.CDSN.GT.0) GO TO 10
C       update counter in first record
        RREC= 1
        RIND= WDRCGO(WDMSFL,RREC)
        WIBUFF(PCNT,RIND)= WIBUFF(PCNT,RIND)- 1
C       update first dsn
        WIBUFF(PFDSN,RIND)= OFDSN
        CALL WDRCUP(WDMSFL,RIND)
      END IF
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   WDRCDL
     I                            (WDMSFL,DREC)
C
C     + + + PURPOSE + + +
C     deletes a record in the WDMSFL and updates pointers as required
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DREC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DREC   - record to delete
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cdrloc.inc'
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RIND,RREC,PRNXRC,PRBKRC,SCNXRC,SCBKRC,FREREC,I,RETCOD
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDDSCK, WDRCGO, WDRCUP
C
C     + + + END SPECIFICATIONS + + +
C
C     get pointers for record to delete
      RIND  = WDRCGO(WDMSFL,DREC)
      PRBKRC= WIBUFF(1,RIND)
      PRNXRC= WIBUFF(2,RIND)
      SCBKRC= WIBUFF(3,RIND)
      SCNXRC= WIBUFF(4,RIND)
      IF (PRBKRC.GT.0) THEN
C       a primary backward pointer exists, update that rec
        CALL WDDSCK (WDMSFL,PRBKRC,
     O               RREC,RETCOD)
        RIND= WDRCGO(WDMSFL,RREC)
        WIBUFF(2,RIND)= PRNXRC
        CALL WDRCUP(WDMSFL,RIND)
      END IF
      IF (PRNXRC.GT.0) THEN
C       a primary forward pointer exists, update that rec
        CALL WDDSCK (WDMSFL,PRNXRC,
     O               RREC,RETCOD)
        RIND= WDRCGO(WDMSFL,RREC)
        WIBUFF(1,RIND)= PRBKRC
        CALL WDRCUP(WDMSFL,RIND)
      END IF
      IF (SCBKRC.GT.0) THEN
C       a secondary backward pointer exists, update that rec
        RIND= WDRCGO(WDMSFL,SCBKRC)
        WIBUFF(4,RIND)= SCNXRC
        CALL WDRCUP(WDMSFL,RIND)
      END IF
      IF (SCNXRC.GT.0) THEN
C       a secondary forward pointer exists, update that rec
        RIND= WDRCGO(WDMSFL,SCNXRC)
        WIBUFF(3,RIND)= SCBKRC
        CALL WDRCUP(WDMSFL,RIND)
      END IF
C
C     get directory record
      RREC= 1
      RIND= WDRCGO(WDMSFL,RREC)
C
C     determine current free record
      FREREC= WIBUFF(PFRREC,RIND)
C
C     initialize current record with 0 and free rec pointer
      RIND= WDRCGO(WDMSFL,DREC)
      DO 10 I= 1,512
        WIBUFF(I,RIND) = 0
 10   CONTINUE
      WIBUFF(2,RIND)= FREREC
C     update current record
      CALL WDRCUP(WDMSFL,RIND)
C     update free record
      RIND= WDRCGO(WDMSFL,RREC)
      WIBUFF(PFRREC,RIND)= DREC
      CALL WDRCUP(WDMSFL,RIND)
C
      WDRCDL= SCNXRC
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDLBAX
     I                    (WDMSFL,DSN,DSTYPE,NDN,NUP,NSA,NSASP,NDP,
     O                     PSA)
C
C     + + + PURPOSE + + +
C     add a new data-set label, but no search attributes or data.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,DSTYPE,NDN,NUP,NSA,NSASP,NDP,PSA
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - data-set number
C     DSTYPE - type of data set
C     NDN    - number of down pointers
C     NUP    - number of up pointers
C     NSA    - number of search attributes
C     NSASP  - amount of search attribute space
C     NDP    - number of data pointers
C     PSA    - pointer to search attribute space
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   NIND,I,PRNREC,PDP,PUP,PDAT,PDATV,PSASTR
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGX, WDRCGO, WDPTCL
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDFCUP, WDFDUP, WDPTCL, WDRCGO, WDRCGX, WDRCUP
C
C     + + + END SPECIFICATIONS + + +
C
C     get a record to put label on
      I     = 0
      PRNREC= 0
      NIND  = WDRCGX (WDMSFL,I,PRNREC)
      PRNREC= RECNO(NIND)
C     fill in new data-set number
      WIBUFF(5,NIND)= DSN
C     fill in new data-set type
      WIBUFF(6,NIND)= DSTYPE
C     update new record
      CALL WDRCUP (WDMSFL,NIND)
C     add new data set to directories
C     first, the data set directory
      CALL WDFDUP (WDMSFL,DSN,PRNREC)
C     next pointer in file def rec and dsn pointers
      I= 1
      CALL WDFCUP (WDMSFL,DSTYPE,DSN,I)
C
C     get NIND again since RECNO array may have changed
      NIND = WDRCGO(WDMSFL,PRNREC)
C
C     reserve position 7 for future use
      WIBUFF(7,NIND)= 0
C     set down position pointer
      PDP= 13
      WIBUFF(8,NIND)= PDP
C     set up position pointer
      PUP= PDP+ 1+ NDN
      WIBUFF(9,NIND)= PUP
C
C     set search attribute position pointer
      PSA= PUP+ 1+ NUP
      WIBUFF(10,NIND)= PSA
C     set search attribute start value pointer
      PSASTR= PSA+ 2+ (2* NSA)
      WIBUFF(PSA+1,NIND)= PSASTR
C     fill in undefined search attribute values
      DO 10 I= PSASTR, PSASTR+ NSASP- 1
        WIBUFF(I,NIND)= -999
 10   CONTINUE
C
C     set data group position pointer
      PDAT= PSASTR+ NSASP
      WIBUFF(11,NIND)= PDAT
C
C     set data group pointer counter
      WIBUFF(PDAT,NIND) = 0
C
C     set data position pointer
      PDATV= PDAT+ 2+ NDP
      WIBUFF(12,NIND)= PDATV
C
C     pointer to first free data position
      WIBUFF(PDAT+1,NIND)= WDPTCL(RECNO(NIND),PDATV)
C
C     update new data-set label
      CALL WDRCUP(WDMSFL,NIND)
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDBOPN
     I                    (WDMSFL,WDNAME,RONWFG,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     Open a WDM file.  File is opened as new or old, depending on
C     the value of RONWFG.  The common block related to the WDM record
C     buffer are initialized the first time this routine is called.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      WDMSFL,RONWFG,RETCOD
      CHARACTER*64 WDNAME
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of the WDM file
C     WDNAME - name of the WDM file
C     RONWFG - read only/new file flag
C              0- normal open of existing WDM file,
C              1- open WDM file as read only (system dependent),
C              2- open new WDM file
C     RETCOD - return code
C               0 - successful open
C               1 - successful open, but invalid WDM file
C              <0 - error on open, -IOSTAT, compiler specific
C
C     + + + SAVES + + +
      INTEGER   INITFG
      SAVE INITFG
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4 IOS
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDBFIN, WDFLCK, WDCREA
C
C     + + + DATA INITIALIZATIONS + + +
      DATA INITFG/0/
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
C     ****** NOTE: THE FOLLOWING OPEN STATEMENTS MAY BE SYSTEM SPECIFIC ******
      IF (RONWFG.EQ.1) THEN
C       open file as 'read only'
        OPEN (UNIT=WDMSFL,FILE=WDNAME,STATUS='OLD',
     1        ACCESS='DIRECT',FORM='UNFORMATTED',RECL=2048,
     2        ERR=10,IOSTAT=IOS)
      ELSE IF (RONWFG.EQ.2) THEN
C       open new wdm file
        OPEN (UNIT=WDMSFL,FILE=WDNAME,STATUS='NEW',
     1        ACCESS='DIRECT',FORM='UNFORMATTED',RECL=2048,
     2        ERR=10,IOSTAT=IOS)
      ELSE
C       open file w/out 'read only'
        OPEN (UNIT=WDMSFL,FILE=WDNAME,STATUS='OLD',
     1        ACCESS='DIRECT',FORM='UNFORMATTED',RECL=2048,
     2        ERR=10,IOSTAT=IOS)
      END IF
C     WDM file opened successfully
      IF (INITFG.EQ.0) THEN
C       first time called, initialize WDM record buffer
        CALL WDBFIN
        INITFG= 1
      END IF
      IF (RONWFG.EQ.2) THEN
C       new file, need to initialize it
        CALL WDCREA (WDMSFL)
      END IF
      IF (RETCOD.EQ.0) THEN
C       check WDM directory records
        CALL WDFLCK (WDMSFL,
     O               RETCOD)
      END IF
      GO TO 20
 10   CONTINUE
C       error on open, exact value of retcod may vary by system,
C       set it to a negative value for consistancy
        RETCOD= IOS
        IF (RETCOD.GT.0) RETCOD= -RETCOD
        IF (RETCOD.EQ.0) RETCOD= -1
 20   CONTINUE
C
      RETURN
      END
C
C
C
      INTEGER*4 FUNCTION   WDPTCL
     I                           (PREC,POFF)
C
C     + + + PURPOSE + + +
C     Calculate a pointer value from record number
C     and the offset within the record.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   PREC,POFF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PREC   - record number
C     POFF   - offset within the record
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4 I
C
C     + + + END SPECIFICATIONS + + +
C
      I= PREC
      WDPTCL = (I*512)+ POFF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDPTSP
     I                   (PTR,
     O                    PREC,POFF)
C
C     + + + PURPOSE + + +
C     split up a pointer into record number and offset within record.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*4 PTR
      INTEGER   PREC,POFF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PTR    - pointer value
C     PREC   - record number
C     POFF   - offset within the record
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + END SPECIFICATIONS + + +
C
      PREC= PTR/ 512
      POFF= MOD(PTR,512)
C
      IF (PREC .LT. 1 .OR. PREC .GT. 200000)
     *           WRITE(*,*) ' IN WDPTSP: PREC,POFF,PTR=',PREC,POFF,PTR
      RETURN
      END
C
C
C
      SUBROUTINE   WBCWSP
     I                   (BCW,
     O                    NOV,TSTEP,TCODE,COMPCD,QUALCD)
C
C     + + + PURPOSE + + +
C     Split up a block control word for a time-series type data set
C     to determine the information stored in it.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*4 BCW,NOV
      INTEGER   TSTEP,TCODE,COMPCD,QUALCD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BCW    - block control word
C     NOV    - number of data values in the block
C     TSTEP  - time step of data block in TCODE units
C     TCODE  - time units code
C              1 - second    4 - day
C              2 - minute    5 - month
C              3 - hour      6 - year
C                            7 - century
C     COMPCD - compression code
C              1 - compressed block of data
C              2 - uncompressed block of data
C     QUALCD - quality code, 0 <= QUALCD <= 31
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4 TMP
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + END SPECIFICATIONS + + +
C
      TMP= BCW
      IF (TMP.LT.0) THEN
C       neg value, integers are in 2's comp form
        TMP   = -TMP
        NOV   = 65536- (TMP/65536)- 1
        TSTEP = 64   - MOD((TMP/1024),64)-1
        TCODE =  8   - MOD((TMP/128) ,8) -1
        COMPCD=  4   - MOD((TMP/32 ) ,4) -1
        QUALCD= 32   - MOD (TMP      ,32)
      ELSE
        NOV   =     (TMP/65536)
        TSTEP = MOD((TMP/1024),64)
        TCODE = MOD((TMP/128) ,8)
        COMPCD= MOD((TMP/32 ) ,4)
        QUALCD= MOD (TMP      ,32)
      END IF
C
      RETURN
      END
C
C
C
      INTEGER*4 FUNCTION   WBCWCL
     I                           (NOV,TSTEP,TCODE,COMPCD,QUALCD)
C
C     + + + PURPOSE + + +
C     Calculate a block control word for time-series type data.
C     Returns a 0 if any one of the components is invalid.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TSTEP,TCODE,COMPCD,QUALCD
      INTEGER*4 NOV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NOV    - number of data values in the block
C     TSTEP  - time step of data block in TCODE units
C     TCODE  - time units code
C              1 - second    4 - day
C              2 - minute    5 - month
C              3 - hour      6 - year
C                            7 - century
C     COMPCD - compression code
C              1 - compressed block of data
C              2 - uncompressed block of data
C     QUALCD - quality code, 0 <= QUALCD <= 31
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4 BCW
C
C     + + + END SPECIFICATIONS + + +
C
      IF (NOV.LE.0.OR.NOV.GT.65535.OR.TSTEP.LT.0.OR.TSTEP.GT.63.OR.
     1  TCODE.LT.0.OR.TCODE.GT.7.OR.COMPCD.LT.0.OR.COMPCD.GT.3.OR.
     2  QUALCD.LT.0.OR.QUALCD.GT.31) THEN
C       bad value, cant make a BCW
        BCW= 0
      ELSE
        BCW= NOV*65536+ TSTEP*1024+ TCODE*128+ COMPCD*32+ QUALCD
      END IF
C
      WBCWCL= BCW
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDATSP
     I                   (DATWRD,
     O                    DAT)
C
C     + + + PURPOSE + + +
C     Split up a WDMS date word.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*4 DATWRD
      INTEGER   DAT(4)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATWRD - date in compressed format
C     DAT    - date (year, month, day, hour)
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + END SPECIFICATIONS + + +
C
C     year
      DAT(1) = MOD((DATWRD/16384),131072)
C     month
      DAT(2) = MOD((DATWRD/1024) ,16)
C     day
      DAT(3) = MOD((DATWRD/32)   ,32)
C     hour
      DAT(4) = MOD (DATWRD       ,32)
C
      RETURN
      END
C
C
C
      INTEGER*4 FUNCTION   WDATCL
     I                           (DAT)
C
C     + + + PURPOSE + + +
C     Calculate a date in compressed format from
C     its components (year- hour).  Returns a 0 if
C     any one of the components is invalid.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DAT(4)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DAT    - date (year, month, day, hour)
C              0 <= hour <= 24
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4   DATWRD,YEAR,MONTH,DAY,HOUR
C
C     + + + END SPECIFICATIONS + + +
C
      YEAR = DAT(1)
      MONTH= DAT(2)
      DAY  = DAT(3)
      HOUR = DAT(4)
      IF (YEAR.LE.0.OR.YEAR.GT.131071.OR.MONTH.LE.0.OR.MONTH.GT.12.
     1  .OR.DAY.LE.0.OR.DAY.GT.31.OR.HOUR.LT.0.OR.HOUR.GT.24) THEN
C       bad value, cant make a date word
        DATWRD= 0
      ELSE
        DATWRD= YEAR*16384+ MONTH*1024+ DAY*32+ HOUR
      END IF
C
      WDATCL= DATWRD
C
      RETURN
      END
C
C
C
      SUBROUTINE   WBCWSQ
     I                   (BCW,
     O                    NOV,TSTEP,TCODE,COMPCD,QUALCD)
C
C     + + + PURPOSE + + +
C     Split up a block control word for time-series type data set.
C     Adjusts TSTEP and NOV for faster operation, if possible.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*4 BCW,NOV
      INTEGER   TSTEP,TCODE,COMPCD,QUALCD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BCW    - block control word
C     NOV    - number of data values in the block
C     TSTEP  - time step of data block in TCODE units
C     TCODE  - time units code
C              1 - second    4 - day
C              2 - minute    5 - month
C              3 - hour      6 - year
C                            7 - century
C     COMPCD - compression code
C              1 - compressed block of data
C              2 - uncompressed block of data
C     QUALCD - quality code, 0 <= QUALCD <= 31
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   DONFG
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   WBCWSP
C
C     + + + END SPECIFICATIONS + + +
C
      CALL WBCWSP (BCW,
     O             NOV,TSTEP,TCODE,COMPCD,QUALCD)
C
      IF (COMPCD.EQ.1) THEN
C       try to adjust units and timestep for faster operation
        DONFG= 0
 20     CONTINUE
          IF (MOD(NOV,7).EQ.0.AND.TSTEP.LT.4000) THEN
            TSTEP= TSTEP* 7
            NOV  = NOV/ 7
          ELSE IF (MOD(NOV,5).EQ.0.AND.TSTEP.LT.6000) THEN
            TSTEP= TSTEP* 5
            NOV  = NOV/ 5
          ELSE IF (MOD(NOV,3).EQ.0.AND.TSTEP.LT.10000) THEN
            TSTEP= TSTEP* 3
            NOV  = NOV/ 3
          ELSE IF (MOD(NOV,2).EQ.0.AND.TSTEP.LT.15000) THEN
            TSTEP= TSTEP* 2
            NOV  = NOV/ 2
          ELSE
C           no more adjustment possible
            DONFG= 1
          END IF
        IF (DONFG.EQ.0) GO TO 20
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WATTSP
     I                   (IATVL,
     O                    ATTYP,ATLEN,ATUSE,ATUPD)
C
C     + + + PURPOSE + + +
C     Split up a word containing parameters for
C     attribute type data set.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IATVL,ATTYP,ATLEN,ATUSE,ATUPD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IATVL  - attribute parameters integer word
C     ATTYP  - attribute type
C              1- integer
C              2- real
C              3- character
C     ATLEN  - attribute length
C              ATTYP= 1 or 2 - number of words
C              ATTYP= 3 - number of characters
C     ATUSE  - array of indicator flags for data-set attribute usage
C              0 - attribute not allowed for data-set type
C              1 - attribute optional for data-set type
C              2 - attribute required for data-set type
C     ATUPD  - attribute update flag
C              0 - don't update attribute
C              1 - update attribute if data exists
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + END SPECIFICATIONS + + +
C
      ATTYP= IATVL/268435456+ 1
      ATLEN= MOD((IATVL/2097152),128)
      ATUSE= MOD((IATVL/2),1048576)
      ATUPD= MOD(IATVL,2)
C
      RETURN
      END
C
C
C
      SUBROUTINE   WATTUS
     I                   (ATUSWD,
     O                    ATUSE)
C
C     + + + PURPOSE + + +
C     Split up word containing array of indicator flags
C     for dataset attribute usage.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  ATUSWD,ATUSE(10)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ATUSWD - attribute usage word
C     ATUSE  - array of indicator flags for data-set attribute usage
C              0 - attribute not allowed for data-set type
C              1 - attribute optional for data-set type
C              2 - attribute required for data-set type
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,IEXP
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + END SPECIFICATIONS + + +
C
      IEXP = 262144
      DO 10 I= 1,10
        ATUSE(I)= MOD((ATUSWD/IEXP),4)
        IEXP= IEXP/4
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   WATWDS
     I                   (IWORD,
     O                    IVAL1,IVAL2)
C
C     + + + PURPOSE + + +
C     Split up a word containing parameters for attribute type data set.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    IWORD,IVAL1,IVAL2
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IWORD  - attribute parameters integer word
C     IVAL1  - either last 2 characters of name or id of info
C     IVAL2  - either attribute index or length of info
C
C     + + + INTRINSICS + + +
      INTRINSIC  MOD
C
C     + + + END SPECIFICATIONS + + +
C
      IVAL1= IWORD/ 512
      IVAL2= MOD(IWORD,512)
C
      RETURN
      END
C
C
C
      SUBROUTINE   WADGTL
     I                   (MESSFL,DSN,ATIND,
     O                    ATNAM,DPTR,ATTYP,ATLEN,ATUSWD,ATUPD)
C
C     + + + PURPOSE + + +
C     given attribute index, get type, length, data set usage,
C     and update flag as well as starting pos of other data
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,DSN,ATIND,DPTR,ATTYP,ATLEN,ATUSWD,ATUPD
      CHARACTER*1 ATNAM(6)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for WDM message file
C     DSN    - data-set number on WDM file
C     ATIND  - attribute index
C     ATNAM  - attribute name
C     DPTR   - pointer to start of other attribute data
C     ATTYP  - attribute typ
C     ATLEN  - attribute length
C     ATUSWD - integer word of required and optional data set usage
C     ATUPD  - attribute update flag
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,ITMP,IVAL(2),LREC,LPOS,LIND,PDAT,BATIND,RETCOD
C
C     + + + FUNCTIONS + + +
      INTEGER     WDRCGO
C
C     + + + INTRINSICS + + +
      INTRINSIC   CHAR, MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDDSCK, WDRCGO, WATWDS, WATTSP
C
C     + + + END SPECIFICATIONS + + +
C
C     get label index
      CALL WDDSCK(MESSFL,DSN,
     O            LREC,RETCOD)
      LIND= WDRCGO(MESSFL,LREC)
C     get needed pointers
      PDAT= WIBUFF(11,LIND)
C     find base attribute
      LPOS= PDAT+ 3
      ITMP= WIBUFF(LPOS,LIND)
      CALL WATWDS (ITMP,
     O             I,BATIND)
C     now find attribute position based on offset of base index
      LPOS= PDAT+ 2+ 4*(ATIND-BATIND)
C     get first two words to build name
      IVAL(1)= WIBUFF(LPOS,LIND)
      ITMP   = WIBUFF(LPOS+1,LIND)
C     split second word into last part of name
      CALL WATWDS (ITMP,
     O             IVAL(2),I)
C     build name
      ITMP= IVAL(1)
      DO 10 J= 1,6
        ATNAM(J)= CHAR(MOD(ITMP,256))
        IF (J.EQ.4) THEN
C         switch to second word
          ITMP= IVAL(2)
        ELSE
          ITMP= ITMP/256
        END IF
 10   CONTINUE
C     get pointer to start of data
      DPTR= WIBUFF(LPOS+2,LIND)
C     get last word of label containing attribute parms
      IVAL(1)= WIBUFF(LPOS+3,LIND)
      CALL WATTSP (IVAL(1),
     O             ATTYP,ATLEN,ATUSWD,ATUPD)
C
      RETURN
      END
C
C
C
      SUBROUTINE   WADDSI
     I                   (MESSFL,DSNMAX,
     O                    DSNCNT,DSN,ATTIND)
C
C     + + + PURPOSE + + +
C     determine the data-set numbers containing the attribute data sets
C     and the first and last attributes on each data set
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,DSNMAX,DSNCNT,DSN(DSNMAX),ATTIND(2,DSNMAX)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for WDM message file
C     DSNMAX - maximum number of attribute data sets
C     DSNCNT - number of data sets containing attributes
C     DSN    - array of data-set numbers containing attributes
C     ATTIND - array of first and last attributes on each data set
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cdrloc.inc'
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,J,LPDAT,PDATV,LREC,LIND,LPOS,ITMP,IVAL,RETCOD
C
C     + + + FUNCTIONS + + +
      INTEGER    WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDDSCK, WDRCGO, WATWDS, ZIPI
C
C     + + + END SPCIFICATIONS + + +
C
C     WRITE(*,*) 'waddsi:',MESSFL,DSNMAX
      J= 0
      CALL ZIPI(DSNMAX,J,DSN)
      I= DSNMAX*2
      CALL ZIPI(I,J,ATTIND)
C
      DSNCNT= 1
 10   CONTINUE
C       try the next data set
        IF (DSN(DSNCNT).GT.0) THEN
C         need to increment counter
          DSNCNT= DSNCNT+ 1
        END IF
C
        IF (DSNCNT.EQ.1) THEN
C         get first data set from main record label
          LREC= 1
          LIND= WDRCGO (MESSFL,LREC)
          I   = PTSNUM+ 15
          DSN(DSNCNT)= WIBUFF(I,LIND)
        ELSE
C         get next data set from last data-set label
          DSN(DSNCNT)= WIBUFF(2,LIND)
        END IF
C       WRITE(*,*) 'next attr data set:',DSNCNT,DSN(DSNCNT)
        IF (DSN(DSNCNT).GT.0) THEN
C         process this data set
          CALL WDDSCK(MESSFL,DSN(DSNCNT),
     O                LREC,RETCOD)
          LIND = WDRCGO(MESSFL,LREC)
          LPDAT= WIBUFF(11,LIND)
          PDATV= WIBUFF(12,LIND)
          LPOS = LPDAT+ 2
          IF (WIBUFF(LPOS,LIND).GT.0) THEN
C           get first label index
            IVAL= WIBUFF(LPOS+1,LIND)
            CALL WATWDS (IVAL,
     O                   I,ATTIND(1,DSNCNT))
C           now find last label
            LPOS= PDATV
 20         CONTINUE
C             back up to last label and get index
              LPOS= LPOS- 4
            IF (WIBUFF(LPOS,LIND).EQ.0) GO TO 20
C           get last label index
            IVAL= WIBUFF(LPOS+1,LIND)
            CALL WATWDS (IVAL,
     O                   I,ATTIND(2,DSNCNT))
C           WRITE(*,*) 'found:',DSNCNT,ATTIND(1,DSNCNT),ATTIND(2,DSNCNT)
          ELSE
C           nothing out on this data set, ignore it
            DSN(DSNCNT)= 0
C           WRITE(*,*) 'nothing in data set'
          END IF
          IF (DSNCNT.GT.1) THEN
C           sort indices
            I= DSNCNT
 30         CONTINUE
              IF (ATTIND(1,I).LT.ATTIND(1,I-1)) THEN
C               need to switch these indices
                ITMP= ATTIND(1,I)
                ATTIND(1,I)  = ATTIND(1,I-1)
                ATTIND(1,I-1)= ITMP
                ITMP= ATTIND(2,I)
                ATTIND(2,I)  = ATTIND(2,I-1)
                ATTIND(2,I-1)= ITMP
                ITMP    = DSN(I)
                DSN(I)  = DSN(I-1)
                DSN(I-1)= ITMP
              END IF
              I= I- 1
            IF (I.GT.1) GO TO 30
          END IF
        END IF
      IF (DSN(DSNCNT).GT.0 .AND. DSNCNT.LT.DSNMAX) GO TO 10
C
      IF (DSN(DSNCNT).EQ.0) THEN
C       count is one ahead of actual total
        DSNCNT= DSNCNT- 1
      END IF
C
C     WRITE(*,*) 'WADDSI:',DSNCNT,DSNMAX
C     IF (DSNCNT.GT.0) THEN
C       DO 40 I= 1,DSNMAX
C         WRITE(*,*) I,DSN(I),ATTIND(1,I),ATTIND(2,I)
C40     CONTINUE
C     END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WADGRA
     I                   (MESSFL,DPTR,ATTYP,
     O                    ATMIN,ATMAX)
C
C     + + + PURPOSE + + +
C     get the min and max values for an attribute off the message file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,DPTR,ATTYP
      REAL      ATMIN,ATMAX
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     DPTR   - pointer to start of details for this attribute
C     ATTYP  - attribute type
C     ATMIN  - minimum value for attribute
C     ATMAX  - maximum value for attribute
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,DREC,DPOS,BCWORD,ID,TLEN,IVAL(2)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (RVAL,IVAL)
      REAL         RVAL(2)
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDPTSP, WDNXDV, WDPRPS, WATWDS, WMSSKB
C
C     + + + END SPECIFICATIONS + + +
C
      CALL WDPTSP (DPTR,
     O             DREC,DPOS)
C     back up one data value, then get the next one to get first value
      CALL WDPRPS (MESSFL,
     M             DREC,DPOS,
     O             I)
      CALL WDNXDV (MESSFL,
     M             DREC,DPOS,
     O             BCWORD)
      CALL WATWDS (BCWORD,
     O             ID,TLEN)
C
 10   CONTINUE
C       loop through details until range is found
        IF (ID.EQ.3) THEN
C         range found
          DO 20 I= 1,2
C           get min and max off message file
            CALL WDNXDV (MESSFL,
     M                   DREC,DPOS,
     O                   IVAL(I))
  20      CONTINUE
          IF (ATTYP.EQ.1) THEN
            ATMIN= IVAL(1)
            ATMAX= IVAL(2)
          ELSE
            ATMIN= RVAL(1)
            ATMAX= RVAL(2)
          END IF
          ID= 0
        ELSE
C         skip to the next block of info
          CALL WMSSKB (MESSFL,TLEN,
     M                 DREC,DPOS)
          CALL WDNXDV (MESSFL,
     M                 DREC,DPOS,
     O                 BCWORD)
          CALL WATWDS (BCWORD,
     O                 ID,TLEN)
        END IF
      IF (ID.GT.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   WADGVA
     I                   (MESSFL,DPTR,MXLEN,
     O                    CLEN,SATVAL)
C
C     + + + PURPOSE + + +
C     get the valid values for an attribute off the message file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,DPTR,MXLEN,CLEN
      CHARACTER*1 SATVAL(MXLEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     DPTR   - pointer to start of details for this attribute
C     MXLEN  - maximum length of valid values
C     CLEN   - total length of valid values
C     SATVAL - array of valid values for attribute
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,DREC,DPOS,BCWORD,ID,TLEN,MLEN,GLEN,CONT,OLEN,OPOS
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDPTSP, WDNXDV, WDPRPS, WATWDS, WMSSKB, WMSGTE
C
C     + + + END SPECIFICATIONS + + +
C
      CALL WDPTSP (DPTR,
     O             DREC,DPOS)
C     back up one data value, then get the next one to get first value
      CALL WDPRPS (MESSFL,
     M             DREC,DPOS,
     O             I)
      CALL WDNXDV (MESSFL,
     M             DREC,DPOS,
     O             BCWORD)
      CALL WATWDS (BCWORD,
     O             ID,TLEN)
C
 10   CONTINUE
C       loop through details until valid is found
        IF (ID.EQ.4) THEN
C         valids found
          GLEN= 0
          MLEN= 0
          I   = 130
          OPOS= 1
 20       CONTINUE
            CALL WMSGTE (MESSFL,TLEN,I,
     M                   DREC,DPOS,GLEN,MLEN,
     O                   OLEN,SATVAL(OPOS),CONT)
            OPOS= OPOS+ OLEN
          IF (CONT.EQ.1) GO TO 20
          ID= 0
        ELSE
C         skip to the next block of info
          CALL WMSSKB (MESSFL,TLEN,
     M                 DREC,DPOS)
          CALL WDNXDV (MESSFL,
     M                 DREC,DPOS,
     O                 BCWORD)
          CALL WATWDS (BCWORD,
     O                 ID,TLEN)
        END IF
      IF (ID.GT.0) GO TO 10
C
      CLEN= TLEN
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDSAGY
     I                    (MESSFL,SAIND,
     O                     SANAM,DPTR,SATYP,SALEN,SARQWD,SAUPFG)
C
C     + + + PURPOSE + + +
C     gets general detail information about specified attribute
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SAIND,DPTR,SATYP,SALEN,SARQWD,SAUPFG
      CHARACTER*1 SANAM(6)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SAIND  - attribute index number
C     SANAM  - name of search attribute
C     DPTR   - pointer to other details of attribute
C     SALEN  - length of attribute
C     SATYP  - type of attribute
C     SARQWD - word containing attribute requirements by dsn type
C     SAUPFG - attribute update flag
C
C     + + + PARAMETERS + + +
      INTEGER    DSNMAX
      PARAMETER (DSNMAX=10)
C
C     + + + SAVES + + +
      INTEGER     DSINIT,DSNCNT,DSN(DSNMAX),ATTIND(2,DSNMAX)
      SAVE        DSINIT,DSNCNT,DSN,ATTIND
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,IGOT
      CHARACTER*1 BLNK
C
C     + + + EXTERNALS + + +
      EXTERNAL    WADDSI, WADGTL, ZIPC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA DSINIT / 0 /
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DSINIT.EQ.0) THEN
C       determine first and last attributes on data sets
        CALL WADDSI (MESSFL,DSNMAX,
     O               DSNCNT,DSN,ATTIND)
        DSINIT= 1
      END IF
C
      I   = 0
      IGOT= 0
 10   CONTINUE
        I= I+ 1
        IF (SAIND.GE.ATTIND(1,I) .AND. SAIND.LE.ATTIND(2,I)) THEN
C         attribute is on this data set
          IGOT= 1
C         get all the label info
          CALL WADGTL (MESSFL,DSN(I),SAIND,
     O                 SANAM,DPTR,SATYP,SALEN,SARQWD,SAUPFG)
        END IF
      IF (IGOT.EQ.0 .AND. I.LT.DSNMAX) GO TO 10
      IF (IGOT.EQ.0) THEN
C       attribute index not found, set output arguments to undefined
        I     = 6
        BLNK  = ' '
        CALL ZIPC (I,BLNK,SANAM)
        DPTR  = 0
        SATYP = 0
        SALEN = 0
        SARQWD= 0
        SAUPFG= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDBSAC
     I                    (WDMSFL,DSN,MESSFL,SAIND,SALEN,SAVAL,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     adds (or modifies) character search attribute on given dsn
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDMSFL,DSN,MESSFL,SAIND,SALEN,RETCOD
      CHARACTER*1 SAVAL(SALEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - data-set number being modified
C     MESSFL - message file unit number
C     SAIND  - index number of attribute or
C              highest attribure number if printing
C     SALEN  - length of attribute
C     SAVAL  - value of attribute
C     RETCOD - return code indicating if add or mod was successful
C                0 - successful
C              -81 - data set does not exist
C             -101 - incorrect character value for attribute
C             -103 - no room on label for attribute
C             -104 - data present, can't update attribute
C             -105 - attribute not allowed for this type data set
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + PARAMETERS + + +
      INTEGER     MLEN
      PARAMETER  (MLEN=300)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,K,RREC,RIND,PSAVAL,DPTR,DELFG,
     1            CNUM,CLEN,SATYP,SAUPFG,SARQWD,LWDMFL,LDSN
      CHARACTER*1 LCBUF(MLEN)
      CHARACTER*4 C4DUM
C
C     + + + FUNCTIONS + + +
      INTEGER     WDRCGO, CHKSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDDSCK, WDRCGO, WDRCUP, CHKSTR
      EXTERNAL    WDSASP, WDSAGY, WADGVA, WDDPAR, WID2UD
C
C     + + + INPUT FORMATS + + +
 1020 FORMAT (A4)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (4A1)
C
C     + + + END SPECIFICATIONS + + +
C
      DELFG = 0
      RETCOD= 0
      SATYP = 3
      CALL WDSAGY (MESSFL,SAIND,
     O             LCBUF(295),DPTR,I,I,SARQWD,SAUPFG)
C     get valid attribute values
      CALL WADGVA (MESSFL,DPTR,MLEN,
     O             CLEN,LCBUF)
C     determine number of valid values
      CNUM= CLEN/SALEN
      IF (CNUM.GT.0) THEN
C       valid values exist
        I= CHKSTR(SALEN,CNUM,SAVAL,LCBUF)
        IF (I.EQ.0) THEN
C         not a valid character attribute value
          RETCOD= -101
        END IF
      END IF
      IF (RETCOD.EQ.0) THEN
C       adjust wdm and dsn as needed
        CALL WID2UD (WDMSFL,DSN,
     O               LWDMFL,LDSN)
C       does data set exist?
        CALL WDDSCK(LWDMFL,LDSN,
     O              RREC,RETCOD)
        IF (RETCOD.EQ.0) THEN
C         it does, get the label
          RIND= WDRCGO(LWDMFL,RREC)
C         is it ok to add/update the attribute?
          CALL WDDPAR (RREC,SARQWD,SAUPFG,WIBUFF(1,RIND),DELFG,
     O                 RETCOD)
        END IF
      END IF
      IF (RETCOD.EQ.0) THEN
C       add the attribute, first get where it starts
        CALL WDSASP (SAIND,SALEN,SATYP,
     M               WIBUFF(1,RIND),
     O               PSAVAL,RETCOD)
        IF (RETCOD.EQ.-102) THEN
C         no problem, it was already on label so adding not required
          RETCOD= 0
        END IF
        IF (PSAVAL.GT.0) THEN
C         ok to add/modify, do it
          K= -1
          DO 30 I= 1,SALEN,4
            K= K+ 1
            WRITE(C4DUM,2000) (SAVAL(J),J=I,I+3)
            READ (C4DUM,1020) WIBUFF(PSAVAL+K,RIND)
 30       CONTINUE
          CALL WDRCUP(LWDMFL,RIND)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDBSAI
     I                    (WDMSFL,DSN,MESSFL,SAIND,SALEN,SAVAL,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     adds (or modifies) integer search attribute on given dsn
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,MESSFL,SAIND,SALEN,RETCOD
      INTEGER   SAVAL(SALEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - data-set number being modified
C     MESSFL - message file unit number
C     SAIND  - index number of attribute or
C              highest attribure number if printing
C     SALEN  - length of attribute
C     SAVAL  - value of attribute
C     RETCOD - return code indicating if add or mod was successful
C                0 - successful
C              -81 - data set does not exist
C             -103 - no room on label for attribute
C             -104 - data present, can't update attribute
C             -105 - attribute not allowed for this type data set
C             -108 - incorrect integer value for attribute
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I0,RREC,RIND,IMIN,IMAX,PSAVAL,DPTR,DELFG,
     1            ICHK,SATYP,SAUPFG,SARQWD,LWDMFL,LDSN
      REAL        RMIN,RMAX
      CHARACTER*1 TBUFF(6)
C
C     + + + FUNCTIONS + + +
      INTEGER     WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDDSCK, WDRCGO, WDRCUP, WDSASP
      EXTERNAL    WDSAGY, WADGRA, CHKINT, WDDPAR, WID2UD
C
C     + + + END SPECIFICATIONS + + +
C
      I0    = 0
      DELFG = 0
      RETCOD= 0
      SATYP = 1
      CALL WDSAGY (MESSFL,SAIND,
     O             TBUFF,DPTR,I,I,SARQWD,SAUPFG)
C     get min, max, default for attribute
      CALL WADGRA (MESSFL,DPTR,SATYP,
     O             RMIN,RMAX)
      IMIN= RMIN
      IMAX= RMAX
      DO 10 I= 1,SALEN
        CALL CHKINT (IMIN,IMAX,I0,SAVAL(I),ICHK)
        IF (ICHK.EQ.0) THEN
C         bad value for integer attribute
          RETCOD= -108
        END IF
 10   CONTINUE
      IF (RETCOD.EQ.0) THEN
C       adjust wdm and dsn as needed
        CALL WID2UD (WDMSFL,DSN,
     O               LWDMFL,LDSN)
C       does data set exist
        CALL WDDSCK(LWDMFL,LDSN,
     M              RREC,RETCOD)
        IF (RETCOD.EQ.0) THEN
C         data set exists
          RIND  = WDRCGO(LWDMFL,RREC)
C         is it ok to add/update the attribute?
          CALL WDDPAR (RREC,SARQWD,SAUPFG,WIBUFF(1,RIND),DELFG,
     O                 RETCOD)
        END IF
      END IF
      IF (RETCOD.EQ.0) THEN
C       add the attribute, first get where it starts
        CALL WDSASP (SAIND,SALEN,SATYP,
     M               WIBUFF(1,RIND),
     O               PSAVAL,RETCOD)
        IF (RETCOD.EQ.-102) THEN
C         no problem, it was already on label so adding not required
          RETCOD= 0
        END IF
        IF (PSAVAL.GT.0) THEN
C         ok to add/modify, do it
          DO 30 I= 1,SALEN
            WIBUFF(PSAVAL+I-1,RIND)= SAVAL(I)
 30       CONTINUE
          CALL WDRCUP(LWDMFL,RIND)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDSAFL
     I                   (SAIND,TIBUFF,
     O                    SAPOS,RETCOD)
C
C     + + + PURPOSE + + +
C     determines where values for particular search attribute
C     start within data-set label, returns 0 if attribute is
C     not present
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   SAIND,SAPOS,RETCOD
      INTEGER*4 TIBUFF(512)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SAIND  - index of particular search attribute
C     TIBUFF - buffer of search attributes
C     SAPOS  - position of search attribute on label
C     RETCOD - return code
C                 0 - attribute found, position returned
C              -107 - attribute not present on this data set
C
C     + + + FUNCTIONS + + +
      INTEGER   WDSASV
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDSASV
C
C     + + + END DECLARATIONS + + +
C
      RETCOD= 0
      SAPOS = WDSASV(SAIND,TIBUFF)
C
      IF (SAPOS.EQ.0) THEN
C       attribute not found
        RETCOD= -107
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDDPAR
     I                   (RREC,SARQWD,SAUPFG,TIBUFF,DELFG,
     O                    RETCOD)
C
C     + + + PURPOSE + + +
C     determines if either data is present and attribute can't be updated
C     or attribute is not allowed for this dataset type
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   RREC,SARQWD,SAUPFG,DELFG,RETCOD
      INTEGER*4 TIBUFF(512)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RREC   - record containing data-set label
C     SARQWD - search attribute required word
C     SAUPFG - flag indicating if attribute may be updated if data present
C              0 - yes, 1 - no
C     TIBUFF - array containing data-set label
C     DELFG  - delete attribute indicator
C              0 - not trying to delete
C              1 - trying to delete
C     RETCOD - return code
C                0 - attribute can be added or updated
C             -104 - data present, can't update attribue
C             -105 - attribute not allowed for this type dataset
C             -106 - attribute required for this type data set, can't delete
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   DSTYPE,SAREQ(10),DPFLG
C
C     + + + FUNCTIONS + + +
      INTEGER   WDDTFG
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDDTFG,WATTUS
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
C     what type data set
      DSTYPE= TIBUFF(6)
C     is data present
      DPFLG = WDDTFG(RREC,TIBUFF(1))
C     determine required attributes for data sets
      CALL WATTUS (SARQWD,
     O             SAREQ)
      IF (DPFLG.EQ.1 .AND. SAUPFG.EQ.1) THEN
C       data present, cant update attribute
        RETCOD= -104
      ELSE IF (SAREQ(DSTYPE).EQ.0) THEN
C       attribute not allowed for this type data set
        RETCOD= -105
      ELSE IF (SAREQ(DSTYPE).EQ.2 .AND. DELFG.EQ.1) THEN
C       attribute required for this type data set
        RETCOD= -106
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDTGET
     I                    (WDMSFL,DSN,DELT,DATES,NVAL,
     I                     DTRAN,QUALFG,TUNITS,
     O                     RVAL,RETCOD)
C
C     + + + PURPOSE + + +
C     gets timeseries information from the WDMSFL
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,DELT,DATES(6),NVAL,DTRAN,QUALFG,
     1          TUNITS,RETCOD
      REAL      RVAL(NVAL)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - data-set number
C     DELT   - time step for get
C     DATES  - starting date
C     NVAL   - number of values
C     DTRAN  - transformation code
C              0 - ave,same
C              1 - sum,div
C              2 - max
C              3 - min
C     QUALFG - allowed quality code
C     TUNITS - time units for get
C     RVAL   - array to place retrieved values in
C     RETCOD - return code
C                0 - everything O.K.
C               -8 - invalid date
C              -14 - date specified not within valid range for data set
C              -20 - problem with one or more of following:
C                    GPFLG, DXX, NVAL, QUALVL, LTSTEP, LTUNIT
C              -21 - date from WDM doesn't match expected date
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -84 - data set number out of range
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   GPFLG,GPOSEN,GPIND,LTSTEP,LTUNIT,TDSFRC,TGROUP,TSPTAD,
     1          ENDDAT(6),GPSDAT(6),GETDAT(6),TSPSC1,TSPSC2,
     2          COMPFG,TSFORM,VBTIME,TSSTEP,TCODE,GETQK,RIND,
     3          LWDMFL,LDSN
      INTEGER*4 I4NVAL
      REAL      DEFVAL,TOLR,TSFILL,GETQRA,RTMP
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + INTRINSICS + + +
      INTRINSIC FLOAT, MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL  ZIPR, WTPMCK, WTFNDG, WDATCP, WTGTVL, WDRCGO, WTDSPX
      EXTERNAL  WTSCSC,WID2UD
C
C     + + + END SPECIFICATIONS + + +
C
      I4NVAL= NVAL
      LTSTEP= DELT
      LTUNIT= TUNITS
      GPFLG = 1
      TSFILL= 0.0
      CALL WID2UD (WDMSFL,DSN,
     O             LWDMFL,LDSN)
C
C     check the user supplied parameters
      CALL WTPMCK (GPFLG,DTRAN,DATES,NVAL,QUALFG,
     M             LTSTEP,LTUNIT,
     O             RETCOD)
      IF (RETCOD.EQ.0) THEN
C       check the data set and figure out which groups have been req.
        CALL WTFNDG (LWDMFL,LDSN,GPFLG,DATES,LTSTEP,LTUNIT,I4NVAL,
     O               TDSFRC,TSFILL,TGROUP,TOLR,TSPTAD,
     1               GPIND,GPOSEN,GPSDAT,ENDDAT,RETCOD)
      END IF
C     fill in RVAL with defaults
      DEFVAL= TSFILL
C     max
      IF (DTRAN.EQ.2) DEFVAL= -1.0E30
C     min
      IF (DTRAN.EQ.3) DEFVAL= 1.0E30
      CALL ZIPR (NVAL,DEFVAL,
     O           RVAL)
C
      IF (RETCOD.EQ.0) THEN
C       get additional parameters
        RIND= WDRCGO(LWDMFL,TDSFRC)
        CALL WTDSPX (WIBUFF(1,RIND),
     O               COMPFG,TSFORM,VBTIME,TSSTEP,TCODE)
        GETQK= 0
C       can we do a quick get?
        IF (VBTIME.EQ.1) THEN
C         yes, if time units and step ok
          IF (TCODE.LE.4.AND.LTUNIT.LE.4) THEN
C           time units days or shorter, a quick get may work
            CALL WTSCSC (LTUNIT,LTSTEP,TSPSC1)
            CALL WTSCSC (TCODE,TSSTEP,TSPSC2)
            GETQRA= 1.0E-8+ FLOAT(TSPSC2)/FLOAT(TSPSC1)
            RTMP  = GETQRA
            IF (RTMP.LT.1.0) THEN
C             wdm interval less than user interval
              RTMP= 1.0/ GETQRA
            END IF
            IF (MOD(RTMP,1.0).LT.1.0E-6) THEN
C             ok to do a quick get
              GETQK= 1
            END IF
          ELSE IF (TCODE.EQ.LTUNIT) THEN
C           time units are the same, a quick get will work
            GETQK = 1
            GETQRA= 1.0E-8+ FLOAT(TSSTEP)/FLOAT(LTSTEP)
          END IF
        ELSE
C         do a general get
          GETQK= 0
        END IF
C       make a working copy of the starting date
        CALL WDATCP (DATES,GETDAT)
C       get the data
        CALL WTGTVL (LWDMFL,LDSN,GPOSEN,NVAL,LTUNIT,LTSTEP,DTRAN,
     I               QUALFG,ENDDAT,TDSFRC,TSFILL,TGROUP,
     I               GETQK,GETQRA,VBTIME,
     M               RVAL,GETDAT,GPSDAT,GPIND,
     O               RETCOD)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTSCSC
     I                    (TUNITS,TSSTEP,
     I                     TSPSEC)
C
C     + + + PURPOSE + + +
C     converts the given time units and time step to seconds
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TUNITS,TSSTEP,TSPSEC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TUNITS - time units
C     TSSTEP - time step
C     TSPSEC - time in seconds
C
C     + + + END SPECIFICATIONS + + +
C
      TSPSEC= TSSTEP
      GO TO (40,30,20,10), TUNITS
 10   CONTINUE
C       day units to hours
        TSPSEC= TSPSEC* 24
 20   CONTINUE
C       hours to minutes
        TSPSEC= TSPSEC* 60
 30   CONTINUE
C       minutes to seconds
        TSPSEC= TSPSEC* 60
 40   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTPMCK
     I                    (GPFLG,DXX,DATES,NVAL,QUALVL,
     M                     LTSTEP,LTUNIT,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     checks the parameters supplied to either WDTPUT or WDTGET
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   GPFLG,LTSTEP,DXX,DATES(6),NVAL,QUALVL,LTUNIT,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GPFLG  - get/put flag
C              1 - get
C              2 - put
C     DXX    - transform or overwrite flag
C     DATES  - starting date
C     NVAL   - number of values
C     QUALVL - quality of data code
C     LTSTEP - time step
C     LTUNIT - time units
C     RETCOD - return code
C                0 - everything is O.K.
C               -8 - invalid date
C              -20 - problem with one or more of following:
C                    GPFLG, DXX, NVAL, QUALVL, LTSTEP, LTUNIT
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   DONFG,I,CONV(7)
C
C     + + + INTRINSICS + + +
      INTRINSIC MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL  TSBINI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CONV/60,60,24,999,12,100,999/
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
C
C     may need to initialize time-series buffer stuff
      CALL TSBINI
C
C     check dates
      IF (DATES(1).LT.1.OR.DATES(1).GT.32000  .OR.
     1    DATES(2).LT.1.OR.DATES(2).GT.12     .OR.
     2    DATES(3).LT.1.OR.DATES(3).GT.31     .OR.
     3    DATES(4).LT.0.OR.DATES(4).GT.24     .OR.
     4    DATES(5).LT.0.OR.DATES(5).GT.59     .OR.
     5    DATES(6).LT.0.OR.DATES(6).GT.59) RETCOD= -8
C     check DXX (DTRAN for get, DTOVWR for put)
      IF (DXX.LT.0.OR.(GPFLG.EQ.1.AND.DXX.GT.3).OR.
     1    (GPFLG.EQ.2.AND.DXX.GT.0)) THEN
C       bad parameter
        RETCOD= -20
      END IF
C     check other parameters
      IF (NVAL.LT.1.OR.
     1    LTUNIT.LT.1.OR.LTUNIT.GT.7.OR.
     2    QUALVL.LT.0.OR.QUALVL.GT.31) THEN
C       bad parameter
        RETCOD= -20
      END IF
      IF (RETCOD.EQ.0) THEN
C       check LTSTEP with regard to specified LTUNIT
        I= LTUNIT
        DONFG= 0
 10     CONTINUE
          IF (MOD(LTSTEP,CONV(I)).EQ.0) THEN
            LTSTEP= LTSTEP/CONV(I)
            I= I+ 1
            LTUNIT= I
          ELSE
            DONFG= 1
          END IF
        IF (DONFG.EQ.0) GO TO 10
        IF (LTSTEP.LT.1.OR.LTSTEP.GT.63) THEN
C         bad parameter
          RETCOD= -20
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTFNDG
     I                    (WDMSFL,DSN,GPFLG,DATES,LTSTEP,LTUNIT,I4NVAL,
     O                     TDSFRC,TSFILL,TGROUP,TOLR,TSPTAD,
     O                     GPOSST,GPOSEN,GPSDAT,ENDDAT,RETCOD)
C
C     + + + PURPOSE + + +
C     check the data set,
C     computes ending date, start and end group pointers and
C     number of values to skip in first group
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,GPFLG,DATES(6),LTSTEP,LTUNIT,
     1          TDSFRC,TGROUP,TSPTAD,GPOSST,GPOSEN,
     2          ENDDAT(6),GPSDAT(6),RETCOD
      INTEGER*4 I4NVAL
      REAL      TSFILL,TOLR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - timeseries data-set number
C     GPFLG  - get/put flag
C              1 - get
C              2 - put
C     DATES  - starting date
C     LTSTEP - time step
C     LTUNIT - time units
C     I4NVAL - number of values
C     TDSFRC - first record in data set
C     TSFILL - missing data filler code
C     TGROUP - data group pointer units
C     TOLR   - compression tolerance
C     TSPTAD - put data transform code
C     GPOSST - start data group pointer index
C     GPOSEN - end data group pointer index
C     GPSDAT - start date of first group
C     ENDDAT - end date for get or put
C     RETCOD - return code
C                0 - everything O.K.
C              -14 - date specified not within valid range for data set
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -84 - data set number out of range
C              -85 - trying to write to a read-only data set
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,TBSDAT(6),TGRPST,RIND,PDAT,PDATV,DSTYP,GRCNT
      INTEGER*4 GRPOFF,TGRNUM
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO, TIMCHK
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDSCHA, WDRCGO, WTDSPM, TIMADD, TIMCNV, WTSGRP, TIMDIF
      EXTERNAL  TIMCHK
C
C     + + + END SPECIFICATIONS + + +
C
C     check basics about data set
      DSTYP= 1
      CALL WDSCHA (WDMSFL,DSN,DSTYP,GPFLG,
     O             TDSFRC,GRCNT,RETCOD)
C
      IF (RETCOD.EQ.0) THEN
C       get data-set parameters
        RIND= WDRCGO(WDMSFL,TDSFRC)
        CALL WTDSPM (WDMSFL,WIBUFF(1,RIND),WRBUFF(1,RIND),
     O               TSFILL,TGROUP,TOLR,TBSDAT,TSPTAD)
C
C       calculate pointer to first group pointer
        PDAT  = WIBUFF(11,RIND)
        TGRPST= PDAT+ 2
C
C       calculate max number of group pointers
        PDATV = WIBUFF(12,RIND)
        TGRNUM= PDATV- PDAT- 2
C
C       calculate ending date
        CALL TIMADD (DATES,LTUNIT,LTSTEP,I4NVAL,
     O               ENDDAT)
C
C       convert end date to old date format
        CALL TIMCNV (ENDDAT)
C
C       determine beginning of first group
        CALL WTSGRP (DATES,TGROUP,
     O               GPSDAT)
C
        IF (TIMCHK(TBSDAT,GPSDAT).EQ.-1) THEN
C         base year after start of data to add
          GRPOFF= -1
        ELSE
C         determine starting pointer index
          I= 1
          CALL TIMDIF (TBSDAT,GPSDAT,TGROUP,I,
     O                 GRPOFF)
        END IF
        IF (GRPOFF.LT.0 .OR. GRPOFF.GE.TGRNUM) THEN
C         date specified not within valid range for data set
          RETCOD= -14
        END IF
        GPOSST= TGRPST+ GRPOFF
C
C       determine ending pointer index
        I= 1
        CALL TIMDIF (TBSDAT,ENDDAT,TGROUP,I,
     O               GRPOFF)
        IF (GRPOFF.LT.0 .OR. GRPOFF.GE.TGRNUM) THEN
C         date specified not within valid range for data set
          RETCOD= -14
        ELSE
C         do we end exactly on boundary
          CALL WTSGRP (ENDDAT,TGROUP,
     O                 TBSDAT)
          IF (TIMCHK(ENDDAT,TBSDAT).EQ.0) THEN
C           yes, we do
            GRPOFF= GRPOFF- 1
          END IF
          GPOSEN= TGRPST+ GRPOFF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTDSPM
     I                    (WDMSFL,TIBUFF,TRBUFF,
     O                     TSFILL,TGROUP,TOLR,TBSDAT,TSPTAD)
C
C     + + + PURPOSE + + +
C     obtains values for a variety of TIMSER parms from labels
C     or defaults
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,TGROUP,TBSDAT(6),TSPTAD
      INTEGER*4 TIBUFF(512)
      REAL      TRBUFF(512),TSFILL,TOLR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     TIBUFF - data-set label - integer version
C     TRBUFF - data-set label - decimal version
C     TSFILL - missing data filler code
C     TGROUP - data group pointer units
C     TOLR   - compression tolerance
C     TBSDAT - beginning date of data
C     TSPTAD - put data transform code
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,SAIND,POS
C
C     + + + FUNCTIONS + + +
      INTEGER   WDSASV
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDSASV, WTBYFX
C
C     + + + END SPECIFICATIONS + + +
C
C     set default dates
      TBSDAT(1)= -999
      TBSDAT(2)= 1
      TBSDAT(3)= 1
      TBSDAT(4)= 0
      TBSDAT(5)= 0
      TBSDAT(6)= 0
C
C     get beginning date pointers
      DO 10 I= 1,4
        SAIND= I+ 26
        POS  = WDSASV(SAIND,TIBUFF)
        IF (POS.GT.0) THEN
C         from label
          TBSDAT(I)= TIBUFF(POS)
        END IF
 10   CONTINUE
C
C     get missing data filler code
      SAIND= 32
      POS  = WDSASV(SAIND,TIBUFF)
      IF (POS.GT.0) THEN
C       from label
        TSFILL= TRBUFF(POS)
      ELSE
C       default
        TSFILL= 0.0
      END IF
C
C     get units for data group pointer
      SAIND= 34
      POS  = WDSASV(SAIND,TIBUFF)
      IF (POS.GT.0) THEN
C       from label
        TGROUP= TIBUFF(POS)
      ELSE
C       default
        TGROUP= 6
      END IF
C
C     get compression tolerance
      SAIND= 36
      POS  = WDSASV(SAIND,TIBUFF)
      IF (POS.GT.0) THEN
C       from label
        TOLR= TRBUFF(POS)
      ELSE
C       default
        TOLR= 1.0E-8
      END IF
C
C     get put transform code
      SAIND= 60
      POS  = WDSASV(SAIND,TIBUFF)
      IF (POS.GT.0) THEN
C       from label
        TSPTAD= TIBUFF(POS)
      ELSE
C       default
        TSPTAD= 0
      END IF
C
      IF (TBSDAT(1).EQ.-999) THEN
C       missing base year attribute, add it
        CALL WTBYFX (WDMSFL,TIBUFF(5),TGROUP,
     O               TBSDAT(1))
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTBYFX
     I                   (WDMSFL,DSN,TGROUP,
     O                    TBSYR)
C
C     + + + PURPOSE + + +
C     add base year attribute to a timeseries data set
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,TGROUP,TBSYR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - timeseries data-set number
C     TGROUP - data group pointer units
C     TBSYR  - base year
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   TDSFRC,RETCOD,RIND,PDAT,DPCNT,I,
     1          GPIND,GPPTR,GREC,GOFF,GIND,GPDAT(6),BASDAT(6),GVAL,
     2          SAIND,SATYP,SALEN,PSAVAL
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDDSCK,WDSASP,WDRCGO,WDRCUP,TIMDIF,WDATSP,WDPTSP
C
C     + + + END SPECIFICATIONS + + +
C
C     determine where data set starts
      CALL WDDSCK (WDMSFL,DSN,
     O             TDSFRC,RETCOD)
C     bring label into memory
      RIND= WDRCGO(WDMSFL,TDSFRC)
C     where does data start
      PDAT= WIBUFF(11,RIND)
C     how many data groups are present
      DPCNT= WIBUFF(PDAT,RIND)
      IF (DPCNT.GT.0) THEN
C       data is present, find out what base year was assumed to be
        GPPTR= 0
        GPIND= PDAT+ 1
C       loop to look for data
 10     CONTINUE
          GPIND= GPIND+ 1
          IF (WIBUFF(GPIND,RIND).GT.0) THEN
C           data exist in this group
            GPPTR= WIBUFF(GPIND,RIND)
C           find where group starts
            CALL WDPTSP (GPPTR,
     O                   GREC,GOFF)
            GIND= WDRCGO(WDMSFL,GREC)
C           find group where data begins
            CALL WDATSP (WIBUFF(GOFF,GIND),
     O                   GPDAT)
C           set other dates
            GPDAT(5) = 0
            GPDAT(6) = 0
            BASDAT(1)= 1899
            BASDAT(2)= 1
            BASDAT(3)= 1
            BASDAT(4)= 0
            BASDAT(5)= 0
            BASDAT(6)= 0
            I =1
            CALL TIMDIF (BASDAT,GPDAT,TGROUP,I,
     O                   GVAL)
C           convert units to groups
            GPIND= GPIND- PDAT- 2
            IF (GPIND.EQ.GVAL) THEN
C             old case
              TBSYR= 1899
            ELSE
C             new case
              TBSYR= 1900
            END IF
          END IF
        IF (GPPTR.EQ.0) GO TO 10
      ELSE
C       no data, default base year
        TBSYR= 1900
      END IF
C     write base year attribute on label to avoid this next time
      SAIND= 27
      SALEN= 1
      SATYP= 1
C     be sure label in memory
      RIND= WDRCGO(WDMSFL,TDSFRC)
C     where do we put it?
      CALL WDSASP (SAIND,SALEN,SATYP,
     M             WIBUFF(1,RIND),
     O             PSAVAL,RETCOD)
      IF (RETCOD.EQ.0) THEN
C       added attribute space successfully, fill in value
        WIBUFF(PSAVAL,RIND)= TBSYR
        CALL WDRCUP(WDMSFL,RIND)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTDSPX
     I                    (TIBUFF,
     O                     COMPFG,TSFORM,VBTIME,TSSTEP,TCODE)
C
C     + + + PURPOSE + + +
C     obtains values for a variety of timeseries parms from labels
C     or defaults
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   COMPFG,TSFORM,VBTIME,TSSTEP,TCODE
      INTEGER*4 TIBUFF(512)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TIBUFF - data-set label - integer version
C     COMPFG - compression flag
C     TSFORM - form of data
C     VBTIME - variable time step option
C     TSSTEP - timeseries timestep
C     TCODE  - timeseries time code
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SAIND,POS
C
C     + + + FUNCTIONS + + +
      INTEGER   WDSASV
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDSASV
C
C     + + + END SPECIFICATIONS + + +
C
C     get compression flag
      SAIND= 83
      POS  = WDSASV(SAIND,TIBUFF)
      IF (POS.GT.0) THEN
C       available from label
        COMPFG= TIBUFF(POS)
      ELSE
C       default to compressed
        COMPFG= 1
      END IF
C
C     form of data
      SAIND= 84
      POS  = WDSASV(SAIND,TIBUFF)
      IF (POS.GT.0) THEN
C       available from label
        TSFORM= TIBUFF(POS)
      ELSE
C       default to mean
        TSFORM= 1
      END IF
C
C     variable time step option
      SAIND= 85
      POS  = WDSASV(SAIND,TIBUFF)
      IF (POS.GT.0) THEN
C       available from label
        VBTIME= TIBUFF(POS)
      ELSE
C       default to time step may vary
        VBTIME= 2
      END IF
C
C     time step
      SAIND= 33
      POS  = WDSASV(SAIND,TIBUFF)
      IF (POS.GT.0) THEN
C       available from label
        TSSTEP= TIBUFF(POS)
      ELSE
C       no default
        TSSTEP= -999
      END IF
C
C     time units
      SAIND= 17
      POS  = WDSASV(SAIND,TIBUFF)
      IF (POS.GT.0) THEN
C       available from label
        TCODE= TIBUFF(POS)
      ELSE
C       no default
        TCODE= -999
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTSKVL
     I                    (WDMSFL,GPIND,GPSDAT,STDAT,
     I                     TDSFRC,TSFILL,TGROUP,BADJFG,ADDAFG,VBTIME,
     O                     CURREC,CURBKS,CURPOS,CURNOV,CURVAL,PREVAL,
     O                     CURTST,CURTUN,CURCMP,CURQUA,CURCNT,CURDAT,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     skips values within a WDMSFL timeseries group, also
C     outputs current block information in common CWTSDS, also
C     end of group date
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,GPIND,GPSDAT(6),STDAT(6),RETCOD,TDSFRC,TGROUP,
     1          BADJFG,ADDAFG,VBTIME,CURREC,
     2          CURBKS,CURPOS,CURTST,CURTUN,CURCMP,CURQUA,CURDAT(6)
      INTEGER*4 CURNOV,CURCNT
      REAL      TSFILL,CURVAL,PREVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     GPIND  - group index number
C     GPSDAT - starting date of group
C     STDAT  - date to skip to
C     TDSFRC - first record in data set
C     TSFILL - missing data filler code
C     TGROUP - data group pointer units
C     BADJFG - block adjustment for efficiency flag
C              0 - no
C              1 - yes
C     ADDAFG - data present flag
C     VBTIME - variable timestep indicator
C     CURREC - current record number
C     CURBKS - starting position of current block within current record
C     CURPOS - current position in current block
C     CURNOV - current number of values in current block
C     CURVAL - current value
C     PREVAL - previous value
C     CURTST - current time step
C     CURTUN - current time units
C     CURCMP - current compression code
C     CURQUA - current quality code
C     CURCNT - current position within block
C     CURDAT - current date of start of current value
C     RETCOD - return code
C                  0 - everything O.K.
C                -10 - no data in this group
C                -11 - no non missing data, data has not started yet
C                -21 - date from WDM doesn't match expected date
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   BLEDAT(6),BLSDAT(6)
C
C     + + + EXTERNALS + + +
      EXTERNAL  WTSKVX
C
C     + + + END SPECIFICATIONS + + +
C
      CALL WTSKVX (WDMSFL,GPIND,GPSDAT,STDAT,
     I             TDSFRC,TSFILL,TGROUP,BADJFG,ADDAFG,VBTIME,
     O             CURREC,CURBKS,CURPOS,CURNOV,CURVAL,PREVAL,
     O             CURTST,CURTUN,CURCMP,CURQUA,CURCNT,CURDAT,
     O             RETCOD,BLSDAT,BLEDAT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTSKVX
     I                    (WDMSFL,GPIND,GPSDAT,STDAT,
     I                     TDSFRC,TSFILL,TGROUP,BADJFG,ADDAFG,VBTIME,
     O                     CURREC,CURBKS,CURPOS,CURNOV,CURVAL,PREVAL,
     O                     CURTST,CURTUN,CURCMP,CURQUA,CURCNT,CURDAT,
     O                     RETCOD,BLSDAT,BLEDAT)
C
C     + + + PURPOSE + + +
C     skips values within a WDMSFL TIMESERIES group, also
C     returns current block information and end of group date
C     can fill an empty group with dummy data if ADDAFG=1
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,GPIND,GPSDAT(6),STDAT(6),
     1          RETCOD,BADJFG,ADDAFG,VBTIME,
     2          CURREC,CURBKS,CURPOS,CURTST,CURTUN,CURCMP,CURQUA,
     3          CURDAT(6),TDSFRC,TGROUP,BLSDAT(6),BLEDAT(6)
      INTEGER*4 CURNOV,CURCNT
      REAL      TSFILL,CURVAL,PREVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     GPIND  - group index number
C     GPSDAT - starting date of group
C     STDAT  - data to skip to
C     TDSFRC - first record in data set
C     TSFILL - missing data filler code
C     TGROUP - data group pointer units
C     BADJFG - block adjustment for efficiency flag
C              0 - no
C              1 - yes
C     ADDAFG - data present flag
C     VBTIME - variable timestep indicator
C     CURREC - current record number
C     CURBKS - starting position of current block within current record
C     CURPOS - current position in current block
C     CURNOV - current number of values in current block
C     CURVAL - current value
C     PREVAL - previous value
C     CURTST - current time step
C     CURTUN - current time units
C     CURCMP - current compression code
C     CURQUA - current quality code
C     CURCNT - current position within block
C     CURDAT - current date of start of current value
C     RETCOD - return code
C                  0 - everything O.K.
C                -10 - no data in this group
C                -11 - no non missing data, data has not started yet
C                -21 - date from WDM doesn't match expected date
C     BLSDAT - block start date array
C     BLEDAT - block end date
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   DIND,RIND,CHK,PDAT,TSPREC,SAIND,I,GTUNIT,GTSTEP,
     1          TDAT(6),EGPFLG,GPEDAT(6),TSTDAT(6),MINQUA,POS
      INTEGER*4 GPPTR,BCW,I4ZRO,NUMSKP,I4ONE,TDFREE,GVAL,LVAL,I4TMP,
     1          SVAL
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO,TIMCHK,WDSASV,WDRCGX
      INTEGER*4 WDPTCL,WDATCL,WBCWCL
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDRCGO, WDSASV, WDPTSP, WDRCGX, WDPTCL, WDATCL, WTEGRP
      EXTERNAL   TIMDIF, TIMADD, TIMCHK, WBCWCL, WTNWBK, WDRCUP, WDATSP
      EXTERNAL   WDSKBK, WBCWSP, WBCWSQ, WDATCP
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
      I4ZRO = 0
      I4ONE = 1
      DIND  = WDRCGO (WDMSFL,TDSFRC)
      GPPTR = WIBUFF(GPIND,DIND)
      IF (GPPTR.EQ.I4ZRO) THEN
        IF (ADDAFG.EQ.0) THEN
C         no data in this group
          RETCOD= -10
        ELSE
C         this group doesnt exist yet, create it with dummy data
C         time step
          SAIND= 33
          POS  = WDSASV(SAIND,WIBUFF(1,DIND))
          IF (POS.GT.0) THEN
C           from label
            GTSTEP= WIBUFF(POS,DIND)
          ELSE
C           default
            GTSTEP= 1
          END IF
C         figure out where free space starts
          PDAT  = WIBUFF(11,DIND)
          TDFREE= WIBUFF(PDAT+1,DIND)
C         get pointer record flag from label if available
          SAIND = 31
          I     = WDSASV(SAIND,WIBUFF(1,DIND))
          IF (I.GT.0) THEN
C           from label
            TSPREC= WIBUFF(I,DIND)
          ELSE
C           default pointer new record flag to no
            TSPREC= 0
          END IF
          CALL WDPTSP (TDFREE,
     O                 CURREC,CURPOS)
          IF (TSPREC.EQ.1.OR.CURPOS.GT.510.OR.CURPOS.EQ.0) THEN
C           the new group will start on a new record, it follows current
            I   = 0
            RIND  = WDRCGX(WDMSFL,I,CURREC)
            CURPOS= 5
            CURREC= RECNO(RIND)
          END IF
C         calc the group pointer to go into the directory later
          GPPTR = WDPTCL(CURREC,CURPOS)
C         fill in date field and undefined values in the new group
          RIND  = WDRCGO(WDMSFL,CURREC)
          WIBUFF(CURPOS,RIND)= WDATCL(GPSDAT)
          CURBKS= CURPOS+ 1
          CURPOS= CURBKS+ 1
          CURCMP= 1
          CURQUA= 31
          IF (VBTIME.NE.2) THEN
C           need constant interval data
            GTUNIT= ADDAFG
          ELSE
C           use most efficient units
            GTUNIT= TGROUP
            GTSTEP= 1
          END IF
C         find end of group
          CALL WTEGRP (GPSDAT,TGROUP,
     O                 GPEDAT)
 10       CONTINUE
C           find number of intervals in group
            CALL TIMDIF (GPSDAT,GPEDAT,GTUNIT,GTSTEP,
     O                   GVAL)
C           be sure we end on a group boundary
            CALL TIMADD (GPSDAT,GTUNIT,GTSTEP,GVAL,
     O                   TDAT)
C           check boundary of starting date
            CALL TIMDIF (GPSDAT,STDAT,
     O                   GTUNIT,GTSTEP,SVAL)
            CALL TIMADD (GPSDAT,GTUNIT,GTSTEP,SVAL,
     O                   TSTDAT)
            EGPFLG= 0
            IF (TIMCHK(TDAT,GPEDAT).NE.0.OR.
     1          TIMCHK(TSTDAT,STDAT).NE.0) THEN
C             oops, did not end on a group boundary
C             or data start not on boundary
              EGPFLG= 1
              GTUNIT= GTUNIT- 1
            END IF
          IF (EGPFLG.EQ.1) GO TO 10
C         fill new group with dummy values
 20       CONTINUE
            LVAL = GVAL
            I4TMP= 32767
            IF (LVAL.GT.I4TMP) LVAL= I4TMP
            WIBUFF(CURBKS,RIND)=WBCWCL(LVAL,GTSTEP,GTUNIT,CURCMP,CURQUA)
            WRBUFF(CURPOS,RIND)= TSFILL
            CURPOS= CURPOS+ 1
            CALL WTNWBK (WDMSFL,
     M                   CURREC,CURPOS,
     O                   CURBKS)
            RIND= WDRCGO(WDMSFL,CURREC)
            GVAL= GVAL- LVAL
          IF (GVAL.GT.I4ZRO) GO TO 20
          CALL WDRCUP(WDMSFL,RIND)
C         update the directory
          DIND  = WDRCGO(WDMSFL,TDSFRC)
          PDAT  = WIBUFF(11,DIND)
C         update active group counter
          WIBUFF(PDAT,DIND)  = WIBUFF(PDAT,DIND)+ 1
C         update the data-set free space pointer
          TDFREE= WDPTCL(CURREC,CURBKS)
          WIBUFF(PDAT+1,DIND)= TDFREE
C         pointer to this groups data
          WIBUFF(GPIND,DIND) = GPPTR
C         write out the revised directory
          CALL WDRCUP(WDMSFL,DIND)
        END IF
      END IF
C
      IF (RETCOD.EQ.0) THEN
C       split up the pointer into record and offset
        CALL WDPTSP (GPPTR,
     O               CURREC,CURPOS)
C       get the starting record of the group
        DIND= WDRCGO (WDMSFL,CURREC)
C       check date from WDMS with expected date
        CALL WDATSP (WIBUFF(CURPOS,DIND),
     O               BLSDAT)
        BLSDAT(5)= 0
        BLSDAT(6)= 0
        IF (TIMCHK(BLSDAT,GPSDAT).NE.0) THEN
C         date from WDM doesn't match expected date
          RETCOD= -21
        END IF
      END IF
      IF (RETCOD.EQ.0) THEN
C       increment the offset to position of first BCW
        CURBKS= CURPOS
        NUMSKP= 1
        MINQUA= 31
 30     CONTINUE
C         skip to next BCW
          CALL WDSKBK (WDMSFL,NUMSKP,
     M                 CURREC,CURBKS)
C         be sure record is in buffer
          DIND= WDRCGO (WDMSFL,CURREC)
C         get the BCW
          BCW= WIBUFF(CURBKS,DIND)
C         split up the BCW
          IF (BADJFG.EQ.0) THEN
            CALL WBCWSP (BCW,
     O                   CURNOV,CURTST,CURTUN,CURCMP,CURQUA)
          ELSE
            CALL WBCWSQ (BCW,
     O                   CURNOV,CURTST,CURTUN,CURCMP,CURQUA)
          END IF
          IF (CURQUA.LT.MINQUA) MINQUA= CURQUA
C         find the end of this block
          CALL TIMADD (BLSDAT,CURTUN,CURTST,CURNOV,
     O                 BLEDAT)
C         are we before of after the start we are after
          CHK= TIMCHK (BLEDAT,STDAT)
          IF (CHK.GE.0) THEN
C           more values to skip, prepare to skip to next block
            NUMSKP= 2
            IF (CURCMP.EQ.0) THEN
C             uncompressed, skip number of values + bcw
              NUMSKP= CURNOV+ 1
            END IF
            CALL WDATCP (BLEDAT,BLSDAT)
          ELSE
C           we are in the block we want
C           recalc BCW without adjustment JLK 3/18/87
            CALL WBCWSP (BCW,
     O                   CURNOV,CURTST,CURTUN,CURCMP,CURQUA)
C           set NUMSKP to stop skipping
            NUMSKP= 0
C           figure out where in block
            CALL TIMDIF (BLSDAT,STDAT,CURTUN,CURTST,
     O                   CURCNT)
            IF (CURCNT.LT.0) THEN
C             reset boundary case
              CURCNT= 0
            END IF
C           figure out which date we are on
            CALL TIMADD (BLSDAT,CURTUN,CURTST,CURCNT,
     O                   CURDAT)
C           update the current position if on boundary
            CHK= TIMCHK(CURDAT,STDAT)
            IF (CHK.GE.0) THEN
C             reset boundary case
              CURCNT= CURCNT+ 1
            END IF
            IF (CURCNT.GT.CURNOV) THEN
C             we are at the beginning of next block, skip to it
              NUMSKP= 2
              IF (CURCMP.EQ.0) THEN
C               uncompressed, skip number of values + bcw
                NUMSKP= CURNOV+ 1
              END IF
              CALL WDATCP (BLEDAT,BLSDAT)
            END IF
          END IF
        IF (NUMSKP.NE.I4ZRO) GO TO 30
C       calculate the current value
        CURPOS= CURBKS+ 1
        IF (CURCMP.EQ.0) THEN
C         where is the current value
          CURPOS= CURBKS+ CURCNT
        END IF
        CURVAL= WRBUFF(CURPOS,DIND)
C       calculate the previous value
        IF (CURCNT.GT.I4ONE) THEN
C         skip ahead
          NUMSKP= CURBKS+ 1
          IF (CURCMP.EQ.0) THEN
C           where is the previous value
            NUMSKP= CURBKS+ CURCNT- 1
          END IF
          PREVAL= WRBUFF(NUMSKP,DIND)
        ELSE
C         don't know where previous value is
          PREVAL= TSFILL
        END IF
        IF (ADDAFG.EQ.0.AND.MINQUA.EQ.31) THEN
C         no non missing data, data has not started yet
          RETCOD= -11
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDSKBK
     I                    (WDMSFL,NUMSKP,
     M                     CURREC,CURPOS)
C
C     + + + PURPOSE + + +
C     skips to next WDMSFL block
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,CURREC,CURPOS
      INTEGER*4 NUMSKP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     NUMSKP - number of elements to skip
C     CURREC - current record number
C     CURPOS - current position within record
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RIND
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDRCGO
C
C     + + + END SPECIFICATIONS + + +
C
      CURPOS= CURPOS+ NUMSKP
      IF (CURPOS.EQ.512) THEN
C       force new record
        CURPOS= 513
      END IF
      IF (CURPOS.GT.512) THEN
C       new record needed
 10     CONTINUE
          RIND= WDRCGO(WDMSFL,CURREC)
C         get the pointer to next record
          CURREC= WIBUFF(4,RIND)
          CURPOS= CURPOS- 508
        IF (CURPOS.GT.512) GO TO 10
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTGTVL
     I                    (WDMSFL,DSN,GPOSEN,NVAL,GTTUN,GTTST,GTTRN,
     I                     QUALFG,ENDDAT,TDSFRC,TSFILL,TGROUP,
     I                     GETQK,GETQRA,VBTIME,
     M                     RVAL,GETDAT,GPSDAT,GPIND,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     fills in RVAL array with data values from WDMS DSN
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    WDMSFL,DSN,GPOSEN,NVAL,GTTUN,GTTST,GTTRN,
     1           QUALFG,ENDDAT(6),TDSFRC,TGROUP,GETQK,
     1           VBTIME,GETDAT(6),GPSDAT(6),GPIND,RETCOD
      REAL       RVAL(NVAL),TSFILL,GETQRA
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - timeseries data set number
C     GPOSEN - end data group pointer index
C     NVAL   - number of values
C     GTTUN  - get time units
C     GTTST  - get time step
C     GTTRN  - get transformation code
C     QUALFG - get quality code
C     ENDDAT - end of get date array
C     TDSFRC - first record in data set
C     TSFILL - missing data filler code
C     TGROUP - data group pointer units
C     GETQK  - do a quick get
C     GETQRA - quick get time step ratio (user/dsn)
C     VBTIME - variable timestep indicator
C     RVAL   - array of values retrieved from WDMS file
C     GETDAT - current get date array
C     GPSDAT - start date of first group
C     GPIND  - get group index number
C     RETCOD - return code
C                  0 - everything O.K.
C                -21 - date from WDM doesn't match expected date
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cwtsds.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   CURNXT(6),GETNXT(6),TMPNXT(6),WDADD,GTADD,IONE,CHK,ICNT,
     1          CHKS,TMPTUN,GPEDAT(6),NEWGRP,BADJFG,ADDAFG,EGPOS,TMPOS,
     2          I,TMPDAT(6)
      INTEGER*4 GETSPN,TMPSPN,CURSPN,I4ONE,I4NVAL,DPOS
      REAL      FRAC,CFRAC,RTMP
C
C     + + + FUNCTIONS + + +
      INTEGER   TIMCHK
C
C     + + + INTRINSICS + + +
      INTRINSIC FLOAT,ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDATCP, WTEGRP, WTSKVL, WTGTNV, TIMADD, TIMDIF, TIMCHK
C
C     + + + END SPECIFICATIONS + + +
C
C     always calculate ending intervals on first time through
      WDADD = 1
      GTADD = 1
      NEWGRP= 1
      IONE  = 1
      I4ONE = 1
      DPOS  = 1
      BADJFG= 1
      ADDAFG= 0
      I4NVAL= NVAL
      FRAC  = 0.0
      CFRAC = 0.0
      EGPOS = 0
      CALL WDATCP (GETDAT,TMPDAT)
C
 10   CONTINUE
        IF (NEWGRP.GE.1) THEN
C         find out the end of the group
          CALL WTEGRP (GPSDAT,TGROUP,
     O                 GPEDAT)
          IF (GPIND.EQ.GPOSEN) THEN
C           this is the last group, dont fill too far
            CALL WDATCP (ENDDAT,GPEDAT)
          END IF
C         skip values in group as required
          CALL WTSKVL (WDMSFL,GPIND,GPSDAT,TMPDAT,
     I                 TDSFRC,TSFILL,TGROUP,BADJFG,ADDAFG,VBTIME,
     O                 CURREC,CURBKS,CURPOS,CURNOV,CURVAL,PREVAL,
     O                 CURTST,CURTUN,CURCMP,CURQUA,CURCNT,CURDAT,
     O                 RETCOD)
          IF (RETCOD.EQ.-11) THEN
C           data has not started yet, this is ok
            RETCOD= 0
          END IF
C         how many intervals in group
          CALL TIMDIF (TMPDAT,GPEDAT,GTTUN,GTTST,
     O                 TMPOS)
          EGPOS = EGPOS+ TMPOS
          IF (GETQK.NE.0.AND.EGPOS.EQ.TMPOS) THEN
C           first quick get, be sure to use correct boundary
            CALL TIMDIF (CURDAT,GETDAT,GTTUN,GTTST,
     O                   GETQK)
            GETQK= GETQK+ 1
          END IF
          NEWGRP= 0
        END IF
        IF (RETCOD.EQ.0) THEN
          IF (WDADD.EQ.1) THEN
C           calculate new ending date on WDMS date
C           get the next WDS value
            CALL WTGTNV (WDMSFL,
     M                   CURCNT,CURNOV,CURCMP,CURREC,CURBKS,CURTST,
     1                   CURTUN,CURQUA,CURPOS,CURDAT,
     O                   CURVAL,CURNXT)
            TMPTUN= GTTUN
            IF (CURTUN.LT.TMPTUN) TMPTUN= CURTUN
          END IF
          IF (GETQK.EQ.0) THEN
C           not a quick get, do it all
            IF (GTADD.EQ.1) THEN
C             calculate new ending date for RVAL
              CALL TIMADD (GETDAT,GTTUN,GTTST,I4ONE,
     O                     GETNXT)
C             how many short units in get and WDMS block
              CALL TIMDIF (GETDAT,GETNXT,TMPTUN,IONE,
     O                     GETSPN)
              IF (GTTRN .LE. 1) THEN
C               some sort of data available, so initialize to zero
                RVAL(DPOS) = 0.0
              END IF
            END IF
C
C           figure out which interval should be incremented
            CHK= TIMCHK(GETNXT,CURNXT)
            IF (CHK.EQ.1) THEN
C             add interval to get counter
              GTADD= 1
              WDADD= 0
            ELSE IF (CHK.EQ.0) THEN
C             add intervals to both counters
              GTADD= 1
              WDADD= 1
            ELSE
C             add interval to WDMS fill counter
              GTADD= 0
              WDADD= 1
            END IF
C
            IF (QUALFG.GE.CURQUA) THEN
C             only process data of acceptable quality
              IF (GTTRN.LE.1) THEN
C               store end of this interval
                IF (CHK.EQ.1) THEN
C                 interval ends due to get
                  CALL WDATCP (GETNXT,TMPNXT)
                ELSE
C                 interval ends due to wdm or both
                  CALL WDATCP (CURNXT,TMPNXT)
                END IF
C               calc short units to end of interval
                CHKS= TIMCHK(GETDAT,CURDAT)
                IF (CHKS.EQ.1) THEN
                  CALL TIMDIF (CURDAT,TMPNXT,TMPTUN,IONE,
     O                         TMPSPN)
                ELSE
                  CALL TIMDIF (GETDAT,TMPNXT,TMPTUN,IONE,
     O                         TMPSPN)
                END IF
C
                IF (GTTRN.EQ.0) THEN
C                 transform is ave,same
                  FRAC= FLOAT(TMPSPN)/FLOAT(GETSPN)
                  RVAL(DPOS)= RVAL(DPOS)+ FRAC* CURVAL
                  CFRAC= CFRAC+ FRAC
C
                ELSE IF (GTTRN.EQ.1) THEN
C                 transform is sum,div
C                 how many short units spanned in WDMS interval
                  CALL TIMDIF (CURDAT,CURNXT,TMPTUN,IONE,
     O                         CURSPN)
                  FRAC= FLOAT(TMPSPN)/ FLOAT(CURSPN)
                  RVAL(DPOS)= RVAL(DPOS)+ FRAC* CURVAL
                  CFRAC= CFRAC+ (FLOAT(TMPSPN)/FLOAT(GETSPN))
                END IF
                IF (FRAC.GT.1.0) THEN
                  WRITE (*,*) 'BAD FRAC,TMPSPN:',FRAC,TMPSPN
                  WRITE (*,*) '  DSN,CTST,CTUN:',DSN,CURTST,CURTUN
                  WRITE (*,*) '  GETSPN,CURSPN:',GETSPN,CURSPN
                  WRITE (*,*) '  CURDAT:       ',CURDAT
                  WRITE (*,*) '  CURNXT:       ',CURNXT
                  WRITE (*,*) '  GETDAT:       ',GETDAT
                  WRITE (*,*) '  GETNXT:       ',GETNXT
                  WRITE (*,*) '  TMPNXT:       ',TMPNXT
                END IF
              ELSE IF (GTTRN.EQ.2) THEN
C               transform is max
                IF (RVAL(DPOS).LT.CURVAL) THEN
                  RVAL(DPOS)= CURVAL
                  CFRAC     = 1.0
                END IF
              ELSE IF (GTTRN.EQ.3) THEN
C               transform is min
                IF (RVAL(DPOS).GT.CURVAL) THEN
                  RVAL(DPOS)= CURVAL
                  CFRAC     = 1.0
                END IF
              END IF
            END IF
C
            IF (GTADD.EQ.1) THEN
C             get ready to increment get counter
              CALL WDATCP (GETNXT,GETDAT)
C             adjust value if some data didnt meet quality
              IF (CFRAC.LT.1.0.AND.CFRAC.GT.0.0) THEN
                RVAL(DPOS)= RVAL(DPOS)/ CFRAC
              ELSE IF (CFRAC.LE.0.0) THEN
                RVAL(DPOS)= TSFILL
              END IF
              CFRAC= 0.0
              DPOS = DPOS+ 1
            END IF
C
            IF (WDADD.EQ.1) THEN
C             get ready to increment WDMS counter
              CALL WDATCP (CURNXT,CURDAT)
              CURCNT= CURCNT+ 1
              IF (TIMCHK(CURDAT,GPEDAT).LE.0) THEN
C               at the group boundary, update start of group date
                CALL WDATCP (GPEDAT,GPSDAT)
                CALL WDATCP (GPEDAT,TMPDAT)
                NEWGRP= 1
                GPIND = GPIND+ 1
              END IF
            END IF
          ELSE
C           a quick get
            FRAC= FRAC+ GETQRA
            IF (ABS(GETQRA-1.0).LT.1.0E-5) THEN
C             no transform required
              IF (QUALFG.GE.CURQUA) THEN
C               only use data of acceptable quality
                RVAL(DPOS)= CURVAL
              END IF
              DPOS= DPOS+ 1
              FRAC= 0.0
            ELSE IF (GETQRA.LT.1.0) THEN
C             dsn interval less than user requested
              IF (QUALFG.GE.CURQUA) THEN
C               only use data of acceptable quality
                IF (GTTRN.LE.1 .AND. CFRAC.LT.1.0E-20) THEN
C                 data available & 1st time,   so initialize to zero
                  RVAL(DPOS) = 0.0
                END IF
                CFRAC= CFRAC+ GETQRA
                IF (GTTRN.EQ.0) THEN
C                 transform is aver
                  RVAL(DPOS)= RVAL(DPOS)+ GETQRA* CURVAL
                ELSE IF (GTTRN.EQ.1) THEN
C                 transform is sum
                  RVAL(DPOS)= RVAL(DPOS)+ CURVAL
                ELSE IF (GTTRN.EQ.2) THEN
C                 transform is max
                  IF (CURVAL.GT.RVAL(DPOS)) RVAL(DPOS)= CURVAL
                ELSE IF (GTTRN.EQ.3) THEN
C                 transform is min
                  IF (CURVAL.LT.RVAL(DPOS)) RVAL(DPOS)= CURVAL
                END IF
              END IF
              IF (ABS(FRAC-1.0).LT.1.0E-5) THEN
C               completed this user interval
                IF (ABS(CFRAC-1.0).GT.1.0E-5) THEN
C                 some missing data
                  IF (GTTRN.LE.1.AND.CFRAC.GT.0.0) THEN
C                   adjust result
                    RVAL(DPOS)= RVAL(DPOS)/CFRAC
                  END IF
                END IF
                DPOS = DPOS+ 1
                FRAC = 0.0
                CFRAC= 0.0
              END IF
            ELSE
C             dsn interval greater than user requested
              ICNT= GETQRA
C             may not start of wdm data boundary
              I   = GETQK
              RTMP= CURVAL
              IF (GTTRN.EQ.1) THEN
C               transform is sum/div
                RTMP= RTMP/GETQRA
              END IF
 20           CONTINUE
                IF (QUALFG.GE.CURQUA) THEN
C                 only use data of acceptable quality
                  RVAL(DPOS)= RTMP
                END IF
                DPOS= DPOS+ 1
                I   = I+ 1
              IF (I.LE.ICNT.AND.DPOS.LE.I4NVAL) GO TO 20
C             assume start on data boundary next time
              GETQK= 1
            END IF
C           always get the next dsn data value
            CALL WDATCP (CURNXT,CURDAT)
            CURCNT= CURCNT+ 1
          END IF
        ELSE IF (RETCOD.EQ.-10) THEN
C         missing entire group
          DPOS  = EGPOS+ 1
C         reset where we are in process of getting data
          CALL WDATCP (GPEDAT,GETDAT)
          GTADD = 1
          RETCOD= 0
        END IF
C
        IF (DPOS.GT.EGPOS .AND. NEWGRP.EQ.0) THEN
C         at the group boundary, update start of group date
          CALL WDATCP (GPEDAT,GPSDAT)
          CALL WDATCP (GPEDAT,TMPDAT)
          NEWGRP= 1
          GPIND = GPIND+ 1
        END IF
C
      IF (DPOS.LE.I4NVAL.AND.RETCOD.EQ.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTGTNV
     I                    (WDMSFL,
     M                     CURCNT,CURNOV,CURCMP,CURREC,CURBKS,CURTST,
     M                     CURTUN,CURQUA,CURPOS,CURDAT,
     O                     CURVAL,CURNXT)
C
C     + + + PURPOSE + + +
C     routine to get the next value from a WDS timeseries DSN
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,CURCMP,CURREC,CURBKS,CURTST,CURTUN,CURQUA,
     1          CURPOS,CURDAT(6),CURNXT(6)
      INTEGER*4 CURCNT,CURNOV
      REAL      CURVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     CURCNT - current position within block
C     CURNOV - current number of values in current block
C     CURCMP - current compression code
C     CURREC - current record number
C     CURBKS - starting position of current block within current record
C     CURTST - current time step
C     CURTUN - current time units
C     CURQUA - current quality code
C     CURPOS - current position in current block
C     CURDAT - current date of start of current value
C     CURVAL - current value
C     CURNXT - internal end date array
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RIND
      INTEGER*4 NUMSKP,BCW,I4ONE
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + INTRINSICS + + +
      INTRINSIC ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDSKBK, WDRCGO, WBCWSP, TIMADD
C
C     + + + END SPECIFICATIONS + + +
C
      I4ONE= 1
C
      IF (CURCNT.GT.CURNOV) THEN
C       time for a new block
        NUMSKP= 2
        IF (CURCMP.EQ.0) NUMSKP= CURNOV+ 1
C       skip to next BCW
        CALL WDSKBK (WDMSFL,NUMSKP,
     M               CURREC,CURBKS)
C       be sure record is in buffer
        RIND= WDRCGO (WDMSFL,CURREC)
C       get the BCW
        BCW= WIBUFF(CURBKS,RIND)
        CURVAL= WRBUFF(CURBKS+1,RIND)
C       split the BCW
        IF (ABS(CURVAL).GT.1.0E-3) THEN
C         zero maybe compressed (JLK 3/12/86 NOT ALL CASES,TEMP OLD WAY)
          CALL WBCWSP (BCW,
     O                 CURNOV,CURTST,CURTUN,CURCMP,CURQUA)
        ELSE
C         non zero, dont adjust CURTST and CURNOV
          CALL WBCWSP (BCW,
     O                 CURNOV,CURTST,CURTUN,CURCMP,CURQUA)
        END IF
C
        CURCNT= 1
      ELSE
C       be sure record is in buffer
        RIND= WDRCGO(WDMSFL,CURREC)
      END IF
C     get the current value
      CURPOS= CURBKS+ 1
      IF (CURCMP.EQ.0.AND.CURCNT.GT.0) CURPOS= CURBKS+ CURCNT
      CURVAL= WRBUFF(CURPOS,RIND)
C     calculate new ending date on WDMS interval
      CALL TIMADD (CURDAT,CURTUN,CURTST,I4ONE,
     O             CURNXT)
      RETURN
      END
C
C
C
      SUBROUTINE   WDTPFX
     I                    (WDMSFL,DSN,DELT,DATES,NVAL,
     I                     DTOVWR,QUALFG,TUNITS,RVAL,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     puts timeseries information into the WDMSFL.  This routine
C     was originally called WDTPUT.  There is a known problem with the
C     DTOVWR = 1 option.  The new WDTPUT traps that condition and has
C     a work-around for it.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,DELT,DATES(6),NVAL,DTOVWR,QUALFG,
     1          TUNITS,RETCOD
      REAL      RVAL(NVAL)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - data-set number
C     DELT   - time step for put
C     DATES  - starting date
C     NVAL   - number of values
C     DTOVWR - data overwrite flag,
C              0 - dont overwrite
C              1 - overwrite O.K.
C     QUALFG - allowed quality code
C     TUNITS - time units for put
C     RVAL   - array for writing out values
C     RETCOD - return code
C                0 - everything is O.K.
C               -8 - invalid date
C               -9 - data not present in current group
C              -10 - no data in this group
C              -11 - no non missing data, data has not started yet
C              -14 - date specified not within valid range for data set
C              -15 - VBTIME=1 and DELT,TUNITS do not agree
C                    with the data set
C              -20 - problem with one or more of following:
C                    DTOVWR, NVAL, QUALFG, TUNITS, DELT
C              -21 - date from WDM doesn't match expected date
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -84 - data set number out of range
C              -85 - trying to write to a read-only data set
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cwtsds.inc'
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   GPFLG,GPOSEN,GPIND,LTSTEP,LTUNIT,
     1          ENDDAT(6),GPSDAT(6),GPEDAT(6),DATNOW(6),TDAT(6),
     2          TDSFRC,TGROUP,TSPTAD,REMTUN,REMTST,RIND,
     3          COMPFG,TSFORM,VBTIME,TSSTEP,TCODE,TSTEPF,TCDCMP
      INTEGER*4 TVAL,DPOS,I4NVAL,I4ONE
      REAL      TOLR,TSFILL,REMVAL
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL   WTPMCK, WTFNDG, WDATCP, WTEGRP, WTGPCK, TIMADD, TIMDIF
      EXTERNAL   WTPTVL, WDRCGO, WTDSPX, CMPTIM
C
C     + + + END SPECIFICATIONS + + +
C
      I4NVAL= NVAL
      I4ONE = 1
      LTSTEP= DELT
      LTUNIT= TUNITS
      GPFLG = 2
      REMTUN= 0
C
C     check the user supplied parameters
      CALL WTPMCK (GPFLG,DTOVWR,DATES,NVAL,QUALFG,
     M             LTSTEP,LTUNIT,
     O             RETCOD)
      IF (RETCOD.EQ.0) THEN
C       check the data set and figure out which groups have been req.
        CALL WTFNDG (WDMSFL,DSN,GPFLG,DATES,LTSTEP,LTUNIT,I4NVAL,
     O               TDSFRC,TSFILL,TGROUP,TOLR,TSPTAD,
     1               GPIND,GPOSEN,GPSDAT,ENDDAT,RETCOD)
      END IF
      IF (RETCOD.EQ.0) THEN
C       get additional parameters
        RIND= WDRCGO(WDMSFL,TDSFRC)
        CALL WTDSPX (WIBUFF(1,RIND),
     O               COMPFG,TSFORM,VBTIME,TSSTEP,TCODE)
        IF (VBTIME.EQ.1) THEN
C         the time units and step must be exactly right
          CALL CMPTIM ( LTUNIT, LTSTEP, TCODE, TSSTEP, TSTEPF, TCDCMP )
          IF (TCDCMP .NE. 0) THEN
C           not a match, quick get wont work
            RETCOD= -15
          END IF
        END IF
      END IF
      IF (RETCOD.EQ.0) THEN
        CALL WDATCP (DATES,DATNOW)
        DPOS= 1
 10     CONTINUE
C         find end of group
          CALL WTEGRP (GPSDAT,TGROUP,
     O                 GPEDAT)
C
C         skip values in this group as required
          CALL WTGPCK (WDMSFL,GPIND,GPSDAT,DATNOW,LTUNIT,
     I                 TDSFRC,TSFILL,TGROUP,VBTIME,
     O                 CURREC,CURBKS,CURPOS,CURNOV,CURVAL,PREVAL,
     O                 CURTST,CURTUN,CURCMP,CURQUA,CURCNT,CURDAT,
     O                 RETCOD)
          IF (RETCOD.EQ.0) THEN
C           update current date if part value starts this group
            IF (REMTUN.GT.0) THEN
              CALL TIMADD (DATNOW,REMTUN,REMTST,I4ONE,
     M                     TDAT)
C              WRITE (*,*) 'WDTPUT:     DATNW1:',DATNOW
C              WRITE (*,*) '            DATNW2:',TDAT
C              WRITE (*,*) '       RTUN,RTST,1:',REMTUN,REMTST,I4ONE
              CALL WDATCP (TDAT,DATNOW)
            END IF
C           find how many values we need to write
            CALL TIMDIF (DATNOW,GPEDAT,LTUNIT,LTSTEP,
     O                   TVAL)
C           put the data for this group
            CALL WTPTVL (WDMSFL,NVAL,LTUNIT,LTSTEP,
     1                   TDSFRC,TSFILL,TOLR,QUALFG,RVAL,TVAL,TSPTAD,
     2                   GPEDAT,COMPFG,
     M                   CURREC,CURBKS,CURPOS,CURNOV,CURVAL,PREVAL,
     1                   CURTST,CURTUN,CURCMP,CURQUA,CURDAT,
     2                   DPOS,REMTUN,REMTST,REMVAL)
            GPIND= GPIND+ 1
C
C           update the current date
            CALL WDATCP (GPEDAT,DATNOW)
C           update date to start group
            CALL WDATCP (DATNOW,GPSDAT)
          END IF
        IF (RETCOD.EQ.0.AND.GPIND.LE.GPOSEN.AND.DPOS.LE.I4NVAL) GO TO 10
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTGPCK
     I                    (WDMSFL,GPIND,GPSDAT,DATNOW,LTUNIT,
     I                     TDSFRC,TSFILL,TGROUP,VBTIME,
     O                     CURREC,CURBKS,CURPOS,CURNOV,CURVAL,PREVAL,
     O                     CURTST,CURTUN,CURCMP,CURQUA,CURCNT,CURDAT,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     checks information related to a group, skip to starting
C     value, fill in current information
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,GPIND,GPSDAT(6),DATNOW(6),LTUNIT,
     1          CURREC,CURBKS,CURPOS,CURTST,CURTUN,CURCMP,CURQUA,
     2          CURDAT(6),RETCOD,TDSFRC,TGROUP,VBTIME
      INTEGER*4 CURNOV,CURCNT
      REAL      TSFILL,CURVAL,PREVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     GPIND  - group index number
C     GPSDAT - starting date of group
C     DATNOW - current date
C     LTUNIT - time units
C     TDSFRC - first record in data set
C     TSFILL - missing data filler code
C     TGROUP - data group pointer units
C     VBTIME - variable timestep flag
C     CURREC - current record number
C     CURBKS - starting position of current block within current record
C     CURPOS - current position in current block
C     CURNOV - current number of values in current block
C     CURVAL - current value
C     PREVAL - previous value
C     CURTST - current time step
C     CURTUN - current time units
C     CURCMP - current compression code
C     CURQUA - current quality code
C     CURCNT - current position within block
C     CURDAT - current date of start of current value
C     RETCOD - return code
C                  0 - everything O.K.
C                 -9 - data not present in current group
C                -10 - no data in this group
C                -11 - no non missing data, data has not started yet
C                -21 - date from WDM doesn't match expected date
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,DIND,SIND,JX,JY,JZ,TREC,TPOS,SREC,SPOS,SQUA,
     1          PDAT,BADJFG,ADDAFG,OREC,DREC
      INTEGER*4 TDFREE,SNOV,I4TMP
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO,WDRCDL
C
C     + + + EXTERNALS + + +
      EXTERNAL   WTSKVL, WDSKBK, WDRCGO, WBCWSP, WDPTSP, WDRCDL
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
      DREC  = 0
      BADJFG= 0
      ADDAFG= LTUNIT
C
C     skip to place to start write
      CALL WTSKVL (WDMSFL,GPIND,GPSDAT,DATNOW,
     I             TDSFRC,TSFILL,TGROUP,BADJFG,ADDAFG,VBTIME,
     O             CURREC,CURBKS,CURPOS,CURNOV,CURVAL,PREVAL,
     O             CURTST,CURTUN,CURCMP,CURQUA,CURCNT,CURDAT,
     O             RETCOD)
C
      IF (RETCOD.EQ.0) THEN
C       update CURNOV for overwriting
        CURNOV= CURCNT- 1
C
C       check for defined values in rest of group and record
C       because overwriting not allowed
        IF (CURQUA.NE.31) THEN
C         data present in current group
          RETCOD= -9
        ELSE
C         may be additional groups on this record unless free
C         skip other undef values if available
          SREC= CURREC
          SPOS= CURBKS
 10       CONTINUE
            OREC= SREC
            I4TMP= 2
            CALL WDSKBK (WDMSFL,I4TMP,
     M                   SREC,SPOS)
            IF (OREC.NE.SREC) THEN
C             we have moved on to a new rec which only contains fillers
              DREC= SREC
            END IF
            IF (SREC.GT.0) THEN
              SIND= WDRCGO(WDMSFL,SREC)
              I4TMP= WIBUFF(SPOS,SIND)
              IF (I4TMP.GT.0) THEN
                CALL WBCWSP (I4TMP,
     O                       SNOV,JX,JY,JZ,SQUA)
              ELSE
                SQUA= 0
              END IF
            ELSE
C             we are at the end of the record chain for this DSN
              SQUA= -1
            END IF
          IF (SQUA.EQ.31) GO TO 10
C
          IF (SQUA.GE.0) THEN
C           check to see if free space points to where we skipped to
            DIND  = WDRCGO(WDMSFL,TDSFRC)
            PDAT  = WIBUFF(11,DIND)
            TDFREE= WIBUFF(PDAT+1,DIND)
            CALL WDPTSP (TDFREE,
     O                   TREC,TPOS)
            IF (TREC.NE.SREC.OR.TPOS.NE.SPOS) THEN
C             data not present in current group
              RETCOD= -9
            END IF
          END IF
        END IF
C
        IF (RETCOD.EQ.0.AND.DREC.NE.0) THEN
C         delete the record containing only fillers
          I= WDRCDL(WDMSFL,DREC)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTPTVL
     I                    (WDMSFL,NVAL,LTUNIT,LTSTEP,
     I                     TDSFRC,TSFILL,TOLR,QUALFG,RVAL,TVAL,TSPTAD,
     I                     GPEDAT,COMPFG,
     M                     CURREC,CURBKS,CURPOS,CURNOV,CURVAL,PREVAL,
     M                     CURTST,CURTUN,CURCMP,CURQUA,CURDAT,
     M                     DPOS,REMTUN,REMTST,REMVAL)
C
C     + + + PURPOSE + + +
C     writes all or part of a WDMS group into a WDMS timeseries data set
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,NVAL,LTUNIT,LTSTEP,QUALFG,
     1          TDSFRC,TSPTAD,GPEDAT(6),COMPFG,REMTUN,REMTST,
     2          CURREC,CURBKS,CURPOS,CURTST,CURTUN,CURCMP,CURQUA,
     3          CURDAT(6)
      INTEGER*4 TVAL,DPOS,CURNOV
      REAL      TSFILL,TOLR,RVAL(NVAL),REMVAL,CURVAL,PREVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     NVAL   - number of values
C     LTUNIT - time units
C     LTSTEP - time step
C     TDSFRC - first record in data set
C     TSFILL - missing data filler code
C     TOLR   - compression tolerance
C     QUALFG - get quality code
C     RVAL   - array of data values to put on WDMS file
C     TVAL   - number of values to write in this group
C     TSPTAD - disaggregation code
C              0 - same
C              1 - div
C     GPEDAT - group ending date
C     COMPFG - compression flag
C              1 - alow compression
C              2 - no compression
C     CURREC - current record number
C     CURBKS - starting position of current block within current record
C     CURPOS - current position in current block
C     CURNOV - current number of values in current block
C     CURVAL - current value
C     PREVAL - previous value
C     CURTST - current time step
C     CURTUN - current time units
C     CURCMP - current compression code
C     CURQUA - current quality code
C     CURDAT - current date of start of current value
C     DPOS   - current position within RVAL
C     REMTUN - remaining time unit across group border
C     REMTST - remaining time step across group border
C     REMVAL - remaining data value across group border
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RIND,CHK,I,DIND,PDAT,TDAT(6),EGPFLG,LCMPFG
      INTEGER*4 BCW,TDFREE,I4ZRO,I4ONE,CVAL,I4NVAL,FVAL,XVAL
C
C     + + + FUNCTIONS + + +
      INTEGER   TIMCHK,WDRCGO
      INTEGER*4 WBCWCL,WDPTCL
C
C     + + + INTRINSICS + + +
      INTRINSIC ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDRCGO, WBCWCL, WTNWBK, TIMADD, WDATCP, TIMCHK, TIMDIF
      EXTERNAL  WDRCUP, WDPTCL
C
C     + + + END SPECIFICATIONS + + +
C
      I4ZRO = 0
      I4ONE = 1
      I4NVAL= NVAL
      CVAL  = 0
      LCMPFG= COMPFG
C
      RIND  = WDRCGO(WDMSFL,CURREC)
C
C     WRITE (*,*) 'WTPTVL:     GPEDAT:',GPEDAT
C     WRITE (*,*) '            CURDAT:',CURDAT
C     WRITE (*,*) '              TVAL:',TVAL
      IF (REMTUN.GT.0) THEN
C       WRITE (*,*) 'STRT:REMTUN,REMTST:',REMTUN,REMTST
C       the current value is partly in the last group
        CURVAL= REMVAL
        CURTST= REMTST
        CURTUN= REMTUN
        REMTUN= 0
        BCW   = WBCWCL (I4ONE,CURTST,CURTUN,I4ZRO,QUALFG)
        WIBUFF(CURBKS,RIND)= BCW
        WRBUFF(CURPOS,RIND)= CURVAL
        CURPOS= CURPOS+ 1
        CALL WTNWBK (WDMSFL,
     M               CURREC,CURPOS,
     O               CURBKS)
        RIND = WDRCGO (WDMSFL,CURREC)
C
        CALL TIMADD (CURDAT,CURTUN,CURTST,I4ONE,
     O               TDAT)
        CALL WDATCP (TDAT,CURDAT)
C       update pointer to current value
        DPOS= DPOS+ 1
      END IF
C
      CURVAL= RVAL(DPOS)
C     figure out if we can continue the last block
      CHK   = 1
      IF (LTSTEP.NE.CURTST.OR.LTUNIT.NE.CURTUN.OR.QUALFG.NE.CURQUA)
     1  CHK= 0
C
      IF (TVAL.GT.0) THEN
 10     CONTINUE
          CVAL= CVAL+ 1
C
          IF (ABS(CURVAL-PREVAL).LE.TOLR.AND.CHK.EQ.1.AND.
     1        CURNOV.LT.32000.AND.LCMPFG.EQ.1) THEN
C           we want to be compressed
            IF (CURCMP.EQ.1) THEN
C             we already are
              CURNOV= CURNOV+ 1
            ELSE
C             currently uncompressed, finish old block
C             delete last value from old block
              CURNOV= CURNOV- 1
              CURPOS= CURPOS- 1
              IF (CURNOV.GT.I4ZRO) THEN
                BCW= WBCWCL(CURNOV,CURTST,CURTUN,CURCMP,CURQUA)
                WIBUFF(CURBKS,RIND)= BCW
                CALL WTNWBK (WDMSFL,
     M                       CURREC,CURPOS,
     O                       CURBKS)
                RIND = WDRCGO( WDMSFL, CURREC )
              END IF
              WRBUFF(CURPOS,RIND)= PREVAL
              CURNOV= 2
              CURCMP= 1
            END IF
          ELSE
C           we want to be uncompressed
            IF (CHK.NE.1.OR.CURCMP.EQ.1) THEN
              IF (CURNOV.GT.I4ZRO) THEN
C               finish the old block
                BCW= WBCWCL(CURNOV,CURTST,CURTUN,CURCMP,CURQUA)
                WIBUFF(CURBKS,RIND)= BCW
                IF (CURCMP.EQ.1.OR.CHK.EQ.0) CURPOS= CURPOS+ 1
                CALL WTNWBK (WDMSFL,
     M                       CURREC,CURPOS,
     O                       CURBKS)
                RIND  = WDRCGO(WDMSFL,CURREC)
                CURNOV= 0
              END IF
              IF (CHK.EQ.0) THEN
                CURQUA= QUALFG
                CURTUN= LTUNIT
                CURTST= LTSTEP
              ELSE IF (CHK.EQ.-1) THEN
                CURQUA= 31
              END IF
            END IF
            CURCMP= 0
            WRBUFF(CURPOS,RIND)= CURVAL
            CURPOS= CURPOS+ 1
            CURNOV= CURNOV+ 1
            IF (CURPOS.GT.512.AND.CVAL.LT.TVAL) THEN
C             out of space in this record, finish block
              BCW= WBCWCL (CURNOV,CURTST,CURTUN,CURCMP,CURQUA)
              WIBUFF(CURBKS,RIND)= BCW
              CALL WTNWBK (WDMSFL,
     M                     CURREC,CURPOS,
     O                     CURBKS)
              RIND= WDRCGO(WDMSFL,CURREC)
              CURNOV= 0
            END IF
          END IF
          CHK= 1
C         dont allow compression across record boundary
          IF (CURNOV.EQ.I4ZRO) THEN
C           jlk 11/89, insure quality code on trailing data
            CHK= 0
            CURQUA= QUALFG
          END IF
C
          IF (CURCMP.EQ.0) THEN
C           update value to compare next value for compressed values
            PREVAL= CURVAL
          END IF
          DPOS= DPOS+ 1
          IF (DPOS.LE.I4NVAL) THEN
            CURVAL= RVAL(DPOS)
          ELSE
            CURVAL= TSFILL
            IF (CURQUA.NE.31) THEN
C             allow compression of trainling values
              LCMPFG= 1
              CHK= -1
            END IF
          END IF
C
        IF (CVAL.LT.TVAL) GO TO 10
C
C       finish the last block
        BCW= WBCWCL(CURNOV,CURTST,CURTUN,CURCMP,CURQUA)
        WIBUFF(CURBKS,RIND)= BCW
C       update the current position
        IF (CURCMP.EQ.1) CURPOS= CURPOS+ 1
C
      END IF
C
C     calculate the current date
      CALL TIMADD (CURDAT,LTUNIT,LTSTEP,TVAL,
     O             TDAT)
      CALL WDATCP (TDAT,CURDAT)
C     are we at the end of the group?
      IF (TIMCHK(CURDAT,GPEDAT).NE.0) THEN
C       no, better fill in, how much?
        CURTUN= LTUNIT
        CURTST= 1
 20     CONTINUE
          CALL TIMDIF (CURDAT,GPEDAT,CURTUN,CURTST,
     O                 FVAL)
          CALL TIMADD (CURDAT,CURTUN,CURTST,FVAL,
     O                 TDAT)
C         WRITE (*,*) 'EGRP:       CURDAT:',CURDAT
          EGPFLG= 0
          IF (TIMCHK(TDAT,GPEDAT).NE.0) THEN
C           still not at even end of group
            CURTUN= CURTUN- 1
            IF (CURTUN.LE.0) THEN
              STOP 'BIG PROBLEM, END OF GROUP CURTUN=0'
            END IF
            EGPFLG= 1
          END IF
        IF (EGPFLG.EQ.1) GO TO 20
C
        IF (FVAL.GT.1) THEN
          CURTST= FVAL
          IF (CURTST.GT.63) THEN
            WRITE (*,*) 'BIG PROBLEM FILLING UP GROUP,STEP:',CURTST
          END IF
        END IF
C       figure out how much goes into next group
        CALL TIMADD (CURDAT,LTUNIT,LTSTEP,I4ONE,
     O               TDAT)
        I= 1
        CALL TIMDIF (GPEDAT,TDAT,CURTUN,I,
     O               XVAL)
        REMTST= XVAL
        IF (REMTST.GT.63) THEN
          WRITE (*,*) 'BIG PROBLEM STARTING GROUP,STEP:',REMTST
        END IF
        REMTUN= CURTUN
C
        CURNOV= 1
        CURCMP= 0
C
C       figure out what the fill value and quality is
        IF (DPOS.LE.I4NVAL) THEN
          IF (TSPTAD.EQ.0) THEN
C           disagg code is same
            CURVAL= RVAL(DPOS)
            REMVAL= CURVAL
          ELSE
C           disagg code is div
            CALL TIMADD (CURDAT,LTUNIT,LTSTEP,I4ONE,
     O                   TDAT)
            CALL TIMDIF (CURDAT,TDAT,CURTUN,CURTST,
     O                   CVAL)
            CURVAL= (RVAL(DPOS)* FVAL)/ CVAL
            REMVAL= RVAL(DPOS)- CURVAL
          END IF
        ELSE
          CURVAL= TSFILL
          REMVAL= TSFILL
          CURQUA= 31
        END IF
C
C       get the next block
        CALL WTNWBK (WDMSFL,
     M               CURREC,CURPOS,
     O               CURBKS)
        RIND= WDRCGO(WDMSFL,CURREC)
C
        BCW= WBCWCL(CURNOV,CURTST,CURTUN,CURCMP,CURQUA)
        WIBUFF(CURBKS,RIND)= BCW
        WRBUFF(CURPOS,RIND)= CURVAL
        CURPOS= CURPOS+ 1
      END IF
C
C     update the current record
      CALL WDRCUP(WDMSFL,RIND)
C     update the data-set free space pointer
      IF (CURPOS.GT.511) CURPOS = 511
      TDFREE= WDPTCL (CURREC,CURPOS)
      DIND  = WDRCGO(WDMSFL,TDSFRC)
      PDAT  = WIBUFF(11,DIND)
      WIBUFF(PDAT+1,DIND)= TDFREE
      CALL WDRCUP(WDMSFL,DIND)
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTNWBK
     I                    (WDMSFL,
     M                     CURREC,CURPOS,
     O                     CURBKS)
C
C     + + + PURPOSE + + +
C     starts a new WDMS timeseries block, on a new record if req.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,CURREC,CURPOS,CURBKS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     CURREC - current record number
C     CURPOS - current position in current block
C     CURBKS - starting position of current block within current record
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   CIND,I
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO,WDRCGX
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDRCGO, WDRCUP, WDRCGX
C
C     + + + END SPECIFICATIONS + + +
C
      IF (CURPOS.GT.511) THEN
C       update the old record
        CIND= WDRCGO(WDMSFL,CURREC)
        CALL WDRCUP (WDMSFL,CIND)
C       new block nust start on new record
        I= 0
C       write(1,*) 'WTNWBK,782: wdmsfl,i,currec=',wdmsfl,i,currec
        CIND  = WDRCGX(WDMSFL,I,CURREC)
        CURREC= RECNO(CIND)
        CURPOS= 5
      END IF
C
      CURBKS= CURPOS
      CURPOS= CURPOS+ 1
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDTPUT
     I                    (WDMSFL,DSN,DELT,DATES,NVAL,
     I                     DTOVWR,QUALFG,TUNITS,RVAL,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     Puts time series data into a WDM file.  This routine traps the
C     problem with overwritting existing data.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,DELT,DATES(6),NVAL,DTOVWR,QUALFG,
     1          TUNITS,RETCOD
      REAL      RVAL(NVAL)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - data-set number
C     DELT   - time step for put
C     DATES  - starting date
C     NVAL   - number of values
C     DTOVWR - data overwrite flag,
C              0 - dont overwrite
C              1 - overwrite O.K.
C     QUALFG - allowed quality code
C     TUNITS - time units for put
C     RVAL   - array for writing out values
C     RETCOD - return code
C                0 - everything is O.K.
C               -8 - invalid date
C               -9 - data not present in current group
C              -10 - no data in this group
C              -11 - no non missing data, data has not started yet
C              -14 - date specified not within valid range for data set
C              -15 - VBTIME=1 and DELT,TUNITS do not agree
C                    with the data set
C              -20 - problem with one or more of following:
C                    DTOVWR, NVAL, QUALFG, TUNITS, DELT
C              -21 - date from WDM doesn't match expected date
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -84 - data set number out of range
C              -85 - trying to write to a read-only data set
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   ALLFLG, DXX, LWDMFL, LDSN
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDTPFX, WTDDEL, TSBCLR, WID2UD
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   ALLFLG, DXX
     #     /      0,   0 /
C
C     + + + END SPECIFICATIONS + + +
C
      CALL WID2UD (WDMSFL,DSN,
     O             LWDMFL,LDSN)
C     try to put time series data into WDM file
      CALL WDTPFX ( LWDMFL, LDSN, DELT, DATES, NVAL,
     I              DXX, QUALFG, TUNITS, RVAL,
     O              RETCOD )
      IF (RETCOD .EQ. -9  .AND.  DTOVWR .EQ. 1) THEN
C       data existed, delete it
        CALL WTDDEL ( LWDMFL, LDSN, DATES, ALLFLG,
     O                RETCOD )
        IF (RETCOD .EQ. 0) THEN
C         data successfully deleted, add data
          CALL WDTPFX ( LWDMFL, LDSN, DELT, DATES, NVAL,
     I                  DXX, QUALFG, TUNITS, RVAL,
     O                  RETCOD )
        END IF
      END IF
C     clear modified data from time-series buffer
      CALL TSBCLR (LWDMFL,LDSN)
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTFNDT
     I                    (WDMSFL,DSN,GPFLG,
     O                     TDSFRC,SDAT,EDAT,RETCOD)
C
C     + + + PURPOSE + + +
C     determine starting and ending dates of data in data set
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,GPFLG,TDSFRC,SDAT(6),EDAT(6),RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - data-set number
C     GPFLG  - get(1)/put(2) flag
C     TDSFRC - data-set first record number
C     SDAT   - starting date of data in dsn
C     EDAT   - ending date of data in dsn
C     RETCOD - return code
C                0 - everything is O.K.
C               -6 - no data present
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -85 - trying to write to a read-only data set
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   GRPSTR,GRPEND,GRPIND,GRSPOS,GREPOS,
     1          PDAT,PDATV,TGRNUM,TGRPST,RIND,TSTEP,
     2          TGROUP,TBSDAT(6),TDAT(6),XDAT(6),NDAT(6),
     3          CURREC,CURBKS,CURPOS,CURTST,CURTUN,CURCMP,CURQUA,
     4          TSPTAD,I,MSFLG,DSTYP,GRCNT,LWDMFL,LDSN
      INTEGER*4 I4ZRO,I4NVAL,GRSPTR,GREPTR,NUMSKP,CURNOV
      REAL      TSFILL,TOLR
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO, TIMCHK
C
C     + + + EXTERNALS + + +
      EXTERNAL  ZIPI, WDSCHA, WDRCGO, WTDSPM, TIMADD, WDPTSP, WDSKBK
      EXTERNAL  WBCWSP, WDATCP, WTEGRP, TIMCHK, TIMCNV, WID2UD
C
C     + + + END SPECIFICATIONS + + +
C
      I4ZRO = 0
      TSTEP = 1
      RETCOD= 0
      I     = 6
      CALL ZIPI (I,RETCOD,SDAT)
      CALL ZIPI (I,RETCOD,EDAT)
C
C     adjust wdm and dsn as needed
      CALL WID2UD (WDMSFL,DSN,
     O             LWDMFL,LDSN)
C
      DSTYP = 1
      CALL WDSCHA (LWDMFL,LDSN,DSTYP,GPFLG,
     O             TDSFRC,GRCNT,RETCOD)
C
      IF (RETCOD.EQ.0) THEN
C       bring label into buffer
        RIND = WDRCGO(LWDMFL,TDSFRC)
        CALL WTDSPM (LWDMFL,WIBUFF(1,RIND),WRBUFF(1,RIND),
     O               TSFILL,TGROUP,TOLR,TBSDAT,TSPTAD)
C
C       calculate pointer to first group pointer
        PDAT  = WIBUFF(11,RIND)
        TGRPST= PDAT+ 2
C
C       calculate max number of group pointers
        PDATV = WIBUFF(12,RIND)
        TGRNUM= PDATV- PDAT- 2
C
C       look for first group with data
        GRPSTR= 0
        GRPIND= 0
        GRPEND= 0
        GRSPOS= TGRPST- 1
C
 10     CONTINUE
          GRPIND= GRPIND+ 1
          GRSPOS= GRSPOS+ 1
          IF (WIBUFF(GRSPOS,RIND).NE.I4ZRO) THEN
            GRPSTR= GRPIND
            GRSPTR= WIBUFF(GRSPOS,RIND)
          END IF
        IF (GRPSTR.EQ.0.AND.GRSPOS.LT.PDATV-1) GOTO 10
C
        IF (GRPSTR.GT.0) THEN
C         look for last group with data
          GREPOS= PDATV- 1
          GRPIND= TGRNUM+ 1
C
 20       CONTINUE
            GRPIND= GRPIND- 1
            IF (WIBUFF(GREPOS,RIND).NE.I4ZRO) THEN
              GRPEND= GRPIND
              GREPTR= WIBUFF(GREPOS,RIND)
            ELSE
              GREPOS= GREPOS- 1
            END IF
          IF (GRPEND.EQ.0) GOTO 20
C
C         calc exact starting date for date
          I4NVAL= GRPSTR- 1
C         find start date of beginning of group
          CALL TIMADD (TBSDAT,TGROUP,TSTEP,I4NVAL,
     O                 SDAT)
C         get starting record for data
          CALL WDPTSP (GRSPTR,
     O                 CURREC,CURPOS)
          RIND= WDRCGO (LWDMFL,CURREC)
C
          CURBKS= CURPOS
          NUMSKP= 1
C         loop to look for defined data
 30       CONTINUE
C           skip to next block control word
            CALL WDSKBK (LWDMFL,NUMSKP,
     M                   CURREC,CURBKS)
C           be sure record is in buffer
            RIND= WDRCGO (LWDMFL,CURREC)
C           split up block control word
            CALL WBCWSP (WIBUFF(CURBKS,RIND),
     O                   CURNOV,CURTST,CURTUN,CURCMP,CURQUA)
            IF (CURQUA.EQ.31) THEN
C             skip this block
              CALL TIMADD (SDAT,CURTUN,CURTST,CURNOV,
     O                     TDAT)
              CALL WDATCP (TDAT,SDAT)
            END IF
            NUMSKP= 2
            IF (CURCMP.EQ.0) NUMSKP= CURNOV+ 1
          IF (CURQUA.EQ.31) GO TO 30
C
C         calc exact ending date for data
          MSFLG = 0
C         find start date of beginning of group
          I4NVAL= GRPEND- 1
          IF (GRPEND.NE.GRPSTR) THEN
C           ending record is not starting record
            CALL TIMADD (TBSDAT,TGROUP,TSTEP,I4NVAL,
     O                   XDAT)
C           get starting record for last group with data
            CALL WDPTSP (GREPTR,
     O                   CURREC,CURPOS)
            RIND  = WDRCGO (LWDMFL,CURREC)
            CURBKS= CURPOS
            NUMSKP= 1
          ELSE
C           we are looking in the starting group, from current date
            CALL TIMADD (SDAT,CURTUN,CURTST,CURNOV,
     O                   XDAT)
C           are we at the end of the starting group and data?
            CALL WTEGRP (SDAT,TGROUP,
     O                   NDAT)
            IF (TIMCHK(NDAT,XDAT).EQ.0) THEN
              MSFLG= -1
            END IF
          END IF
          IF (MSFLG.EQ.0) THEN
C           find start of next group
            CALL WTEGRP (XDAT,TGROUP,
     O                   NDAT)
C
C           loop to look for end of defined data
 40         CONTINUE
C             skip to next block control word
              CALL WDSKBK (LWDMFL,NUMSKP,
     M                     CURREC,CURBKS)
C             be sure record is in buffer
              RIND= WDRCGO (LWDMFL,CURREC)
C             split up block control word
              CALL WBCWSP (WIBUFF(CURBKS,RIND),
     O                     CURNOV,CURTST,CURTUN,CURCMP,CURQUA)
              IF (CURQUA.EQ.31.AND.MSFLG.EQ.0) THEN
C               save start of this block
                CALL WDATCP (XDAT,EDAT)
                MSFLG= 1
              ELSE
                MSFLG= 0
              END IF
C             skip this block
              CALL TIMADD (XDAT,CURTUN,CURTST,CURNOV,
     O                     TDAT)
              CALL WDATCP (TDAT,XDAT)
              NUMSKP= 2
              IF (CURCMP.EQ.0) THEN
                NUMSKP= CURNOV+ 1
              END IF
            IF (TIMCHK(XDAT,NDAT).EQ.1.AND.CURNOV.GT.I4ZRO) GO TO 40
          END IF
C
          IF (MSFLG.LE.0) THEN
            CALL WDATCP (XDAT,EDAT)
          END IF
C         convert ending date to old format
          CALL TIMCNV (EDAT)
        ELSE
C        no data present
         RETCOD= -6
        END IF
      END IF
C
      RETURN
      END
C
C
      SUBROUTINE   WTDDEL
     I                    (WDMSFL,DSN,DELDAT,ALLFLG,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     delete all data following a specified date in the given
C     data set
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,DELDAT(6),ALLFLG,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - data-set number
C     DELDAT - delete from date array
C     ALLFLG - delete all flag, 0 - only delete group
C                               1 - all data after date
C     RETCOD - return code
C                0 - everything is O.K.
C               -6 - no data present
C              -10 - no data in this group
C              -11 - no non missing data, data has not started yet
C              -14 - date specified not within valid range for data set
C              -21 - date from WDM doesn't match expected date
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -84 - data set number out of range
C              -85 - trying to write to a read-only data set
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RIND,DREC,NREC,SREC,SPOS,STST,STUN,SCMP,SQUA,SCNT,
     1          SDAT(6),SBKS,STSPT,SGRP,ADDAFG,BADJFG,I,GRDLCT,
     2          GRPIND,GRPDEL,GRPEND,GPSDAT(6),GPEDAT(6),PDAT,GPFLG,
     3          ENDDAT(6),VBTIME
      INTEGER*4 SFREE,SNOV,SGNOV,TNOV,LNOV,I4TMP,I4ZRO,GRPPTR
      REAL      SVAL,SFIL,STOLR
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO,WDRCDL,TIMCHK
      INTEGER*4 WDPTCL,WBCWCL
C
C     + + + EXTERNALS + + +
      EXTERNAL   WTFNDT, TIMDFX, WTFNDG, TIMCHK, WDRCGO, WDPTSP, WTSKVL
      EXTERNAL   WBCWCL, WDRCUP, WTEGRP, WTNWBK, WDPTCL, WDRCDL
C
C     + + + END SPECIFICATIONS + + +
C
      ADDAFG= 0
      VBTIME= 0
      BADJFG= 0
      GPFLG = 2
      I4ZRO = 0
      GRDLCT= 0
C
C     get end date for data
      CALL WTFNDT (WDMSFL,DSN,GPFLG,
     O             DREC,SDAT,ENDDAT,RETCOD)
C     are we deleting any data
      I= TIMCHK(DELDAT,ENDDAT)
      IF (I.EQ.1) THEN
C       figure out how much time is being deleted
        CALL TIMDFX (DELDAT,ENDDAT,
     O               SGNOV,STUN,STST)
C
C       figure out which groups to delete
        CALL WTFNDG (WDMSFL,DSN,GPFLG,DELDAT,STST,STUN,SGNOV,
     O               DREC,SFIL,SGRP,STOLR,STSPT,
     O               GRPDEL,GRPEND,GPSDAT,SDAT,RETCOD)
C
        IF (RETCOD.EQ.0) THEN
C         lets do the delete
          IF (TIMCHK(DELDAT,GPSDAT).EQ.0.OR.ALLFLG.EQ.1) THEN
C           on group boundary or deleting everything,
C           delete whole group or first one with data
            RIND= WDRCGO(WDMSFL,DREC)
 10         CONTINUE
              GRPPTR= WIBUFF(GRPDEL,RIND)
              IF (GRPPTR.EQ.I4ZRO) THEN
C               nothing in current group, try next one
                GRPDEL= GRPDEL+ 1
              ELSE
                CALL WDPTSP (GRPPTR,
     O                       SREC,SPOS)
              END IF
            IF (GRPPTR.EQ.I4ZRO) GO TO 10
C           increment number of groups deleted counter
            GRPDEL= GRPDEL- 1
          ELSE
C           skip to where delete starts
            CALL WTSKVL (WDMSFL,GRPDEL,GPSDAT,DELDAT,
     1                   DREC,SFIL,SGRP,BADJFG,ADDAFG,VBTIME,
     O                   SREC,SBKS,SPOS,SNOV,SVAL,SVAL,
     1                   STST,STUN,SCMP,SQUA,SCNT,SDAT,
     2                   RETCOD)
            IF (RETCOD.EQ.0) THEN
              IF (SCNT.EQ.1) THEN
C               at beginning of block BLOCK, DELETE IT ALL
                SPOS= SBKS
              ELSE
C               save part of block
                SNOV= SCNT- 1
                RIND= WDRCGO(WDMSFL,SREC)
                WIBUFF(SBKS,RIND)= WBCWCL(SNOV,STST,STUN,SCMP,SQUA)
                CALL WDRCUP(WDMSFL,RIND)
                IF (SCMP .EQ. 1) SPOS = SPOS + 1
              END IF
C             fill rest of group with undefined values
C             find end of group
              CALL WTEGRP (SDAT,SGRP,
     O                     GPEDAT)
              SCMP= 1
              SQUA= 31
C             figure out how much time left in group
              CALL TIMDFX (SDAT,GPEDAT,
     O                     TNOV,STUN,STST)
              I4TMP= 32767
C
 20           CONTINUE
                LNOV= TNOV
                IF (LNOV.GT.I4TMP) LNOV= I4TMP
                CALL WTNWBK (WDMSFL,
     M                       SREC,SPOS,
     O                       SBKS)
                RIND= WDRCGO(WDMSFL,SREC)
                WIBUFF(SBKS,RIND)= WBCWCL(LNOV,STST,STUN,SCMP,SQUA)
                WRBUFF(SPOS,RIND)= SFIL
                SPOS= SPOS+ 1
                TNOV= TNOV- LNOV
              IF (TNOV.GT.I4ZRO) GO TO 20
C             write out revised record
              CALL WDRCUP(WDMSFL,RIND)
            END IF
          END IF
        END IF
C
        IF (RETCOD.EQ.0) THEN
C         fix directory
          RIND= WDRCGO(WDMSFL,DREC)
C         fix group pointers to show no data
          DO 70 GRPIND= GRPDEL+1,GRPEND
            IF (WIBUFF(GRPIND,RIND).GT.0) GRDLCT= GRDLCT+ 1
            WIBUFF(GRPIND,RIND)= 0
 70       CONTINUE
C         update free pos and number of groups
          SFREE= WDPTCL(SREC,SPOS)
          PDAT = WIBUFF(11,RIND)
          WIBUFF(PDAT,RIND)= WIBUFF(PDAT,RIND)- GRDLCT
          WIBUFF(PDAT+1,RIND)= SFREE
C         update directory
          CALL WDRCUP(WDMSFL,RIND)
C
C         get record where data to delete starts
          RIND= WDRCGO(WDMSFL,SREC)
C         zero out rest of current record
          DO 80 I= SPOS,512
            WIBUFF(I,RIND)= 0
 80       CONTINUE
C         update retcord
          CALL WDRCUP(WDMSFL,RIND)
          NREC= WIBUFF(4,RIND)
C
          IF (NREC.GT.0) THEN
C           delete any and all records which follow
 90         CONTINUE
              SREC= NREC
              NREC= WDRCDL(WDMSFL,SREC)
            IF (NREC.GT.0) GOTO 90
          END IF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTEGRP
     I                    (DAT,TGROUP,
     O                     EGRDAT)
C
C     + + + PURPOSE + + +
C     determines end of group which contains a given date,
C     if at group boundary, returns date of end of group
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DAT(6),TGROUP,EGRDAT(6)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DAT    - current date array
C     TGROUP - group time unit
C     EGRDAT - end of group date array
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMON
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZIPI, DAYMON
C
C     + + + END SPECIFICATIONS + + +
C
      I= 6
      J= -999
      CALL ZIPI(I,J,EGRDAT)
C
      GO TO (10,20,30,40,50,60,70), TGROUP
 10   CONTINUE
C       second group pointers are not supported
        GO TO 90
C
 20   CONTINUE
C       minute group pointers are not supported
        GO TO 90
C
 30   CONTINUE
C       hour pointer
        EGRDAT(1)= DAT(1)
        EGRDAT(2)= DAT(2)
        EGRDAT(3)= DAT(3)
C       always increment hour for end of group
        EGRDAT(4)= DAT(4)+ 1
        IF (EGRDAT(4).GT.24) THEN
C         new day
          EGRDAT(4)= 1
          EGRDAT(3)= EGRDAT(3)+ 1
          IF (EGRDAT(3).GT.DAYMON(EGRDAT(1),EGRDAT(2))) THEN
C           new month
            EGRDAT(3)= 1
            EGRDAT(2)= EGRDAT(2)+ 1
            IF (EGRDAT(2).GT.12) THEN
C             new year
              EGRDAT(2)= 1
              EGRDAT(1)= EGRDAT(1)+ 1
            END IF
          END IF
        END IF
        GO TO 90
C
 40   CONTINUE
C       day pointer
        EGRDAT(1)= DAT(1)
        EGRDAT(2)= DAT(2)
        IF (DAT(4).EQ.24) THEN
C         on boundary
          EGRDAT(3)= DAT(3)+ 1
          IF (EGRDAT(3).GT.DAYMON(EGRDAT(1),EGRDAT(2))) THEN
C           new month
            EGRDAT(3)= 1
            EGRDAT(2)= EGRDAT(2)+ 1
            IF (EGRDAT(2).GT.12) THEN
C             new year
              EGRDAT(2)= 1
              EGRDAT(1)= EGRDAT(1)+ 1
            END IF
          END IF
        ELSE
          EGRDAT(3)= DAT(3)
        END IF
        EGRDAT(4)= 24
        GO TO 90
C
 50   CONTINUE
C       month pointer
        EGRDAT(1)= DAT(1)
        EGRDAT(2)= DAT(2)
        IF (DAT(3).EQ.DAYMON(DAT(1),DAT(2)).AND.DAT(4).EQ.24) THEN
C         on boundary
          EGRDAT(2)= EGRDAT(2)+ 1
          IF (EGRDAT(2).GT.12) THEN
C           new year
            EGRDAT(2)= 1
            EGRDAT(1)= EGRDAT(1)+ 1
          END IF
        END IF
        EGRDAT(3)= DAYMON(EGRDAT(1),EGRDAT(2))
        EGRDAT(4)= 24
        GO TO 90
C
 60   CONTINUE
C       year pointer
        EGRDAT(1)= DAT(1)
        IF (DAT(2).EQ.12.AND.DAT(3).EQ.31.AND.DAT(4).EQ.24) THEN
C         on boundary
          EGRDAT(1)= EGRDAT(1)+ 1
        END IF
        EGRDAT(2)= 12
        EGRDAT(3)= 31
        EGRDAT(4)= 24
        GO TO 90
C
 70   CONTINUE
C       century pointers
        IF (MOD(DAT(1)+1,100).EQ.0.AND.DAT(2).EQ.12.AND.
     1      DAT(3).EQ.31.AND.DAT(4).EQ.24) THEN
C         on boundary
          EGRDAT(1)= DAT(1)+ 100
        ELSE
          EGRDAT(1)= ((DAT(1)/100)+1)*100- 1
        END IF
        EGRDAT(2)= 12
        EGRDAT(3)= 31
        EGRDAT(4)= 24
        GO TO 90
C
 90   CONTINUE
      EGRDAT(5)= 0
      EGRDAT(6)= 0
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTSGRP
     I                    (DAT,TGROUP,
     O                     SGRDAT)
C
C     + + + PURPOSE + + +
C     determines start of group which contains given date,
C     if at group boundary, returns given date
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DAT(6),TGROUP,SGRDAT(6)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DAT    - current date array
C     TGROUP - group time unit
C     SGRDAT - start of group date array
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMON
C
C     + + + INTRINSICS + + +
      INTRINSIC MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL  ZIPI, DAYMON
C
C     + + + END SPECIFICATIONS + + +
C
      I= 6
      J= -999
      CALL ZIPI(I,J,SGRDAT)
C
      GO TO (10,20,30,40,50,60,70), TGROUP
 10   CONTINUE
C       second gorup pointers not supported
        GO TO 90
C
 20   CONTINUE
C       minute group pointers not supported
        GO TO 90
C
 30   CONTINUE
C       hour pointers
        SGRDAT(1)= DAT(1)
        SGRDAT(2)= DAT(2)
        SGRDAT(3)= DAT(3)
        SGRDAT(4)= DAT(4)
        IF (SGRDAT(4).EQ.0) THEN
C         back a day
          SGRDAT(4)= 24
          SGRDAT(3)= SGRDAT(3)- 1
          IF (SGRDAT(3).EQ.0) THEN
C           back a month
            SGRDAT(2)= SGRDAT(2)- 1
            IF (SGRDAT(2).EQ.0) THEN
C             back a year
              SGRDAT(2)= 12
              SGRDAT(1)= SGRDAT(1)- 1
            END IF
            SGRDAT(3)= DAYMON(SGRDAT(1),SGRDAT(2))
          END IF
        END IF
        GO TO 90
C
 40   CONTINUE
C       day pointers
        SGRDAT(1)= DAT(1)
        SGRDAT(2)= DAT(2)
        IF (DAT(4).EQ.24) THEN
C         on boundary
          SGRDAT(3)= DAT(3)
        ELSE
          SGRDAT(3)= DAT(3)- 1
          IF (SGRDAT(3).EQ.0) THEN
C           back a month
            SGRDAT(2)= SGRDAT(2)- 1
            IF (SGRDAT(2).EQ.0) THEN
C             back a year
              SGRDAT(2)= 12
              SGRDAT(1)= SGRDAT(1)- 1
            END IF
            SGRDAT(3)= DAYMON(SGRDAT(1),SGRDAT(2))
          END IF
        END IF
        SGRDAT(4)= 24
        GO TO 90
C
 50   CONTINUE
C       month pointers
        SGRDAT(1)= DAT(1)
        IF (DAT(3).EQ.DAYMON(DAT(1),DAT(2)).AND.DAT(4).EQ.24) THEN
C         on a boundary
          SGRDAT(2)= DAT(2)
        ELSE
          SGRDAT(2)= DAT(2)- 1
          IF (SGRDAT(2).EQ.0) THEN
C           back a year
            SGRDAT(2)= 12
            SGRDAT(1)= SGRDAT(1)- 1
          END IF
        END IF
        SGRDAT(3)= DAYMON(SGRDAT(1),SGRDAT(2))
        SGRDAT(4)= 24
        GO TO 90
C
 60   CONTINUE
C       year pointer
        IF (DAT(2).EQ.12.AND.DAT(3).EQ.31.AND.DAT(4).EQ.24) THEN
C         on year boundary
          SGRDAT(1)= DAT(1)
        ELSE
          SGRDAT(1)= DAT(1)- 1
        END IF
        SGRDAT(2)= 12
        SGRDAT(3)= 31
        SGRDAT(4)= 24
        GO TO 90
C
 70   CONTINUE
C       century pointer
        IF (MOD(DAT(1)+1,100).EQ.0.AND.DAT(2).EQ.12.AND.
     1      DAT(3).EQ.31.AND.DAT(4).EQ.24) THEN
C         on the boundary
          SGRDAT(1)= DAT(1)
        ELSE
          SGRDAT(1)= (DAT(1)/100)*100- 1
        END IF
        SGRDAT(2)= 12
        SGRDAT(3)= 31
        SGRDAT(4)= 24
        GO TO 90
C
 90   CONTINUE
C
      SGRDAT(5)= 0
      SGRDAT(6)= 0
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDATCP
     I                   (ODAT,NDAT)
C
C     copies an old array date into a new one
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ODAT(6),NDAT(6)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ODAT   - from date array
C     NDAT   - to date array
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1,6
        NDAT(I)= ODAT(I)
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSBCLR
     I                   (WDMSFL,DSN)
C
C     + + + PURPOSE + + +
C     Remove data from the time-series buffer which may have been
C     modified in the WDM file, but not in the buffer.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     DSN    - data-set number on WDM file
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctsbuf.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I0,I6,ID
C
C     + + + EXTERNALS + + +
      EXTERNAL   COPYI, ZIPI
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I6 = 6
C
      IF (NUMID.GT.0) THEN
C       data stored in buffer
        DO 10 ID= 1,NUMID
C         see if input data set matches any data in buffer
          IF (BFILUN(ID).EQ.WDMSFL .AND. BDSN(ID).EQ.DSN) THEN
C           WDM file and data set match, remove them
            IF (ID.LT.NUMID) THEN
C             shift stored data information in buffer
              DO 50 I= ID,NUMID-1
C               move all info stored after this ID
                BFILUN(I)= BFILUN(I+1)
                BDSN(I)  = BDSN(I+1)
                BTU(I)   = BTU(I+1)
                BTS(I)   = BTS(I+1)
                BTRANS(I)= BTRANS(I+1)
                CALL COPYI (I6,BSDATE(1,I+1),BSDATE(1,I))
                BNVAL(I) = BNVAL(I+1)
                BQUAL(I) = BQUAL(I+1)
                BSREC(I) = BSREC(I+1)
                BNREC(I) = BNREC(I+1)
                BDATID(I)= BDATID(I+1)
 50           CONTINUE
            END IF
C           zero out last ID of information
            BFILUN(NUMID)= 0
            BDSN(NUMID)  = 0
            BTU(NUMID)   = 0
            BTS(NUMID)   = 0
            BTRANS(NUMID)= 0
            CALL ZIPI (I6,I0,BSDATE(1,NUMID))
            BNVAL(NUMID) = 0
            BQUAL(NUMID) = 0
            BSREC(NUMID) = 0
            BNREC(NUMID) = 0
            BDATID(NUMID)= 0
            NUMID= NUMID- 1
          END IF
 10     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSBINI
C
C     + + + PURPOSE + + +
C     Initialize time-series buffer common variables.
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctsbuf.inc'
C
C     + + + SAVES + + +
      INTEGER    INITFG
      SAVE       INITFG
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I0
      REAL       R0
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZIPI, ZIPR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA INITFG /0/
C
C     + + + EXTERNALS + + +
C
      I0 = 0
      R0 = 0.0
C
      IF (INITFG.EQ.0) THEN
C       init data buffer specification parameters
        CALL ZIPI (MXID,I0,BFILUN)
        CALL ZIPI (MXID,I0,BDSN)
        CALL ZIPI (MXID,I0,BTU)
        CALL ZIPI (MXID,I0,BTS)
        CALL ZIPI (MXID,I0,BTRANS)
        CALL ZIPI (MXID,I0,BQUAL)
        CALL ZIPI (6*MXID,I0,BSDATE)
        CALL ZIPI (MXID,I0,BNVAL)
        CALL ZIPI (MXID,I0,BSREC)
        CALL ZIPI (MXID,I0,BNREC)
        CALL ZIPI (MXID,I0,BDATID)
C       init counter and free data buffer position
        NUMID = 0
        FREREC= 1
C       init actual data buffer
        CALL ZIPR (BUFLEN*MXID,R0,TSBUF)
C       indicate intialization has been performed
        INITFG= 1
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WID2UD
     I                   (WDFLG,ID,
     O                    WDMSFL,DSN)
C
C     + + + PURPOSE + + +
C     convert an id to a wdm unit number and dsn
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDFLG,ID,WDMSFL,DSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDFLG  - wdm unit number flag, > 0 means id is actual dsn
C     ID     - id for unit number and dsn
C     WDMSFL - unit number of wdm file
C     DSN    - dataset number
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*4 WDID
C
C     + + + EXTERNALS + + +
      EXTERNAL    WID2UA
C
C     + + + END SPECIFICATIONS + + +
C
      CALL WID2UA (WDFLG,ID,
     O             WDMSFL,DSN,WDID)
C
      RETURN
      END
C
C
C
      SUBROUTINE   WID2UA
     I                   (WDFLG,ID,
     O                    WDMSFL,DSN,WDID)
C
C     + + + PURPOSE + + +
C     convert an id to a wdm unit number and dsn and return text id
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDFLG,ID,WDMSFL,DSN
      CHARACTER*4 WDID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDFLG  - wdm unit number flag, > 0 means id is actual dsn
C     ID     - id for unit number and dsn
C     WDMSFL - unit number of wdm file
C     DSN    - dataset number
C     WDID   - character identifier for wdm file
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cwdmid.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J
C
C     + + + END SPECIFICATIONS + + +
C
      IF (WDFLG .GT. 0) THEN
C       not using this scheme
        WDMSFL= WDFLG
        DSN   = ID
        WDID  = '    '
      ELSE
C       figure out what id
        I  = 0
        J  = 0
 10     CONTINUE
          I = I+ 1
          IF (I .LT. WIDCNT) THEN
            IF (ID .LT. WIDBSE(I+1)) THEN
              J= 1
            END IF
          ELSE
            J= 1
          END IF
          IF (J .EQ. 1) THEN
            WDMSFL= WIDFUN(I)
            DSN   = ID - WIDBSE(I)
            WDID  = WIDNAM(I)
          END IF
        IF (J .EQ. 0) GO TO 10
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDBSGC
     I                    (WDMSFL,DSN,SAIND,SALEN,
     O                     SAVAL,RETCOD)
C
C     + + + PURPOSE + + +
C     gets values of character search attribute for a dsn
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDMSFL,SAIND,SALEN,DSN,RETCOD
      CHARACTER*1 SAVAL(SALEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - data-set number to add
C     SAIND  - index number of attribute
C     SALEN  - length of attribute
C     SAVAL  - value of attribute
C     RETCOD - return code,
C                 0 - attribute value returned
C               -81 - data set does not exist
C              -107 - attribute not present on this data set
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     SAPOS,RREC,RIND,I,J,K,LWDMFL,LDSN
      CHARACTER*4 C4DUM
C
C     + + + FUNCTIONS + + +
      INTEGER     WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDDSCK, WDRCGO, WDSAFL, WID2UD
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (4A1)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (A4)
C
C     + + + END SPECIFICATIONS + + +
C
C     adjust wdm and dsn as needed
      CALL WID2UD (WDMSFL,DSN,
     O             LWDMFL,LDSN)
      CALL WDDSCK (LWDMFL,LDSN,
     O             RREC,RETCOD)
      IF (RETCOD.EQ.0) THEN
C       data set exists, get position within buffer
        RIND= WDRCGO(LWDMFL,RREC)
C       get starting position for search attribute
        CALL WDSAFL (SAIND,WIBUFF(1,RIND),
     O               SAPOS,RETCOD)
        IF (RETCOD.EQ.0) THEN
          J= SAPOS
          DO 10 I= 1,SALEN,4
            WRITE (C4DUM,2000) WIBUFF(J,RIND)
            READ  (C4DUM,1000) (SAVAL(K),K=I,I+3)
            J= J+ 1
 10       CONTINUE
        END IF
      END IF
      IF (RETCOD.NE.0) THEN
C       no value found, return blanks
        DO 20 I= 1,SALEN
          SAVAL(I)= ' '
 20     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDBSGI
     I                    (WDMSFL,DSN,SAIND,SALEN,
     O                     SAVAL,RETCOD)
C
C     + + + PURPOSE + + +
C     gets the values of integer search attribute for a dsn
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,SAIND,SALEN,DSN,RETCOD
      INTEGER   SAVAL(SALEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - data-set number to add
C     SAIND  - index number of attribute
C     SALEN  - length of attribute
C     SAVAL  - value of attribute
C     RETCOD - return code,
C                 0 - attribute value returned
C               -81 - data set does not exist
C              -107 - attribute not present on this data set
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SAPOS,RREC,RIND,I,J,LWDMFL,LDSN
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDDSCK, WDRCGO, WDSAFL, WID2UD
C
C     + + + END SPECIFICATIONS + + +
C
C     adjust wdm and dsn as needed
      CALL WID2UD (WDMSFL,DSN,
     O             LWDMFL,LDSN)
      CALL WDDSCK(LWDMFL,LDSN,
     O            RREC,RETCOD)
      IF (RETCOD.EQ.0) THEN
C       data set exists, get position within buffer
        RIND= WDRCGO(LWDMFL,RREC)
C       get starting position for search attribute
        CALL WDSAFL (SAIND,WIBUFF(1,RIND),
     O               SAPOS,RETCOD)
        IF (RETCOD.EQ.0) THEN
C         get the value
          DO 10 I= 1,SALEN
            J= SAPOS+I-1
            SAVAL(I)= WIBUFF(J,RIND)
 10       CONTINUE
        END IF
      END IF
      IF (RETCOD.NE.0) THEN
C       no value found, return dummy value
        DO 20 I= 1,SALEN
          SAVAL(I)= -999
 20     CONTINUE
      END IF
C
      RETURN
      END
