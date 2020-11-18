C
C     THIS COMMON BLOCK CONTAINS INFO ABOUT THE CURRENT TIMESERIES
C
      COMMON /CWTSDS/ CURREC,CURBKS,CURPOS,CURNOV,CURTST,CURTUN,CURCMP,
     1                CURQUA,CURCNT,CURDAT,   CURVAL,PREVAL
C
      INTEGER         CURREC,CURBKS,CURPOS,       CURTST,CURTUN,CURCMP,
     1                CURQUA,       CURDAT(6)
      INTEGER*4                            CURNOV,
     1                       CURCNT
      REAL                                    CURVAL,PREVAL
C
