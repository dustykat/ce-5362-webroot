C
C
C
      SUBROUTINE   DRFILE
     O                   ( RETC )
C
C     + + + PURPOSE + + +
C     This routine makes sure all of the necessary files are open.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   RETC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RETC   - return code
C              0 - all necessary files opened successfully
C              1 - problem opening a file
C
C     + + + PARAMETERS + + +
      INCLUDE 'pinout.inc'
      INCLUDE 'plimt.inc'
C
C     + + + COMMONS + + +
      INCLUDE 'cf1a3.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   MAX, MAXT, KNT, FUNT(11), FTYP(11), FOPN(11), TYP
      CHARACTER*4 FCOD(7)
      CHARACTER*64 SCRTCH, WDNAME
C
C     + + + EXTERNALS + + +
      EXTERNAL   GETFLS, OPNFIL
C
C     + + + DATA INITIALIZATIONS + + +
      DATA FCOD  / 'WDM ','INP ','OUT ','DMP ','UNT ','PKS ','SGS '/
      DATA FUNT  /  12,     0,     0,    11,    50,    51,    15,
     #                  16,    18,    19,    20   /
      DATA FTYP  /   1,     2,     3,     3,     3,     3,     4,
     #                   5,     5,     5,     5   /
C
C     + + + OUTPUT FORMATS + + +
 2004 FORMAT (    '     opened      ', 'temp', I6, 3X, A64 )
 2005 FORMAT (    ' ** not opened   ', 'temp', I6, 3X, A64 )
 2007 FORMAT (    ' -------------   ----------   ', 50('-') )
 2008 FORMAT (    '     opened      ', 'message', I3, 3X, A64 )
 2009 FORMAT (    ' ** not opened   ', 'message', I3, 3X, A64 )
C
C     + + + END SPECIFICATIONS + + +
C
      SCRTCH = '    '
      MAX = 7
      MAXT = 11
      FUNT(2) = INFIL
      FUNT(3) = OUTFIL
      IFILE = FUNT(7)
      IFILED = FUNT(8)
      IFILEP = FUNT(9)
      WDMFL = FUNT(1)
      MESSFL = 9
C
      DO 50 KNT = 1, MAXT
C       set file open status to no action
        FOPN(KNT) = 0
 50   CONTINUE
C
C     open user specified files
      CALL GETFLS ( MAX, FCOD, FUNT, FTYP,
     O              FOPN, RETC )
      IF (RETC .EQ. 0) THEN
C       open temporary files
        KNT = MAX
 100    CONTINUE
          IF (FOPN(KNT) .EQ. 0) THEN
            CALL OPNFIL ( FUNT(KNT), FTYP(KNT), SCRTCH, RETC )
            IF (RETC .EQ. 0) THEN
C             successful file open
              FOPN(KNT) = 1
              WRITE (IOOT,2004) FUNT(KNT)
            ELSE
C             file open not successful
              FOPN(KNT) = 2
              WRITE (IOOT,2005) FUNT(KNT)
            END IF
          END IF
          KNT = KNT + 1
        IF (KNT .LE. MAXT  .AND.  RETC .EQ. 0) GO TO 100
C
        IF (RETC .EQ. 0) THEN
C         open annie message file
          TYP = 6
          INCLUDE 'fmessg.inc'
          CALL OPNFIL ( MESSFL, TYP, WDNAME, RETC )
          IF (RETC .EQ. 0) THEN
C           successful open of message file
            WRITE (IOOT,2008) MESSFL, WDNAME
          ELSE
C           message file not opened
            WRITE (IOOT,2009) MESSFL, WDNAME
          END IF
        END IF
      END IF
C
      WRITE (IOOT,2007)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GETFLS
     I                   ( MAX, FCOD, FUNT, FTYP,
     O                     FOPN, RETC )
C
C     + + + PURPOSE + + +
C     Reads a list of files from a master file and opens these files
C     for input or output.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MAX, RETC
      INTEGER   FUNT(MAX), FTYP(MAX), FOPN(MAX)
      CHARACTER*4 FCOD(MAX)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MAX    - number of files defined
C     FCOD   - array of defined file type codes
C     FUNT   - array of fortran unit numbers corresponding to FCODs
C     FTYP   - array of file types
C              1 - WDM file
C              2 - input file
C              3 - output file
C     FOPN   - flag indicating final status of file
C              0 - no open attempted
C              1 - file open was successful
C              2 - file open was unsuccessful
C     RETC   - return code
C              0 - all files opened successfully
C              1 - problem opening one or more files
C
C     + + + PARAMETERS + + +
      INCLUDE 'pinout.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   KNT, KNTMX, IUNIT, ITYP, DONE
      CHARACTER*10 CODES
      CHARACTER*64 NAME
C
C     + + + EXTERNALS + + +
      EXTERNAL   OPNFIL
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT ( A64 )
 1001 FORMAT ( A10, A64 )
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (//, ' Enter name of master file:' )
 2001 FORMAT (//, ' Unable to open master file ', A64,
     #         /, ' try again.' )
 2002 FORMAT (//, ' Unable to open master file ', A64,
     #         /, ' Check your directory for valid file name.'
     #         / )
 2003 FORMAT (//, '     status         codes     file name'
     #         /, ' -------------   ----------   ', 50('-') )
 2004 FORMAT (    '     opened      ', A10, 3X, A64 )
 2005 FORMAT (    ' ** not opened   ', A10, 3X, A64 )
 2006 FORMAT (    ' ** invalid      ', A10, 3X, A64 )
 2007 FORMAT (    ' -------------   ----------   ', 50('-') )
 2008 FORMAT (    '                 ', A10, 3X, A64 )
C
C     + + + END SPECIFICATIONS + + +
C
      IUNIT = 60
      ITYP = 2
      KNT = 0
      KNTMX = 3
 100  CONTINUE
C       get name of master file and open it
        WRITE (IOOT,2000)
        READ (IOIN,1000) NAME
        CALL OPNFIL ( IUNIT, ITYP, NAME, RETC )
        IF (RETC .NE. 0  .AND.  KNT+1 .LT. KNTMX) THEN
C         problem opening master file, try again
          KNT = KNT + 1
          WRITE (IOOT,2001) NAME
        ELSE IF (RETC .NE. 0) THEN
C         unable to open master file
          WRITE (IOOT,2002) NAME
        END IF
      IF (RETC .NE. 0  .AND.  KNT .LT. KNTMX) GO TO 100
C
      IF (RETC .EQ. 0) THEN
C       read master file
        DONE = 0
        WRITE (IOOT,2003)
 200    CONTINUE
          READ (IUNIT,1001,END=290) CODES, NAME
C           successful read, check for match
            KNT = 1
 250        CONTINUE
              IF (CODES(1:3) .EQ. '***') THEN
C               comment record
                WRITE (IOOT,2008) CODES, NAME
                KNT = 0
              ELSE IF (CODES(1:4) .EQ. FCOD(KNT)) THEN
C               matches file type, open it
                CALL OPNFIL ( FUNT(KNT), FTYP(KNT), NAME, RETC )
                IF (RETC .EQ. 0) THEN
C                 file opened successfully
                  FOPN(KNT) = 1
                  KNT = 0
                  WRITE (IOOT,2004) CODES, NAME
                ELSE
C                 problem opening file
                  FOPN(KNT) = 2
                  KNT = -1
                  WRITE (IOOT,2005) CODES, NAME
                END IF
              ELSE IF (KNT .LT. MAX) THEN
C               no match, keep looking
                KNT = KNT + 1
              ELSE
C               no match, end of file types
                KNT = 0
                WRITE (IOOT,2006) CODES, NAME
              END IF
            IF (KNT .GT. 0) GO TO 250
            GO TO 299
 290      CONTINUE
C           end of file
            DONE = 1
            WRITE (IOOT,2007)
 299      CONTINUE
        IF (RETC .EQ. 0  .AND.  DONE .EQ. 0) GO TO 200
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   OPNFIL
     I                   ( IUNIT, ITYP, NAME,
     O                     RETC )
C
C     + + + PURPOSE + + +
C     Opens a file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IUNIT, ITYP, RETC
      CHARACTER*64 NAME
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IUNIT  - Fortran unit number to be used for open
C     ITYP   - type of file to be opened
C              1 - WDM data file
C              2 - input file
C              3 - output file
C              4 - direct access, unformatted
C              5 - temporary, unformatted, sequential access
C              6 - WDM message file
C     NAME   - name of file to be opened
C              for ITYP = 4, if blank, will be opened as scratch
C                  ITYP = 5, leave blank
C     RETC   - return code
C              0 - file opened successfully
C              1 - file not opened
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   IOS, IOERR, RLNGTH, RONWFG
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDBOPN
C
C     + + + DATA INITIALIZATIONS + + +
      INCLUDE 'xrecl.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ITYP .LE. 0  .OR.  ITYP .GT. 6) THEN
C       invalid file type
        RETC = 1
      ELSE
C       open file using appropriate open statement
C               wdm   in  out   da  temp mess
        GO TO ( 100, 200, 300, 400, 500, 100 ), ITYP
 100      CONTINUE
C           wdm file:  1-users, 6-message
            IF (ITYP .EQ. 1) THEN
C             users wdm, open read and write
              RONWFG = 0
            ELSE
C             message wdm, open read only
              RONWFG = 1
            END IF
            CALL WDBOPN ( IUNIT, NAME, RONWFG,
     O                    RETC )
            IF (RETC .NE. 0) RETC = 1
            GO TO 900
 200      CONTINUE
C           input file
            OPEN ( UNIT = IUNIT,
     #             FILE = NAME,
     #           STATUS = 'OLD',
     #           IOSTAT = IOS,
     #              ERR = 210 )
              RETC = 0
              GO TO 900
 210        CONTINUE
C             problem with open
              RETC = 1
              GO TO 900
 300      CONTINUE
C           output file
            OPEN ( UNIT = IUNIT,
     #             FILE = NAME,
     #           STATUS = 'NEW',
     #           IOSTAT = IOS,
     #              ERR = 310 )
              RETC = 0
              GO TO 900
 310        CONTINUE
C             problem with open
              IF (IOS .EQ. IOERR) THEN
C               file exists, try to delete it
                OPEN ( UNIT=IUNIT, FILE=NAME, IOSTAT=IOS )
                IF (IOS .EQ. 0)
     #            CLOSE ( UNIT=IUNIT, STATUS='DELETE', IOSTAT=IOS )
                IF (IOS .EQ. 0) THEN
C                 old file deleted, open new one
                  OPEN ( UNIT = IUNIT,
     #                   FILE = NAME,
     #                 STATUS = 'NEW',
     #                 IOSTAT = IOS )
                  IF (IOS .EQ. 0) THEN
C                   successful open
                    RETC = 0
                  ELSE
C                   no success, give it up
                    RETC = 1
                  END IF
                ELSE
C                 could not delete old file
                  RETC = 1
                END IF
              ELSE
C               unknown problem with file
                RETC = 1
              END IF
              GO TO 900
 400      CONTINUE
C           direct access file, unformatted
            IF (NAME(1:4) .EQ. '    ') THEN
C             open scratch file
              OPEN ( UNIT = IUNIT, STATUS = 'SCRATCH',
     #                               FORM = 'UNFORMATTED',
     #                             ACCESS = 'DIRECT',
     #                               RECL = RLNGTH )                     KF 1192
            ELSE
C             open file by name
              OPEN ( UNIT = IUNIT, FILE = NAME,
     #                             FORM = 'UNFORMATTED',
     #                           ACCESS = 'DIRECT',
     #                             RECL = RLNGTH )                       KF 1192
              RETC = 0
            END IF
            GO TO 900
 500      CONTINUE
C           temporary, unformatted, sequential access file
            OPEN (UNIT = IUNIT, STATUS = 'SCRATCH',
     #                            FORM = 'UNFORMATTED',
     #                          ACCESS = 'SEQUENTIAL' )
            RETC = 0
            GO TO 900
C
 900    CONTINUE
      END IF
C
      RETURN
      END
