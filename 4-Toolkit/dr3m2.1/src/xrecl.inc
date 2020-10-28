C     DATA  IOERR /  204 /, RLNGTH / 120 /                              DG (vms)
      DATA  IOERR /  204 /, RLNGTH / 480 /                              DG ansi
C     DATA  IOERR / 2013 /, RLNGTH / 480 /                              PC
C     DATA  IOERR /   70 /, RLNGTH / 480 /                              PC Lahey
C     DATA  IOERR /   18 /, RLNGTH / 240 /                              PRIME
C     DATA  IOERR /    1 /, RLNGTH / 240 /                              VAX
C
C     IOERR  - iostat from open, value returned when the file exists
C     RLNGTH - record length for file open statement for the
C              unformatted direct access file for segment q.
C              The record length for this file is 480 bytes.
C              480 bytes = 240 half words = 120 words
