C
C     storm related variables
      COMMON /CSTRM/ K1, K2, NF, NFE, NFS, POBS, TESTNO, BFL, ITEST,
     #               FPK, FVOL, NDATE, NHOUR, OUTVOL,
     #               SFPK, SFVOL, PSUM, QINPT
C
      INTEGER   K1(MXST), K2(MXST), NF(MXST), NFE(MXST), NFS(MXST),
     #          TESTNO(MXST), NDATE(MXST,3), NHOUR(MXST), ITEST(MXSG)
      REAL      POBS(MXST,MXRG), BFL(MXST), FPK(MXST), FVOL(MXST),
     #          OUTVOL(MXST), SFPK(MXST), SFVOL(MXST), PSUM(MXST2,3),
     #          QINPT(MXST)
C
      SAVE /CSTRM/
