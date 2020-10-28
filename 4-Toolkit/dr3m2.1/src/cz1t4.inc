C
      COMMON /Z1T4 / B3, DA, EO, FO, NN, NK, NO, IMP, KNN, NO9, OPT,
     #               NDAY, NDELS, NOFE, NUPD, PDEL, RITE,
     #               DEL5P, EPSLN, CORF, A, D, E, F, G, H,
     #               U, OPTNO, X2, DIMP, INDP, IRES, NOUP, X, IEAC,
     #               OF
C
      INTEGER   B3, EO, FO, NK, NN, NO, KNN, NO9, OPT, NDAY,
     #          NDELS, NOFE, NUPD, RITE, DEL5P, E(14), F(14),
     #          OPTNO(14), INDP(45), IRES(30), NOUP(150), IEAC
      REAL      DA, IMP, PDEL, EPSLN, CORF, A(200), D(14), G(40),
     #          H(40), U(3), X2(16), DIMP(9,2), X(40), OF
C
      COMMON /Z1T4C/ IOUT, OPTION
C
      CHARACTER*4 IOUT(2), OPTION
C
      SAVE /Z1T4 /, /Z1T4C/
