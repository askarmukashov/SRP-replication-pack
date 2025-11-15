FMOBFE(F)    = 0;
FACTFE(F)    = 0;
FMOBUE(F)    = 0;

FMOBFE(F) = 1;
* FMOBUE(FLAB) = 1;
FACTFE(FLND) = 1;
FACTFE(FLIV) = 1;
FACTFE(FCAP) = 1;
NUMERAIRE = 1;
*SICLOS=5;
SICLOS=3;
ROWCLOS=1;
PVA.LO(A,RG)$PVA0(A,RG) = 0;
QVA.LO(A,RG)$QVA0(A,RG) = 0;
QH.LO(C,H)$QH0(C,H)                = 0;
QHA.LO(A,H)$QHA0(A,H)              = 0;

*Macro closures ---------------------------------

  mps01(INSDNG)  = MPS01SIM(INSDNG);
  tins01(INSDNG) = TINS01SIM(INSDNG);

*Numeraire
 IF(NUMERAIRE EQ 1,
  CPI.FX     = CPI.L;
  DPI.LO     = -INF;
  DPI.UP     = +INF;
  );
 IF(NUMERAIRE EQ 2,
  DPI.FX     = DPI.L;
  CPI.LO     = -INF;
  CPI.UP     = +INF;
  );

*Government
 IF(GOVCLOS EQ 1,
  TINSADJ.FX = TINSADJ.L;
  DTINS.FX   = DTINS.L;
  GSAV.LO    = -INF;
  GSAV.UP    = +INF;
  );
 IF(GOVCLOS EQ 2,
  TINSADJ.FX = TINSADJ.L;
  DTINS.LO   = -INF;
  DTINS.UP   = +INF;
  GSAV.FX    = GSAV.L;
  );
 IF(GOVCLOS EQ 3,
  TINSADJ.LO = -INF;
  TINSADJ.UP = +INF;
  DTINS.FX   = DTINS.L;
  GSAV.FX    = GSAV.L;
  );

*Current account
* in the l-t ROWCLOS =1 (exr adjusts)
 IF(ROWCLOS EQ 1,
  FSAV.FX   = FSAV.L;
  EXR.LO    = -INF;
  EXR.UP    = +INF;
  );
 IF(ROWCLOS EQ 2,
  EXR.FX    = EXR.L;
  FSAV.LO   = -INF;
  FSAV.UP   = +INF;
  );

*SAVINGS AND INVESTMENT
 IF(SICLOS EQ 1,
  MPSADJ.FX = MPSADJ.L;
  DMPS.LO   = -INF;
  DMPS.UP   = +INF;
  IADJ.FX   = IADJ.L;
  INVSHR.LO = -INF;
  INVSHR.UP = +INF;
  GADJ.FX   = GADJ.L;
  GOVSHR.LO = -INF;
  GOVSHR.UP = +INF;
  );
 IF(SICLOS EQ 2,
  MPSADJ.LO = -INF;
  MPSADJ.UP = +INF;
  DMPS.FX   = DMPS.L;
  IADJ.FX   = IADJ.L;
  INVSHR.LO = -INF;
  INVSHR.UP = +INF;
  GADJ.FX   = GADJ.L ;
  GOVSHR.LO = -INF;
  GOVSHR.UP = +INF;
  );
 IF(SICLOS EQ 3,
  MPSADJ.FX = MPSADJ.L;
  DMPS.FX   = DMPS.L;
  IADJ.LO   = -INF;
  IADJ.UP   = +INF;
  INVSHR.LO = -INF;
  INVSHR.UP = +INF;
  GADJ.FX   = GADJ.L;
  GOVSHR.LO = -INF;
  GOVSHR.UP = +INF;
  );
 IF(SICLOS EQ 4,
  MPSADJ.FX = MPSADJ.L;
  DMPS.LO   = -INF;
  DMPS.UP   = +INF;
  IADJ.LO   = -INF;
  IADJ.UP   = +INF;
  INVSHR.FX = INVSHR.L;
  GADJ.LO   = -INF;
  GADJ.UP   = +INF;
  GOVSHR.FX = GOVSHR.L;
  );
 IF(SICLOS EQ 5,
  MPSADJ.LO = -INF;
  MPSADJ.UP = +INF;
  DMPS.FX   = DMPS.L;
  IADJ.LO   = -INF;
  IADJ.UP   = +INF;
  INVSHR.FX = INVSHR.L;
  GADJ.LO   = -INF;
  GADJ.UP   = +INF;
  GOVSHR.FX = GOVSHR.L;
  );

*Factor closures ------------------

 LOOP(F$FMOBFE(F),
  WFDIST.FX(F,A,RG) = WFDIST.L(F,A,RG);
  QFS.FX(F) = QFS.L(F);
  WF.LO(F) = -INF;
  WF.UP(F) = +INF;
  QF.LO(F,A,RG)$(QF0(F,A,RG)) = -INF;
  QF.UP(F,A,RG)$(QF0(F,A,RG)) = +INF;
 );

 LOOP(F$FACTFE(F),
  WF.FX(F)   = WF.L(F);
  QF.FX(F,A,RG) = QF.L(F,A,RG);
  WFDIST.LO(F,A,RG)$(QF0(F,A,RG)) = -INF;
  WFDIST.UP(F,A,RG)$(QF0(F,A,RG)) = +INF;
  QFS.LO(F) = -INF;
  QFS.UP(F) = +INF;
 );

 LOOP(F$FMOBUE(F),
  WFDIST.FX(F,A,RG) = WFDIST.L(F,A,RG);
  WF.FX(F) = WF.L(F);
  QF.LO(F,A,RG)$(QF0(F,A,RG)) = 0;
  QF.UP(F,A,RG)$(QF0(F,A,RG)) = +INF;
  QFS.LO(F) = -INF;
  QFS.UP(F) = +INF;
 );

 QF.FX(FCAP,A,RG)$(AFX(A,RG) AND QF0(FCAP,A,RG)) = QF.L(FCAP,A,RG);
 WFDIST.LO(FCAP,A,RG)$(AFX(A,RG) AND QF0(FCAP,A,RG)) = -INF;
 WFDIST.UP(FCAP,A,RG)$(AFX(A,RG) AND QF0(FCAP,A,RG)) = +INF;

*---Factor utilization ---------------

 UF.LO(F,A,RG)$FUTL(F,A,RG) = 0;
 UF.UP(F,A,RG)$FUTL(F,A,RG) = 1;
 UF.FX(F,A,RG)$(NOT FUTL(F,A,RG)) = 1;
