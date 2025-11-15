WF_c(F)                = WF_c(F);
tq(c) = tq0(c);
ta(a,rg) = ta0(a,rg);
te(c) = te0(c);
tm(c) = tm0(c);
icd(C,CP) = icd0(C,CP);
ice(C,CP) = ice0(C,CP);
icm(C,CP) = icm0(C,CP);

UF.FX(F,A,RG) = 1;

CPI.L                  = CPI0;
DMPS.L                 = DMPS_c;
DPI.L                  = DPI0;
DTINS.L                = DTINS0;
EG.L                   = EG0;
EH.L(H)                = EH0(H);
EXR.L                  = EXR_c;
FSAV.L                 = FSAV_c;
GADJ.L                 = GADJ_c;
GOVSHR.L               = GOVSHR_c;
GSAV.L                 = GSAV0;
IADJ.L                 = IADJ_c;
INVSHR.L               = INVSHR_c;
MPS.L(INSDNG)          = MPS0(INSDNG);
MPSADJ.L               = MPSADJ_c;
PA.L(A)                = PA0(A);
PAR.L(A,RG)            = PAR0(A,RG);
PDD.L(C)               = PDD0(C);
PDS.L(C)               = PDS0(C);
PINTA.L(A,RG)          = PINTA0(A,RG) ;
PE.L(C)                = PE0(C);
PM.L(C)                = PM0(C);
PQ.L(C)                = PQ0(C);
PVA.L(A,RG)            = PVA0(A,RG);
PX.L(C)                = PX0(C);
PXAC.L(A,C)            = PXAC0(A,C);
QA.L(A)                = QA0(A);
QAR.L(A,RG)            = QAR0(A,RG);
QD.L(C)                = QD0(C);
QE.L(C)                = QE0(C);
QF.L(F,A,RG)           = QF_c(F,A,RG);
QFS.L(F)               = QFS_c(F);
QG.L(C)                = QG0(C);
QH.L(C,H)              = QH0(C,H);
QHA.L(A,H)             = QHA0(A,H);
QINT.L(C,A)            = QINT0(C,A);
QINTA.L(A,RG)          = QINTA0(A,RG) ;
QINV.L(C)              = QINV0(C);
QM.L(C)                = QM0(C);
QQ.L(C)                = QQ0(C);
QT.L(C)                = QT0(C);
QVA.L(A,RG)            = QVA0(A,RG);
QX.L(C)                = QX0(C);
QXAC.L(A,C)            = QXAC0(A,C);
TABS.L                 = TABS0;
TRII.L(INSDNG,INSDNGP) = TRII0(INSDNG,INSDNGP);
TINS.L(INSDNG)         = TINS0(INSDNG);
TINSADJ.L              = TINSADJ0;
WALRAS.L               = WALRAS0;
WALRASSQR.L            = 0 ;
WF.L(F)                = WF_c(F);
WFDIST.L(F,A,RG)       = WFDIST_c(F,A,RG);
YF.L(F)                = YF0(f);
YG.L                   = YG0;
YI.L(INS)              = YI0(INS);
YIF.L(INS,F)           = YIF0(INS,F);

betam(C,H)            = betam0(C,H);
betah(A,H)            = betah0(A,H);

gammam(C,H)            = gammam0(C,H);
gammah(A,H)            = gammah0(A,H);
pwe(C)                 = pwe_c(C);
pwm(C)                 = pwm_c(C);
trnsfr(INS,AC)         = trnsfr0(INS,AC);
fprd(F,A,RG)           = 1;

GDP_sh(A) =  SUM(RG, PVA0(A,RG)*QVA0(A,RG) ) /
 SUM((AP, RG), PVA0(AP ,RG)*QVA0(AP,RG));
alphava(A,RG) = alphava_c(A,RG);

alphavaadj(A,RG)=1;

hpop(H)                = hpop0(H);
