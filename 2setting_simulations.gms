$setglobal ide "ide=%gams.ide% lo=%gams.lo% errorlog=%gams.errorlog% errmsg=1"
$ontext
$ONSYMLIST ONSYMXREF OFFUPPER
$ONEMPTY
$offlisting
$offsymxref offsymlist
option
  limrow = 0,
  limcol = 0,
  solprint = off,
  sysout = off
  SOLVELINK=5
;
$offtext

SETS
*Time periods
 T                       time periods
 TC(T)                   active time periods
 T1(T)                   base year of simulation

 TN(T)                   final year
*Calibration parameters
 MISCAC                  miscellaneous simulation data elements          / QG, GTRH, HREM, HSAV, EXR, FSAV, GSAV, FGRN, PWE, PWM, DRATE, KSTK /
;

ALIAS
 (T,TP,TPP), (TC,TCP)
;


PARAMETERS
 TOLERATE                solution tolerance level                         / 1e-4 /
*Macroeconomic closures
 NUMERAIRE            numeraire
 SICLOS               value for savings-investment closure
 ROWCLOS              value for rest-of-world closure
 GOVCLOS              value for government closure
 MPS01SIM(INS)         0-1 par for potential flexing of savings rates
 TINS01SIM(INS)        0-1 par for potential flexing of direct tax rates
 USSELAS(F)              supply elasticities for factors with upward-sloping supply (USS) curves
*Factor market closures
 FMOBFE(F)             factor is fully employed and mobile
 FACTFE(F)             factor is fully employed and activity-specific
 FMOBUE(F)             factor is unemployed and mobile
;


*------------------------------------------------------------------------------
*1. Baseline and l-t macro simulation scenarios
*------------------------------------------------------------------------------

*Import and calibrate economic scenarios



SETS
 FL(A,RG)                sector-specific (fixed) land demand
 AFX(A,RG)               sectors with calibrated capital stock growth
 ANFX(A,RG)              sectors without calibrated capital stock growth

;

PARAMETER
*Excel-inputted baseline data
 BASE_CLOSURE(AC)        baseline closure
 BASE_CAPACC(MISCAC)     baseline capital accumulation parameters
 BASE_TFP(A,RG)        baseline sectoral TFP growth projections
 BASE_FACPROD(F)       baseline factor-specific productivity growth projections
 BASE_FACSUPPLY(F)     baseline sector factor demand growth projections
 BASE_FIXLAND(A,RG)    baseline sectoral land growth projections
 BASE_FIXCAP(A,RG)     baseline growth rate for sectors with fixed resources
 BASE_POP(H)           baseline household population growth projections
 BASE_PW(C)            baseline world price growth projections
 BASE_MISC(MISCAC)     baseline miscellaneous growth projections

*Final baseline and simulation growth rates
 FIN_CLOSURES(AC)      closures
 FIN_FACDGR(F,A,RG) labor force and land growth rate
 FIN_FACSGR(F)     total factor supply growth rate
 FIN_TFPGR(A,RG)   total factor productivity growth rate
 FIN_FPRDGR(F,A,RG) factor-specific productivity growth rate
 FIN_GOVGR        government consumption expenditure growth
 FIN_EXRGR        exchange rate growth rate (positive if increasing)
 FIN_FSAVGR       foreign savings growth rate (positive if increasing)
 FIN_GSAVGR       government savings growth rate (positive if increasing)
 FIN_MPSGR        household savings rate growth rate (positive if increasing)
 FIN_POPGR(H)      household population growth
 FIN_PWEGR(C)      world export price changes
 FIN_PWMGR(C)      world import price changes
 FIN_TRNSFRGR(INS,AC) exogenous institutional transfer changes
 FIN_AFXGR(A,RG)   growth rate for sectors with fixed resources
 FIN_DRATE(A,RG)   sector and simulation specific capital depreciation rates
 FIN_BETAMGR(C,H)  household marketed consumption shares
 FIN_GAMMAMGR(C,H) household marketed subsistence consumption
 FIN_UF(F,A,RG)    factor utilization rate
*Capital accumulation rates
 NATDRATE                national annual capital depreciation rate
 ACCRATE                 annual capital accumulation rate
;

*Import time periods (T TC) from 2baseline.xlsx
$call "gdxxrw i=data\%country%\2baseline_%country%.xlsx o=baseline.gdx index=index!a5"
$gdxin baseline.gdx
$load T
$loaddc  USSELAS BASE_CLOSURE BASE_CAPACC BASE_TFP BASE_FACSUPPLY BASE_FACPROD BASE_FIXLAND BASE_FIXCAP BASE_POP BASE_PW BASE_MISC
$gdxin


*Factors with upward-sloping supply functions
 USS(F)$(USSELAS(F) NE 0 AND USSELAS(F) NE EPS)  = YES;
 elasuss(F)$(USSELAS(F) NE EPS) = USSELAS(F);


 BASE_TFP(A,RG)$(QA0(A)=0) = 0;


 FIN_TFPGR(A,RG) = BASE_TFP(A,RG)/100 ;
 FIN_FPRDGR(F,A,RG) = BASE_FACPROD(F)/100 ;

*Exogenous labour force demand
 FIN_FACDGR(F,A,RG) = BASE_FACSUPPLY(F)/100 ;

*Exogenous labour force supply
 FIN_FACSGR(F) = BASE_FACSUPPLY(F)/100 ;

*Government payments to households (e.g., social security)
 FIN_TRNSFRGR(H,'GOV') = BASE_MISC('GTRH')/100 ;

*Household foreign receipts (e.g., remittance incomes)
 FIN_TRNSFRGR(H,'ROW') = BASE_MISC('HREM')/100 ;

*Government foreign receipts (e.g., development assistance)
 FIN_TRNSFRGR('GOV','ROW') = BASE_MISC('FSAV')/100 ;

*Government recurrent expenditure
 FIN_GOVGR = BASE_MISC('QG')/100 ;

*Government recurrent expenditure
 FIN_GSAVGR = BASE_MISC('GSAV')/100;

*Foreign capital inflow changes (current account balance)
 FIN_EXRGR = BASE_MISC('EXR')/100;

*Foreign capital inflow changes (current account balance)
 FIN_FSAVGR = BASE_MISC('FSAV')/100;

*Household savings rate growth rate (positive if increasing)
 FIN_MPSGR = BASE_MISC('HSAV')/100 ;

*World import prices
 FIN_PWMGR(C) = BASE_PW(C)/100 ;

*World export prices
 FIN_PWEGR(C) = BASE_PW(C)/100 ;

*Depreciation rates
 FIN_DRATE(A,RG) = BASE_CAPACC('DRATE')/100;

*Household population growth rates
 FIN_POPGR(H) = BASE_POP(H)/100;

*Household marketed consumption shares and subsistence levels
 FIN_BETAMGR(C,H) = 0;
 FIN_GAMMAMGR(C,H) = 0;

*Factor utilization rate
 FIN_UF(F,A,RG) = 1;

*Fixed agricultural land growth
 FL(A,RG)$(BASE_FIXLAND(A,RG) NE EPS) = YES;
 FIN_FACDGR(FLND,A,RG)$FL(A,RG) = BASE_FIXLAND(A,RG)/100;

*Fixed capital stock growth
 AFX(A,RG)$(BASE_FIXCAP(A,RG) NE EPS) = YES;
 ANFX(A,RG)$(NOT AFX(A,RG)) = YES;
 FIN_AFXGR(A,RG)$AFX(A,RG) = BASE_FIXCAP(A,RG)/100;

*Capital stock units for recursive updating
*  Baseline capital accumulation and depreciation rates
 accrate  = BASE_CAPACC('KSTK')/100;
 natdrate = BASE_CAPACC('DRATE')/100;
*  Calculate investment units to achieve targeted initial year capital accumulation rate
 QINVK    = (natdrate + accrate) * SUM(FCAP, QFS0(FCAP));
 iwts(C)  = qbarinv(C) / SUM(CP, qbarinv(CP));
 alphainv = ((natdrate + accrate) * SUM(FCAP, QFS0(FCAP))) / PROD(C, QINV0(C)**iwts(C));


*Macroeconomic closures
 NUMERAIRE = BASE_CLOSURE('DUM');
 ROWCLOS   = BASE_CLOSURE('ROW');
 GOVCLOS   = BASE_CLOSURE('GOV');
 SICLOS    = BASE_CLOSURE('S-I');

*Institutional savings and tax rate adjustments
 MPS01SIM(INSDNG)  = 1;
 TINS01SIM(INSDNG) = 1;

*Factor markets closures
 FMOBFE(F)$(BASE_CLOSURE(F) EQ 1) = 1;
 FACTFE(F)$(BASE_CLOSURE(F) EQ 2) = 1;
 FMOBUE(F)$(BASE_CLOSURE(F) EQ 3) = 1;

*If no value is specified for a factor, impose FMOBFE:
 FMOBFE(F)$(FMOBFE(F) + FACTFE(F) + FMOBUE(F) EQ 0) = 1;


*------------
*Module exogenous shocks
SETS
X                          all main simulations
XC(X)                      active main simulations
vars
;

ALIAS
 (X,XP), (XC,XCP)
;
PARAMETER
shocks(X,T, vars)
PH_export(X,T,*)
model_stat_export(X,T,*)
solvestat
modelstat
XLTEST
;

$call "gdxxrw i=data\%country%\2sampled_scenarios_%country%.xlsx o=simulation.gdx index=index!a5"
$gdxin simulation.gdx
$loaddc X vars shocks


***** new super slim report (even slimmed cannot be run because of the memory capacity)
** => include report in the loop
SET
 RA                      all activity reporting accounts
 RAG(RA)                 aggregate activity reporting accounts
 FA                      all factor reporting accounts
 FAG(FA)                 aggregate factor reporting accounts
 HA                      all household reporting accounts
 HAG(HA)                 aggregate household reporting accounts
 AFSAC                   agGDP+ components                                       / TOT+, AFS+, DIR+, AGR+, OFF+, PRC+, INP+, INP_AGR+, INP_PRC+, TRD+, TRD_AGR+, TRD_PRC+, HFS+, HOT+, FSV+ /
 FDAC                    food price accounts                                     / ALL, FOOD, NON-FOOD, FOOD_REAL /

 IGDP                    items for GDP and national accounts     / ABSORP, PRVCON, FIXINV, DSTOCK, GOVCON, EXPORTS, IMPORTS, GDPMP, GDPMP2, NETITAX, GDPFC2 /
*RIAPA Sets
R_RA(RA)                shortened list of reporting activities
R_FA(FA)                shortened list of reporting factors
R_HA(HA)                shortened list of reporting households
R_AAGRI(A)              agricultural activities for AgGDP+ calculations
R_ANAGR(A)              nonagricultural activities for AgGDP+ calculations
R_APROC(A)              agro-processing activities for AgGDP+ calculations
R_ATRTR(A)              trade and transport activities for AgGDP+ calculations
R_AHOTL(A)              hotels activities for AgGDP+ calculations
R_AFSRV(A)              catering activities for AgGDP+ calculations
R_AFOOD(A)              food producing activities (incl. food food services)
R_CAGRI(C)              agricultural commodities for AgGDP+ calculations
R_CNAGR(C)              nonagricultural commodities for AgGDP+ calculations
R_CPROC(C)              agro-processing activities for AgGDP+ calculations
R_CTRTR(C)              trade and transport activities for AgGDP+ calculations
R_CHOTL(C)              hotels activities for AgGDP+ calculations
R_CFSRV(C)              catering activities for AgGDP+ calculations
R_CFOOD(C)              food products (incl. food food services)
A_CLIM_SH(A)
;

ALIAS (RA,RAP), (RAG,RAGP), (FA,FAP), (FAG,FAGP), (HA,HAP), (HAG,HAGP), (A_CLIM_SH, A_CLIM_SHP);

PARAMETER
*Mappings
 ACTAGG(A,RG,RA)         model activities to reporting activities
 COMAGG(C,RA)            model commodities to reporting activities
 FACAGG(F,FA)            model factors to reporting factors
 HHDAGG(H,HA)            model households to reporting households
*Gross domestic product
 GDPX(X, T, RA)          GDP at factor cost
 GDPMP_REAL(IGDP, X,T)       GDP by expenditure group
 GDPMP_NOMINAL(IGDP, X,T)       GDP by expenditure group
 GDP_sec_realX(A, RG, X,T)       GDP by sectors
 PQX(C, X,T)         prices by sectors
 GDP_sec_realXA(A, RG, X)       GDP by sectors
 PQXA(C, X)         prices by sectors
 STRUCBASE2(RA,STRCOL)   economic structure in the base
*Household consumption and welfare
 HHDP(HA)         household populations
 HHDC(HA)         household real consumption
*International trade
 TRDE(RA)         real exports
 TRDM(RA)         real imports
*Prices
 PRC_FD(FDAC)     prices - food and nonfood products
*AgGDP+ and AgEMP+
 INPUT(*,C)        input value added estimates
 TRADE(*)          trade and transport value added estimates
 GDPX_PLUS(X, T, AFSAC)  agri-food system GDP
 EMPX_PLUS(AFSAC)  agri-food system employment

 GDP_CLIM_SH_shX(X,T, A)
 FSAVX(X,T,*)
 EXRX(X,T,*)
;

$call "gdxxrw i=data\0.reporting_specification.xlsx o=report.gdx index=index!a6"
$gdxin report.gdx
$load RA FA HA
$loaddc RAG FAG HAG ACTAGG FACAGG HHDAGG  R_RA R_FA R_HA R_AAGRI R_APROC R_ATRTR R_AHOTL R_AFSRV R_AFOOD
$gdxin

 COMAGG(C,RA)$SUM((A,RG), MAC(A,C) AND ACTAGG(A,RG,RA)) = YES;

 R_CAGRI(C)$SUM(R_AAGRI, MAC(R_AAGRI,C)) = YES;
 R_CPROC(C)$SUM(R_APROC, MAC(R_APROC,C)) = YES;
 R_CTRTR(C)$SUM(R_ATRTR, MAC(R_ATRTR,C)) = YES;
 R_CHOTL(C)$SUM(R_AHOTL, MAC(R_AHOTL,C)) = YES;
 R_CFSRV(C)$SUM(R_AFSRV, MAC(R_AFSRV,C)) = YES;
 R_CFOOD(C)$SUM(R_AFOOD, MAC(R_AFOOD,C)) = YES;

 R_ANAGR(A)$(NOT R_AAGRI(A)) = YES;
 R_CNAGR(C)$(NOT R_CAGRI(C)) = YES;

A_CLIM_SH(A)$R_AAGRI(A) = YES;

*------------------------------------------------------------------------------
*1. Poverty (FGT), inequality (Theil) amd diet deprivation (REDD) indicators
*------------------------------------------------------------------------------
SCALAR
 CSTCALADJ       ratio of actual to minimum calorie prices (country specific adjustment to match FAOs reported national prevalence of undernourishment)
;

*------Kenya
$setglobal survey       "KEN_2015"
 CSTCALADJ = 1.3785;

*------Malawi
*$setglobal survey       "MWI_2019"
* CSTCALADJ = 1.46;
*------Rwanda
*$setglobal survey       "RWA_2016"
* CSTCALADJ = 1.665;

SET
*Survey households
 HID                     survey households
 MHIDH(HID,H)            mapping survey households (HID) and RIAPA households (H)
 SVY                     survey data parameters                          / HS, AE, HWT, PWT, EXP_T, EXP_F, PL_NAT, PL_USD /
*Expenditure groups
 G                       commodity groups
 GF(G)                   food commodity groups
 D                       reference diets
 DS(D)                   selected reference diet
 MCG(C,G)                mapping between model commodities (C) and group commodities (G)
 MGGF(G,G)               mapping commodity groups (G) to major food groups (GF)
*Reporting dimensions
 DIM                     reporting dimensions
 DIMS(DIM)               selected reporting dimensions
 NR                      reporting number or rate                        / NUM, RATE /
*Poverty indicators
 FGT                     FGT indicators                                  / P0, P1, P2 /
 POOR(HID)           household status
*Diet indicators
 GAP                     power of the dietary indicator parameter        / G0, G1, G2 /
 DR                      REDD index reporting elements                   / POP, M0, M1, M2, H, A, G, S /
 DEPS(HID)           household overall deprivation status (for selected reference diet)
 FDPOOR(HID)         is total household spending on food greater than total CORD excl. gdisc (1 = yes 0 = no)
;

ALIAS (G,GP);

PARAMETER
*Imported data
 DATA_H(HID,SVY)         household demographic information
 DATA_O(HID,G)           observed household consumption by group
 DATA_L(HID,G)           latent household consumption by group
 DATA_C(G,D)             cost of reference diets (estimated in latent.do)
*Reporting dimensions
 MHIDDIM(HID,DIM)        mapping survey households (HID) to reporting dimensions (DIM)
*Projected expenditures
 MODEXP(H,G)         changes in total representative household expenditures by commodity groups relative to base year
 HIDEXP(HID,G)       projected survey household per adult equivalent expenditures for all commodity group
 GRPEXP(HID,G)       projected survey household per adult equivalent expenditures for major food groups only
 TOTEXP(HID)         projected total survey household per adult equivalent expenditures
 AVGEXP(DIM)         average per adult equivalent total expenditure by reporting dimension (for Theil index)
*Projected populations
 MODPOP(H)           changes in representative household adult equivalent populations relative to base year
 HIDPOP(HID)         projected survey household population weights
 DIMPOP(DIM)         projected total population by reporting dimension
 HIDAE(HID)          projected survey adult equivalent population by reporting dimension
 DIMAE(DIM)          projected total adult equivalent population by reporting dimension
*Projected real prices
 GRPPRC(G)           projected real price indices for commodity groups
 TOTPRC             projected real price indices for all commodities
*Poverty analysis
 PTAB(NR,FGT,DIM)    poverty indicator table
 P0, P1, P2              selected poverty indicators
 PL                      selected poverty line
*Inequality analysis
 ITAB(DIM)           theil inquality index
*Diet analysis
 CORD(G,D)           cost of reference diets (CORD) by major food groups
 CTAB(NR,*)          cost of the select reference diet by food group
 DEPG(GAP,HID,G)     household deprivation gap by major food group (for selected reference diet)
 DEPT(GAP,HID)       household overall deprivation gap (for selected reference diet)
 DTAB(NR,DR,DIM)     diet indicator table
 GTAB(NR,GAP,DIM,G)  deprivation rates by food group
 TOTFDEXP(HID)       total food expendiuture by survey household (incl. discretionary items)
*Summary table
 SUMTAB(X,T,*,*,DIM)     summary poverty and diet deprivation table
;

$call "gdxxrw i=data\%country%\povdiet_%survey%.xlsx o=povdiet.gdx index=index!a5"
$gdxin povdiet.gdx
$load HID G D DIM

$loaddc GF MHIDH MGGF MCG MHIDDIM DATA_H DATA_O DATA_L DATA_C
$gdxin

*Arg 5 (Dimensions) : (1) National  (2) National, rural and urban  (3) All
  DIMS('NAT') = YES;
  DIMS('RUR') = YES;
  DIMS('URB') = YES;



*Arg 7 (Pov report) : (1) P0  (2) P0 and P1  (3) P0, P1 and P2
 P0 = 1;
 P1 = 0;
 P2 = 0;

*Arg 8 (Ref diet) : (1) Healthy  (2) Flexitarian  (3) Pescatarian  (4) Vegetarian
 DS('HEAL') = YES;
* DS('FLEX') = YES;
* DS('PESC') = YES;
* DS('VEGE') = YES;

*------------------------------------------------------------
* Prevelance of Undernourishment (PoU)
*------------------------------------------------------------

TABLE REFDIET(G,D)
         heal    flex    pesc    vege
gstap    811     1048    1048    1048
groot    39      128     128     128
gvege    78      96      107     114
gfrui    126     95      103     108
gdair    153     90      90      90
gprot    151     191     89      14
glegu    575     352     435     498
gafat    447     405     405     405
gdisc    120     95      95      95
;

PARAMETER
 MDER                    minimum dietary energy requirement              / 1670 /
 CSTCAL(G)               average cost per calorie in reference diet
 TOTCAL(HID)         total number of calcories by survey household
 HUNGRY(HID)         undernourishment status (1 = undernourished)
 HTAB(NR,FGT,DIM)    hunger indicator tab
;



PARAMETERS
QFS_c(F)
QF_c(F,A,RG)
WF_c(F)
WFDIST_c(F,A,RG)
FSAV_c
EXR_c
MPSADJ_c
DMPS_c
IADJ_c
INVSHR_c
GADJ_c
GOVSHR_c
pwm_c(C)
pwe_c(C)
alphava_c(A,RG)

MODEXP0(H,G)
TOTPRC0
GRPPRC0(G)
CORD0(GF,D)
 CTAB0(NR,*)
PRC_FD0(FDAC)
;

QFS_c(F) = QFS0(F);
QF_c(F,A,RG) = QF0(F,A,RG);
WF_c(F)= WF0(F);
WFDIST_c(F,A,RG) = WFDIST0(F,A,RG);
pwm_c(C)$pwm0(C) = pwm0(C);
pwe_c(C)$pwe0(C) = pwe0(C);
FSAV_c = FSAV0;
EXR_c= EXR0;
MPSADJ_c= MPSADJ.l;
DMPS_c=DMPS.l;
IADJ_c=IADJ.l;
INVSHR_c=INVSHR.l;
GADJ_c=GADJ.l;
GOVSHR_c=GOVSHR.l;
alphava_c(A,RG) = alphava0(A,RG);

* when moving poverty inside loop, MODEXP(t1) is not working
MODEXP0(H,G)$SUM(C, QH0(C,H)) =
SUM(C$MCG(C,G), PQ0(C)*QH0(C,H)) +
SUM((A,C)$(MAC(A,C) AND MCG(C,G)), PA0(A)*QHA0(A,H));

*Average price across all commodities
TOTPRC0
   = (SUM((C,H), PQ0(C)*QH0(C,H)*hpop0(h)) + SUM((A,C,H)$MAC(A,C), PA0(A)*QHA0(A,H)*hpop0(h)))
   / (SUM((C,H), QH0(C,H)*hpop0(h)) + SUM((A,C,H)$MAC(A,C), QHA0(A,H)*hpop0(h)));

GRPPRC0(G)$(SUM((C,H)$MCG(C,G), QH0(C,H)*hpop0(h)) + SUM((A,C,H), QHA0(A,H)*hpop0(h)))
  = (SUM((C,H)$MCG(C,G), PQ0(C)*QH0(C,H)*hpop0(h)) + SUM((A,C,H)$(MAC(A,C) AND MCG(C,G)), PA0(A)*QHA0(A,H)* hpop0(h)))
  / (SUM((C,H)$MCG(C,G), QH0(C,H)*hpop0(h)) + SUM((A,C,H)$(MAC(A,C) AND MCG(C,G)), QHA0(A,H)*hpop0(h)));

  CORD0(GF,D) = SUM(G$MGGF(G,GF), DATA_C(G,D) *1) ;
  CTAB0('NUM',GF) = SUM(DS, CORD0(GF,DS));
  CTAB0('NUM','TOTAL') = SUM(GF, CTAB0('NUM',GF));

  PRC_FD0('ALL')       = SUM(C, cwts(C) * PQ0(C));
  PRC_FD0('FOOD')      = SUM(C$R_CFOOD(C), cwts(C)/SUM(CP$R_CFOOD(CP), cwts(CP)) * PQ0(C));
  PRC_FD0('NON-FOOD')  = SUM(C$(NOT R_CFOOD(C)), cwts(C)/SUM(CP$(NOT R_CFOOD(CP)), cwts(CP)) * PQ0(C));


*New structural base table
   STRUCBASE2(RA,'VAshr')           = SUM((A,RG)$ACTAGG(A,RG,RA), PVA0(A,RG)*QVA0(A,RG)) / SUM((A,RG), PVA0(A,RG)*QVA0(A,RG)) * 100;
   STRUCBASE2(RA,'PRDshr')          = SUM((A,RG)$ACTAGG(A,RG,RA), PAR0(A,RG)*(1-tva0(A,RG))*QAR0(A,RG)) / SUM((A,RG), PAR0(A,RG)*(1-tva0(A,RG))*QAR0(A,RG)) * 100;
   STRUCBASE2(RA,'EMPshr')          = SUM((FLAB,A,RG)$ACTAGG(A,RG,RA), QF0(FLAB,A,RG)) / SUM((FLAB,A,RG), QF0(FLAB,A,RG)) * 100;
   STRUCBASE2(RA,'EXPshr')          = SUM(C$COMAGG(C,RA), pwe0(C)*EXR0*QE0(C)) / SUM(C, pwe0(C)*EXR0*QE0(C)) * 100;
   STRUCBASE2(RA,'IMPshr')          = SUM(C$COMAGG(C,RA), pwm0(C)*EXR0*QM0(C)) / SUM(C, pwm0(C)*EXR0*QM0(C)) * 100;
   STRUCBASE2(RA,'EXP-OUTshr')$SUM(C$COMAGG(C,RA), PX0(C)*QX0(C)) = SUM(C$COMAGG(C,RA), PE0(C)*EXR0*QE0(C)) / SUM(C$COMAGG(C,RA), PX0(C)*QX0(C)) * 100;
   STRUCBASE2(RA,'IMP-DEMshr')$SUM(C$COMAGG(C,RA), PQ0(C)*(1-tq0(C))*QQ0(C)) = SUM(C$COMAGG(C,RA), PM0(C)*EXR0*QM0(C)) / SUM(C$COMAGG(C,RA), PQ0(C)*(1-tq0(C))*QQ0(C)) * 100;
   STRUCBASE2('aa_GDP','VAshr')     = SUM(RA$(NOT RAG(RA)), STRUCBASE2(RA,'VAshr') );
   STRUCBASE2('aa_GDP','PRDshr')    = SUM(RA$(NOT RAG(RA)), STRUCBASE2(RA,'PRDshr'));
   STRUCBASE2('aa_GDP','EMPshr')    = SUM(RA$(NOT RAG(RA)), STRUCBASE2(RA,'EMPshr'));
   STRUCBASE2('aa_GDP','EXPshr')    = SUM(RA$(NOT RAG(RA)), STRUCBASE2(RA,'EXPshr'));
   STRUCBASE2('aa_GDP','IMPshr')    = SUM(RA$(NOT RAG(RA)), STRUCBASE2(RA,'IMPshr'));
   STRUCBASE2('aa_GDP','EXP-OUTshr')$SUM(C, PX0(C)*QX0(C)) = SUM(C, PE0(C)*EXR0*QE0(C)) / SUM(C, PX0(C)*QX0(C)) * 100;
   STRUCBASE2('aa_GDP','IMP-DEMshr')$SUM(C, PQ0(C)*(1-tq0(C))*QQ0(C)) = SUM(C, PM0(C)*EXR0*QM0(C)) / SUM(C, PQ0(C)*(1-tq0(C))*QQ0(C)) * 100;


* Askar
STRUCBASE2(RA,'VA')
 = SUM((A,RG)$ACTAGG(A,RG,RA), PVA0(A,RG)*QVA0(A,RG)) ;

STRUCBASE2(RA,'DEM')$SUM(C$COMAGG(C,RA), PQ0(C)*(1-tq0(C))*QQ0(C)) = SUM(C$COMAGG(C,RA), PQ0(C)*(1-tq0(C))*QQ0(C)) ;


STRUCBASE2(RA,'IMP')$SUM(C$COMAGG(C,RA), PQ0(C)*(1-tq0(C))*QQ0(C))  = SUM(C$COMAGG(C,RA), PM0(C)*EXR0*QM0(C)) ;


 STRUCBASE2(RA,'EXP')$SUM(C$COMAGG(C,RA), PX0(C)*QX0(C)) =  SUM(C$COMAGG(C,RA), PE0(C)*EXR0*QE0(C));


   STRUCBASE2(RA,'OUT')$SUM(C$COMAGG(C,RA), PX0(C)*QX0(C))        = SUM(C$COMAGG(C,RA), PX0(C)*QX0(C)) ;

***********
SETS
APOL(A)
ANPL(A)
HURB(H)
HURB_P(H)
HURB_M(H)
HURB_R(H)
HRUR(H)
HRUR_P(H)
HRUR_M(H)
HRUR_R(H)
;
alias(APOL,APOLP);

HURB('hhd-u1') = yes;
HURB('hhd-u2') = yes;
HURB('hhd-u3') = yes;
HURB('hhd-u4') = yes;
HURB('hhd-u5') = yes;

HURB_P('hhd-u1') = yes;
HURB_P('hhd-u2') = yes;
HURB_M('hhd-u3') = yes;
HURB_M('hhd-u4') = yes;
HURB_R('hhd-u5') = yes;

HRUR(H)$(not HURB(H)) = yes;
HRUR_P('hhd-f1') = yes;
HRUR_P('hhd-n1') = yes;
HRUR_P('hhd-f2') = yes;
HRUR_P('hhd-n2') = yes;

HRUR_M('hhd-f3') = yes;
HRUR_M('hhd-f4') = yes;
HRUR_M('hhd-n3') = yes;
HRUR_M('hhd-n4') = yes;

HRUR_R('hhd-f5') = yes;
HRUR_R('hhd-n5') = yes;

parameters
pdwt(C)                         weight of commodity c in PD index
pwewt(C)                        weight of commodity c in pwe index
pwmwt(C)                        weight of commodity c in pwm index
pwwt(*)                         weight of aggregate exports-imports in pw (tradables) index

GDP_sh(A) l-t GDP share
MACROTABX(X,T, *)             macro table
HHD_CONS_X(X,T, *)
HHD_CONS_XD(X,T, *)
HHD_INC_X(X,T, *)
HHD_INC_XD(X,T, *)
YI_X(X,T, INS)
YIF_X(X,T, INS, F)
WF_X(X,T,F)
alphava_prev(A,RG)
* alphava_tot_gr
alphava_tot_gr_target
alphava_tot_gr_anpl
alphava_gr_X(T, A,RG)
GDP_shX(T, A)
alphava_tot_grX(T)
QFSX(F,T)
;
pdwt(C)         = PDD0(C)*QD0(C) /SUM(CP, PDD0(CP)*QD0(CP));
pwewt(C)        = pwe0(C)*QE0(C) /SUM(CP, pwe0(CP)*QE0(CP));
pwmwt(C)        = pwm0(C)*QM0(C) /SUM(CP, pwm0(CP)*QM0(CP));
pwwt('EXP')     = (SUM(CP, pwe0(CP)*QE0(CP))) / (SUM(CP, pwe0(CP)*QE0(CP)) + SUM(CP, pwm0(CP)*QM0(CP)));
pwwt('IMP')     = (SUM(CP, pwm0(CP)*QM0(CP))) / (SUM(CP, pwe0(CP)*QE0(CP)) + SUM(CP, pwm0(CP)*QM0(CP)));

GDP_sh(A) =  SUM(RG, PVA0(A,RG)*QVA0(A,RG) ) /
SUM((AP, RG), PVA0(AP ,RG)*QVA0(AP,RG));

alphava_prev(A,RG) = alphava0(A,RG);
