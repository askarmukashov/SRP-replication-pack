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
*Country_Year
$setglobal country       "KEN_2019"

*--------------------------------------------------------------------------------------------
*1. SETS
*--------------------------------------------------------------------------------------------

SETS
*Model sets
 AC                      global set for model accounts - aggregated microsam accounts
 ACNT(AC)                all elements in AC except TOTAL
 AA(AC)                  aggregate activities
 ARG(AC)                 input in the form A*_RG* (activity*region)
 A(AC)                   activities
 RG(AC)                  internal regions
 ACES2(A)                activities with CES activity output aggregation function
 ACET2(A)                activities with CET activity output disaggregation function
 AFLEO(A)
 C(AC)                   commodities
 CD(C)                   commodities with domestic sales of output
 CDN(C)                  commodities without domestic sales of output
 CE(C)                   exported commodities
 CEN(C)                  non-export commodities
 CM(C)                   imported commodities
 CMN(C)                  non-imported commodities
 CX(C)                   commodities with output
 F(AC)                   factors
 FLAB(F)                 labor
 FLND(F)                 land
 FLIV(F)                 livestock
 FUTL(F,A,RG)            factors-sectors with endogenous utilization rates (based on WFDIST floor)
 FCAP(F)                 capital
 INS(AC)                 institutions
 INSD(INS)               domestic institutions
 INSDNG(INSD)            domestic non-government institutions
 H(INSDNG)               households
 EN(INSDNG)              enterprises
*Calibration sets
 CT(C)                   transaction service commodities
 CTD(AC)                 domestic transactions cost account
 CTE(AC)                 export transactions cost account
 CTM(AC)                 import transactions cost account
*Mappings
 MAC(A,C)                mapping between activities and commodities
 MARG(ARG,A,RG)          mapping between SAM regions
;

*--------------------------------------------------------------------------------------------
*2. DATABASE
*--------------------------------------------------------------------------------------------

PARAMETER
 SAM(AC,AC)              standard SAM
 SAMBALCHK(AC)           column minus row total for SAM
 FACNEST(F,F)            nested structure of factors in the model
 FACTREE(F,F)            direct and indirect factor mapping in nested factor structure
 QF2BASE(F,AC)           qnty of fac f employed by act a (extracted data)
 TNUM                    number of active years (used for reporting)
;

$include data\%country%\data_%country%.inc

*SAM adjustments ----------------------------------------------

*Adjustment for sectors with only exports and no domestic sales.
*If there is a very small value for domestic sales, add the discrepancy to exports.
 SAM(C,'ROW')$(ABS(SUM(ARG, SAM(ARG,C)) - (SAM(C,'ROW') - TAXPAR('EXPTAX',C) - SUM(CTE, SAM(CTE,C))) ) LT 1.E-6)
                 = SUM(ARG, SAM(ARG,C)) +  TAXPAR('EXPTAX',C) + SUM(CTE, SAM(CTE,C)) ;

*Netting transfers between domestic institutions and RoW.
 SAM(INSD,'ROW')   = SAM(INSD,'ROW') - SAM('ROW',INSD);
 SAM('ROW',INSD)   = 0;

*Netting transfers between factors and RoW.
 SAM('ROW',F)  = SAM('ROW',F) - SAM(F,'ROW');
 SAM(F,'ROW')  = 0;

*Netting transfers between government and domestic non-government institutions.
 SAM(INSDNG,'GOV') = SAM(INSDNG,'GOV') - SAM('GOV',INSDNG);
 SAM('GOV',INSDNG) = 0;

*Eliminating payments of any account to itself.
 SAM(ACNT,ACNT) = 0;

*Remove home consumption
* SAM(C,H) = SAM(C,H) + SUM(ARG, SAM(ARG,C)/SUM(CP, SAM(ARG,CP))*SAM(ARG,H));
* SAM(ARG,H) = 0;
* SAM(ARG,C)$SUM(CP, SAM(ARG,CP)) = SAM(ARG,C)/SUM(CP, SAM(ARG,CP)) * (SUM(ACNT, SAM(ACNT,ARG))-SUM(HP, SAM(ARG,HP)));

*Checking SAM balance -----------------------------------------

*Account totals are recomputed. Check for SAM balance.
 SAM('TOTAL',ACNT) = SUM(ACNTP, SAM(ACNTP,ACNT));
 SAM(ACNT,'TOTAL') = SUM(ACNTP, SAM(ACNT,ACNTP));
 SAMBALCHK(AC)   = SAM('TOTAL',AC) - SAM(AC,'TOTAL');
 SAMBALCHK(AC)$(abs( SAMBALCHK(AC)) lt 1e-6) = 0;

 DISPLAY "SAM after standard adjustments", SAMBALCHK;
 DISPLAY "SAM after standard adjustments", SAM;

*Remove stock changes that are effectively zero
 SAM(C,'DSTK')$(ABS( SAM(C,'DSTK')) LT 1E-8) = 0;

*Additional set definitions based on country SAM --------------

 CD(C)  = YES$(SUM(ARG, SAM(ARG,C)) GT (SAM(C,'ROW') - TAXPAR('EXPTAX',C) - SUM(CTE, SAM(CTE,C))) );

 CDN(C) = NOT CD(C);

 CE(C)  = YES$(SAM(C,'ROW'));
 CEN(C) = NOT CE(C);

 CM(C)  = YES$(SAM('ROW',C));
 CMN(C) = NOT CM(C);

 CX(C) = YES$SUM(ARG, SAM(ARG,C));

 CT(C)$(SUM(CTD, SAM(C,CTD)) + SUM(CTE, SAM(C,CTE)) + SUM(CTM, SAM(C,CTM))) = YES;

*Sector-factor combinations with underutilized capital
 FUTL(F,A,RG) = NO;

*Fine-tuning non-SAM data -------------------------------------

*Eliminating superfluous elasticity data

* TRADELAS(C,'SIGMAT')$(CEN(C) OR (CE(C) AND CDN(C))) = 0;
 TRADELAS(C,'SIGMAQ')$(CMN(C) OR (CM(C) AND CDN(C))) = 0;

 PRODELAS(ARG)$(NOT SAM('TOTAL',ARG))     = 0;

 ELASAC(C)$(NOT SUM(ARG, SAM(ARG,C)))     = 0;

 LESELAS1(C,H)$(NOT SAM(C,H))         = 0;
 LESELAS2(A,H)$(NOT SUM((ARG,RG)$MARG(ARG,A,RG), SAM(ARG,H)))  = 0;


*Physical factor quantities -----------------------------------

*If there is a SAM payment from A to F and supply (but not
*demand) quantities have been defined in the country data file,
*then the supply values are used to compute demand quantities.
 QF2BASE(F,ARG)$(SAM(F,ARG)$((NOT QFBASE(F,ARG)) AND QFSBASE(F))) = QFSBASE(F)*SAM(F,ARG)/SUM(ARGP, SAM(F,ARGP));

*If there is a SAM payment from A to F and neither supply nor
*demand quantities have been defined in the country data file,
*then SAM values are used as quantities
 QF2BASE(F,ARG)$(SAM(F,ARG)$((QFBASE(F,ARG) EQ 0) AND (QFSBASE(F) EQ 0))) = SAM(F,ARG);

*If there is a SAM payment from A to F and demand quantities have
*been defined in the country data file, then this information is used.
 QF2BASE(F,ARG)$QFBASE(F,ARG) = QFBASE(F,ARG);

DISPLAY QF2BASE, QFBASE, QFSBASE;


*--------------------------------------------------------------------------------------------
*3. PARAMETER DECLARATIONS
*--------------------------------------------------------------------------------------------

$ontext
This section is divided into the following subsections:
a. Parameters appearing in model equations
b. Parameters used for model calibration (to initialize variables and
   to define model parameters)

In each group, the parameters are declared in alphabetical order.
$offtext

PARAMETERS
*a. Parameters appearing in model equations
*Parameters other than tax rates
 alphaac(C)              shift parameter for domestic commodity aggregation fn
 alphaq(C)               shift parameter for Armington function
 alphat(C)               shift parameter for CET function
 alphava(A,RG)           shift parameter for CES activity production function
 alphaa2(A)              shift parameter on ces activity output aggregation function
 alphaca(A)              shift parameter on ces activity output disaggregation function
 beta1                   capital mobility parameter by type                      / 2.00 /
 beta2                   capital mobility by sector                              / 2.00 /
 betah(A,H)              marg shr of hhd cons on home com c from act a
 betam(C,H)              marg share of hhd cons on marketed commodity c
 cwts(C)                 consumer price index weights
 cwtsh(AC,H)             consumer price index weight for com'y c or act a for hhd h
 deltaac(A,C)            share parameter for domestic commodity aggregation fn
 deltaq(C)               share parameter for Armington function
 deltat(C)               share parameter for CET function
 deltava(F,A,RG)         share parameter for CES activity production function
 deltaa2(A,RG)           share parameters on the activity output aggregation function
 deltaca(A,C)            share parameter for CET output disaggregation
 dwts(C)                 domestic sales price weights
 fprd(F,A,RG)            factor-specific productivity
 gammah(A,H)             per-cap subsist cons for hhd h on home com c fr act a
 gammam(C,H)             per-cap subsist cons of marketed com c for hhd h
 ica(C,A,RG)             intermediate input c per unit of aggregate intermediate
 inta(A,RG)              aggregate intermediate input coefficient
 iva(A,RG)               aggregate value added coefficient
 icd(C,CP)               trade input of c per unit of comm'y cp produced & sold dom'ly
 ice(C,CP)               trade input of c per unit of comm'y cp exported
 icm(C,CP)               trade input of c per unit of comm'y cp imported
 iwts(C)                 investment commodity demand weight
 ifa(F,A,RG)             fixed factor shares for leontief factor demand
 minrtn(A,RG)            minimum factor returns before underutilization (0-1 ratio of base year WFDIST)
 mps01(INS)              0-1 par for potential flexing of savings rates
 mpsbar(INS)             marg prop to save for dom non-gov inst ins (exog part)
 pwe(C)                  world price of exports
 pwm(C)                  world price of imports
 qdst(C)                 inventory investment by sector of origin
 qbarg(C)                exogenous (unscaled) government demand
 qbarinv(C)              exogenous (unscaled) investment demand
 rhoac(C)                domestic commodity aggregation function exponent
 rhoq(C)                 Armington function exponent
 rhot(C)                 CET function exponent
 rhova(A,RG)             CES activity production function exponent
 rhoa2(A)                CES activity output aggregation function exponent
 rhoca(A)                CET activity disaggregation function exponent
 rf(F)                   factor foreign remittances
 elasva(A,RG)            CES elasticity of substitution in production
 shif(INS,F)             share of dom. inst'on i in income of factor f
 shii(INS,INSP)          share of inst'on i in post-tax post-sav income of inst ip
 supernum(H)             LES supernumerary income
 theta(A,C)              yield of commodity C per unit of activity A
 tins01(INS)             0-1 par for potential flexing of dir tax rates
 trnsfr(INS,AC)          transfers fr. inst. or factor ac to institution ins
 elasuss(F)              elasticity of supply for factors with upward-sloping supply curves
*Tax rates
 ta(A,RG)                rate of tax on producer gross output value
 te(C)                   rate of tax on exports
 tf(F)                   rate of direct tax on factors (soc sec tax)
 tinsbar(INS)            rate of (exog part of) direct tax on dom inst ins
 tm(C)                   rate of import tariff
 tq(C)                   rate of sales tax
 tva(A,RG)               rate of value-added tax
*b. Parameters used for model calibration
*Parameters for definition of model parameters
 alphainv                investment shift parameter
 alphava0(A,RG)          shift parameter for CES activity production function
 betah0(A,H)             marg shr of hhd cons on home com c from act a
 betam0(C,H)             marg share of hhd cons on marketed commodity c
 qdst0(C)                stock change
 qbarg0(C)               exogenous (unscaled) government demand
 gammah0(A,H)            per-cap subsist cons for hhd h on home com c fr act a
 gammam0(C,H)            per-cap subsist cons of marketed com c for hhd h
 pwe0(C)                 world price of exports
 pwm0(C)                 world price of imports
 ta0(A,RG)               rate of tax on producer gross output value
 te0(C)                  rate of tax on exports
 tf0(F)                  rate of direct tax on factors -- soc sec tax
 tins0(INS)              rate of direct tax on domestic institutions ins
 tm0(C)                  rate of import tariff
 tq0(C)                  rate of sales tax
 trnsfr0(INS,AC)         transfers fr. inst. or factor ac to institution ins
 tva0(A,RG)              rate of value-added tax
 icd0(C,CP)              trade input of c per unit of comm'y cp produced & sold dom'ly
 ice0(C,CP)              trade input of c per unit of comm'y cp exported
 icm0(C,CP)              trade input of c per unit of comm'y cp imported
*Check parameters
 cwtschk                 check that CPI weights sum to unity
 cwtshchk(h)             check that CPI weights sum to unity for hhd h
 dwtschk                 check that PDIND weights sum to unity
 shifchk(F)              check that factor payment shares sum to unity
*Parameters for variable initialization
 CPI0                    consumer price index -- PQ-based
 DPI0                    index for domestic producer prices --PDS-based
 DMPS0                   change in marginal propensity to save for selected inst
 DTINS0                  change in domestic institution tax share
 EG0                     total current government expenditure
 EH0(H)                  household consumption expenditure
 EXR0                    exchange rate
 FSAV0                   foreign savings
 GADJ0                   government demand scaling factor
 GOVSHR0                 govt consumption share of absorption
 GSAV0                   government savings
 IADJ0                   investment scaling factor for fixed capital formation
 INVSHR0                 investment share of absorption
 MPS0(INS)               marginal propensity to save for dom non-gov inst ins
 MPSADJ0                 savings rate scaling factor
 PA0(A)                  output price of aggregate national a
 PAR0(A,RG)              output price of region specific activity
 PDD0(C)                 demand price for com'y c produced & sold domestically
 PDS0(C)                 supply price for com'y c produced & sold domestically
 PE0(C)                  price of exports
 PINTA0(A,RG)            price of intermediate aggregate
 PM0(C)                  price of imports
 PQ0(C)                  price of composite good c
 PVA0(A,RG)              value added price
 PX0(C)                  average output price
 PXAC0(A,C)              price of commodity c from activity a
 QA0(A)                  level of domestic activity nationally
 QAR0(A,RG)              level of domestic activity regionally
 QANET0(A)               QA net of home consumption
 QD0(C)                  quantity of domestic sales
 QE0(C)                  quantity of exports
 QF0(F,A,RG)             quantity demanded of factor f from activity a
 QFS0(F)                 quantity of factor supply
 QG0(C)                  quantity of government consumption
 QH0(C,H)                quantity consumed of marketed commodity c by hhd h
 QHA0(A,H)               quantity consumed of home commodity c fr act a by hhd h
 QINT0(C,A)              quantity of intermediate demand for c from activity a
 QINTA0(A,RG)            quantity of aggregate intermediate input
 QINV0(C)                quantity of fixed investment demand
 QM0(C)                  quantity of imports
 QQ0(C)                  quantity of composite goods supply
 QT0(C)                  quantity of trade and transport demand for commodity c
 QVA0(A,RG)              quantity of aggregate value added
 QX0(C)                  quantity of aggregate marketed commodity output
 QXAC0(A,C)              quantity of ouput of commodity c from activity a
 TABS0                   total absorption
 TINS0(INS)              rate of direct tax on domestic institutions ins
 TINSADJ0                direct tax scaling factor
 TRII0(INS,INSP)         transfers to dom. inst. insdng from insdngp
 WALRAS0                 savings-investment imbalance (should be zero)
 WF0(F)                  economy-wide wage (rent) for factor f
 WF00(F)                 economy-wide wage (rent) for factor f
 WFDIST0(F,A,RG)         factor wage distortion variable
 YF0(F)                  factor income
 YG0                     total current government income
 YIF0(INS,F)             income of institution ins from factor f
 YI0(INS)                income of (domestic non-governmental) institution ins
*Capital stock updating parameters (only used in the simulation file)
 CAPSHR1(F)              shares of aggregate capital by type (sums to one)
 CAPSHR2(F,A,RG)         sectoral shares of capital by type (rows sum to one)
 CAPSHR1TOT              used to speed up capital accumulation calculations
 CAPSHR2TOT(F)           used to speed up capital accumulation calculations
 DKAP(F,A,RG)            change in sectoral real capital stock
 DKAPS(F)                change in aggregate real capital stock
 INVSHR1(F)              investment shares by type of capital
 INVSHR2(F,A,RG)         investment shares by sector for each capital type
 NGFCF                   GFCF net of exogenous capital adjustments in fixed sectors
 QINVK                   quantity of new capital stock
 RKAP(F,A,RG)            annual rate of growth of sectoral capital stock by type
 RKAPS(F)                annual rate of growth of aggregate capital stock by type
 WFADJ(F)                WF adjusted to exclude fixed sectors
 WFK1AV                  average rental on all capital (economywide)
 WFK2AV(F)               average rental on capital by type (across all activities)
 WFDIST2(F,A,RG)         ratio of sectoral to average rental by capital type
 WFDISTADJ(F,A,RG)       WFDIST adjusted to exclude fixed sectors
*Calibration parameters
 PSUP(C)                 initial supply-side market price for commodity c
 SHCTD(C)                share of comm'y ct in trans services for domestic sales
 SHCTM(C)                share of comm'y ct in trans services for imports
 SHCTE(C)                share of comm'y ct in trans services for exports
 WFA(F,A,RG)             wage for factor f in activity a (used for calibration)
 predeltaa(A)            dummy used to define deltaa
 predelta(C)             dummy used to define deltaq
 BUDSHR(C,H)             budget share for marketed commodity c and household h
 BUDSHR2(A,H)            budget share for home commodity c - act a - hhd h
 BUDSHRCHK(H)            check that budget shares some to unity
 ELASCHK(H)              check that expenditure elasticities satisfy Engel aggr
 SUBSIST(H)              subsistence spending
 FRISCH2(H)              alt. defn of Frisch -- ratio of cons to supernumerary cons
 LESCHK(H)               check on LES parameter definitions (error mssg if error)
 PRODSHR(A,RG)
 PSUPA(AA)
;

*Price block --------------------------------------------------

IF(AGRIPROD EQ 0,
 PSUP(C)             = 1;
 PE0(C)$CE(C)        = PSUP(C);
 PX0(C)$CX(C)        = PSUP(C);
 PDS0(C)$CD(C)       = PSUP(C);
 PXAC0(A,C)$SUM((ARG,RG)$MARG(ARG,A,RG),SAM(ARG,C)) = PSUP(C);
 PA0(A)              = 1;
 PAR0(A,RG)          = 1;
 EXR0                = 1;
ELSE
 PA0(A)              = 1;
 PAR0(A,RG)          = 1;
 PAR0(A,RG)$SUM(ARG$MARG(ARG,A,RG), PRODUCTION(ARG))
         = SUM(ARG$MARG(ARG,A,RG), SAM('TOTAL',ARG)) / SUM(ARG$MARG(ARG,A,RG), PRODUCTION(ARG));
 PRODSHR(A,RG)$SUM((RGP,ARG)$MARG(ARG,A,RGP), PRODUCTION(ARG))
         = SUM(ARG$MARG(ARG,A,RG), PRODUCTION(ARG)) /SUM((RGP,ARG)$MARG(ARG,A,RGP), PRODUCTION(ARG));
 PA0(A)$SUM(RG, PRODSHR(A,RG)*PAR0(A,RG))  = SUM(RG, PRODSHR(A,RG)*PAR0(A,RG));
 PSUP(C) = SUM(A$MAC(A,C), PA0(A));
 PE0(C)$CE(C)        = PSUP(C);
 PX0(C)$CX(C)        = PSUP(C);
 PDS0(C)$CD(C)       = PSUP(C);
 PXAC0(A,C)$SUM((RG,ARG)$MARG(ARG,A,RG), SAM(ARG,C)) = PSUP(C);
 EXR0                = 1;
);

*Activity quantity = payment to activity divided by activity price
*QA covers both on-farm consumption and marketed output output GROSS of tax
 QAR0(A,RG)        =  SUM(ARG$MARG(ARG,A,RG), SAM('TOTAL',ARG))/PAR0(A,RG) ;
 QA0(A)            =  SUM(RG, QAR0(A,RG));

*Unit value-added price = total value-added / activity quantity define pva gross of tax
 QVA0(A,RG)              = SUM(ARG$MARG(ARG,A,RG), SUM(F, SAM(F,ARG))+ TAXPAR('VATAX',ARG)) ;
 PVA0(A,RG)$QVA0(A,RG)   = SUM(ARG$MARG(ARG,A,RG), SUM(F, SAM(F,ARG))+ TAXPAR('VATAX',ARG))/QVA0(A,RG);
 iva(A,RG)$QAR0(A,RG)    = QVA0(A,RG)/QAR0(A,RG) ;
 QXAC0(A,C)$PXAC0(A,C)   =  SUM((ARG,RG)$MARG(ARG,A,RG),SAM(ARG,C)) / PXAC0(A,C);

*QHA0(A,H)$SHRHOME(A,H)   = SHRHOME(A,H)*SAM(A,H)/PXAC0(A,C);
*pc QHA0(A,H)               = SUM((ARG,RG)$MARG(ARG,A,RG),SAM(ARG,H))/PA0(A);
 QHA0(A,H)               = SUM((ARG,RG)$MARG(ARG,A,RG),SAM(ARG,H))/PA0(A)/hpop(H);
*pc QANET0(A)               = QA0(A) - SUM(H, QHA0(A,H)) ;
 QANET0(A)               = QA0(A) - SUM(H, QHA0(A,H)*hpop(H)) ;

*Output quantity = value received by producers divided by producer price
*QX covers only marketed output
 QX0(C)$SUM(ARG, SAM(ARG,C)) = SUM(ARG, SAM(ARG,C)) / PX0(C);

*Export quantity = export revenue received by producers
*(ie. minus tax and transactions cost) divided by export price.
 QE0(C)$SAM(C,'ROW') = (SAM(C,'ROW') - TAXPAR('EXPTAX',C) - SUM(CTE, SAM(CTE,C)))/PE0(C);

*RoW export price = RoW export payment (in for curr) / export qnty
 pwe0(C)$QE0(C) = (SAM(C,'ROW')/EXR0) / QE0(C);
 pwe(C) = pwe0(C);

 te0(C)$SAM(C,'ROW') = TAXPAR('EXPTAX',C)/SAM(C,'ROW');
 te(C) =  te0(C);

*Quantity of output sold domestically = output quantity less quantity
*exported = value of domestic sales divided by domestic supply price
*QD0 covers only marketed output
 QD0(C)$CD(C) =  QX0(C) - QE0(C);

*Domestic demander price = demander payment divided by quantity bought
 PDD0(C)$QD0(C)= (PDS0(C)*QD0(C) + SUM(CTD, SAM(CTD,C)))/QD0(C);

*Define import price to equal domestic price so that import and domestic
*units are the same to the purchaser. If no domestic good, set PM to 1.
 PM0(C) = PDD0(C) ;
 PM0(C)$(QD0(C) EQ 0) = 1 ;

*Import quantity = demander payment for imports (including tariffs
*and marketing cost) divided by demander price.
 QM0(C)$CM(C) = (SAM('ROW',C) + TAXPAR('IMPTAX',C) + SUM(CTM, SAM(CTM,C)))/PM0(C);

*World price = import value (in foreign currency / import quantity
 pwm0(C)$QM0(C)= (SAM('ROW',C)/EXR0) / QM0(C);
 pwm(C) = pwm0(C);
 tm0(C)$SAM('ROW',C) = TAXPAR('IMPTAX',C) / SAM('ROW',C);
 tm(C) = tm0(C);

*Composite supply is the sum of domestic market sales and imports
*(since they are initialized at the same price).
 QQ0(C)$(CD(C) OR CM(C)) = QD0(C) + QM0(C) ;
 PQ0(C)$QQ0(C) = (SAM(C,'TOTAL') - SAM(C,'ROW'))/QQ0(C);
 tq0(C)$QQ0(C) = TAXPAR('COMTAX',C)/(PQ0(C)*QQ0(C)) ;
 tq(C) = TQ0(C) ;

 SHCTD(CT)$SUM(CTD, SAM('TOTAL',CTD)) = SUM(CTD, SAM(CT,CTD)/SAM('TOTAL',CTD)) ;
 SHCTM(CT)$SUM(CTM, SAM('TOTAL',CTM)) = SUM(CTM, SAM(CT,CTM)/SAM('TOTAL',CTM)) ;
 SHCTE(CT)$SUM(CTE, SAM('TOTAL',CTE)) = SUM(CTE, SAM(CT,CTE)/SAM('TOTAL',CTE)) ;

*Transactions input coefficients
 icd(CT,C)$QD0(C) = (shctd(ct)*SUM(CTD, SAM(CTD,C))/PQ0(ct)) / QD0(C);
 icm(CT,C)$QM0(C) = (shctm(ct)*SUM(CTM, SAM(CTM,C))/PQ0(ct)) / QM0(C);
 ice(CT,C)$QE0(C) = (shcte(ct)*SUM(CTE, SAM(CTE,C))/PQ0(ct)) / QE0(C);
 icd0(C,CP) = icd(C,CP);
 ice0(C,CP) = ice(C,CP);
 icm0(C,CP) = icm(C,CP);

*Indirect activity tax rate = tax payment / output value
*Tax is here applied to total output value (incl. on-farm cons.)
 tva0(A,RG)$QVA0(A,RG) = SUM(ARG$MARG(ARG,A,RG),TAXPAR('VATAX',ARG))/(PVA0(A,RG)*QVA0(A,RG));
 tva(A,RG) = tva0(A,RG);

*QA is GROSS of tax, so base for ta is as well
 ta0(A,RG)$QAR0(A,RG) = SUM(ARG$MARG(ARG,A,RG), TAXPAR('ACTTAX',ARG)/(SAM(ARG,'TOTAL')));
 ta(A,RG) = ta0(A,RG);

*Yield coefficient = quantity produced and delivered to market.
*Home consumption is assumed to come from activities
 theta(A,C)$PXAC0(A,C) =  QXAC0(A,C)/(QA0(A)- SUM(H,QHA0(A,H)*hpop(H))) ;

*Intermediate input coefficient = input use / output quantity
 QINTA0(A,RG) = SUM(C$PQ0(C), SUM(ARG$MARG(ARG,A,RG), SAM(C,ARG))  / PQ0(C)) ;

 ica(C,A,RG)$(QINTA0(A,RG) AND PQ0(C)) = SUM(ARG$MARG(ARG,A,RG), SAM(C,ARG))/PQ0(C) / QINTA0(A,RG) ;

 inta(A,RG)$QAR0(A,RG) = QINTA0(A,RG) / QAR0(A,RG) ;
 pinta0(A,RG)      = SUM(C, ica(C,A,RG)*PQ0(C)) ;

*CPI weight by comm'y = hhd cons value for comm'y / total hhd cons value
*CPI does not consider on-farm consumption.
 cwts(C) = SUM(H, SAM(C,H)) / SUM((CP,H), SAM(CP,H));

*CPI weights by household
* cwtsh(C,H)    = SAM(C,H)/(SUM(CP, SAM(CP,H)) + SUM(A, QHA0(A,H)*PA0(A)));
* cwtsh(A,H)    = QHA0(A,H)*PA0(A)/(SUM(CP, SAM(CP,H)) + SUM(AP, QHA0(AP,H)*PA0(ap)));
 cwtsh(C,H)    = SAM(C,H)/(SUM(CP, SAM(CP,H)) + SUM(A, QHA0(A,H)*PA0(A)*hpop(H)));
 cwtsh(A,H)    = QHA0(A,H)*PA0(A)/(SUM(CP, SAM(CP,H)) + SUM(AP, QHA0(AP,H)*PA0(AP)*hpop(H)));
 cwtshchk(h)   = SUM(acnt, cwtsh(acnt,h)) - 1 ;

*Domestic sales price index weight = dom sales value for comm'y
*/ total domestic salues value
*Domestic sales price index does not consider on-farm consumption.
 dwts(C)       = (SUM(ARG, SAM(ARG,C)) - (SAM(C,'ROW') - SUM(cte, SAM(cte,C))))/
                  SUM(CP, SUM(ARG, SAM(ARG,CP)) - (SAM(CP,'ROW') - SUM(cte, SAM(cte,CP))));

 CWTSCHK       = SUM(C, cwts(C)) - 1;
 DWTSCHK       = SUM(C, dwts(C)) - 1;

 CPI0          = SUM(C, cwts(C)*PQ0(C)) ;
* DPI0          = SUM(CD, dwts(CD)*PDS0(CD)) ;
 DPI0          = SUM(CD$(NOT CER(CD)), dwts(CD)*PDS0(CD)) ;

DISPLAY
 CWTSCHK, CWTSHCHK, DWTSCHK ;

*Production and trade block -----------------------------------

*Compute exponents from elasticites
 rhoq(C)$(CM(C) AND CD(C)) = (1/TRADELAS(C,'SIGMAQ')) - 1;
 rhot(C)$(CE(C) AND CD(C)) = (1/TRADELAS(C,'SIGMAT')) + 1;
 rhova(A,RG)$SUM(ARG$MARG(ARG,A,RG),PRODELAS(ARG)) = SUM(ARG$MARG(ARG,A,RG),(1/PRODELAS(ARG)) - 1);

*Add elasticity of substitution in the CES for the cost function
 elasva(A,RG)  = 1/(1 + rhova(A,RG)) ;

*Aggregation of domestic output from different activities

 RHOAC(C)$ELASAC(C) = 1/ELASAC(C) - 1;

 deltaac(A,C)$ (QXAC0(A,C)$ELASAC(C)) = (PXAC0(A,C)*QXAC0(A,C)**(1/ELASAC(C)))/SUM(AP, PXAC0(AP,C)*QXAC0(AP,C)**(1/ELASAC(C)));

 alphaac(C)$SUM(A,deltaac(A,C)) = QX0(C)/(SUM(A$deltaac(A,C), deltaac(A,C) * QXAC0(A,C)**(-RHOAC(C))) )**(-1/RHOAC(C));

*CET disaggregation function (multiple outputs from same activity)

 rhoca(A)$ELASCA(A)  = (1/ELASCA(A)) + 1 ;

 deltaca(A,C)$QXAC0(A,C) = (PXAC0(A,C)*(QXAC0(A,C))**(1-rhoca(A)))/SUM(CP$QXAC0(A,CP), PXAC0(A,CP)*(QXAC0(A,CP))**(1-rhoca(A))) ;

*pc alphaca(A)$(NOT ACHK(A)) = (QA0(A) - SUM(H, QHA0(A,H)))/(SUM(C$deltaca(A,C),deltaca(A,C)*QXAC0(A,C)**rhoca(A)))**(1/rhoca(A));
 alphaca(A)$(NOT ACHK(A)) = (QA0(A) - SUM(H, QHA0(A,H)*hpop(H)))/(SUM(C$deltaca(A,C),deltaca(A,C)*QXAC0(A,C)**rhoca(A)))**(1/rhoca(A));

*Factor supply elasticities (initially equal to zero)

 elasuss(F) = 0;

*Demand computations ----

*Defining factor employment and supply.
 QF0(F,A,RG)  = SUM(ARG$MARG(ARG,A,RG),QF2BASE(F,ARG));
*Defining employment for aggregate factors in factor nesting
 QFS0(F)      = SUM((A,RG), QF0(F,A,RG));

*Activity-specific wage is activity labor payment over employment
 WFA(F,A,RG)$QF0(F,A,RG) = SUM(ARG$MARG(ARG,A,RG),SAM(F,ARG))/QF0(F,A,RG);

*Economy-wide wage average is total factor income over employment
 WF0(F)$SUM((A,RG), QF0(F,A,RG))   = SUM(ARG, SAM(F,ARG))/SUM((A,RG), QF0(F,A,RG));
 WF00(F) = WF0(F);

DISPLAY
"If the value of WF0 for any factor is very different from one (< 0.1"
"or >10) the user may consider rescaling the initial values for QFBASE"
"or QFSBASE for this factor to get a value of WF0 such that"
"0.1 < WF0 < 10"
 WF0
 ;

*Wage distortion factor
 wfdist0(F,A,RG)$WF0(F) = WFA(F,A,RG)/WF0(F);

*Floor on factor returns before underutilization
 minrtn(A,RG) = 0.9;

*Fixed factor shares for leotief factor aggregation functions
 ifa(F,A,RG)$QVA0(A,RG) = QF0(F,A,RG) / QVA0(A,RG);

*CES activity production function
 deltava(F,A,RG)$QF0(F,A,RG) = (wfdist0(F,A,RG) * WF0(F) * (QF0(F,A,RG))**(1+rhova(A,RG)) )
              / SUM(FP$QF0(FP,A,RG), wfdist0(FP,A,RG) * WF0(FP)*(QF0(FP,A,RG))**(1+rhova(A,RG)));

 alphava0(A,RG)$QVA0(A,RG) = QVA0(A,RG)/( SUM(F$(QF0(F,A,RG)), deltava(F,A,RG)*QF0(F,A,RG)
               **(-rhova(A,RG))) )**(-1/rhova(A,RG));

 alphava(A,RG) = alphava0(A,RG);

 fprd(F,A,RG) = 1;

 rhoa2(A)$ELASCES(A) = (1/ELASCES(A))-1;

 deltaa2(A,RG)$QAR0(A,RG) = (PAR0(A,RG)*(QAR0(A,RG))**(1+rhoa2(A)))/SUM(RGP, PAR0(A,RGP)*(QAR0(A,RGP))**(1+rhoa2(A)));

 alphaa2(A)$(NOT ACHK(A)) = QA0(A)/(SUM(RG$QAR0(A,RG), deltaa2(A,RG)*QAR0(A,RG)**(-rhoa2(A))))**(-1/rhoa2(A));

 rhoca(A)  = (1/elasca(A)) + 1 ;

 deltaca(A,C)$QXAC0(A,C) = (PXAC0(A,C)*(QXAC0(A,C))**(1-rhoca(A)))/SUM(CP$QXAC0(A,CP), PXAC0(A,CP)*(QXAC0(A,CP))**(1-rhoca(A))) ;

*Intermediate demand
 QINT0(C,A)$PQ0(C) = SUM((ARG,RG)$MARG(ARG,A,RG),SAM(C,ARG)) / PQ0(C);

*Transactions demand
 QT0(CT) = ( SUM(CTD, SAM(CT,CTD)) + SUM(CTE, SAM(CT,CTE)) + SUM(CTM, SAM(CT,CTM)) ) / PQ0(CT) ;

*CET transformation
 deltat(C)$(CE(C) AND CD(C)) = 1 / (1 + PDS0(C)/PE0(C)*(QE0(C)/QD0(C))**(rhot(C)-1));

 alphat(C)$(CE(C) AND CD(C)) = QX0(C) / (deltat(C)*QE0(C)**rhot(C) + (1-deltat(C)) *QD0(C)**rhot(C))**(1/rhot(C));

*Armington aggregation

 predelta(C)$(CM(C) AND CD(C)) = (PM0(C)/(PDD0(C)))*(QM0(C)/QD0(C))**(1+rhoq(C)) ;

 deltaq(C)$(CM(C) AND CD(C)) = predelta(C)/(1 + predelta(C)) ;

 alphaq(C)$(CM(C) AND CD(C)) = QQ0(C)/(deltaq(C)*QM0(C)**(-rhoq(C))+(1-deltaq(C))*QD0(C)**(-rhoq(C)))**(-1/rhoq(C)) ;

*Institution block --------------------------------------------

*Institutional income
 YI0(INSDNG) = SAM('TOTAL',INSDNG);

*Factor income by factor category
 YF0(F)      = SUM(ARG, SAM(F,ARG));

*Institution income from factors
 YIF0(INSD,F) = SAM(INSD,F);

*Transfers to RoW from factors
 trnsfr('ROW',F)    = SAM('ROW',F)/EXR0;

*Transfers that are fixed shares of factor incomes
 rf(f) = 0;
 trnsfr('ROW','FCAP-M') = 0;
 rf('FCAP-M') = SAM('ROW','FCAP-M') / (SAM('TOTAL','FCAP-M') - TAXPAR('FACTAX','FCAP-M'));

*Transfers from RoW to institutions
 trnsfr(INSD,'ROW') = SAM(INSD,'ROW')/EXR0;

*Government transfers
 trnsfr(INSD,'GOV') = SAM(INSD,'GOV')/CPI0;

*Factor taxes
 tf0(F)$SAM('TOTAL',F) = TAXPAR('FACTAX',F)/SAM('TOTAL',F);
 tf(F)                 = tf0(F);

*Shares of domestic institutions in factor income (net of factor taxes and transfers to RoW).
 shif(INSD,F)$SAM(F,'TOTAL')  = SAM(INSD,F)/(SAM(F,'TOTAL') - TAXPAR('FACTAX',F) - SAM('ROW',F));

 SHIFCHK(F)    = SUM(INSD, shif(INSD,F)) - 1 ;

*Inter-institution transfers
 TRII0(INSDNG,INSDNGP) = SAM(INSDNG,INSDNGP);

*Share of dom non-gov institution in income of other dom non-gov institutions (net of direct taxes and savings).
 shii(INSDNG,INSDNGP) = SAM(INSDNG,INSDNGP)
   /(SAM('TOTAL',INSDNGP) - TAXPAR('INSTAX',INSDNGP) - SAM('S-I',INSDNGP));

*Scaling factors for savings and direct tax shares
 MPSADJ0      = 0;
 TINSADJ0     = 0;

*Savings rates
 MPS0(INSDNG)   = SAM('S-I',INSDNG)/(SAM('TOTAL',INSDNG) - TAXPAR('INSTAX',INSDNG));
 mpsbar(INSDNG) = MPS0(INSDNG);

*Direct tax rates
 TINS0(INSDNG)   = TAXPAR('INSTAX',INSDNG) / SAM('TOTAL',INSDNG);
 tinsbar(INSDNG) = TINS0(INSDNG);

*"Point" change in savings and direct tax shares
 DMPS0  = 0;
 DTINS0 = 0;

*Selecting institutions for potential "point" change in savings and tax rates

*If DMPS or MPSADJ is flexible, institutions with a value of 1 for mps01
*change their savings rates.
 mps01(INSDNG)  = 1;

*If DTIMS is flexible, institutions with a value of 1 for tins01 change
*their savings rates.
 tins01(INSDNG) = 1;

*Household consumption spending and consumption quantities.
* EH0(H)         = SUM(C, SAM(C,H)) + SUM(A, QHA0(A,H)*PA0(A));
 EH0(H)         = SUM(C, SAM(C,H)) + SUM(A, QHA0(A,H)*PA0(A)*hpop(H));
*pc QH0(C,H)$PQ0(C) = SAM(C,H)/PQ0(C);
 QH0(C,H)$PQ0(C) = SAM(C,H)/PQ0(C)/hpop(H);

*Government indicators
 YG0           = SAM('TOTAL','GOV');
 EG0           = SAM('TOTAL','GOV') - SAM('S-I','GOV');
 QG0(C)$PQ0(C) = SAM(C,'GOV')/PQ0(C);

 qbarg0(C)     = QG0(C);
 qbarg(C)      = qbarg0(C);
 GADJ0         = 1;
 GSAV0         = SAM('S-I','GOV');

*LES calibration ----------------------------------------------

*pc BUDSHR(C,H)    = SAM(C,H)/(SUM(CP, SAM(CP,H)) + SUM(AP, QHA0(AP,H)*PA0(AP)));
*pc BUDSHR2(A,H)   = QHA0(A,H)*PA0(A)/(SUM(CP, SAM(CP,H)) + SUM(AP, QHA0(AP,H)*PA0(AP)));
 BUDSHR(C,H)    = SAM(C,H)/hpop(H)/(SUM(CP, SAM(CP,H)/hpop(H)) + SUM(AP, QHA0(AP,H)*PA0(AP)));
 BUDSHR2(A,H)   = QHA0(A,H)*PA0(A)/(SUM(CP, SAM(CP,H)/hpop(H)) + SUM(AP, QHA0(AP,H)*PA0(AP)));
 BUDSHRCHK(H)   = SUM(C, BUDSHR(C,H)) + SUM(A, BUDSHR2(A,H)) - 1 ;
 ELASCHK(H)     = SUM(C, BUDSHR(C,H)*LESELAS1(C,H)) + SUM(A, BUDSHR2(A,H)*LESELAS2(A,H)) - 1 ;

DISPLAY BUDSHR, BUDSHR2, BUDSHRCHK, LESELAS1, LESELAS2, ELASCHK;

*Correct elasticities to make them satisfy Engle aggregation exactly
 LESELAS1(C,H)   = LESELAS1(C,H)/(ELASCHK(H) + 1);
 LESELAS2(A,H)   = LESELAS2(A,H)/(ELASCHK(H) + 1);

*Check Engle aggregation again
 ELASCHK(H)      = SUM(C, BUDSHR(C,H)*LESELAS1(C,H)) + SUM(A, BUDSHR2(A,H)*LESELAS2(A,H)) - 1;

DISPLAY "*#*#*# Engle aggregation after correction",ELASCHK, LESELAS1, LESELAS2;

 betam0(C,H)   = BUDSHR(C,H)*LESELAS1(C,H);
 betah0(A,H)   = BUDSHR2(A,H)*LESELAS2(A,H);
 betam(C,H)    = betam0(C,H);
 betah(A,H)    = betah0(A,H);
*pc gammam0(C,H)$BUDSHR(C,H) = ( (SUM(CP, SAM(CP,H)) + SUM(AP, QHA0(AP,H)*PA0(AP))) / PQ0(C) ) * ( BUDSHR(C,H) + betam(C,H)/FRISCH(H));
*pc gammah0(A,H)$BUDSHR2(A,H) = ( (SUM(CP, SAM(CP,H)) + SUM(AP, QHA0(AP,H)*PA0(AP))) / PA0(A) ) * ( BUDSHR2(A,H) + betah(A,H)/FRISCH(H));

 gammam0(C,H)$BUDSHR(C,H) = ( (SUM(CP, SAM(CP,H)/hpop(H)) + SUM(AP, QHA0(AP,H)*PA0(AP))) / PQ0(C) ) * ( BUDSHR(C,H) + betam(C,H)/FRISCH(H));
 gammah0(A,H)$BUDSHR2(A,H) = ( (SUM(CP, SAM(CP,H)/hpop(H)) + SUM(AP, QHA0(AP,H)*PA0(AP))) / PA0(A) ) * ( BUDSHR2(A,H) + betah(A,H)/FRISCH(H));
 gammam(C,H)   =  gammam0(C,H);
 gammah(A,H)   =  gammah0(A,H);

*Checking LES parameters --------------------------------------

PARAMETERS
 SUBSIST(H)  subsistence spending
 FRISCH2(H)  alt. defn of Frisch -- ratio of cons to supernumerary cons
 LESCHK(H)   check on LES parameter definitions (error mssg if error)
 LESELASP1(C,H) Own price elasticity for marketed commodities
 LESELASP2(A,H) Own price elasticity for home consumed activities
 Gammamshare0(C,H) share of subsistence  of h on comm c
 Gammahshare0(A,H) share of subsistence  of h on comm a (home)
 ;

 SUPERNUM(H)  = SUM(A, gammah(A,H)*PA0(A)) + SUM(C, gammam(C,H)*PQ0(C)) ;
******** Askar correction: add /hpop(h)
* FRISCH2(H)   = -EH0(H)/(EH0(H) - SUPERNUM(H));
 FRISCH2(H)$hpop(h)   = -(EH0(H)/hpop(h))/((EH0(H)/hpop(h)) - SUPERNUM(H));
  LESCHK(H)$(ABS(FRISCH(H) - FRISCH2(H)) GT 0.00000001) = 1/0;
 Gammamshare0(C,H)$BUDSHR(C,H)  = 100* PQ0(C)*gammam0(C,H)/(SAM(C,H)/hpop(H)) ;
 Gammahshare0(A,H)$BUDSHR2(A,H) = 100*PA0(A)*gammah0(A,H)/(SAM(A,H)/hpop(H)) ;
*Cross-price elasticities

 LESELASP1(C,H) = -LESELAS1(C,H)*(PQ0(C)*gammam(C,H) / (SUM(CP, SAM(CP,H)) + SUM(AP, QHA0(AP,H)*PA0(AP))) - 1/FRISCH(H));
 LESELASP2(A,H) = -LESELAS2(A,H)*(PA0(A)*gammah(A,H) / (SUM(CP, SAM(CP,H)) + SUM(AP, QHA0(AP,H)*PA0(AP))) - 1/FRISCH(H));

 DISPLAY SUPERNUM, FRISCH, FRISCH2, LESCHK, LESELASP1, LESELASP2 ;


*System-constraint block --------------------------------------

*Fixed investment
 qbarinv(C)$PQ0(C) = SAM(C,'S-I')/PQ0(C);
 QINV0(C)          = qbarinv(C);
 IADJ0             = 1;

*Set proper units of capital in the XXX.DAT file
*Capital stock calibration for recursive investment-based updating
* QINVK = (natdrate + accrate) * SUM(FCAP, QFSBASE(FCAP));
* iwts(C)   = qbarinv(C) / SUM(CP, qbarinv(CP));
* alphainv  = ((natdrate + accrate) * SUM(FCAP, QFSBASE(FCAP))) / PROD(C, QINV0(C)**iwts(C));

*Stock changes
 qdst0(C)$PQ0(C) = SAM(C,'DSTK')/PQ0(C);
 qdst(C)         = qdst0(C);

 FSAV0         = SAM('S-I','ROW')/EXR0;

*pc TABS0         = SUM((C,H), SAM(C,H)) + SUM((A,H), QHA0(A,H)*PA0(A)) + SUM(C, SAM(C,'GOV')) + SUM(C, SAM(C,'S-I')) + SUM(C, SAM(C,'DSTK'));
 TABS0         = SUM((C,H), SAM(C,H)) + SUM((A,H), QHA0(A,H)*PA0(A)*hpop(H)) + SUM(C, SAM(C,'GOV')) + SUM(C, SAM(C,'S-I')) + SUM(C, SAM(C,'DSTK'));

 INVSHR0       = SAM('TOTAL','S-I')/TABS0;
 GOVSHR0       = SUM(C, SAM(C,'GOV'))/TABS0;

 WALRAS0       = 0;

 trnsfr0(INS,AC) = trnsfr(INS,AC);

*Diagnostics on CES and CET function share parameters
*   Test for small values of CES and CET function parameters in production
*   and trade aggregation.  DELTAVA(A), DELTAA(A), DELTAM(C,R), DELTAE(C,R)
*   DELTAT(C), DELTAQ(C)

 Parameter
  DELTATEST1(F,A,RG)  Small delta parameters in CES production function
  DELTATESTME(C,*) Small delta parameters in top trade aggregation functions
 ;
 DELTATEST1(F,A,RG)$(deltava(F,A,RG) LT 1e-4)       = deltava(F,A,RG)  ;
 DELTATESTME(C,"deltat")$(DELTAT(C) LT 1.e-4) = deltat(C) ;
 DELTATESTME(C,"deltaQ")$(DELTAQ(C) LT 1.e-4) = deltaq(C) ;


 DISPLAY$SUM((F,A,RG), deltatest1(F,A,RG))
  "*#*#*# Warning. Small deltas in CES production functions *#*#*#" ;
 DISPLAY$(SUM(C, deltatestme(C,"deltat") + deltatestme(C,"deltaq")) gt 1.e-8)
  "*#*#*# Warning. Small deltas in top trade aggregation functions *#*#*#" ;
 DISPLAY deltatest1, deltatestme ;


*--------------------------------------------------------------------------------------------
*5. VARIABLE DECLARATIONS
*--------------------------------------------------------------------------------------------
*This section only includes variables that appear in the model.
*The variables are declared in alphabetical order.

VARIABLES
 CPI                     consumer price index (PQ-based)
 DPI                     index for domestic producer prices (PDS-based)
 DMPS                    change in marginal propensity to save for selected inst
 DTINS                   change in domestic institution tax share
 EG                      total current government expenditure
 EH(H)                   household consumption expenditure
 EXR                     exchange rate
 FSAV                    foreign savings
 GADJ                    government demand scaling factor
 GOVSHR                  govt consumption share of absorption
 GSAV                    government savings
 IADJ                    investment scaling factor (for fixed capital formation)
 INVSHR                  investment share of absorption
 MPS(INS)                marginal propensity to save for dom non-gov inst ins
 MPSADJ                  savings rate scaling factor
 PA(A)                   output price of national aggregate activity a
 PAR(A,RG)               output price of regional activity a
 PDD(C)                  demand price for com'y c produced & sold domestically
 PDS(C)                  supply price for com'y c produced & sold domestically
 PE(C)                   price of exports
 PINTA(A,RG)             price of intermediate aggregate
 PM(C)                   price of imports
 PQ(C)                   price of composite good c
 PVA(A,RG)               value added price
 PX(C)                   average output price
 PXAC(A,C)               price of commodity c from activity a
 QA(A)                   level of domestic aggregate activity
 QAR(A,RG)               level of domestic regional activity
 QD(C)                   quantity of domestic sales
 QE(C)                   quantity of exports
 QF(F,A,RG)              quantity demanded of factor f from activity a
 QFS(F)                  quantity of factor supply
 QG(C)                   quantity of government consumption
 QH(C,H)                 quantity consumed of marketed commodity c by household h
 QHA(A,H)                quantity consumed of home act a by hhd h
 QINT(C,A)               quantity of intermediate demand for c from activity a
 QINTA(A,RG)             quantity of aggregate intermediate input
 QINV(C)                 quantity of fixed investment demand
 QM(C)                   quantity of imports
 QQ(C)                   quantity of composite goods supply
 QT(C)                   quantity of trade and transport demand for commodity c
 QVA(A,RG)               quantity of aggregate value added
 QX(C)                   quantity of aggregate marketed commodity output
 QXAC(A,C)               quantity of ouput of commodity c from activity a
 TABS                    total absorption
 TINS(INS)               rate of direct tax on domestic institutions ins
 TINSADJ                 direct tax scaling factor
 TRII(INS,INSP)          transfers to dom. inst. insdng from insdngp
 UF(F,A,RG)              factor utilization rate (1 is fully utilized)
 WALRAS                  savings-investment imbalance (should be zero)
 WALRASSQR               Walras squared
 WF(F)                   economy-wide wage (rent) for factor f
 WFDIST(F,A,RG)          factor wage distortion variable
 YF(F)                   factor income
 YG                      total current government income
 YIF(INS,F)              income of institution ins from factor f
 YI(INS)                 income of (domestic non-governmental) institution ins
;

 CPI.L                  = CPI0;
 DMPS.L                 = DMPS0;
 DPI.L                  = DPI0;
 DTINS.L                = DTINS0;
 EG.L                   = EG0;
 EH.L(H)                = EH0(H);
 EXR.L                  = EXR0;
 FSAV.L                 = FSAV0;
 GADJ.L                 = GADJ0;
 GOVSHR.L               = GOVSHR0;
 GSAV.L                 = GSAV0;
 IADJ.L                 = IADJ0;
 INVSHR.L               = INVSHR0;
 MPS.L(INSDNG)          = MPS0(INSDNG);
 MPSADJ.L               = MPSADJ0;
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
 QF.L(F,A,RG)           = QF0(F,A,RG);
 QFS.L(F)               = QFS0(F);
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
 WF.L(F)                = WF0(F);
 WFDIST.L(F,A,RG)       = WFDIST0(F,A,RG);
 YF.L(F)                = YF0(f);
 YG.L                   = YG0;
 YI.L(INS)              = YI0(INS);
 YIF.L(INS,F)           = YIF0(INS,F);

*--------------------------------------------------------------------------------------------
*5. EQUATIONS
*--------------------------------------------------------------------------------------------

EQUATIONS
*Price block
 PMDEF(C)                domestic import price
 PEDEF(C)                domestic export price
 PDDDEF(C)               dem price for comy c produced and sold domestically
 PQDEF(C)                value of sales in domestic market
 PXDEF(C)                value of marketed domestic output
 PXDEF2(C)               commodities with residual export treatment
 PADEF(A)                output price for national aggregate activity a
 PADEF2(A,RG)            output price for regional activity a
 PINTADEF(A,RG)          price of aggregate intermediate input
 PVADEF(A,RG)            value-added price
 CPIDEF                  consumer price index
 DPIDEF                  domestic producer price index
*Production and trade block
 QADEF(A)                define national aggregate as sum of regional production
 LEOAGGINT(A,RG)         Leontief aggreg intermed dem (if Leontief top nest)
 LEOAGGVA(A,RG)          Leontief aggreg value-added dem (if Leontief top nest)
 CESVAPRD(A,RG)          CES value-added production function
 CESVAFOC(F,A,RG)        CES value-added first-order condition
 UFEQ(F,A,RG)            endogenous factor utilization rate (MCP) equation
 QACES(A)                CES activity output aggregation function
 QACESFOC(A,RG)          CES activity output aggregation function first-order condition
 INTDEM(C,A)             intermediate demand for commodity c from activity a
 COMPRDFN1(A,C)          production function for commodity c and activity a (LEO)
 COMPRDFN2(A,C)          production function for commodity c and activity a (CET)
 OUTAGGFN(C)             output aggregation function
 OUTAGGFOC(A,C)          first-order condition for output aggregation function
 CET(C)                  CET function
 CET2(C)                 domestic sales and exports for outputs without both
 ESUPPLY(C)              export supply
 EXPRESID1(C)            export supply quantity
 EXPRESID2(C)            domestic world price equality
 ARMINGTON(C)            composite commodity aggregation function
 COSTMIN(C)              first-order condition for composite commodity cost min
 ARMINGTON2(C)           comp supply for com's without both dom. sales and imports
 QTDEM(C)                demand for transactions (trade and transport) services
 LEONFAC1(A,RG)
 LEONFAC2(F,A,RG)
 OBJEQ
*Institution block
 YFDEF(F)                factor incomes
 YIFDEF(INS,F)           factor incomes to domestic institutions
 YIDEF(INS)              total incomes of domest non-gov't institutions
 EHDEF(H)                household consumption expenditures
 TRIIDEF(INS,INSP)       transfers to inst'on ins from inst'on insp
 HMDEM(C,H)              LES cons demand by hhd h for marketed commodity c
 HADEM(A,H)              LES cons demand by hhd h for home commodity c fr act a
 INVDEM(C)               fixed investment demand
 GOVDEM(C)               government consumption demand
 EGDEF                   total government expenditures
 YGDEF                   total government income
*System constraint block
 COMEQUIL(C)             composite commodity market equilibrium
 FACEQUIL(F)             factor market equilibrium
 CURACCBAL               current account balance (of RoW)
 GOVBAL                  government balance
 TINSDEF(INS)            direct tax rate for inst ins
 MPSDEF(INS)             marg prop to save for inst ins
 SAVINVBAL               savings-investment balance
 TABSEQ                  total absorption
 INVABEQ                 investment share in absorption
 GDABEQ                  government consumption share in absorption
;

*--- Price block

 PMDEF(C)$CM(C)..                PM(C) =E= pwm(C)*(1 + tm(C))*EXR + SUM(CT, PQ(CT)*icm(CT,C));

 PEDEF(C)$CE(C)..                PE(C) =E= pwe(C)*(1 - te(C))*EXR - SUM(CT, PQ(CT)*ice(CT,C));

 PDDDEF(C)$CD(C)..               PDD(C) =E= PDS(C) + SUM(CT, PQ(CT)*icd(CT,C));

 PQDEF(C)$((CD(C) OR CM(C)))..   PQ(C)*(1 - tq(C))*QQ(C) =E= PDD(C)*QD(C) + PM(C)*QM(C);

 PXDEF(C)$(CX(C) AND NOT CER(C)).. PX(C)*QX(C) =E= PDS(C)*QD(C) + PE(C)*QE(C);

 PXDEF2(C)$(CX(C) AND CER(C))..  PX(C) =E= PDS(C);

 PADEF(A)$QA0(A)..               PA(A) =E= SUM(C, PXAC(A,C)*theta(A,C));

 PINTADEF(A,RG)$QVA0(A,RG)..     PINTA(A,RG) =E= SUM(C, PQ(C)*ica(C,A,RG)) ;

 PVADEF(A,RG)$QVA0(A,RG)..       PAR(A,RG)*(1-ta(A,RG))*QAR(A,RG) =E= PVA(A,RG)*QVA(A,RG) + PINTA(A,RG)*QINTA(A,RG) ;

 CPIDEF..                        CPI =E= SUM(C, cwts(C)*PQ(C)) ;

 DPIDEF..                        DPI =E= SUM(CD$(NOT CER(CD)), dwts(CD)*PDS(CD)) ;

*--- Production and trade block

 LEOAGGINT(A,RG)$QVA0(A,RG)..    QINTA(A,RG) =E= inta(A,RG)*QAR(A,RG);

 LEOAGGVA(A,RG)$QVA0(A,RG)..     QVA(A,RG) =E= iva(A,RG)*QAR(A,RG);

 LEONFAC1(A,RG)$(AFLEO(A) AND QVA0(A,RG)).. PVA(A,RG)*QVA(A,RG) =E= SUM(F, QF(F,A,RG)*UF(F,A,RG)*WF(F)*WFDIST(F,A,RG));

 LEONFAC2(F,A,RG)$(AFLEO(A) AND QF0(F,A,RG)).. QVA(A,RG)*ifa(F,A,RG) =E= QF(F,A,RG)*UF(F,A,RG);

PARAMETER
 alphavaadj(A,RG)
 fprdadj(F,A,RG)
;

 alphavaadj(A,RG) = 1;
  fprdadj(F,A,RG) = 1;

 CESVAPRD(A,RG)$(QVA0(A,RG) AND NOT AFLEO(A))..
                                 QVA(A,RG) =E= alphava(A,RG)*alphavaadj(A,RG)*(SUM(F$QF0(F,A,RG), deltava(F,A,RG)*(fprd(F,A,RG)*fprdadj(F,A,RG)*QF(F,A,RG)*UF(F,A,RG))**(-rhova(A,RG))) )**(-1/rhova(A,RG)) ;

 CESVAFOC(F,A,RG)$(QF0(F,A,RG) AND QVA0(A,RG)  AND NOT AFLEO(A))..
                                 WF(F)*WFDIST(F,A,RG) =E= PVA(A,RG)*(1-tva(A,RG)) * QVA(A,RG) * SUM(FP, deltava(FP,A,RG)*(fprd(FP,A,RG)*QF(FP,A,RG)*UF(F,A,RG))**(-rhova(A,RG)))**(-1) *deltava(F,A,RG)*fprd(F,A,RG)**(-rhova(A,RG))*(QF(F,A,RG)*UF(F,A,RG))**(-rhova(A,RG)-1);

 UFEQ(F,A,RG)$(QF0(F,A,RG) AND FUTL(F,A,RG)).. WFDIST(F,A,RG) =G= minrtn(A,RG)*wfdist0(F,A,RG)*CPI/CPI0;

 QADEF(A)$(NOT ACES2(A))..       QA(A) =E=  SUM(RG, QAR(A,RG));

 PADEF2(A,RG)$(NOT ACES2(A) AND QAR0(A,RG)).. PA(A)=E= PAR(A,RG);

 QACES(A)$(ACES2(A) AND NOT ACHK(A)).. QA(A) =E= alphaa2(A)*(SUM(RG, deltaa2(A,RG)*QAR(A,RG)**(-rhoa2(A))))**(-1/rhoa2(A));

 QACESFOC(A,RG)$(ACES2(A) AND NOT ACHK(A) AND deltaa2(A,RG))..
                                 PAR(A,RG) =E= PA(A)*deltaa2(A,RG)*(SUM(RGP, deltaa2(A,RGP)*QAR(A,RGP)**(-rhoa2(A))))**(-1)*QA(A)*QAR(A,RG)**(-rhoa2(A)-1);

 INTDEM(C,A)$SUM(RG,ica(C,A,RG)).. QINT(C,A) =E= SUM(RG,ica(C,A,RG)*QINTA(A,RG));

 COMPRDFN2(A,C)$(ACET2(A) AND deltaca(A,C)).. QXAC(A,C) =E= (QA(A) - SUM(H, QHA(A,H)*hpop(H)))*(PXAC(A,C)/(PA(A)*deltaca(A,C)*alphaca(A)**rhoca(A)))**(1/(rhoca(A)-1)) ;

 COMPRDFN1(A,C)$(NOT ACET2(A) AND theta(A,C)).. QXAC(A,C) =E= theta(A,C)*(QA(A) - SUM(H, QHA(A,H)*hpop(H))) ;

 OUTAGGFN(C)$CX(C)..             QX(C) =E= alphaac(C)*SUM(A, deltaac(A,C)*QXAC(A,C)**(-rhoac(C)))**(-1/rhoac(C));

 OUTAGGFOC(A,C)$deltaac(A,C)..   PXAC(A,C) =E= PX(C)*QX(C) * SUM(AP, deltaac(AP,C)*QXAC(AP,C)**(-rhoac(C)) )**(-1)*deltaac(A,C)*QXAC(A,C)**(-rhoac(C)-1);

 CET(C)$(CE(C) AND CD(C) AND (NOT CER(C))).. QX(C) =E= alphat(C)*(deltat(C)*QE(C)**rhot(C) + (1 - deltat(C))*QD(C)**rhot(C))**(1/rhot(C)) ;

 ESUPPLY(C)$(CE(C) AND CD(C) AND (NOT CER(C))).. QE(C) =E= QD(C)*((PE(C)/PDS(C))*((1 - deltat(C))/deltat(C)))**(1/(rhot(C)-1)) ;

 EXPRESID1(C)$(CX(C) AND CER(C)).. QE(C)  =E= QX(C) - QD(C);

 EXPRESID2(C)$(CX(C) AND CER(C)).. PDS(C) =E= PE(C);

 CET2(C)$((CD(C) AND CEN(C)) OR (CE(C) AND CDN(C))).. QX(C) =E= QD(C) + QE(C);

 ARMINGTON(C)$(CM(C) AND CD(C)).. QQ(C) =E= alphaq(C)*(deltaq(C)*QM(C)**(-rhoq(C)) + (1 -deltaq(C))*QD(C)**(-rhoq(C)))**(-1/rhoq(C)) ;

 COSTMIN(C)$(CM(C) AND CD(C))..  QM(C) =E= QD(C)*((PDD(C)/PM(C))*(deltaq(C)/(1 - deltaq(C))))**(1/(1 + rhoq(C)));

 ARMINGTON2(C)$( ((CD(C) AND CMN(C)) OR (CM(C) AND CDN(C)))).. QQ(C) =E= QD(C) + QM(C);

 QTDEM(C)$CT(C)..                QT(C) =E= SUM(CP, icm(C,CP)*QM(CP)+ ice(C,CP)*QE(CP)+ icd(C,CP)*QD(CP));

*--- Institution block

 YFDEF(F)..                      YF(F) =E= WF(F)*SUM((A,RG), WFDIST(F,A,RG)*QF(F,A,RG)*UF(F,A,RG));

 YIFDEF(INSD,F)$shif(INSD,F)..   YIF(INSD,F) =E= shif(INSD,F)*((1-tf(f))*(1-rf(F))*YF(F) - trnsfr('ROW',F)*EXR);

 YIDEF(INSDNG)..                 YI(INSDNG) =E= SUM(F, YIF(INSDNG,F))  + SUM(INSDNGP, TRII(INSDNG,INSDNGP)) + trnsfr(INSDNG,'GOV')*CPI + trnsfr(INSDNG,'ROW')*EXR;

 TRIIDEF(INSDNG,INSDNGP)$( shii(INSDNG,INSDNGP)).. TRII(INSDNG,INSDNGP) =E= shii(INSDNG,INSDNGP) * (1 - MPS(INSDNGP)) * (1 - TINS(INSDNGP))* YI(INSDNGP);

 EHDEF(H)..                      EH(H) =E= (1 - SUM(INSDNG, shii(INSDNG,H))) * (1 - MPS(H)) * (1 - TINS(H)) * YI(H);

 HMDEM(C,H)$( betam(C,H))..      PQ(C)*QH(C,H) =E= PQ(C)*gammam(C,H) + betam(C,H)*( EH(H)/hpop(H) -
 SUM(CP, PQ(CP)*gammam(CP,H)) - SUM(A, PA(A)*gammah(A,H))) ;

 HADEM(A,H)$( betah(A,H))..      PA(A)*QHA(A,H) =E= PA(A)*gammah(A,H) + betah(A,H)*(EH(H)/hpop(H) - SUM(CP, PQ(CP)*gammam(CP,H)) - SUM(AP, PA(AP)*gammah(AP,H))) ;

 INVDEM(C)$( qbarinv(C))..       QINV(C) =E= IADJ*qbarinv(C);

 GOVDEM(C)$( qbarg(C))..         QG(C) =E= GADJ*qbarg(C);

 YGDEF..                         YG =E= SUM(INSDNG, TINS(INSDNG)*YI(INSDNG)) + SUM(F, tf(F)*YF(F))
                                 + SUM((A,RG), tva(A,RG)*PVA(A,RG)*QVA(A,RG))  + SUM((A,RG), ta(A,RG)*PAR(A,RG)*QAR(A,RG))
                                 + SUM(C, tm(C)*pwm(C)*QM(C))*EXR + SUM(C, te(C)*pwe(C)*QE(C))*EXR + SUM(C, tq(C)*PQ(C)*QQ(C))
                                 + SUM(F, YIF('GOV',F)) + trnsfr('GOV','ROW')*EXR;

 EGDEF..                         EG =E= SUM(C, PQ(C)*QG(C)) + SUM(INSDNG, trnsfr(INSDNG,'GOV'))*CPI;

*--- System constraint block

 FACEQUIL(F)$QFS0(F)..           SUM((A,RG), QF(F,A,RG)) =E= QFS(F) * (WF(F)/WF00(F))**elasuss(F);

 COMEQUIL(C)..                   QQ(C) =E= SUM(A, QINT(C,A)) + SUM(H, QH(C,H)*hpop(H)) + QG(C) + QINV(C) + qdst(C) + QT(C);

 CURACCBAL..                     SUM(C, pwm(C)*QM(C)) + SUM(F, trnsfr('ROW',F)) + SUM(F, (1-tf(f))*rf(F)*YF(F))/EXR =E= SUM(C, pwe(C)*QE(C)) + SUM(INSD, trnsfr(INSD,'ROW')) + FSAV;

 GOVBAL..                        YG =E= EG + GSAV;

 TINSDEF(INSDNG)..               TINS(INSDNG) =E= tinsbar(INSDNG)*(1 + TINSADJ*tins01(INSDNG)) + DTINS*tins01(INSDNG);

 MPSDEF(INSDNG)..                MPS(INSDNG)  =E= mpsbar(INSDNG)*(1 + MPSADJ*mps01(INSDNG)) + DMPS*mps01(INSDNG);

 SAVINVBAL..                     SUM(INSDNG, MPS(INSDNG) * (1 - TINS(INSDNG)) * YI(INSDNG)) + GSAV + FSAV*EXR =E= SUM(C, PQ(C)*QINV(C)) + SUM(C, PQ(C)*qdst(C)) + WALRAS;

 TABSEQ..                        TABS =E= SUM((C,H), PQ(C)*QH(C,H)*hpop(H)) + SUM((A,H), PA(A)*QHA(A,H)*hpop(H)) + SUM(C, PQ(C)*QG(C)) + SUM(C, PQ(C)*QINV(C)) + SUM(C, PQ(C)*qdst(C));

 INVABEQ..                       INVSHR*TABS =E= SUM(C, PQ(C)*QINV(C)) + SUM(C, PQ(C)*qdst(C));

 GDABEQ..                        GOVSHR*TABS =E= SUM(C, PQ(C)*QG(C));

 OBJEQ..                         WALRASSQR   =E= WALRAS*WALRAS ;


*--------------------------------------------------------------------------------------------
*6. MODEL
*--------------------------------------------------------------------------------------------

MODEL STANDCGE  standard CGE model /
*Price block
 PMDEF.PM
 PEDEF.PE
 PQDEF.PQ
 PXDEF
 PXDEF2
 PDDDEF.PDD
 PADEF
 PINTADEF.PINTA
 PVADEF.PVA
 CPIDEF
 DPIDEF
*Production and trade block
 QADEF.QA
 PADEF2.PAR
 QACES
 QACESFOC
 LEOAGGINT
 LEOAGGVA
 CESVAPRD.QVA
 CESVAFOC
 UFEQ.UF
 INTDEM.QINT
 COMPRDFN1
 COMPRDFN2
 OUTAGGFN.QX
 OUTAGGFOC.QXAC
 CET
 CET2
 ESUPPLY
 EXPRESID1
 EXPRESID2
 ARMINGTON
 COSTMIN
 ARMINGTON2
 QTDEM.QT
 LEONFAC1
 LEONFAC2
*Institution block
 YFDEF.YF
 YIFDEF.YIF
 YIDEF.YI
 EHDEF.EH
 TRIIDEF.TRII
 HMDEM.QH
 HADEM.QHA
 EGDEF.EG
 YGDEF.YG
 GOVDEM.QG
 GOVBAL
 INVDEM.QINV
*System-constraint block
 FACEQUIL
 COMEQUIL
 CURACCBAL
 SAVINVBAL.WALRAS
 TINSDEF.TINS
 MPSDEF.MPS
 TABSEQ.TABS
 INVABEQ
 GDABEQ
/;

MODEL STANDNLP  standard CGE model /
*Price block
 PMDEF
 PEDEF
 PQDEF
 PXDEF
 PXDEF2
 PDDDEF
 PADEF
 PINTADEF
 PVADEF
 CPIDEF
 DPIDEF
*Production and trade block
 QADEF
 PADEF2
 QACES
 QACESFOC
 LEOAGGINT
 LEOAGGVA
 CESVAPRD
 CESVAFOC
 INTDEM
 COMPRDFN1
 COMPRDFN2
 OUTAGGFN
 OUTAGGFOC
 CET
 CET2
 ESUPPLY
 EXPRESID1
 EXPRESID2
 ARMINGTON
 COSTMIN
 ARMINGTON2
 QTDEM
 LEONFAC1
 LEONFAC2
 OBJEQ
*Institution block
 YFDEF
 YIFDEF
 YIDEF
 EHDEF
 TRIIDEF
 HMDEM
 HADEM
 EGDEF
 YGDEF
 GOVDEM
 GOVBAL
 INVDEM
*System-constraint block
 FACEQUIL
 COMEQUIL
 CURACCBAL
 SAVINVBAL
 TINSDEF
 MPSDEF
 TABSEQ
 INVABEQ
 GDABEQ
/;

*--------------------------------------------------------------------------------------------
*7. FIXING VARIABLES NOT IN MODEL AT ZERO
*--------------------------------------------------------------------------------------------

 QA.FX(A)$(NOT QA0(A)) = 0;
 PA.FX(A)$(NOT QA0(A)) = 0;
 QAR.FX(A,RG)$(NOT QAR0(A,RG)) = 0;
 PAR.FX(A,RG)$(NOT QAR0(A,RG)) = 0;
 QVA.FX(A,RG)$(NOT QVA0(A,RG)) = 0;
 PVA.FX(A,RG)$(NOT QVA0(A,RG)) = 0;
 PDD.FX(C)$(NOT CD(C)) = 0;
 PDS.FX(C)$(NOT CD(C)) = 0;
 PE.FX(C)$(NOT CE(C)) = 0;
 PM.FX(C)$(NOT CM(C)) = 0;
 PX.FX(C)$(NOT CX(C)) = 0;
 PXAC.FX(A,C)$(NOT PXAC0(A,C)) = 0;
 QD.FX(C)$(NOT CD(C)) = 0;
 QE.FX(C)$(NOT CE(C)) = 0;
 QF.FX(F,A,RG)$(NOT QF0(F,A,RG)) = 0;
 QG.FX(C)$(NOT SAM(C,'GOV')) = 0;
 QH.FX(C,H)$(NOT QH0(C,H)) = 0;
 QHA.FX(A,H)$(NOT BETAH(A,H)) = 0;
 QINT.FX(C,A)$(NOT QINT0(C,A)) = 0;
 QINV.FX(C)$(NOT QINV0(C)) = 0;
 QM.FX(C)$(NOT CM(C)) = 0;
 QQ.FX(C)$(NOT (CD(C) OR CM(C))) = 0;
 QT.FX(C)$(NOT CT(C)) = 0;
 QX.FX(C)$(NOT CX(C)) = 0;
 QXAC.FX(A,C)$(NOT QXAC0(A,C)) = 0;
 TRII.FX(INSDNG,INSDNGP)$(NOT SAM(INSDNG,INSDNGP)) = 0;
 YI.FX(INS)$(NOT INSD(INS)) = 0;
 YIF.FX(INS,F)$((NOT INSD(INS)) OR (NOT SAM(INS,F))) = 0;

*Bound factor utilization rate
 UF.LO(F,A,RG)$FUTL(F,A,RG) = 0;
 UF.UP(F,A,RG)$FUTL(F,A,RG) = 1;
 UF.L(F,A,RG) = 1;
 UF.FX(F,A,RG)$(NOT FUTL(F,A,RG)) = 1;


*--------------------------------------------------------------------------------------------
*8. MODEL CLOSURE
*--------------------------------------------------------------------------------------------

*Factor markets
 QFS.FX(F)          = QFS0(F);
 WF.LO(F)           = -INF;
 WF.UP(F)           = +INF;
 WFDIST.FX(F,A,RG)  = WFDIST0(F,A,RG);

*Current account of RoW
* EXR.FX       = EXR0;
 FSAV.FX      = FSAV0;

*Current government balance
* GSAV.FX     = GSAV0 ;
 TINSADJ.FX  = TINSADJ0;
 DTINS.FX    = DTINS0;
 GADJ.FX     = GADJ0;
* GOVSHR.FX   = GOVSHR0 ;

*Savings-investment balance
 MPSADJ.FX = MPSADJ0;
 DMPS.FX   = DMPS0;
* IADJ.FX   = IADJ0;
* INVSHR.FX = INVSHR0 ;

*Numeraire price index
 CPI.FX        = CPI0;
* DPI.FX        = DPI0;


*--------------------------------------------------------------------------------------------
*9. SOLUTION STATEMENT
*--------------------------------------------------------------------------------------------

OPTIONS
 ITERLIM = 20, LIMROW = 30, LIMCOL = 30, SOLPRINT=ON, MCP=PATH, NLP=PATHNLP
;

 STANDCGE.HOLDFIXED   = 1 ;
 STANDCGE.TOLINFREP   = .0001 ;

 SOLVE STANDCGE USING MCP ;
* SOLVE STANDNLP MINIMIZING WALRASSQR USING NLP ;

$INCLUDE includes\1repbase.inc
$INCLUDE includes\1parmout.inc

*--------------------------------------------------------------------------------------------
*10. CALIBRATION DATA
*--------------------------------------------------------------------------------------------

SET
 SMLTRD(C,*)             sectors with small export and import intensities
 EAC                     aggregate sectors for employment table  / e_tot, e_agr, e_min, e_man, e_utl, e_con, e_trd, e_trc, e_hfs, e_fbs, e_res, e_gsv, e_osv /
;

PARAMETER
*Wage calibration
 EMP_CALC(EAC,F)         employment data for calculating average wages by sector
 VAD_CALC(EAC,F)         labor value added for calculating average wages by sector
 SCALE_EMP               scale employment data                   / 1000 /
 SCALE_VAD               scale value added data                  / 1000 /
;

*Sectors with small export and import intensities
 SMLTRD(C,'EXP-OUTshr')$(STRUCBASE(C,'EXP-OUTshr') LT 1 AND STRUCBASE(C,'EXP-OUTshr') GT 0) = 1;
 SMLTRD(C,'IMP-DEMshr')$(STRUCBASE(C,'IMP-DEMshr') LT 1 AND STRUCBASE(C,'IMP-DEMshr') GT 0) = 1;

*Employment data for calculating average wages by sector
 EMP_CALC('e_agr',FLAB) = SUM((A,RG)$EAGR(A), QF0(FLAB,A,RG))*SCALE_EMP;
 EMP_CALC('e_min',FLAB) = SUM((A,RG)$EMIN(A), QF0(FLAB,A,RG))*SCALE_EMP;
 EMP_CALC('e_man',FLAB) = SUM((A,RG)$EMAN(A), QF0(FLAB,A,RG))*SCALE_EMP;
 EMP_CALC('e_utl',FLAB) = SUM((A,RG)$EUTL(A), QF0(FLAB,A,RG))*SCALE_EMP;
 EMP_CALC('e_con',FLAB) = SUM((A,RG)$ECON(A), QF0(FLAB,A,RG))*SCALE_EMP;
 EMP_CALC('e_trd',FLAB) = SUM((A,RG)$ETRD(A), QF0(FLAB,A,RG))*SCALE_EMP;
 EMP_CALC('e_trc',FLAB) = SUM((A,RG)$ETRC(A), QF0(FLAB,A,RG))*SCALE_EMP;
 EMP_CALC('e_hfs',FLAB) = SUM((A,RG)$EHFS(A), QF0(FLAB,A,RG))*SCALE_EMP;
 EMP_CALC('e_fbs',FLAB) = SUM((A,RG)$EFBS(A), QF0(FLAB,A,RG))*SCALE_EMP;
 EMP_CALC('e_res',FLAB) = SUM((A,RG)$ERES(A), QF0(FLAB,A,RG))*SCALE_EMP;
 EMP_CALC('e_gsv',FLAB) = SUM((A,RG)$EGSV(A), QF0(FLAB,A,RG))*SCALE_EMP;
 EMP_CALC('e_osv',FLAB) = SUM((A,RG)$EOSV(A), QF0(FLAB,A,RG))*SCALE_EMP;
 EMP_CALC('e_tot',FLAB) = SUM((A,RG),         QF0(FLAB,A,RG))*SCALE_EMP;

*Labor value added for calculating average wages by sector
 VAD_CALC('e_agr',FLAB) = SUM((A,RG)$EAGR(A), WF0(FLAB)*WFDIST0(FLAB,A,RG)*QF0(FLAB,A,RG))*SCALE_VAD;
 VAD_CALC('e_min',FLAB) = SUM((A,RG)$EMIN(A), WF0(FLAB)*WFDIST0(FLAB,A,RG)*QF0(FLAB,A,RG))*SCALE_VAD;
 VAD_CALC('e_man',FLAB) = SUM((A,RG)$EMAN(A), WF0(FLAB)*WFDIST0(FLAB,A,RG)*QF0(FLAB,A,RG))*SCALE_VAD;
 VAD_CALC('e_utl',FLAB) = SUM((A,RG)$EUTL(A), WF0(FLAB)*WFDIST0(FLAB,A,RG)*QF0(FLAB,A,RG))*SCALE_VAD;
 VAD_CALC('e_con',FLAB) = SUM((A,RG)$ECON(A), WF0(FLAB)*WFDIST0(FLAB,A,RG)*QF0(FLAB,A,RG))*SCALE_VAD;
 VAD_CALC('e_trd',FLAB) = SUM((A,RG)$ETRD(A), WF0(FLAB)*WFDIST0(FLAB,A,RG)*QF0(FLAB,A,RG))*SCALE_VAD;
 VAD_CALC('e_trc',FLAB) = SUM((A,RG)$ETRC(A), WF0(FLAB)*WFDIST0(FLAB,A,RG)*QF0(FLAB,A,RG))*SCALE_VAD;
 VAD_CALC('e_hfs',FLAB) = SUM((A,RG)$EHFS(A), WF0(FLAB)*WFDIST0(FLAB,A,RG)*QF0(FLAB,A,RG))*SCALE_VAD;
 VAD_CALC('e_fbs',FLAB) = SUM((A,RG)$EFBS(A), WF0(FLAB)*WFDIST0(FLAB,A,RG)*QF0(FLAB,A,RG))*SCALE_VAD;
 VAD_CALC('e_res',FLAB) = SUM((A,RG)$ERES(A), WF0(FLAB)*WFDIST0(FLAB,A,RG)*QF0(FLAB,A,RG))*SCALE_VAD;
 VAD_CALC('e_gsv',FLAB) = SUM((A,RG)$EGSV(A), WF0(FLAB)*WFDIST0(FLAB,A,RG)*QF0(FLAB,A,RG))*SCALE_VAD;
 VAD_CALC('e_osv',FLAB) = SUM((A,RG)$EOSV(A), WF0(FLAB)*WFDIST0(FLAB,A,RG)*QF0(FLAB,A,RG))*SCALE_VAD;
 VAD_CALC('e_tot',FLAB) = SUM((A,RG),         WF0(FLAB)*WFDIST0(FLAB,A,RG)*QF0(FLAB,A,RG))*SCALE_VAD;

DISPLAY WALRAS.l, SAMBALCHK, SMLTRD, STRUCBASE;
scalar stop /0/;
