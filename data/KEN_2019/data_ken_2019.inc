$onempty
$TITLE Rural Investment and Policy Analysis (RIAPA) Model: Version 1A

$ontext
This file (1country.dat) imports country data for the core RIAPA CGE model.

The file is called from within 1model.gms. Most adjustments to this file are done in the Excel file:
1model_riapa.xlsx
$offtext

*Switches for controlling country data use
PARAMETER
* SCALE                   scaling parameter for SAM (multiplier)          / 0.001 /
 SCALE                   scaling parameter for SAM (multiplier)          / 1 /
 SCALING                 scaling from SAM units to single units          / 1000000 /
 XRATE                   exchange rate (LCU per USD)                     / 101.991/
*Employment and production quantities
 AGRIPROD                use agricultural prod data (0 no)               / 1 /
 FCALTYPE                SAM (1) wage (2) employment (3)                 / 2 /
 PRODADJ_CRP             crop production adjustment (div)                / 1000 /
 PRODADJ_LIV             livestock production adjustment (div)           / 1000 /
 AREAADJ_CRP             crop land area adjustment (div)                 / 1000 /
 AREAADJ_LIV             livestock TLU adjustment (div)                  / 1000 /
 WAGEADJ                 labor wage adjustment (div)                     / 1000 /
 EMPLADJ                 labor employment adjustment (div)               / 1000 /
;

*--------------------------------------------------------------------------------------------
*1. SETS DEFINITION AND CALIBRATION DATA
*--------------------------------------------------------------------------------------------

SETS
*Adjustments to the SAM
 REMEXP(C)               remove exports with small shares
 REMIMP(C)               remove imports with small shares
 REEXPORT(C)             remove imports with re-export problem
 NETEXPS(C)              net exports with imports (imports minus exports)
 NETIMPS(C)              net imports with exports (exports minus imports)
 CER(C)                  commodities with residual export treatment
*Trade and production elasticities
 TRDELAS                 trade elasticities                              / SIGMAQ, SIGMAT /
 PRDELAS                 production elasticities                         / PRODELAS, PRODELAS2 /
 USS(F)                  factors with upward-sloping supply functions
*Employment and production quantities
 AL                      labor data activities
 MALA(AL,ARG)            mapping between labor data and sam activities
 APAC                    agricultural accounts                           / AREA, PROD, YIELD /
*Other sets
 TX                      taxes in the model                              / INSTAX, FACTAX, IMPTAX, EXPTAX, VATAX, ACTTAX, COMTAX /
 EAGR(A)                 employment in agriculture
 EMIN(A)                 employment in amining
 EMAN(A)                 employment in amanufacturing
 EUTL(A)                 employment in autilities
 ECON(A)                 employment in aconstruction
 ETRD(A)                 employment in trade services
 ETRC(A)                 employment in transport and communication
 EHFS(A)                 employment in hotel and food services
 EFBS(A)                 employment in financial and business services
 ERES(A)                 employment in real estate
 EGSV(A)                 employment in public admin health and education
 EOSV(A)                 employment in other services
;

PARAMETER
*Data imported from Excel
 TRADELAS(AC,TRDELAS)    armington and CET elasticities
 PRODELASTAB(AC,PRDELAS) factor substitition elasticities
 PRODDATA(A,RG,APAC)     agricultural production data
 QFINPUT(AL,F)           labor employment data
 WFINPUT(AL,F)           labor wage data
 POPTAB(H)               household population data
 LESELAS1(C,H)           household expenditure or income elasticities
*Trade, production and aggregation elasticities
 PRODELAS(AC)            elasticity of substitution between factors - bottom of technology nest
 PRODELAS2(AC)           elasticity of substitution between aggregate factors and intermediates - top of tech nest
 PRODELAS3(F,AC)         elasticity of substitution between output from regional activities
 ELASAC(C)               output aggregation elasticity for commodity C
 ELASCA(A)               CET elasticity for output disaggregation
 ELASCES(A)              CES activity output aggregation function
*Household consumption elasticities and population
 FRISCH(H)               Frisch parameter for household LES demand
 LESELAS2(A,H)           expenditure elasticity of home demand by activity
 SHRHOME(A,H)            value share for activity a in home consumption of household h from activity a
 HPOP0(H)                base year household populations
 HPOP(H)                 household populations
*Employment and production quantities
 PRODUCTION(ARG)         production quantities for calibration
 QFBASE(F,AC)            sectoral employment
 QFSBASE(AC)             total employment
*Taxes
 TAXPAR(TX,AC)           payment by account ac to tax account tx
;


ALIAS
 (AC,ACP), (ACNT,ACNTP), (AA,AAP), (A,AP,APP,APPP), (C,CP,CPP), (CE,CEP), (CM,CMP)
 (F,FP,FPP,FPPP), (FLAB,FLABP), (FCAP,FCAPP), (FLND,FLNDP), (FLIV,FLIVP), (INS,INSP)
 (INSD,INSDP), (INSDNG,INSDNGP), (H,HP), (ARG,ARGP), (RG,RGP), (CTE,CTEP), (CTM,CTMP)
;

$call "gdxxrw i=data\%country%\1model_%country%.xlsx o=model.gdx index=index!a15"
$gdxin model.gdx
$load AC AL
$loaddc A ARG C F FLAB FCAP FLND FLIV RG INS INSD INSDNG EN H CTD CTE CTM MAC MARG MALA EAGR EMIN EMAN EUTL ECON ETRD ETRC EHFS EFBS ERES EGSV EOSV
* Askar
$loaddc SAM REMEXP REMIMP REEXPORT NETEXPS NETIMPS CER TRADELAS PRODELASTAB PRODDATA QFINPUT WFINPUT POPTAB LESELAS1 LESELAS2 FRISCH
$gdxin

*Regional activity CES aggregation function?
 ACES2(A)               = NO;
*CET disaggregation function in the make matrix?
 ACET2(A)               = NO;

 ACNT(AC)               = YES;
 ACNT('TOTAL')          = NO;

 AFLEO(A)               = NO;

 USS(F)                 = NO;

*$offtext
*----------------------------------------------------------------------------
*2. SAM ADJUSTMENTS
*----------------------------------------------------------------------------

*$INCLUDE includes\1SAMBAL.INC

*Move consumption from home to market --------------
SET
 AMOVE(A)        / /
 HMOVE(H)        / hhd-u1, hhd-u2, hhd-u3, hhd-u4, hhd-u5 /
;

*Activities
 SAM(C,H) = SAM(C,H) + SUM(AMOVE$MAC(AMOVE,C), SAM(AMOVE,H));
 SAM(AMOVE,C)$MAC(AMOVE,C) = SAM(AMOVE,C) + SUM(H, SAM(AMOVE,H));
 SAM(AMOVE,H) = 0;
*Households
 SAM(C,HMOVE) = SAM(C,HMOVE) + SUM(A$MAC(A,C), SAM(A,HMOVE));
 SAM(A,C)$MAC(A,C) = SAM(A,C) + SUM(HMOVE, SAM(A,HMOVE));
 SAM(A,HMOVE) = 0;



*Remove small trade flows --------------------------
*Net out trade flows
*  Imports
 SAM(NETIMPS,'ROW') = SAM(NETIMPS,'ROW')-SAM('ROW',NETIMPS);
 SAM('ROW',NETIMPS) = 0;
* SAM('STAX',NETIMPS) = SAM('STAX',NETIMPS) + SAM('MTAX',NETIMPS);
 SAM('ETAX',NETIMPS) = SAM('ETAX',NETIMPS) + SAM('MTAX',NETIMPS);
 SAM('MTAX',NETIMPS) = 0;
 SAM('GOV','MTAX') = SUM(C, SAM('MTAX',C));
 SAM('GOV','STAX') = SUM(C, SAM('STAX',C));
 SAM('GOV','ETAX') = SUM(C, SAM('ETAX',C));
*  Exports
 SAM('ROW',NETEXPS) = SAM('ROW',NETEXPS) - SAM(NETEXPS,'ROW');
 SAM(NETEXPS,'ROW') = 0;

*Move exports to changes in inventories
 SAM(REMEXP,'DSTK') = SAM(REMEXP,'DSTK') + SAM(REMEXP,'ROW');
SAM('STAX',REMEXP) = SAM('STAX',REMEXP) + SAM('ETAX',REMEXP);
SAM('ETAX',REMEXP) = 0;
 SAM('DSTK','S-I')  = SAM('DSTK','S-I') + SUM(REMEXP, SAM(REMEXP,'ROW'));
 SAM('S-I','ROW')   = SAM('S-I','ROW') + SUM(REMEXP, SAM(REMEXP,'ROW'));
 SAM(REMEXP,'ROW')  = 0;
SAM('GOV','ETAX')  = SUM(C, SAM('ETAX',C));
SAM('ROW','GOV')   = 0;
SAM('ROW','GOV')   = SUM(ACNT, SAM(ACNT,'ROW')-SAM('ROW',ACNT));

*Move imports to changes in inventories
 SAM('ROW',REMIMP) = SAM('ROW',REMIMP) + SAM('MTAX',REMIMP);
 SAM('MTAX',REMIMP) = 0;
 SAM(REMIMP,'DSTK') = SAM(REMIMP,'DSTK') - SAM('ROW',REMIMP);
 SAM('DSTK','S-I')  = SAM('DSTK','S-I') - SUM(REMIMP, SAM('ROW',REMIMP));
 SAM('S-I','ROW')   = SAM('S-I','ROW') - SUM(REMIMP, SAM('ROW',REMIMP));
 SAM('ROW',REMIMP)  = 0;
 SAM('GOV','MTAX')  = SUM(C, SAM('MTAX',C));
 SAM('ROW','GOV')   = 0;
 SAM('ROW','GOV')   = SUM(ACNT, SAM(ACNT,'ROW')-SAM('ROW',ACNT));

*Move trade transactions costs to domestic costs
 SAM(CTD,REMEXP) = SAM(CTD,REMEXP) + SUM(CTE, SAM(CTE,REMEXP));
 SAM(CTD,REMIMP) = SAM(CTD,REMIMP) + SUM(CTM, SAM(CTM,REMIMP));
 SAM(CTE,REMEXP) = 0;
 SAM(CTM,REMIMP) = 0;
 SAM(C,CTM)$SUM(CP, SAM(CP,CTM)) = SAM(C,CTM)/SUM(CP, SAM(CP,CTM)) * SUM(ACNT, SAM(CTM,ACNT));
 SAM(C,CTD)$SUM(CP, SAM(CP,CTD)) = SAM(C,CTD)/SUM(CP, SAM(CP,CTD)) * SUM(ACNT, SAM(CTD,ACNT));
 SAM(C,CTE)$SUM(CP, SAM(CP,CTE)) = SAM(C,CTE)/SUM(CP, SAM(CP,CTE)) * SUM(ACNT, SAM(CTE,ACNT));

*Move import tariffs to sales taxes
 SAM('STAX',REMIMP) = SAM('STAX',REMIMP) + SAM('MTAX',REMIMP);
 SAM('MTAX',REMIMP) = 0;
 SAM('GOV','STAX')    = SUM(ACNT, SAM('STAX',ACNT));
 SAM('GOV','DTAX')    = SUM(ACNT, SAM('DTAX',ACNT));

*Remove any small imbalances that remain
 SAM(C,'DSTK') = SUM(ACNT, SAM(ACNT,C) - SAM(C,ACNT)) + SAM(C,'DSTK');
 SAM('DSTK','S-I') = SUM(ACNT, SAM(ACNT,'DSTK'));
 SAM('S-I','ROW') = SAM('S-I','ROW') + SUM(ACNT, SAM(ACNT,'S-I')-SAM('S-I',ACNT));

*Remove re-exports ---------------------------------

*Remove re-exports from SAM (eliminates exports by reducing imports)
 SAM('ROW',REEXPORT) = SAM('ROW',REEXPORT) - SAM(REEXPORT,'ROW') + SUM(A, SAM(A,REEXPORT));
 SAM(REEXPORT,'ROW') = SUM(A, SAM(A,REEXPORT));



*Country-specific

****************************
*Compute transaction cost margins ------------------
*$ontext
******** Askar correction
PARAMETER
DOMX(C)    domestic production, total, net of TRC
DOMSALX(C) domestic sales of domestically produced, net of TRC
EXPX(C) export, net of TRC
IMPX(C) import, net of TRC
m(c) transaction MARGIN (implied to be the same for domestic, import and export)
TRSHR(C)
;

DOMX(C)$SUM(ARG, SAM(ARG,C)) = SUM(ARG, SAM(ARG,C)) ;
IMPX(C)$SAM('ROW',C) = SAM('ROW',C)  + SAM('mtax',C) ;

m(C)$SAM('TRC',C)  = SAM('TRC',C)/ (
  IMPX(C)+
  DOMX(C)
  );

EXPX(C)$SAM(C,'ROW') =(SAM(C,'ROW') - SAM('etax',C))  / (1 + m(c)) ;

DOMSALX(C)$SUM(ARG, SAM(ARG,C)) =  DOMX(C) - EXPX(C);

SAM(CTE,C)$EXPX(C)    = EXPX(C) * m(C);
SAM(CTM,C)$IMPX(C)    = IMPX(C) * m(C);
SAM(CTD,C)$DOMSALX(C) = DOMSALX(C) * m(C);

TRSHR(C)$SUM(CP, SAM(CP,'TRC')) = SAM(C,'TRC')/SUM(CP, SAM(CP,'TRC'));

SAM(C,CTE) = TRSHR(C) * SUM(CP, SAM(CTE,CP));
SAM(C,CTM) = TRSHR(C) * SUM(CP, SAM(CTM,CP));
SAM(C,CTD) = TRSHR(C) * SUM(CP, SAM(CTD,CP));

SAM('TRC',AC) = 0;
SAM(AC,'TRC') = 0;
*$offtext

* current approach
$ontext
PARAMETER EXPX(C), IMPX(C), DOMX(C), TRSHR(C);

 SAM('TRC',C) = SAM('TRD',C) + SAM('TRE',C) + SAM('TRM',C) + SAM('TRC',C);
 SAM(C,'TRC') = SAM(C,'TRD') + SAM(C,'TRE') + SAM(C,'TRM') + SAM(C,'TRC');
 SAM('TRD',C) = 0;
 SAM('TRE',C) = 0;
 SAM('TRM',C) = 0;
 SAM(C,'TRD') = 0;
 SAM(C,'TRE') = 0;
 SAM(C,'TRM') = 0;

* EXPX(C) = SAM(C,'ROW')/(1+(SAM('TRC',C) / (SUM(ARG, SAM(ARG,C)) + SAM('ROW',C))));
 EXPX(C) = SAM(C,'ROW');
 IMPX(C) = SAM('ROW',C);
 DOMX(C) = SUM(ARG, SAM(ARG,C));
 IMPX(C)$(IMPX(C)+DOMX(C)) = IMPX(C) + IMPX(C)/(IMPX(C)+DOMX(C)) * SAM('TRC',C);
 DOMX(C)$(IMPX(C)+DOMX(C)) = DOMX(C) + DOMX(C)/(IMPX(C)+DOMX(C)) * SAM('TRC',C);
 DOMX(C)$DOMX(C) = SUM(ACNT, SAM(C,ACNT)) - EXPX(C) - IMPX(C);

*Country-specific
* EXPX('CPETR') = 0;
* DOMX('CPETR') = 0;
* IMPX('CPETR') = 1;
* EXPX('CNGAS') = 1;
* DOMX('CNGAS') = 0;
* IMPX('CNGAS') = 0;
* SAM('CNGAS','ROW') = SAM('CNGAS','ROW') - SAM('ROW','CNGAS');
* SAM('ROW','CNGAS') = 0;

 TRSHR(C)$SUM(CP, SAM(CP,'TRC')) = SAM(C,'TRC')/SUM(CP, SAM(CP,'TRC'));

 SAM(CTE,C)$(EXPX(C) + IMPX(C) + DOMX(C)) = EXPX(C) / (EXPX(C) + IMPX(C) + DOMX(C)) * SAM('TRC',C);
 SAM(CTM,C)$(EXPX(C) + IMPX(C) + DOMX(C)) = IMPX(C) / (EXPX(C) + IMPX(C) + DOMX(C)) * SAM('TRC',C);
 SAM(CTD,C)$(EXPX(C) + IMPX(C) + DOMX(C)) = DOMX(C) / (EXPX(C) + IMPX(C) + DOMX(C)) * SAM('TRC',C);
 SAM(C,CTE) = TRSHR(C) * SUM(CP, SAM(CTE,CP));
 SAM(C,CTM) = TRSHR(C) * SUM(CP, SAM(CTM,CP));
 SAM(C,CTD) = TRSHR(C) * SUM(CP, SAM(CTD,CP));

 SAM('TRC',AC) = 0;
 SAM(AC,'TRC') = 0;
$offtext
*********************
*Scale SAM and check totals ------------------------

*Scaling
 SAM(AC,ACP) =  SAM(AC,ACP) * SCALE;
*XD
*Converting to $USD
 SAM(AC,ACP) =  SAM(AC,ACP)/XRATE ;
*Check totals
 SAM('TOTAL',AC) = 0;
 SAM(AC,'TOTAL') = 0;
 SAM('TOTAL',AC) = SUM(ACNT, SAM(ACNT,AC));
 SAM(AC,'TOTAL') = SUM(ACNT, SAM(AC,ACNT));
 SAMBALCHK(AC)   = SAM('TOTAL',AC) - SAM(AC,'TOTAL');
 SAMBALCHK(AC)$(abs( SAMBALCHK(AC)) lt 1e-6) = 0;

*Check for activities with no output
SET
 ACHK(A)         Activities with no output
 FCHK(F)         Factors with no values
 HCHK(H)         Households with no values
 CCHK(C)         Commodities with no values
;

 ACHK(A)$(SUM((ARG,RG)$MARG(ARG,A,RG), SAM(ARG,'TOTAL')) EQ 0) = YES;
 FCHK(F)$(SAM(F,'TOTAL') EQ 0) = YES;
 HCHK(H)$(SAM(H,'TOTAL') EQ 0) = YES;
 CCHK(C)$(SAM(C,'TOTAL') EQ 0) = YES;

SET ADDTRADE(C) /

/;

 SAM('ROW',ADDTRADE) = SAM('ROW',ADDTRADE) + 0.15*SUM(A, SAM(A,ADDTRADE));
 SAM(ADDTRADE,'ROW') = SAM(ADDTRADE,'ROW') + 0.15*SUM(A, SAM(A,ADDTRADE));

*-----------------------------------------------------------------------------
*3. ELASTICITIES
*-----------------------------------------------------------------------------

*Trade elasticities --------------------------------

 PRODELAS(ARG)     = PRODELASTAB(ARG,'PRODELAS');
 PRODELAS2(ARG)    = PRODELASTAB(ARG,'PRODELAS2');
 PRODELAS3(F,ARG)  = 1.5;
 ELASAC(C)         = 10.0;
 ELASCA(A)         = 0.5;
 ELASCES(A)        = 0.5;

*----------------------------------------------------------------------------
*4. PHYSICAL FACTOR QUANTITIES AND FACTOR MARKET STRUCTURES
*----------------------------------------------------------------------------

*Make regional production consistent with national data
* PRODUCTION(ARG)$SAM('TOTAL',ARG) = SUM((A,RG)$MARG(ARG,A,RG), PRODDATA(A,RG,'PROD'))/PRODADJ;
 PRODUCTION(ARG)$(SAM('TOTAL',ARG) AND SUM(FLND, SAM(FLND,ARG))) = SUM((A,RG)$MARG(ARG,A,RG), PRODDATA(A,RG,'PROD'))/PRODADJ_CRP;
 PRODUCTION(ARG)$(SAM('TOTAL',ARG) AND SUM(FLIV, SAM(FLIV,ARG))) = SUM((A,RG)$MARG(ARG,A,RG), PRODDATA(A,RG,'PROD'))/PRODADJ_LIV;

 QFBASE(FLND,ARG)$SAM('TOTAL',ARG) = SUM((A,RG)$MARG(ARG,A,RG), PRODDATA(A,RG,'AREA'))/AREAADJ_CRP;
 QFBASE(FLIV,ARG)$SAM('TOTAL',ARG) = SUM((A,RG)$MARG(ARG,A,RG), PRODDATA(A,RG,'AREA'))/AREAADJ_LIV;

*Labor
IF (FCALTYPE EQ 2,
 QFBASE(FLAB,ARG)$(SAM(FLAB,ARG) AND SUM(AL$MALA(AL,ARG), WFINPUT(AL,FLAB)))
         = SAM(FLAB,ARG) / (SUM(AL$MALA(AL,ARG), WFINPUT(AL,FLAB)) / WAGEADJ);
);
IF (FCALTYPE EQ 3,
 QFBASE(FLAB,ARG)$SAM(FLAB,ARG) = SUM(AL$MALA(AL,ARG), QFINPUT(AL,FLAB)) / EMPLADJ;
);

*Default: SAM value
 QFBASE(F,ARG)$(NOT QFBASE(F,ARG)) = SAM(F,ARG);
* QFBASE(F,ARG)$(NOT QFBASE(F,ARG)) = 1/0;
* QFBASE(F,ARG) = SAM(F,ARG);
 QFBASE(F,ARG)$(NOT SAM(F,ARG)) = 0;

 QFSBASE(F) = SUM(ARG, QFBASE(F,ARG));

*-----------------------------------------------------------------------------
*5. COMMODITY VALUE SHARES FOR HOME CONSUMPTION
*-----------------------------------------------------------------------------

 hpop0(H) = POPTAB(H)/1000;
 hpop(H) = hpop0(H);

*If needed, manually define shrhome.
 shrhome(A,H) = 0;

*-----------------------------------------------------------------------------
*6. INITIALIZATION OF TAX DATA
*-----------------------------------------------------------------------------

*direct taxes on domestic institutions
 TAXPAR('INSTAX',INSD)  = SAM('DTAX',INSD);
*direct factor taxes
 TAXPAR('FACTAX',F)     = SAM('FTAX',F);
*import taxes
 TAXPAR('IMPTAX',C)     = SAM('MTAX',C);
*export taxes
 TAXPAR('EXPTAX',C)     = SAM('ETAX',C);
*value-added taxes
 TAXPAR('VATAX',ARG)    = 0;
*taxes on activity revenue
 TAXPAR('ACTTAX',ARG)   = SAM('ATAX',ARG);
*taxes on commodity sales in domestic market
 TAXPAR('COMTAX',C)     = SAM('STAX',C);
