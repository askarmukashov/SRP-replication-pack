$STITLE Input file: 1REPBASE.INC. Report on base solution. Standard CGE modeling system

$ontext

This is an include file to MOD102.GMS that produces the following reports
on the economy based on data for the base solution (with the name of the
relevant parameter in parentheses):
1. Economic structure table (STRUCBASE)
2. GDP table (GDPBASE)
3. Macro SAM (MACROSAM)
4. Input shares, factor wages, and elasticities (FACTAB1 - 4)
5. Household incomes (HHDTAB1 - 2)

For STRUCBASE, note that both activity and commodity data is presented
under commodity headings.

$offtext


*1. Economic structure table (STRUCBASE)=============================

SETS

 STRROW  Rows (only used to control order in display)
 /
 TOTAL-1
 TAGR
 TNAGR
 TOTAL-2
 /

 STRCOL  Columns
 /
 VAshr       value-added share (%)
 PRDshr      production share (%)
 EMPshr      share in total employment (%)
 EXPshr      sector share in total exports (%)
 EXP-OUTshr  exports as share in sector output (%)
 IMPshr   sector share in total imports (%)
 IMP-DEMshr  imports as share of domestic demand (%)

* Askar
 VA       value-added value
 DEM      demand value
 IMP      import value
 EXP      export value
 OUT      output value
 /

 STRCOL2(STRCOL)  Columns for summation operation
/
 VAshr    value-added share (%)
 PRDshr   production share (%)
 EMPshr   share in total employment (%)
 EXPshr   sector share in total exports (%)
 IMPshr   sector share in total imports (%)
/
;

SET
 MAPAC(A,C) mapping from activities to commodities
 ;

MAPAC(A,C)$SAM(A,C) = YES;

DISPLAY MAPAC;

PARAMETER
 STRUCBASE(*,STRCOL)  Economic structure in the base
 ;

*Note: For VAshr, PRDshr, and EMPshr, data scaled by share of activity
*in output of commodity to avoid double-counting when there is not a
*one-to-one mapping between activities and commodities.

 STRUCBASE(C,'VAshr')
   = 100*SUM(A$MAPAC(A,C),
     (PXAC.L(A,C)*QXAC.L(A,C)/SUM(CP, PXAC.L(A,CP)*QXAC.L(A,CP)))*
     SUM(RG, PVA.L(A,RG)*(1-tva(A,RG))*QVA.L(A,RG))
     /SUM((AP,RG), PVA.L(AP,RG)*(1-tva(AP,RG))*QVA.L(AP,RG)) );

 STRUCBASE(C,'PRDshr')
   = 100*SUM(A$MAPAC(A,C),
     (PXAC.L(A,C)*QXAC.L(A,C)/SUM(CP, PXAC.L(A,CP)*QXAC.L(A,CP)))
     * SUM(RG, PAR.L(A,RG)*QAR.L(A,RG))/SUM((AP,RG), PAR.L(AP,RG)*QAR.L(AP,RG)) );

 STRUCBASE(C,'EMPshr')$SUM(F, FLAB(F))
   = 100*SUM(A$MAPAC(A,C),
        (PXAC.L(A,C)*QXAC.L(A,C)/SUM(CP, PXAC.L(A,CP)*QXAC.L(A,CP)))
       *SUM((F,RG)$FLAB(F), QF.L(F,A,RG))/
                                  SUM((F,AP,RG)$FLAB(F), QF.L(F,AP,RG)) );

 STRUCBASE(C,'EXPshr')
   = 100*pwe(C)*QE.L(C)/SUM(CP, pwe(CP)*QE.L(CP));

 STRUCBASE(C,'IMPshr')
   = 100*pwm(C)*QM.L(C)/SUM(CP, pwm(CP)*QM.L(CP));

 STRUCBASE(C,'EXP-OUTshr')$(PX.L(C)*QX.L(C))
   = 100*PE.L(C)*QE.L(C)/(PX.L(C)*QX.L(C));

 STRUCBASE('TOTAL-1','EXP-OUTshr')
   = 100*SUM(C, PE.L(C)*QE.L(C))/SUM(CP, PX.L(CP)*QX.L(CP)) ;

 STRUCBASE(C,'IMP-DEMshr')$(PQ.L(C)*(1 - tq(C))*QQ.L(C))
   = 100*PM.L(C)*QM.L(C)/(PQ.L(C)*(1 - tq(C))*QQ.L(C));

 STRUCBASE('TOTAL-1','IMP-DEMshr')
   = 100*SUM(C, PM.L(C)*QM.L(C))
                            /SUM(CP, PQ.L(CP)*(1 - tq(CP))*QQ.L(CP));

 STRUCBASE('TOTAL-1',STRCOL2) = SUM(C, STRUCBASE(C,STRCOL2)) ;

* Askar
STRUCBASE(C,'VA')
  = SUM(A$MAPAC(A,C),
    (PXAC.L(A,C)*QXAC.L(A,C)/SUM(CP, PXAC.L(A,CP)*QXAC.L(A,CP)))*
    SUM(RG, PVA.L(A,RG)*(1-tva(A,RG))*QVA.L(A,RG))
    );
STRUCBASE(C,'DEM')$(PQ.L(C)*(1 - tq(C))*QQ.L(C))
  = (PQ.L(C)*(1 - tq(C))*QQ.L(C));
STRUCBASE(C,'IMP')$(PQ.L(C)*(1 - tq(C))*QQ.L(C))
  = PM.L(C)*QM.L(C);

  STRUCBASE(C,'EXP')$(PX.L(C)*QX.L(C))
    = PE.L(C)*QE.L(C);
    STRUCBASE(C,'OUT')$(PX.L(C)*QX.L(C))
      = (PX.L(C)*QX.L(C));

*=======

OPTION STRUCBASE:1;
DISPLAY STRUCBASE ;


*2. GDP table (GDPBASE===============================================

SETS

 IGDPX  Items for GDP and national accounts
  /
  ABSORP   absorption
  PRVCON   private consumption
  FIXINV   fixed investment
  DSTOCK   stock change
  GOVCON   government consumption
  EXPORTS  exports
  IMPORTS  imports
  GDPMP    GDP at market prices (alt. 1: spending)
  GDPMP2   GDP at market prices (alt. 2: value-added)
  NETITAX  net indirect taxes
  GDPFC2   GDP at factor cost
  /

 IGDPXX(IGDPX)  Items for GDPMP summation
  /
  PRVCON   private consumption
  FIXINV   fixed investment
  DSTOCK   stock change
  GOVCON   government consumption
  EXPORTS  exports
  IMPORTS  imports
  /

 KGDPX  second index in GDP tables
 /
 VALUE, PERC-GDP, PERC-TOT
 /

*================================

PARAMETERS
 GDPBASE(IGDPX,KGDPX) Aggregate national accounts summary
 GDPERR                error if alt GDP definitions are not identical
 ;
*================================

 GDPBASE('PRVCON','VALUE') = SUM((C,H), PQ.L(C)*QH.L(C,H)*hpop(H)) + SUM((A,H), PA.L(A)*QHA.L(A,H)*hpop(H));

 GDPBASE('FIXINV','VALUE')  =  SUM(C, PQ.L(C)*QINV.L(C));
 GDPBASE('DSTOCK','VALUE')  =  SUM(C, PQ.L(C)*QDST(C));
 GDPBASE('GOVCON','VALUE')  =  SUM(C, PQ.L(C)*QG.L(C));
 GDPBASE('EXPORTS','VALUE') =  SUM(CE, PWE(CE)*EXR.L*QE.L(CE));
 GDPBASE('IMPORTS','VALUE') = -SUM(CM, PWM(CM)*EXR.L*QM.L(CM));
 GDPBASE('GDPMP','VALUE')   =  SUM(IGDPXX, GDPBASE(IGDPXX,'VALUE'));

 GDPBASE('ABSORP','VALUE')  = GDPBASE('GDPMP','VALUE') - GDPBASE('IMPORTS','VALUE') - GDPBASE('EXPORTS','VALUE');

 GDPBASE('GDPFC2','VALUE') = SUM((A,RG), PVA.L(A,RG)*(1-tva(A,RG))*QVA.L(A,RG));

 GDPBASE('NETITAX','VALUE') =
            SUM((A,RG), ta(A,RG)*PAR.L(A,RG)*QAR.L(A,RG))
          + SUM((A,RG), tva(A,RG)*PVA.L(A,RG)*QVA.L(A,RG))
          + SUM(CM, tm(CM)*QM.L(CM)*PWM(CM))*EXR.L
          + SUM(CE, te(CE)*QE.L(CE)*PWE(CE))*EXR.L
          + SUM(C, tq(C)*PQ.L(C)*QQ.L(C))
*         + SUM((F,A), TFA.L(F,A)*WFDIST.L(F,A)*QF.L(F,A))
  ;

 GDPBASE('GDPMP2','VALUE')  = GDPBASE('GDPFC2','VALUE') + GDPBASE('NETITAX','VALUE');

 GDPERR$(ABS(GDPBASE('GDPMP2','VALUE') - GDPBASE('GDPMP','VALUE')) GT 0.00001) = 1/0;

 GDPBASE(IGDPX,'PERC-GDP')$GDPBASE('GDPMP','VALUE') = 100*GDPBASE(IGDPX,'VALUE')/GDPBASE('GDPMP','VALUE');

*==================

OPTION GDPBASE:6;
DISPLAY GDPERR, GDPBASE;


*3. Macro SAM (MACSAM)===============================================


SET
 ACMAC     macrosam accounts
 /
 ACT2        Activities
 COM2        Commodities
 FAC2        Factors
 HOU2        "Domestic non-gov institution (households + enterprises)"
 GOV2        Current government
 ROW2        Rest of World
 S-I2        Savings investment account
 INSTAX2     direct taxes on domestic institutions
 FACTAX2     direct factor taxes
 IMPTAX2     import taxes
 EXPTAX2     export taxes
 VATAX2      value-added taxes
 ACTTAX2     indirect taxes on activity revenue
 COMTAX2     indirect taxes on commodity sales in domestic market
 TOTAL2      totals
 /

 ACMACNT(ACMAC)  all macrosam accounts except TOTAL2
 ACMACTAX(ACMAC) tax accounts
 /
 INSTAX2     direct taxes on domestic institutions
 FACTAX2     direct factor taxes
 IMPTAX2     import taxes
 EXPTAX2     export taxes
 VATAX2      value-added taxes
 ACTTAX2     indirect taxes on activity revenue
 COMTAX2     indirect taxes on commodity sales in domestic market
 /
 ;

ACMACNT(ACMAC)    = YES;
ACMACNT('TOTAL2') = NO;

ALIAS(ACMAC,ACMACP),(ACMACNT,ACMACNTP);

PARAMETER
 MACROSAM(ACMAC,ACMACP) Macro SAM
 BALCHK2(ACMAC)         total balance check
 ;

*Defining SAM cells==============

 MACROSAM('COM2','ACT2')    = SUM((C,A),  PQ.L(C)*QINT.L(C,A));
 MACROSAM('FAC2','ACT2')    = SUM((A,RG), PVA.L(A,RG)*(1-tva(a,rg))*QVA.L(A,RG));
 MACROSAM('VATAX2','ACT2')  = SUM((A,RG), tva(A,RG)*PVA.L(A,RG)*QVA.L(A,RG));
 MACROSAM('ACTTAX2','ACT2') = SUM((A,RG), ta(A,RG)*PAR.L(A,RG)*QAR.L(A,RG));

 MACROSAM('COM2','COM2')    = SUM(C, PQ.L(C)*QT.L(C));
 MACROSAM('ACT2','COM2')    = SUM((C,A), QXAC.L(A,C)*PXAC.L(A,C));
 MACROSAM('ROW2','COM2')    = SUM(C, pwm(C)*QM.L(C))*EXR.L;
 MACROSAM('COMTAX2','COM2') = SUM(C, tq(C)*PQ.L(C)*QQ.L(C));
 MACROSAM('IMPTAX2','COM2') = SUM(C, tm(C)*pwm(C)*QM.L(C))*EXR.L;
 MACROSAM('EXPTAX2','COM2') = SUM(C, te(C)*pwe(C)*QE.L(C))*EXR.L;

 MACROSAM('HOU2','FAC2')    = SUM((INSDNG,F), YIF.L(INSDNG,F));
 MACROSAM('GOV2','FAC2')    = SUM(F, YIF.L('GOV',F)) ;
*   + SUM((F,A), TFA.L(F,A)*WF.L(F)*WFDIST.L(F,A)*QF.L(F,A)) ;
 MACROSAM('ROW2','FAC2')    = SUM(F, trnsfr('ROW',F))*EXR.L + sum(f, (1-tf(f))*RF(F)*YF.l(F));
 MACROSAM('FACTAX2','FAC2') = SUM(f, tf(f)*YF.L(F));

 MACROSAM('COM2','HOU2')    = SUM((C,H), PQ.L(C)*QH.L(C,H)*hpop(H));
 MACROSAM('ACT2','HOU2')    = SUM((A,H), PA.L(A)*QHA.L(A,H)*hpop(H));
 MACROSAM('INSTAX2','HOU2') = SUM(INSDNG, TINS.L(INSDNG)*YI.L(INSDNG));
 MACROSAM('HOU2','HOU2')    = SUM((INSDNG,INSDNGP), TRII.L(INSDNG,INSDNGP));
 MACROSAM('S-I2','HOU2')    =
    SUM(INSDNG, MPS.L(INSDNG) * (1 - TINS.L(INSDNG)) * YI.L(INSDNG));

 MACROSAM('COM2','GOV2')    = SUM(C, PQ.L(C)*QG.L(C));
 MACROSAM('HOU2','GOV2')    = SUM(INSD, trnsfr(INSD,'GOV'))*CPI.L;
 MACROSAM('S-I2','GOV2')    = GSAV.L ;

 MACROSAM('COM2','ROW2')    = SUM(C, pwe(C)*QE.L(C))*EXR.L;
 MACROSAM('HOU2','ROW2')    = SUM(INSDNG, trnsfr(INSDNG,'ROW'))*EXR.L;
 MACROSAM('GOV2','ROW2')    = trnsfr('GOV','ROW')*EXR.L;
 MACROSAM('S-I2','ROW2')    = FSAV.L*EXR.L ;

 MACROSAM('COM2','S-I2')
  = SUM(C, PQ.L(C)*QINV.L(C)) + SUM(C, PQ.L(C)*qdst(C));

 MACROSAM('GOV2',ACMACTAX)  = SUM(ACMAC, MACROSAM(ACMACTAX,ACMAC));


*Computing account totals========

 MACROSAM('TOTAL2', ACMAC)    = 0;
 MACROSAM(ACMAC,'TOTAL2')     = 0;

 MACROSAM('TOTAL2', ACMACNTP) = SUM(ACMACNT,  MACROSAM(ACMACNT,ACMACNTP));
 MACROSAM(ACMACNT,'TOTAL2')   = SUM(ACMACNTP, MACROSAM(ACMACNT,ACMACNTP));


*Checking account balances=======

 BALCHK2(ACMACNT) = MACROSAM('TOTAL2',ACMACNT) - MACROSAM(ACMACNT,'TOTAL2');

PARAMETER
 MACCHK  error message if Macro SAM does not balance
 ;

 MACCHK(ACMACNT)$(ABS(BALCHK2(ACMACNT)) GT 0.00001) = 1/0;
*If error, either correct the SAM or exclude the include file.

*==========

OPTION MACROSAM:2;
DISPLAY BALCHK2, MACCHK, MACROSAM;


*4. Input shares, factor wages, and elasticities (FACTAB1 - 4)=======

*The parameters FACTAB1, FACTAB2, FACTAB3, FACTAB4 are covered in this
*section.


SETS
 FF(AC)   all factor accounts and total
 A2(AC)   all activity accounts and total
 RG2(AC)  all regional accounts and total
 ;

 FF(F)        = YES;
 FF('TOTAL')  = YES;
 A2(A)        = YES;
 A2('TOTAL')  = YES;
 RG2(RG)      = yes ;
 RG2("total") = yes ;


PARAMETERS
 FACTAB1(AC,ACP) Factor payment shares within sector
 FACTAB2(AC,ACP) Factor payment shares across sectors
 FACTAB3(AC,*)   Agg factor & intd. input shares & their sub. elasticity
 FACTAB4(AC,ACP) Factor wages by sector
 FACTAB5(AC,AC)  Factor payment shares across regions
 FACTAB6(AC,AC)  Factor payment shares within regions
 ;

 FACTAB1(A,F)       = SUM(RG, WF.L(F)*wfdist.L(F,A,RG)*QF.L(F,A,RG)) ;
 FACTAB1('TOTAL',F) = SUM(A, FACTAB1(A,F)) ;
 FACTAB1(A,'TOTAL') = SUM(F, FACTAB1(A,F)) ;
 FACTAB1('TOTAL','TOTAL') = SUM(F, FACTAB1('TOTAL',F)) ;
 FACTAB2(A2,FF)    = FACTAB1(A2,FF) ;
 FACTAB4(A2,FF)    = FACTAB1(A2,FF) ;

 FACTAB2(A2,FF)$FACTAB2('TOTAL',FF) = 100*FACTAB2(A2,FF)/FACTAB2('TOTAL',FF) ;
 FACTAB1(A2,FF)$FACTAB1(A2,'TOTAL') = 100*FACTAB1(A2,FF)/FACTAB1(A2,'TOTAL') ;

 FACTAB3(A,'QVASHR')$(NOT ACHK(A))  = 100*SUM(RG, PVA.L(A,RG)*QVA.L(A,RG)) /
                             SUM(RG, PAR.L(A,RG)*(1-ta(A,RG))*QAR.L(A,RG)) ;
 FACTAB3(A,'QINTSHR')$(NOT ACHK(A)) = 100*SUM(RG, PINTA.L(A,RG)*QINTA.L(A,RG))/
                             SUM(RG, PAR.L(A,RG)*(1-ta(A,RG))*QAR.L(A,RG)) ;
 FACTAB3('TOTAL','QVASHR')  = 100*SUM((A,RG), PVA.L(A,RG)*QVA.L(A,RG)) /
                             SUM((A,RG), PAR.L(A,RG)*(1-ta(A,RG))*QAR.L(A,RG)) ;
 FACTAB3('TOTAL','QINTSHR') = 100*SUM((A,RG), PINTA.L(A,RG)*QINTA.L(A,RG)) /
                             SUM((A,RG), PAR.L(A,RG)*(1-ta(A,RG))*QAR.L(A,RG)) ;
 FACTAB3(A,'ACES')            = ACES2(A) ;
 FACTAB3(A,'SIGMAQA')$ACES2(A) = PRODELAS2(A) ;
 FACTAB3(A,'SIGMAVA')         = PRODELAS(A) ;

 FACTAB4(A2,'TOTAL')          = 0 ;
 FACTAB4(A,F)$SUM(RG, QF.L(F,A,RG)) =
               FACTAB4(A,F)/SUM(RG, QF.L(F,A,RG)) ;
 FACTAB4('TOTAL',F)$SUM((A,RG), QF.L(F,A,RG)) =
               FACTAB4('TOTAL',F)/SUM((A,RG), QF.L(F,A,RG)) ;

 FACTAB5(F,RG)             = SUM(A, WF.L(F)*wfdist.L(F,A,RG)*QF.L(F,A,RG)) ;
 FACTAB5("total",RG)       = SUM(F, FACTAB5(F,RG)) ;
 FACTAB5(FF,"total")       = SUM(RG, FACTAB5(FF,RG)) ;
 FACTAB6(FF,RG)            = FACTAB5(FF,RG) ;
 FACTAB6(FF,"TOTAL")       = SUM(RG, FACTAB5(FF,RG)) ;

 FACTAB5(FF,RG2)$FACTAB5("total",RG2)= 100*FACTAB5(FF,RG2)/FACTAB5("total",RG2) ;
 FACTAB6(FF,RG2)$FACTAB6(FF,"total") = 100*FACTAB6(FF,RG2)/FACTAB6(FF,"total")

 OPTION FACTAB1:1, FACTAB2:1, FACTAB3:1, FACTAB4:3, FACTAB5:1, FACTAB6:1 ;
 Display FACTAB1, FACTAB2, FACTAB3, FACTAB4, FACTAB5, FACTAB6 ;


*5. Household incomes (HHDTAB1 - 2)==================================

PARAMETERS
 HHDTAB1(AC,ACP) Share of item ACP in the total income of hhd H (%)
 HHDTAB2(AC,ACP) Share of hhd AC in total hhd income from item ACP (%)
 ;

 HHDTAB1(H,ACNT)$(NOT HCHK(H)) =
    100*SAM(H,ACNT)/SUM(ACNTP, SAM(H,ACNTP));
 HHDTAB1('TOTAL',ACNT)         =
    100*SUM(H, SAM(H,ACNT))/SUM((H,ACNTP), SAM(H,ACNTP));
 HHDTAB1(AC,'TOTAL')           = SUM(ACNT, HHDTAB1(AC,ACNT));

 HHDTAB2(H,ACNT)$SUM(HP, SAM(HP,ACNT)) =
    100*SAM(H,ACNT)/SUM(HP, SAM(HP,ACNT));
 HHDTAB2(H,'TOTAL')            = 100*SUM(ACNT, SAM(H,ACNT))/
                                     SUM((HP,ACNT), SAM(HP,ACNT));

 HHDTAB2('TOTAL',AC)           = SUM(H, HHDTAB2(H,AC));

 OPTION  HHDTAB1:1, HHDTAB2:1;
 DISPLAY HHDTAB1, HHDTAB2;


*#*#*#*#*# THE END OF 1REPBASE.INC #*#*#*#*
