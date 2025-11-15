*** report part - select only necessary parts
*------------------------------------------------------------------------------
*1. Gross domestic product
*------------------------------------------------------------------------------

*GDP at factor cost
* GDPX(%1, %2,'aa_GDP')  = SUM((A,RG)$ACTAGG(A,RG,'aa_GDP'), PVA0(A,RG)*QVA.l(A,RG));
* GDPX(%1, %2,'aa_AGR')  = SUM((A,RG)$ACTAGG(A,RG,'aa_AGR'), PVA0(A,RG)*QVA.l(A,RG));
 GDPX(%1, %2,'aa_IND')  = SUM((A,RG)$ACTAGG(A,RG,'aa_IND'), PVA0(A,RG)*QVA.l(A,RG));
 GDPX(%1, %2,'aa_MAN')  = SUM((A,RG)$ACTAGG(A,RG,'aa_MAN'), PVA0(A,RG)*QVA.l(A,RG));
 GDPX(%1, %2,'aa_SRV')  = SUM((A,RG)$ACTAGG(A,RG,'aa_SRV'), PVA0(A,RG)*QVA.l(A,RG));


*GDP by expenditure groups
*  Real
 GDPMP_REAL('PRVCON',%1, %2)     = SUM((C,H), PQ0(C)*QH.l(C,H)*hpop(H)) + SUM((A,H), PA0(A)*QHA.l(A,H)*hpop(H));
 GDPMP_REAL('FIXINV',%1, %2)     = SUM(C, PQ0(C)*QINV.l(C));
 GDPMP_REAL('DSTOCK',%1, %2)     = SUM(C, PQ0(C)*qdst0(C));
 GDPMP_REAL('GOVCON',%1, %2)     = SUM(C, PQ0(C)*QG.l(C));
 GDPMP_REAL('EXPORTS',%1, %2)    = SUM(C, pwe0(C)*EXR0*QE.l(C));
 GDPMP_REAL('IMPORTS',%1, %2)    = -SUM(C, pwm0(C)*EXR0*QM.l(C));
 GDPMP_REAL('GDPMP',%1, %2)      = GDPMP_REAL('PRVCON',%1, %2) +
                                 GDPMP_REAL('FIXINV',%1, %2)  +
                                 GDPMP_REAL('DSTOCK',%1, %2) +
                                 GDPMP_REAL('GOVCON',%1, %2) +
                                 GDPMP_REAL('EXPORTS',%1, %2) +
                                 GDPMP_REAL('IMPORTS',%1, %2);
 GDPMP_REAL('ABSORP',%1, %2)     = GDPMP_REAL('GDPMP',%1, %2)  -
                                 GDPMP_REAL('IMPORTS',%1, %2) -
                                 GDPMP_REAL('EXPORTS',%1, %2);
 GDPMP_REAL('GDPFC2',%1, %2)     = SUM((RG,A), PVA0(A,RG)*(1-tva0(A,RG))*QVA.l(A,RG));
 GDPMP_REAL('NETITAX',%1, %2)    = GDPMP_REAL('GDPMP',%1, %2)  -
                               GDPMP_REAL('GDPFC2',%1, %2);
 GDPMP_REAL('GDPMP2',%1, %2)     = GDPMP_REAL('GDPFC2',%1, %2) +
                               GDPMP_REAL('NETITAX',%1, %2);

 GDPMP_NOMINAL('PRVCON',%1, %2)             = SUM((C,H), PQ.l(C)*QH.l(C,H)*hpop(H)) +
  SUM((A,H), PA.l(A)*QHA.l(A,H)*hpop(H));
 GDPMP_NOMINAL('FIXINV',%1, %2)             = SUM(C, PQ.l(C)*QINV.l(C));
 GDPMP_NOMINAL('DSTOCK',%1, %2)             = SUM(C, PQ.l(C)*QDST(C));
 GDPMP_NOMINAL('GOVCON',%1, %2)             = SUM(C, PQ.l(C)*QG.l(C));
 GDPMP_NOMINAL('EXPORTS',%1, %2)            = SUM(CE, pwe(CE)*EXR.l*QE.l(CE));
 GDPMP_NOMINAL('IMPORTS',%1, %2)            = -SUM(CM, pwm(CM)*EXR.l*QM.l(CM));
 GDPMP_NOMINAL('GDPMP',%1, %2) =  GDPMP_NOMINAL('PRVCON',%1, %2) +
                                 GDPMP_NOMINAL('FIXINV',%1, %2)  +
                                 GDPMP_NOMINAL('DSTOCK',%1, %2) +
                                 GDPMP_NOMINAL('GOVCON',%1, %2) +
                                 GDPMP_NOMINAL('EXPORTS',%1, %2) +
                                 GDPMP_NOMINAL('IMPORTS',%1, %2);
 GDPMP_NOMINAL('ABSORP',%1, %2)     = GDPMP_NOMINAL('GDPMP',%1, %2)  -
                                 GDPMP_NOMINAL('IMPORTS',%1, %2) -
                                 GDPMP_NOMINAL('EXPORTS',%1, %2);
 GDPMP_NOMINAL('GDPFC2',%1, %2)     = SUM((RG,A), PVA.l(A,RG)*(1-tva(A,RG))*QVA.l(A,RG));
 GDPMP_NOMINAL('NETITAX',%1, %2)    = GDPMP_NOMINAL('GDPMP',%1, %2)  -
                               GDPMP_NOMINAL('GDPFC2',%1, %2);
 GDPMP_NOMINAL('GDPMP2',%1, %2)     = GDPMP_NOMINAL('GDPFC2',%1, %2) +
                               GDPMP_NOMINAL('NETITAX',%1, %2);

 GDP_sec_realX(A, RG, %1, %2) =  PVA0(A,RG)*QVA.l(A,RG);
 PQX(C, %1, %2)      = PQ.l(C);


 GDP_CLIM_SH_shX(%1, %2, A_CLIM_SH) =  100*SUM(RG, PVA0(A_CLIM_SH,RG)*QVA.l(A_CLIM_SH,RG) ) /
  SUM((A_CLIM_SHP, RG), PVA0(A_CLIM_SHP ,RG)*QVA.l(A_CLIM_SHP ,RG));
FSAVX(%1, %2,'value') =  FSAV.l;
EXRX(%1, %2,'value') =  EXR.l;

QFSX(FCAP,%2) = QFS.l(FCAP);
 GDP_shX(%2, A) = GDP_sh(A);
 alphava_gr_X(%2, A,RG)$alphava0(A,RG) = (alphava(A,RG) / alphava_prev(A,RG)-1)*100;
 alphava_tot_grX(%2) = SUM((A,RG), GDP_sh(A) * alphava_gr_X(%2, A,RG));

*------------------------------------------------------------------------------
*2. Household consumption and welfare
*------------------------------------------------------------------------------

*Household population levels
 HHDP(HA) = SUM(H$HHDAGG(H,HA), hpop(H));

*Household real consumption
 HHDC(HA) = SUM((H,C)$HHDAGG(H,HA), PQ0(C)*QH.l(C,H)*hpop(H)) + SUM((H,A)$HHDAGG(H,HA), PA0(A)*QHA.l(A,H)*hpop(H));

*------------------------------------------------------------------------------
*3. International trade
*------------------------------------------------------------------------------

*Real value
 TRDE(RA) = SUM((A,C,RG)$(MAC(A,C) AND ACTAGG(A,RG,RA)), PE0(C)*QE.l(C));
 TRDM(RA) = SUM((A,C,RG)$(MAC(A,C) AND ACTAGG(A,RG,RA)), PM0(C)*QM.l(C));

*------------------------------------------------------------------------------
*4. Prices
*------------------------------------------------------------------------------

*Food and nonfood products
*  Index values
 PRC_FD('ALL')       = SUM(C, cwts(C) * PQ.l(C));
 PRC_FD('FOOD')      = SUM(C$R_CFOOD(C), cwts(C)/SUM(CP$R_CFOOD(CP), cwts(CP)) * PQ.l(C));
 PRC_FD('NON-FOOD')  = SUM(C$(NOT R_CFOOD(C)), cwts(C)/SUM(CP$(NOT R_CFOOD(CP)), cwts(CP)) * PQ.l(C));
 PRC_FD(FDAC)$PRC_FD0(FDAC) = PRC_FD(FDAC) / PRC_FD0(FDAC) * 100;
 PRC_FD('FOOD_REAL')$PRC_FD('ALL')  = PRC_FD('FOOD') / PRC_FD('ALL') * 100;




*------------------------------------------------------------------------------
*5. Agricultural GDP Plus (AgGDP+) and Employment Plus (AgEMP+)
*------------------------------------------------------------------------------

*--- Agricultural GDP Plus (AgGDP+) ---

*   Agriculture and agro-processing
 GDPX_PLUS(%1, %2,'AGR+') = SUM((R_AAGRI,RG), PVA0(R_AAGRI,RG) * QVA.l(R_AAGRI,RG));
 GDPX_PLUS(%1, %2,'PRC+') = SUM((R_APROC,RG), PVA0(R_APROC,RG) * QVA.l(R_APROC,RG));
 GDPX_PLUS(%1, %2,'DIR+') = GDPX_PLUS(%1, %2,'AGR+') + GDPX_PLUS(%1, %2,'PRC+');

*   Inputs into agriculture and agro-processing
  INPUT('AGR',R_CNAGR)$(QQ.l(R_CNAGR)+QE.l(R_CNAGR))
         = SUM(R_AAGRI, PQ0(R_CNAGR)*QINT.l(R_CNAGR,R_AAGRI)) / (PQ0(R_CNAGR)*QQ.l(R_CNAGR)+PE0(R_CNAGR)*QE.l(R_CNAGR)) * SUM((R_ANAGR,RG)$MAC(R_ANAGR,R_CNAGR), PVA0(R_ANAGR,RG) * QVA.l(R_ANAGR,RG));
 INPUT('PRC',R_CNAGR)$(QQ.l(R_CNAGR)+QE.l(R_CNAGR))
         = SUM(R_APROC, PQ0(R_CNAGR)*QINT.l(R_CNAGR,R_APROC)) / (PQ0(R_CNAGR)*QQ.l(R_CNAGR)+PE0(R_CNAGR)*QE.l(R_CNAGR)) * SUM((R_ANAGR,RG)$MAC(R_ANAGR,R_CNAGR), PVA0(R_ANAGR,RG) * QVA.l(R_ANAGR,RG));
 GDPX_PLUS(%1, %2,'INP_AGR+') = SUM(C, INPUT('AGR',C)) - SUM(R_CTRTR, INPUT('AGR',R_CTRTR));
 GDPX_PLUS(%1, %2,'INP_PRC+') = SUM(C, INPUT('PRC',C)) - SUM(R_CTRTR, INPUT('PRC',R_CTRTR));
 GDPX_PLUS(%1, %2,'INP+') = GDPX_PLUS(%1, %2,'INP_AGR+') + GDPX_PLUS(%1, %2,'INP_PRC+');

*   Trade and transport margins and intermediate demand
 TRADE('AGR')$SUM(R_CTRTR, QQ.l(R_CTRTR)+QE.l(R_CTRTR))
         = SUM((R_CTRTR,R_CAGRI), icm0(R_CTRTR,R_CAGRI)*PQ0(R_CTRTR)*QM.l(R_CAGRI)+ ice0(R_CTRTR,R_CAGRI)*PQ0(R_CTRTR)*QE.l(R_CAGRI)+ icd0(R_CTRTR,R_CAGRI)*PQ0(R_CTRTR)*QD.l(R_CAGRI))
         / SUM(R_CTRTR, PQ0(R_CTRTR)*QQ.l(R_CTRTR)+PQ0(R_CTRTR)*QE.l(R_CTRTR)) * SUM((R_ATRTR,RG), PVA0(R_ATRTR,RG) * QVA.l(R_ATRTR,RG));
 TRADE('PRC')$SUM(R_CTRTR, QQ.l(R_CTRTR)+QE.l(R_CTRTR))
         = SUM((R_CTRTR,R_CPROC), icm0(R_CTRTR,R_CPROC)*PQ0(R_CTRTR)*QM.l(R_CPROC)+ ice0(R_CTRTR,R_CPROC)*PQ0(R_CTRTR)*QE.l(R_CPROC)+ icd0(R_CTRTR,R_CPROC)*PQ0(R_CTRTR)*QD.l(R_CPROC))
         / SUM(R_CTRTR, PQ0(R_CTRTR)*QQ.l(R_CTRTR)+PQ0(R_CTRTR)*QE.l(R_CTRTR)) * SUM((R_ATRTR,RG), PVA0(R_ATRTR,RG) * QVA.l(R_ATRTR,RG));
 GDPX_PLUS(%1, %2,'TRD_AGR+') = SUM(R_CTRTR, INPUT('AGR',R_CTRTR)) + TRADE('AGR');
 GDPX_PLUS(%1, %2,'TRD_PRC+') = SUM(R_CTRTR, INPUT('PRC',R_CTRTR)) + TRADE('PRC');
 GDPX_PLUS(%1, %2,'TRD+') = GDPX_PLUS(%1, %2,'TRD_AGR+') + GDPX_PLUS(%1, %2,'TRD_PRC+');

*   Hotels and catering
 GDPX_PLUS(%1, %2,'HOT+')$SUM((C,R_AHOTL), PQ0(C)*QINT.l(C,R_AHOTL)) = (SUM((R_CAGRI,R_AHOTL), PQ0(R_CAGRI)*QINT.l(R_CAGRI,R_AHOTL)) + SUM((R_CPROC,R_AHOTL), PQ0(R_CPROC)*QINT.l(R_CPROC,R_AHOTL))) / SUM((C,R_AHOTL), PQ0(C)*QINT.l(C,R_AHOTL)) * SUM((R_AHOTL,RG), PVA0(R_AHOTL,RG) * QVA.l(R_AHOTL,RG));
 GDPX_PLUS(%1, %2,'FSV+') = SUM((R_AFSRV,RG), PVA0(R_AFSRV,RG) * QVA.l(R_AFSRV,RG));
 GDPX_PLUS(%1, %2,'HFS+') = GDPX_PLUS(%1, %2,'HOT+') + GDPX_PLUS(%1, %2,'FSV+');

*   Totals
 GDPX_PLUS(%1, %2,'AFS+') = GDPX_PLUS(%1, %2,'AGR+') + GDPX_PLUS(%1, %2,'PRC+') + GDPX_PLUS(%1, %2,'INP+') + GDPX_PLUS(%1, %2,'TRD+') + GDPX_PLUS(%1, %2,'HFS+');
 GDPX_PLUS(%1, %2,'OFF+') = GDPX_PLUS(%1, %2,'PRC+') + GDPX_PLUS(%1, %2,'INP+') + GDPX_PLUS(%1, %2,'TRD+') + GDPX_PLUS(%1, %2,'HFS+');
 GDPX_PLUS(%1, %2,'TOT+') = SUM((A,RG), PVA0(A,RG) * QVA.l(A,RG));


************************ poverty

*------------------------------------------------------------------------------
*1. Projected changes in survey household expenditures and populations
*------------------------------------------------------------------------------

*Total representative household expenditures by commodity groups relative to base year
 MODEXP(H,G)$SUM(C, QH.l(C,H)) =
 SUM(C$MCG(C,G), PQ0(C)*QH.l(C,H)) +
 SUM((A,C)$(MAC(A,C) AND MCG(C,G)), PA0(A)*QHA.l(A,H));


*Changes relative to base year values
 MODEXP(H,G)$(MODEXP0(H,G) AND SUM(C, QH.l(C,H)))
    =
    MODEXP(H,G)
    /
     MODEXP0(H,G) - 1;


*Projected survey household per adult equivalent expenditures (changes applied to latent expenditures)
 HIDEXP(HID,G) = DATA_O(HID,G) + DATA_L(HID,G) * SUM(H$MHIDH(HID,H), MODEXP(H,G));
*Representative household population changes relative to base year values
 MODPOP(H)$hpop0(H) = hpop(H) / hpop0(H);

*Projected survey household population weights
 HIDPOP(HID) = DATA_H(HID,'HWT') * DATA_H(HID,'HS')  * SUM(H$MHIDH(HID,H), MODPOP(H));

*Projected total population by reporting dimension (e.g., national, rural, urban)
 DIMPOP(DIMS) = SUM(HID$MHIDDIM(HID,DIMS), HIDPOP(HID));


*------------------------------------------------------------
*2. Poverty indicators (FGT)
*------------------------------------------------------------

*Arg 2 (Indicators) : (1) Poverty, inequality and diets  (2) Poverty and diets  (3) Poverty  (4) Inequality  (5) Diets

*--- Consumption measure ---

*Total survey household per adult equivalent expenditure
 TOTEXP(HID) = SUM(G, HIDEXP(HID,G));

*--- Poverty status and indicators ---

*Household poverty status
  POOR(HID) = NO;

  POOR(HID)$(TOTEXP(HID) LT DATA_H(HID,'PL_NAT')) = YES;
  PTAB('NUM','P0',DIMS)  = SUM(HID$(MHIDDIM(HID,DIMS) AND POOR(HID)), HIDPOP(HID));
  PTAB('RATE','P0',DIMS)$P0 = 1/DIMPOP(DIMS) * SUM(HID$(MHIDDIM(HID,DIMS) AND POOR(HID)), HIDPOP(HID)) * 100;
*--- Summary table ---
*Number of poor people (NUM)
 SUMTAB(%1, %2,'POV_NAT_L','FGT_NUM',DIMS) = PTAB('NUM','P0',DIMS);
*Poverty headcount (P0)
 SUMTAB(%1, %2,'POV_NAT_L','FGT_P0',DIMS)  = PTAB('RATE','P0',DIMS);
*Poverty gap (P1)
* SUMTAB(%1, %2,'POV_NAT','FGT_P1',DIMS)  = PTAB('RATE','P1',DIMS);
*Squared poverty gap (P2)
* SUMTAB(%1, %2,'POV_NAT','FGT_P2',DIMS)  = PTAB('RATE','P2',DIMS);

  POOR(HID) = NO;
  POOR(HID)$(TOTEXP(HID) LT DATA_H(HID,'PL_USD')) = YES;
  PTAB('NUM','P0',DIMS)  = SUM(HID$(MHIDDIM(HID,DIMS) AND POOR(HID)), HIDPOP(HID));
  PTAB('RATE','P0',DIMS)$P0 = 1/DIMPOP(DIMS) * SUM(HID$(MHIDDIM(HID,DIMS) AND POOR(HID)), HIDPOP(HID)) * 100;
  PTAB('RATE','P1',DIMS)$P1 = 1/DIMPOP(DIMS) * SUM(HID$(MHIDDIM(HID,DIMS) AND POOR(HID)), (1 - (TOTEXP(HID)/DATA_H(HID,'PL_NAT')))*HIDPOP(HID)) * 100;
  PTAB('RATE','P2',DIMS)$P2 = 1/DIMPOP(DIMS) * SUM(HID$(MHIDDIM(HID,DIMS) AND POOR(HID)), (1 - (TOTEXP(HID)/DATA_H(HID,'PL_NAT')))**2*HIDPOP(HID)) * 100;

  SUMTAB(%1, %2,'POV_USD_L','FGT_NUM',DIMS) = PTAB('NUM','P0',DIMS);
*Poverty headcount (P0)
  SUMTAB(%1, %2,'POV_USD_L','FGT_P0',DIMS)  = PTAB('RATE','P0',DIMS);



*------------------------------------------------------------
*5. Prevelance of Undernourishment (PoU)
*------------------------------------------------------------
*Imputed average cost per calorie per adult equivalent
 CSTCAL(G)$REFDIET(G,'HEAL') = DATA_C(G,'HEAL') / REFDIET(G,'HEAL') * CSTCALADJ;

*Total number of calcories by survey household
 TOTCAL(HID) = SUM(G$CSTCAL(G), HIDEXP(HID,G) / CSTCAL(G));

*Calculate number of undernourished people, and prevalence and depth of undernourishment
  HUNGRY(HID) = NO;
 HUNGRY(HID)$(TOTCAL(HID) LT MDER) = YES;
 HTAB('NUM','P0',DIMS)  = SUM(HID$(MHIDDIM(HID,DIMS) AND HUNGRY(HID)), HIDPOP(HID));
 HTAB('RATE','P0',DIMS)$P0 = 1/DIMPOP(DIMS) * SUM(HID$(MHIDDIM(HID,DIMS) AND HUNGRY(HID)), HIDPOP(HID)) * 100;
 HTAB('RATE','P1',DIMS)$P1 = 1/DIMPOP(DIMS) * SUM(HID$(MHIDDIM(HID,DIMS) AND HUNGRY(HID)), (1 - (TOTCAL(HID)/MDER))*HIDPOP(HID)) * 100;

SUMTAB(%1, %2,'HUN','FGT_NUM',DIMS) = HTAB('NUM','P0',DIMS);
SUMTAB(%1, %2,'HUN','FGT_P0',DIMS)  = HTAB('RATE','P0',DIMS);


* rates
PH_export(%1, %2,'POVR_N_USD_L') = SUMTAB(%1, %2,'POV_USD_L','FGT_P0','NAT') ;
PH_export(%1, %2,'POVR_R_USD_L') = SUMTAB(%1, %2,'POV_USD_L','FGT_P0','RUR') ;
PH_export(%1, %2,'POVR_U_USD_L') = SUMTAB(%1, %2,'POV_USD_L','FGT_P0','URB') ;

PH_export(%1, %2,'POVR_N_NAT_L') = SUMTAB(%1, %2,'POV_NAT_L','FGT_P0','NAT') ;
PH_export(%1, %2,'POVR_R_NAT_L') = SUMTAB(%1, %2,'POV_NAT_L','FGT_P0','RUR') ;
PH_export(%1, %2,'POVR_U_NAT_L') = SUMTAB(%1, %2,'POV_NAT_L','FGT_P0','URB') ;

PH_export(%1, %2,'HUNR_N') = SUMTAB(%1, %2,'HUN','FGT_P0','NAT') ;
PH_export(%1, %2,'HUNR_R') = SUMTAB(%1, %2,'HUN','FGT_P0','RUR') ;
PH_export(%1, %2,'HUNR_U') = SUMTAB(%1, %2,'HUN','FGT_P0','URB') ;

* numbers
PH_export(%1, %2,'POVN_N_USD_L') = SUMTAB(%1, %2,'POV_USD_L','FGT_NUM','NAT') ;
PH_export(%1, %2,'POVN_R_USD_L') = SUMTAB(%1, %2,'POV_USD_L','FGT_NUM','RUR') ;
PH_export(%1, %2,'POVN_U_USD_L') = SUMTAB(%1, %2,'POV_USD_L','FGT_NUM','URB') ;

PH_export(%1, %2,'POVN_N_NAT_L') = SUMTAB(%1, %2,'POV_NAT_L','FGT_NUM','NAT') ;
PH_export(%1, %2,'POVN_R_NAT_L') = SUMTAB(%1, %2,'POV_NAT_L','FGT_NUM','RUR') ;
PH_export(%1, %2,'POVN_U_NAT_L') = SUMTAB(%1, %2,'POV_NAT_L','FGT_NUM','URB') ;

PH_export(%1, %2,'HUNN_N') = SUMTAB(%1, %2,'HUN','FGT_NUM','NAT') ;
PH_export(%1, %2,'HUNN_R') = SUMTAB(%1, %2,'HUN','FGT_NUM','RUR') ;
PH_export(%1, %2,'HUNN_U') = SUMTAB(%1, %2,'HUN','FGT_NUM','URB') ;

 model_stat_export(%1, %2,'solvestat') = solvestat;
 model_stat_export(%1, %2,'modelstat') = modelstat;
 model_stat_export(%1, %2,'WALRASSQR') = WALRASSQR.l;


MACROTABX(%1, %2, 'PRVCON')     = GDPMP_REAL('PRVCON',%1, %2) ;
MACROTABX(%1, %2, 'FIXINV')     = GDPMP_REAL('FIXINV',%1, %2) ;
MACROTABX(%1, %2, 'DSTOCK')     = GDPMP_REAL('DSTOCK',%1, %2) ;
MACROTABX(%1, %2, 'GOVCON')     = GDPMP_REAL('GOVCON',%1, %2) ;
MACROTABX(%1, %2, 'EXPORTS')    = GDPMP_REAL('EXPORTS',%1, %2) ;
MACROTABX(%1, %2, 'IMPORTS')    = -GDPMP_REAL('IMPORTS',%1, %2) ;
MACROTABX(%1, %2, 'GDPMP') = GDPMP_REAL('GDPMP',%1, %2) ;
MACROTABX(%1, %2, 'ABSORP')   = GDPMP_REAL('ABSORP',%1, %2);
MACROTABX(%1, %2, 'NETITAX') = GDPMP_REAL('NETITAX',%1, %2) ;
MACROTABX(%1, %2, 'GDPFC') = GDPMP_REAL('GDPFC2',%1, %2) ;

*Prices
 MACROTABX(%1, %2, 'CPI')       = 100*CPI.l / CPI0;
 MACROTABX(%1, %2, 'NEXR')      = 100*EXR.l;
 MACROTABX(%1, %2, 'PDIND')     = 100*SUM(C, pdwt(C)*PDD.l(C));
 MACROTABX(%1, %2, 'PWEIND')    = 100*(SUM(C, pwe(C)*pwewt(C)) / SUM(C, pwe0(C)*pwewt(C)));
 MACROTABX(%1, %2, 'PWMIND')    = 100*(SUM(C, pwm(C)*pwmwt(C)) / SUM(C, pwm0(C)*pwmwt(C)));
 MACROTABX(%1, %2, 'TOFT')      = 100*MACROTABX(%1, %2, 'PWEIND') / MACROTABX(%1, %2, 'PWMIND');
 MACROTABX(%1, %2, 'PWIND')     = pwwt('EXP')*MACROTABX(%1, %2, 'PWEIND') + pwwt('IMP')*MACROTABX(%1, %2, 'PWMIND');
 MACROTABX(%1, %2, 'REXR')      = MACROTABX(%1, %2, 'NEXR')*MACROTABX(%1, %2, 'PWIND') / MACROTABX(%1, %2, 'PDIND');
*GDP shares
 MACROTABX(%1, %2, 'INVGDP')    = 100*SUM(C, PQ.l(C)*QINV.l(C)) / GDPMP_NOMINAL('GDPMP', %1, %2);
 MACROTABX(%1, %2, 'PRVSAVGDP') = 100*SUM(INSDNG, MPS.l(INSDNG)*((1-TINS.l(INSDNG))*YI.l(INSDNG))) / GDPMP_NOMINAL('GDPMP', %1, %2);
 MACROTABX(%1, %2, 'FORSAVGDP') = 100*EXR.l*FSAV.l / GDPMP_NOMINAL('GDPMP', %1, %2);
 MACROTABX(%1, %2, 'TRDDEFGDP') = 100*SUM(C, PM.l(C)*QM.l(C) - PE.l(C)*QE.l(C)) / GDPMP_NOMINAL('GDPMP', %1, %2);
 MACROTABX(%1, %2, 'GOVSAVGDP') = 100*GSAV.l / GDPMP_NOMINAL('GDPMP', %1, %2);
 MACROTABX(%1, %2, 'IMPTAXGDP') = 100*SUM(C, tm(C)*pwm(C)*QM.l(C)*EXR.l) / GDPMP_NOMINAL('GDPMP', %1, %2);
 MACROTABX(%1, %2, 'DIRTAXGDP') = 100*(SUM(INSDNG, TINS.l(INSDNG)*YI.l(INSDNG)) + SUM(F, tf(F)*YF.l(F))) / GDPMP_NOMINAL('GDPMP', %1, %2);
 MACROTABX(%1, %2, 'SALTAXGDP') = 100*SUM(C, tq(C)*PQ.l(C)*QQ.l(C)) / GDPMP_NOMINAL('GDPMP', %1, %2);
 MACROTABX(%1, %2, 'FACTAXGDP') = 100*SUM(F, tf(F)*YF.l(F)) / GDPMP_NOMINAL('GDPMP', %1, %2);

 HHD_CONS_X(%1, %2, 'urban')  = SUM((C,HURB), PQ0(C)*QH.l(C,HURB)*hpop(HURB)) + SUM((A,HURB), PA0(A)*QHA.l(A,HURB)*hpop(HURB));
 HHD_CONS_X(%1, %2, 'urban_poor')  = SUM((C,HURB_P), PQ0(C)*QH.l(C,HURB_P)*hpop(HURB_P)) + SUM((A,HURB_P), PA0(A)*QHA.l(A,HURB_P)*hpop(HURB_P));
 HHD_CONS_X(%1, %2, 'urban_midd')  = SUM((C,HURB_M), PQ0(C)*QH.l(C,HURB_M)*hpop(HURB_M)) + SUM((A,HURB_M), PA0(A)*QHA.l(A,HURB_M)*hpop(HURB_M));
 HHD_CONS_X(%1, %2, 'urban_rich')  = SUM((C,HURB_R), PQ0(C)*QH.l(C,HURB_R)*hpop(HURB_R)) + SUM((A,HURB_R), PA0(A)*QHA.l(A,HURB_R)*hpop(HURB_R));

 HHD_CONS_X(%1, %2, 'rural')  = SUM((C,HRUR), PQ0(C)*QH.l(C,HRUR)*hpop(HRUR)) + SUM((A,HRUR), PA0(A)*QHA.l(A,HRUR)*hpop(HRUR));
 HHD_CONS_X(%1, %2, 'rural_poor')  = SUM((C,HRUR_P), PQ0(C)*QH.l(C,HRUR_P)*hpop(HRUR_P)) + SUM((A,HRUR_P), PA0(A)*QHA.l(A,HRUR_P)*hpop(HRUR_P));
 HHD_CONS_X(%1, %2, 'rural_midd')  = SUM((C,HRUR_M), PQ0(C)*QH.l(C,HRUR_M)*hpop(HRUR_M)) + SUM((A,HRUR_M), PA0(A)*QHA.l(A,HRUR_M)*hpop(HRUR_M));
 HHD_CONS_X(%1, %2, 'rural_rich')  = SUM((C,HRUR_R), PQ0(C)*QH.l(C,HRUR_R)*hpop(HRUR_R)) + SUM((A,HRUR_R), PA0(A)*QHA.l(A,HRUR_R)*hpop(HRUR_R));


 HHD_INC_X(%1, %2, 'urban')  = SUM(HURB, YI.l(HURB));
 HHD_INC_X(%1, %2, 'urban_poor')  = SUM(HURB_P, YI.l(HURB_P));
 HHD_INC_X(%1, %2, 'urban_midd')  = SUM(HURB_M, YI.l(HURB_M));
 HHD_INC_X(%1, %2, 'urban_rich')  = SUM(HURB_R, YI.l(HURB_R));

 HHD_INC_X(%1, %2, 'rural')  = SUM(HRUR, YI.l(HRUR));
 HHD_INC_X(%1, %2, 'rural_poor')  = SUM(HRUR_P, YI.l(HRUR_P));
 HHD_INC_X(%1, %2, 'rural_midd')  =  SUM(HRUR_M, YI.l(HRUR_M));
 HHD_INC_X(%1, %2, 'rural_rich')  = SUM(HRUR_R, YI.l(HRUR_R));

 YI_X(%1, %2, INSDNG)  = YI.l(INSDNG);
 WF_X(%1, %2, F)  = WF.l(F);
YIF_X(%1, %2, INS,F) = YIF.l(INS,F); 
