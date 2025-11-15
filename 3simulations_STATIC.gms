$setglobal ide "ide=%gams.ide% lo=%gams.lo% errorlog=%gams.errorlog% errmsg=1"
*$ontext
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
*$offtext

Parameter
FORSAVGDP_C
FORSAVGDP
GDP_MP
;
GDP_MP = (
 SUM((C,H), PQ0(C)*QH0(C,H)*hpop(H)) +
  SUM((A,H), PA0(A)*QHA0(A,H)*hpop(H)) +
   SUM(C, PQ0(C)*QINV0(C))+
    SUM(C, PQ0(C)*QDST(C)) +
     SUM(C, PQ0(C)*QG0(C))+
      SUM(CE, pwe(CE)*EXR0*QE0(CE))
       -SUM(CM, pwm(CM)*EXR0*QM0(CM)));
 FORSAVGDP_C= 100*EXR0*FSAV0 /GDP_MP;

 TC('2019') = YES;
 T1('2019') = YES;
* solve for true base
XC('base')=YES;
MODEL STANDCGE_NLP_STATIC / STANDNLP /;
STANDCGE_NLP_STATIC.holdfixed = 1;
STANDCGE_NLP_STATIC.solvelink = 1;
STANDCGE_NLP_STATIC.tolinfeas = 1.0e-5;
 option iterlim = 1000;
$include includes\2closures.inc
STANDCGE_NLP_STATIC.savepoint = 0 ;
 SOLVE STANDCGE_NLP_STATIC MINIMIZING WALRASSQR USING NLP ;
 solvestat = STANDCGE_NLP_STATIC.solvestat;
 modelstat = STANDCGE_NLP_STATIC.modelstat;

$BATINCLUDE includes\2results_slim.inc  XC TC

* now solve for pseudo base
XC('base')=NO;
XC('0')=YES;
pwe_c('ccere')$pwe0('ccere') =    pwe0('ccere')* (1+shocks('0', '2019','pw_cere')/100);
pwe_c('cpuls')$pwe0('cpuls') =    pwe0('cpuls')* (1+shocks('0', '2019','pw_cere')/100);
pwe_c('coils')$pwe0('coils') =    pwe0('coils')* (1+shocks('0', '2019','pw_oils')/100);
pwe_c('croot')$pwe0('croot') =    pwe0('croot')* (1+shocks('0', '2019','pw_cere')/100);
pwe_c('cvege')$pwe0('cvege') =    pwe0('cvege')* (1+shocks('0', '2019','pw_frui')/100);
pwe_c('cfrui')$pwe0('cfrui') =    pwe0('cfrui')* (1+shocks('0', '2019','pw_frui')/100);
pwe_c('cbecr')$pwe0('cbecr') =    pwe0('cbecr')* (1+shocks('0', '2019','pw_becr')/100);
pwe_c('cocrp')$pwe0('cocrp') =    pwe0('cocrp')* (1+shocks('0', '2019','pw_ocrp')/100);
pwe_c('clive')$pwe0('clive') =    pwe0('clive')* (1+shocks('0', '2019','pw_meat')/100);
pwe_c('cmilk')$pwe0('cmilk') =    pwe0('cmilk')* (1+shocks('0', '2019','pw_dair')/100);
pwe_c('cfore')$pwe0('cfore') =    pwe0('cfore')* (1+shocks('0', '2019','pw_wood')/100);
pwe_c('cfish')$pwe0('cfish') =    pwe0('cfish')* (1+shocks('0', '2019','pw_fish')/100);
pwe_c('cmine')$pwe0('cmine') =    pwe0('cmine')* (1+shocks('0', '2019','pw_ener')/100);
pwe_c('cmino')$pwe0('cmino') =    pwe0('cmino')* (1+shocks('0', '2019','pw_metl')/100);
pwe_c('cgmll')$pwe0('cgmll') =    pwe0('cgmll')* (1+shocks('0', '2019','pw_cere')/100);
pwe_c('cfoil')$pwe0('cfoil') =    pwe0('cfoil')* (1+shocks('0', '2019','pw_oils')/100);
pwe_c('cfveg')$pwe0('cfveg') =    pwe0('cfveg')* (1+shocks('0', '2019','pw_frui')/100);
pwe_c('cmeat')$pwe0('cmeat') =    pwe0('cmeat')* (1+shocks('0', '2019','pw_meat')/100);
pwe_c('cfsea')$pwe0('cfsea') =    pwe0('cfsea')* (1+shocks('0', '2019','pw_fish')/100);
pwe_c('cdair')$pwe0('cdair') =    pwe0('cdair')* (1+shocks('0', '2019','pw_dair')/100);
pwe_c('cfobe')$pwe0('cfobe') =    pwe0('cfobe')* (1+shocks('0', '2019','pw_becr')/100);
pwe_c('coapr')$pwe0('coapr') =    pwe0('coapr')* (1+shocks('0', '2019','pw_ocrp')/100);
pwe_c('ctext')$pwe0('ctext') =    pwe0('ctext')* (1+shocks('0', '2019','pw_manu')/100);
pwe_c('cwood')$pwe0('cwood') =    pwe0('cwood')* (1+shocks('0', '2019','pw_wood')/100);
pwe_c('cpetr')$pwe0('cpetr') =    pwe0('cpetr')* (1+shocks('0', '2019','pw_ener')/100);
pwe_c('cfert')$pwe0('cfert') =    pwe0('cfert')* (1+shocks('0', '2019','pw_fert')/100);
pwe_c('cchem')$pwe0('cchem') =    pwe0('cchem')* (1+shocks('0', '2019','pw_manu')/100);
pwe_c('cnmet')$pwe0('cnmet') =    pwe0('cnmet')* (1+shocks('0', '2019','pw_metl')/100);
pwe_c('cmetl')$pwe0('cmetl') =    pwe0('cmetl')* (1+shocks('0', '2019','pw_metl')/100);
pwe_c('cmach')$pwe0('cmach') =    pwe0('cmach')* (1+shocks('0', '2019','pw_manu')/100);
pwe_c('coman')$pwe0('coman') =    pwe0('coman')* (1+shocks('0', '2019','pw_manu')/100);
pwe_c('celec')$pwe0('celec') =    pwe0('celec')* (1+shocks('0', '2019','pw_ener')/100);
pwe_c('cwatr')$pwe0('cwatr') =    pwe0('cwatr')* (1+shocks('0', '2019','pw_manu')/100);
pwe_c('ccons')$pwe0('ccons') =    pwe0('ccons')* (1+shocks('0', '2019','pw_manu')/100);
pwe_c('ctrad')$pwe0('ctrad') =    pwe0('ctrad')* (1+shocks('0', '2019','pw_serv')/100);
pwe_c('ctran')$pwe0('ctran') =    pwe0('ctran')* (1+shocks('0', '2019','pw_serv')/100);
pwe_c('chotl')$pwe0('chotl') =    pwe0('chotl')* (1+shocks('0', '2019','pw_serv')/100);
pwe_c('crest')$pwe0('crest') =    pwe0('crest')* (1+shocks('0', '2019','pw_serv')/100);
pwe_c('ccomm')$pwe0('ccomm') =    pwe0('ccomm')* (1+shocks('0', '2019','pw_serv')/100);
pwe_c('cfsrv')$pwe0('cfsrv') =    pwe0('cfsrv')* (1+shocks('0', '2019','pw_serv')/100);
pwe_c('creal')$pwe0('creal') =    pwe0('creal')* (1+shocks('0', '2019','pw_serv')/100);
pwe_c('cbsrv')$pwe0('cbsrv') =    pwe0('cbsrv')* (1+shocks('0', '2019','pw_serv')/100);
pwe_c('cpadm')$pwe0('cpadm') =    pwe0('cpadm')* (1+shocks('0', '2019','pw_serv')/100);
pwe_c('ceduc')$pwe0('ceduc') =    pwe0('ceduc')* (1+shocks('0', '2019','pw_serv')/100);
pwe_c('cheal')$pwe0('cheal') =    pwe0('cheal')* (1+shocks('0', '2019','pw_serv')/100);
pwe_c('cosrv')$pwe0('cosrv') =    pwe0('cosrv')* (1+shocks('0', '2019','pw_serv')/100);

pwm_c('ccere')$pwm0('ccere') =    pwm0('ccere')* (1+shocks('0', '2019','pw_cere')/100);
pwm_c('cpuls')$pwm0('cpuls') =    pwm0('cpuls')* (1+shocks('0', '2019','pw_cere')/100);
pwm_c('coils')$pwm0('coils') =    pwm0('coils')* (1+shocks('0', '2019','pw_oils')/100);
pwm_c('croot')$pwm0('croot') =    pwm0('croot')* (1+shocks('0', '2019','pw_cere')/100);
pwm_c('cvege')$pwm0('cvege') =    pwm0('cvege')* (1+shocks('0', '2019','pw_frui')/100);
pwm_c('cfrui')$pwm0('cfrui') =    pwm0('cfrui')* (1+shocks('0', '2019','pw_frui')/100);
pwm_c('cbecr')$pwm0('cbecr') =    pwm0('cbecr')* (1+shocks('0', '2019','pw_becr')/100);
pwm_c('cocrp')$pwm0('cocrp') =    pwm0('cocrp')* (1+shocks('0', '2019','pw_ocrp')/100);
pwm_c('clive')$pwm0('clive') =    pwm0('clive')* (1+shocks('0', '2019','pw_meat')/100);
pwm_c('cmilk')$pwm0('cmilk') =    pwm0('cmilk')* (1+shocks('0', '2019','pw_dair')/100);
pwm_c('cfore')$pwm0('cfore') =    pwm0('cfore')* (1+shocks('0', '2019','pw_wood')/100);
pwm_c('cfish')$pwm0('cfish') =    pwm0('cfish')* (1+shocks('0', '2019','pw_fish')/100);
pwm_c('cmine')$pwm0('cmine') =    pwm0('cmine')* (1+shocks('0', '2019','pw_ener')/100);
pwm_c('cmino')$pwm0('cmino') =    pwm0('cmino')* (1+shocks('0', '2019','pw_metl')/100);
pwm_c('cgmll')$pwm0('cgmll') =    pwm0('cgmll')* (1+shocks('0', '2019','pw_cere')/100);
pwm_c('cfoil')$pwm0('cfoil') =    pwm0('cfoil')* (1+shocks('0', '2019','pw_oils')/100);
pwm_c('cfveg')$pwm0('cfveg') =    pwm0('cfveg')* (1+shocks('0', '2019','pw_frui')/100);
pwm_c('cmeat')$pwm0('cmeat') =    pwm0('cmeat')* (1+shocks('0', '2019','pw_meat')/100);
pwm_c('cfsea')$pwm0('cfsea') =    pwm0('cfsea')* (1+shocks('0', '2019','pw_fish')/100);
pwm_c('cdair')$pwm0('cdair') =    pwm0('cdair')* (1+shocks('0', '2019','pw_dair')/100);
pwm_c('cfobe')$pwm0('cfobe') =    pwm0('cfobe')* (1+shocks('0', '2019','pw_becr')/100);
pwm_c('coapr')$pwm0('coapr') =    pwm0('coapr')* (1+shocks('0', '2019','pw_ocrp')/100);
pwm_c('ctext')$pwm0('ctext') =    pwm0('ctext')* (1+shocks('0', '2019','pw_manu')/100);
pwm_c('cwood')$pwm0('cwood') =    pwm0('cwood')* (1+shocks('0', '2019','pw_wood')/100);
pwm_c('cpetr')$pwm0('cpetr') =    pwm0('cpetr')* (1+shocks('0', '2019','pw_ener')/100);
pwm_c('cfert')$pwm0('cfert') =    pwm0('cfert')* (1+shocks('0', '2019','pw_fert')/100);
pwm_c('cchem')$pwm0('cchem') =    pwm0('cchem')* (1+shocks('0', '2019','pw_manu')/100);
pwm_c('cnmet')$pwm0('cnmet') =    pwm0('cnmet')* (1+shocks('0', '2019','pw_metl')/100);
pwm_c('cmetl')$pwm0('cmetl') =    pwm0('cmetl')* (1+shocks('0', '2019','pw_metl')/100);
pwm_c('cmach')$pwm0('cmach') =    pwm0('cmach')* (1+shocks('0', '2019','pw_manu')/100);
pwm_c('coman')$pwm0('coman') =    pwm0('coman')* (1+shocks('0', '2019','pw_manu')/100);
pwm_c('celec')$pwm0('celec') =    pwm0('celec')* (1+shocks('0', '2019','pw_ener')/100);
pwm_c('cwatr')$pwm0('cwatr') =    pwm0('cwatr')* (1+shocks('0', '2019','pw_manu')/100);
pwm_c('ccons')$pwm0('ccons') =    pwm0('ccons')* (1+shocks('0', '2019','pw_manu')/100);
pwm_c('ctrad')$pwm0('ctrad') =    pwm0('ctrad')* (1+shocks('0', '2019','pw_serv')/100);
pwm_c('ctran')$pwm0('ctran') =    pwm0('ctran')* (1+shocks('0', '2019','pw_serv')/100);
pwm_c('chotl')$pwm0('chotl') =    pwm0('chotl')* (1+shocks('0', '2019','pw_serv')/100);
pwm_c('crest')$pwm0('crest') =    pwm0('crest')* (1+shocks('0', '2019','pw_serv')/100);
pwm_c('ccomm')$pwm0('ccomm') =    pwm0('ccomm')* (1+shocks('0', '2019','pw_serv')/100);
pwm_c('cfsrv')$pwm0('cfsrv') =    pwm0('cfsrv')* (1+shocks('0', '2019','pw_serv')/100);
pwm_c('creal')$pwm0('creal') =    pwm0('creal')* (1+shocks('0', '2019','pw_serv')/100);
pwm_c('cbsrv')$pwm0('cbsrv') =    pwm0('cbsrv')* (1+shocks('0', '2019','pw_serv')/100);
pwm_c('cpadm')$pwm0('cpadm') =    pwm0('cpadm')* (1+shocks('0', '2019','pw_serv')/100);
pwm_c('ceduc')$pwm0('ceduc') =    pwm0('ceduc')* (1+shocks('0', '2019','pw_serv')/100);
pwm_c('cheal')$pwm0('cheal') =    pwm0('cheal')* (1+shocks('0', '2019','pw_serv')/100);
pwm_c('cosrv')$pwm0('cosrv') =    pwm0('cosrv')* (1+shocks('0', '2019','pw_serv')/100);
 alphava_c('acere',RG)$alphava0('acere',RG) =  alphava0('acere',RG)*(1+shocks('0', '2019', 'pd_cere')/100);
 alphava_c('apuls',RG)$alphava0('apuls',RG) =  alphava0('apuls',RG)*(1+shocks('0', '2019', 'pd_puls')/100);
 alphava_c('aoils',RG)$alphava0('aoils',RG) =  alphava0('aoils',RG)*(1+shocks('0', '2019', 'pd_oils')/100);
 alphava_c('aroot',RG)$alphava0('aroot',RG) =  alphava0('aroot',RG)*(1+shocks('0', '2019', 'pd_root')/100);
 alphava_c('avege',RG)$alphava0('avege',RG) =  alphava0('avege',RG)*(1+shocks('0', '2019', 'pd_vege')/100);
 alphava_c('afrui',RG)$alphava0('afrui',RG) =  alphava0('afrui',RG)*(1+shocks('0', '2019', 'pd_frui')/100);
 alphava_c('abecr',RG)$alphava0('abecr',RG) =  alphava0('abecr',RG)*(1+shocks('0', '2019', 'pd_becr')/100);
 alphava_c('aocrp',RG)$alphava0('aocrp',RG) =  alphava0('aocrp',RG)*(1+shocks('0', '2019', 'pd_ocrp')/100);
 alphava_c('alive',RG)$alphava0('alive',RG) =  alphava0('alive',RG)*(1+shocks('0', '2019', 'pd_live')/100);
 alphava_c('amilk',RG)$alphava0('amilk',RG) =  alphava0('amilk',RG)*(1+shocks('0', '2019', 'pd_milk')/100);
 alphava_c('afore',RG)$alphava0('afore',RG) =  alphava0('afore',RG)*(1+shocks('0', '2019', 'pd_fore')/100);
 alphava_c('afish',RG)$alphava0('afish',RG) =  alphava0('afish',RG)*(1+shocks('0', '2019', 'pd_fish')/100);
 alphava_c('aelec',RG)$alphava0('aelec',RG) =  alphava0('aelec',RG)*(1+shocks('0', '2019', 'pd_elec')/100);

 FORSAVGDP = FORSAVGDP_C - shocks('0','2019', 'FSAV');
 FSAV_c   = (FORSAVGDP    /  100) * GDP_MP;
$include includes\2varinit.inc
$include includes\2closures.inc
STANDCGE_NLP_STATIC.savepoint = 1 ;
SOLVE STANDCGE_NLP_STATIC MINIMIZING WALRASSQR USING NLP ;
solvestat = STANDCGE_NLP_STATIC.solvestat;
modelstat = STANDCGE_NLP_STATIC.modelstat;
STANDCGE_NLP_STATIC.savepoint = 0 ;
$BATINCLUDE includes\2results_slim.inc  XC TC

* now solve for all other
XC('0')=NO;

XC(X)$shocks(X, '2019','pw_cere')=YES;

shocks(X,T,vars)$(not (XC(X) and TC(T)))= 0;
LOOP(TC,
  FORSAVGDP_C = FORSAVGDP ;
$include includes\2varinit.inc
LOOP(XC,
  execute_loadpoint 'STANDCGE_NLP_STATIC_P.gdx';
* short-term
pwe('ccere')$pwe0('ccere') =    pwe_c('ccere')* (1+shocks(XC, TC,'pw_cere')/100);
pwe('cpuls')$pwe0('cpuls') =    pwe_c('cpuls')* (1+shocks(XC, TC,'pw_cere')/100);
pwe('coils')$pwe0('coils') =    pwe_c('coils')* (1+shocks(XC, TC,'pw_oils')/100);
pwe('croot')$pwe0('croot') =    pwe_c('croot')* (1+shocks(XC, TC,'pw_cere')/100);
pwe('cvege')$pwe0('cvege') =    pwe_c('cvege')* (1+shocks(XC, TC,'pw_frui')/100);
pwe('cfrui')$pwe0('cfrui') =    pwe_c('cfrui')* (1+shocks(XC, TC,'pw_frui')/100);
pwe('cbecr')$pwe0('cbecr') =    pwe_c('cbecr')* (1+shocks(XC, TC,'pw_becr')/100);
pwe('cocrp')$pwe0('cocrp') =    pwe_c('cocrp')* (1+shocks(XC, TC,'pw_ocrp')/100);
pwe('clive')$pwe0('clive') =    pwe_c('clive')* (1+shocks(XC, TC,'pw_meat')/100);
pwe('cmilk')$pwe0('cmilk') =    pwe_c('cmilk')* (1+shocks(XC, TC,'pw_dair')/100);
pwe('cfore')$pwe0('cfore') =    pwe_c('cfore')* (1+shocks(XC, TC,'pw_wood')/100);
pwe('cfish')$pwe0('cfish') =    pwe_c('cfish')* (1+shocks(XC, TC,'pw_fish')/100);
pwe('cmine')$pwe0('cmine') =    pwe_c('cmine')* (1+shocks(XC, TC,'pw_ener')/100);
pwe('cmino')$pwe0('cmino') =    pwe_c('cmino')* (1+shocks(XC, TC,'pw_metl')/100);
pwe('cgmll')$pwe0('cgmll') =    pwe_c('cgmll')* (1+shocks(XC, TC,'pw_cere')/100);
pwe('cfoil')$pwe0('cfoil') =    pwe_c('cfoil')* (1+shocks(XC, TC,'pw_oils')/100);
pwe('cfveg')$pwe0('cfveg') =    pwe_c('cfveg')* (1+shocks(XC, TC,'pw_frui')/100);
pwe('cmeat')$pwe0('cmeat') =    pwe_c('cmeat')* (1+shocks(XC, TC,'pw_meat')/100);
pwe('cfsea')$pwe0('cfsea') =    pwe_c('cfsea')* (1+shocks(XC, TC,'pw_fish')/100);
pwe('cdair')$pwe0('cdair') =    pwe_c('cdair')* (1+shocks(XC, TC,'pw_dair')/100);
pwe('cfobe')$pwe0('cfobe') =    pwe_c('cfobe')* (1+shocks(XC, TC,'pw_becr')/100);
pwe('coapr')$pwe0('coapr') =    pwe_c('coapr')* (1+shocks(XC, TC,'pw_ocrp')/100);
pwe('ctext')$pwe0('ctext') =    pwe_c('ctext')* (1+shocks(XC, TC,'pw_manu')/100);
pwe('cwood')$pwe0('cwood') =    pwe_c('cwood')* (1+shocks(XC, TC,'pw_wood')/100);
pwe('cpetr')$pwe0('cpetr') =    pwe_c('cpetr')* (1+shocks(XC, TC,'pw_ener')/100);
pwe('cfert')$pwe0('cfert') =    pwe_c('cfert')* (1+shocks(XC, TC,'pw_fert')/100);
pwe('cchem')$pwe0('cchem') =    pwe_c('cchem')* (1+shocks(XC, TC,'pw_manu')/100);
pwe('cnmet')$pwe0('cnmet') =    pwe_c('cnmet')* (1+shocks(XC, TC,'pw_metl')/100);
pwe('cmetl')$pwe0('cmetl') =    pwe_c('cmetl')* (1+shocks(XC, TC,'pw_metl')/100);
pwe('cmach')$pwe0('cmach') =    pwe_c('cmach')* (1+shocks(XC, TC,'pw_manu')/100);
pwe('coman')$pwe0('coman') =    pwe_c('coman')* (1+shocks(XC, TC,'pw_manu')/100);
pwe('celec')$pwe0('celec') =    pwe_c('celec')* (1+shocks(XC, TC,'pw_ener')/100);
pwe('cwatr')$pwe0('cwatr') =    pwe_c('cwatr')* (1+shocks(XC, TC,'pw_manu')/100);
pwe('ccons')$pwe0('ccons') =    pwe_c('ccons')* (1+shocks(XC, TC,'pw_manu')/100);
pwe('ctrad')$pwe0('ctrad') =    pwe_c('ctrad')* (1+shocks(XC, TC,'pw_serv')/100);
pwe('ctran')$pwe0('ctran') =    pwe_c('ctran')* (1+shocks(XC, TC,'pw_serv')/100);
pwe('chotl')$pwe0('chotl') =    pwe_c('chotl')* (1+shocks(XC, TC,'pw_serv')/100);
pwe('crest')$pwe0('crest') =    pwe_c('crest')* (1+shocks(XC, TC,'pw_serv')/100);
pwe('ccomm')$pwe0('ccomm') =    pwe_c('ccomm')* (1+shocks(XC, TC,'pw_serv')/100);
pwe('cfsrv')$pwe0('cfsrv') =    pwe_c('cfsrv')* (1+shocks(XC, TC,'pw_serv')/100);
pwe('creal')$pwe0('creal') =    pwe_c('creal')* (1+shocks(XC, TC,'pw_serv')/100);
pwe('cbsrv')$pwe0('cbsrv') =    pwe_c('cbsrv')* (1+shocks(XC, TC,'pw_serv')/100);
pwe('cpadm')$pwe0('cpadm') =    pwe_c('cpadm')* (1+shocks(XC, TC,'pw_serv')/100);
pwe('ceduc')$pwe0('ceduc') =    pwe_c('ceduc')* (1+shocks(XC, TC,'pw_serv')/100);
pwe('cheal')$pwe0('cheal') =    pwe_c('cheal')* (1+shocks(XC, TC,'pw_serv')/100);
pwe('cosrv')$pwe0('cosrv') =    pwe_c('cosrv')* (1+shocks(XC, TC,'pw_serv')/100);

pwm('ccere')$pwm0('ccere') =    pwm_c('ccere')* (1+shocks(XC, TC,'pw_cere')/100);
pwm('cpuls')$pwm0('cpuls') =    pwm_c('cpuls')* (1+shocks(XC, TC,'pw_cere')/100);
pwm('coils')$pwm0('coils') =    pwm_c('coils')* (1+shocks(XC, TC,'pw_oils')/100);
pwm('croot')$pwm0('croot') =    pwm_c('croot')* (1+shocks(XC, TC,'pw_cere')/100);
pwm('cvege')$pwm0('cvege') =    pwm_c('cvege')* (1+shocks(XC, TC,'pw_frui')/100);
pwm('cfrui')$pwm0('cfrui') =    pwm_c('cfrui')* (1+shocks(XC, TC,'pw_frui')/100);
pwm('cbecr')$pwm0('cbecr') =    pwm_c('cbecr')* (1+shocks(XC, TC,'pw_becr')/100);
pwm('cocrp')$pwm0('cocrp') =    pwm_c('cocrp')* (1+shocks(XC, TC,'pw_ocrp')/100);
pwm('clive')$pwm0('clive') =    pwm_c('clive')* (1+shocks(XC, TC,'pw_meat')/100);
pwm('cmilk')$pwm0('cmilk') =    pwm_c('cmilk')* (1+shocks(XC, TC,'pw_dair')/100);
pwm('cfore')$pwm0('cfore') =    pwm_c('cfore')* (1+shocks(XC, TC,'pw_wood')/100);
pwm('cfish')$pwm0('cfish') =    pwm_c('cfish')* (1+shocks(XC, TC,'pw_fish')/100);
pwm('cmine')$pwm0('cmine') =    pwm_c('cmine')* (1+shocks(XC, TC,'pw_ener')/100);
pwm('cmino')$pwm0('cmino') =    pwm_c('cmino')* (1+shocks(XC, TC,'pw_metl')/100);
pwm('cgmll')$pwm0('cgmll') =    pwm_c('cgmll')* (1+shocks(XC, TC,'pw_cere')/100);
pwm('cfoil')$pwm0('cfoil') =    pwm_c('cfoil')* (1+shocks(XC, TC,'pw_oils')/100);
pwm('cfveg')$pwm0('cfveg') =    pwm_c('cfveg')* (1+shocks(XC, TC,'pw_frui')/100);
pwm('cmeat')$pwm0('cmeat') =    pwm_c('cmeat')* (1+shocks(XC, TC,'pw_meat')/100);
pwm('cfsea')$pwm0('cfsea') =    pwm_c('cfsea')* (1+shocks(XC, TC,'pw_fish')/100);
pwm('cdair')$pwm0('cdair') =    pwm_c('cdair')* (1+shocks(XC, TC,'pw_dair')/100);
pwm('cfobe')$pwm0('cfobe') =    pwm_c('cfobe')* (1+shocks(XC, TC,'pw_becr')/100);
pwm('coapr')$pwm0('coapr') =    pwm_c('coapr')* (1+shocks(XC, TC,'pw_ocrp')/100);
pwm('ctext')$pwm0('ctext') =    pwm_c('ctext')* (1+shocks(XC, TC,'pw_manu')/100);
pwm('cwood')$pwm0('cwood') =    pwm_c('cwood')* (1+shocks(XC, TC,'pw_wood')/100);
pwm('cpetr')$pwm0('cpetr') =    pwm_c('cpetr')* (1+shocks(XC, TC,'pw_ener')/100);
pwm('cfert')$pwm0('cfert') =    pwm_c('cfert')* (1+shocks(XC, TC,'pw_fert')/100);
pwm('cchem')$pwm0('cchem') =    pwm_c('cchem')* (1+shocks(XC, TC,'pw_manu')/100);
pwm('cnmet')$pwm0('cnmet') =    pwm_c('cnmet')* (1+shocks(XC, TC,'pw_metl')/100);
pwm('cmetl')$pwm0('cmetl') =    pwm_c('cmetl')* (1+shocks(XC, TC,'pw_metl')/100);
pwm('cmach')$pwm0('cmach') =    pwm_c('cmach')* (1+shocks(XC, TC,'pw_manu')/100);
pwm('coman')$pwm0('coman') =    pwm_c('coman')* (1+shocks(XC, TC,'pw_manu')/100);
pwm('celec')$pwm0('celec') =    pwm_c('celec')* (1+shocks(XC, TC,'pw_ener')/100);
pwm('cwatr')$pwm0('cwatr') =    pwm_c('cwatr')* (1+shocks(XC, TC,'pw_manu')/100);
pwm('ccons')$pwm0('ccons') =    pwm_c('ccons')* (1+shocks(XC, TC,'pw_manu')/100);
pwm('ctrad')$pwm0('ctrad') =    pwm_c('ctrad')* (1+shocks(XC, TC,'pw_serv')/100);
pwm('ctran')$pwm0('ctran') =    pwm_c('ctran')* (1+shocks(XC, TC,'pw_serv')/100);
pwm('chotl')$pwm0('chotl') =    pwm_c('chotl')* (1+shocks(XC, TC,'pw_serv')/100);
pwm('crest')$pwm0('crest') =    pwm_c('crest')* (1+shocks(XC, TC,'pw_serv')/100);
pwm('ccomm')$pwm0('ccomm') =    pwm_c('ccomm')* (1+shocks(XC, TC,'pw_serv')/100);
pwm('cfsrv')$pwm0('cfsrv') =    pwm_c('cfsrv')* (1+shocks(XC, TC,'pw_serv')/100);
pwm('creal')$pwm0('creal') =    pwm_c('creal')* (1+shocks(XC, TC,'pw_serv')/100);
pwm('cbsrv')$pwm0('cbsrv') =    pwm_c('cbsrv')* (1+shocks(XC, TC,'pw_serv')/100);
pwm('cpadm')$pwm0('cpadm') =    pwm_c('cpadm')* (1+shocks(XC, TC,'pw_serv')/100);
pwm('ceduc')$pwm0('ceduc') =    pwm_c('ceduc')* (1+shocks(XC, TC,'pw_serv')/100);
pwm('cheal')$pwm0('cheal') =    pwm_c('cheal')* (1+shocks(XC, TC,'pw_serv')/100);
pwm('cosrv')$pwm0('cosrv') =    pwm_c('cosrv')* (1+shocks(XC, TC,'pw_serv')/100);
 alphavaadj('acere',RG)$alphava0('acere',RG) =  (1+shocks(XC, TC, 'pd_cere')/100);
 alphavaadj('apuls',RG)$alphava0('apuls',RG) =  (1+shocks(XC, TC, 'pd_puls')/100);
 alphavaadj('aoils',RG)$alphava0('aoils',RG) =  (1+shocks(XC, TC, 'pd_oils')/100);
 alphavaadj('aroot',RG)$alphava0('aroot',RG) =  (1+shocks(XC, TC, 'pd_root')/100);
 alphavaadj('avege',RG)$alphava0('avege',RG) =  (1+shocks(XC, TC, 'pd_vege')/100);
 alphavaadj('afrui',RG)$alphava0('afrui',RG) =  (1+shocks(XC, TC, 'pd_frui')/100);
 alphavaadj('abecr',RG)$alphava0('abecr',RG) =  (1+shocks(XC, TC, 'pd_becr')/100);
 alphavaadj('aocrp',RG)$alphava0('aocrp',RG) =  (1+shocks(XC, TC, 'pd_ocrp')/100);
 alphavaadj('alive',RG)$alphava0('alive',RG) =  (1+shocks(XC, TC, 'pd_live')/100);
 alphavaadj('amilk',RG)$alphava0('amilk',RG) =  (1+shocks(XC, TC, 'pd_milk')/100);
 alphavaadj('afore',RG)$alphava0('afore',RG) =  (1+shocks(XC, TC, 'pd_fore')/100);
 alphavaadj('afish',RG)$alphava0('afish',RG) =  (1+shocks(XC, TC, 'pd_fish')/100);
 alphavaadj('aelec',RG)$alphava0('aelec',RG) =  (1+shocks(XC, TC, 'pd_elec')/100);

 FORSAVGDP = FORSAVGDP_C - shocks(XC,TC, 'FSAV');
 FSAV.L   = (FORSAVGDP    /  100) * GDP_MP;
$include includes\2closures.inc
 SOLVE STANDCGE_NLP_STATIC MINIMIZING WALRASSQR USING NLP ;
 solvestat = STANDCGE_NLP_STATIC.solvestat;
 modelstat = STANDCGE_NLP_STATIC.modelstat;
$BATINCLUDE includes\2results_slim.inc  XC TC
* sim loop (XC)
);
* year loop (TC)
);

execute_unload "output\%country%\results_STATIC.gdx" GDPX GDPX_PLUS GDP_CLIM_SH_shX FSAVX MACROTABX HHD_CONS_X PH_export model_stat_export
execute 'xlstalk.exe -m output\%country%\%country%_STATIC.xlsx';
 XLTEST = ERRORLEVEL;
IF(XLTEST = 1,
execute 'xlstalk.exe -c output\%country%\%country%_STATIC.xlsx';
);
IF(XLTEST = 2,
execute 'xlstalk.exe -s output\%country%\%country%_STATIC.xlsx';
);
execute 'gdxxrw.exe i=output\%country%\results_STATIC.gdx o=output\%country%\%country%_STATIC.xlsx index=index!a5' ;

HHD_CONS_XD(XC, TC, 'urban')  = 100*(HHD_CONS_X(XC, TC, 'urban') / HHD_CONS_X('0', '2019', 'urban') -1)  ;
HHD_CONS_XD(XC, TC, 'urban_poor')  = 100*(HHD_CONS_X(XC, TC, 'urban_poor') / HHD_CONS_X('0', '2019', 'urban_poor') -1)  ;
HHD_CONS_XD(XC, TC, 'urban_midd')  =  100*(HHD_CONS_X(XC, TC, 'urban_midd') / HHD_CONS_X('0', '2019', 'urban_midd') -1)  ;
HHD_CONS_XD(XC, TC, 'urban_rich')  = 100*(HHD_CONS_X(XC, TC, 'urban_rich') / HHD_CONS_X('0', '2019', 'urban_rich') -1)  ;

HHD_CONS_XD(XC, TC, 'rural')  = 100*(HHD_CONS_X(XC, TC, 'rural') / HHD_CONS_X('0', '2019', 'rural') -1)  ;
HHD_CONS_XD(XC, TC, 'rural_poor')  = 100*(HHD_CONS_X(XC, TC, 'rural_poor') / HHD_CONS_X('0', '2019', 'rural_poor') -1)  ;
HHD_CONS_XD(XC, TC, 'rural_midd')  =  100*(HHD_CONS_X(XC, TC, 'rural_midd') / HHD_CONS_X('0', '2019', 'rural_midd') -1)  ;
HHD_CONS_XD(XC, TC, 'rural_rich')  = 100*(HHD_CONS_X(XC, TC, 'rural_rich') / HHD_CONS_X('0', '2019', 'rural_rich') -1)  ;

HHD_INC_XD(XC, TC, 'urban')  = 100*(HHD_INC_X(XC, TC, 'urban') / HHD_INC_X('0', '2019', 'urban') -1)  ;
HHD_INC_XD(XC, TC, 'urban_poor')  = 100*(HHD_INC_X(XC, TC, 'urban_poor') / HHD_INC_X('0', '2019', 'urban_poor') -1)  ;
HHD_INC_XD(XC, TC, 'urban_midd')  =  100*(HHD_INC_X(XC, TC, 'urban_midd') / HHD_INC_X('0', '2019', 'urban_midd') -1)  ;
HHD_INC_XD(XC, TC, 'urban_rich')  = 100*(HHD_INC_X(XC, TC, 'urban_rich') / HHD_INC_X('0', '2019', 'urban_rich') -1)  ;

HHD_INC_XD(XC, TC, 'rural')  = 100*(HHD_INC_X(XC, TC, 'rural') / HHD_INC_X('0', '2019', 'rural') -1)  ;
HHD_INC_XD(XC, TC, 'rural_poor')  = 100*(HHD_INC_X(XC, TC, 'rural_poor') / HHD_INC_X('0', '2019', 'rural_poor') -1)  ;
HHD_INC_XD(XC, TC, 'rural_midd')  =  100*(HHD_INC_X(XC, TC, 'rural_midd') / HHD_INC_X('0', '2019', 'rural_midd') -1)  ;
HHD_INC_XD(XC, TC, 'rural_rich')  = 100*(HHD_INC_X(XC, TC, 'rural_rich') / HHD_INC_X('0', '2019', 'rural_rich') -1)  ;
