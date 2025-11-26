**** SGH Advanced Econometrics Homework 3 
***** Name: Fasika Mulu Nega 
***** ID: fn112715
*Excercise 1 : consumption expenditure of households 
* Data prepartion 
use"http://web.sgh.waw.pl/~jmuck/AAE/Datasets/ConsumptionUS.dta"
sort date 
gen t=_n
tsset t
*i) Estimation 
gen lnct= ln(C)
gen lnYt=ln(Y)
reg lnct lnYt
*ii) Plotting a series of consumption and disposable income  
tsline lnct
tsline lnYt
tsline lnct lnYt
*iii) Order of integration of lnct and lnyt 
reg lnct lnYt
estimate store ts_1
dfuller lnct, regress 
dfuller lnYt, regress
gen Dlnyt=D.lnYt
dfuller Dlnyt, regress
* iv) Test if consumption is trend stationary 
dfuller lnct, regress trend 
dfuller lnct, lags(1) regress trend
dfuller lnct, lags(4) regress trend
dfuller lnct, lags(8) regress trend
* Graphically 
reg lnct t
predict res1, res
tsline res1 
tsline lnct t
reg lnYt t 
predict res2, res 
tsline res2 
tsline lnYt t
*v) Second model estimation ADL(1,1)
reg D.lnct D.L.lnct D.lnYt D.L.lnYt 
estat bgodfrey
* Calculation of longrun and short run multiplier 
nlcom(b_sr:_b[D.lnYt]) (b_lr:_b[D.lnYt]+_b[D.L.lnYt]/(1-_b[D.L.lnct]))
* Taking 3 lag length given there is serial correlation 
reg D.lnct D.L(1/3).lnct D.lnYt D.L.lnYt 
estat bgodfrey
* short run and long run multiplier 
nlcom(b_sr:_b[D.lnYt]) (b_lr:_b[D.lnYt]+_b[D.L.lnYt]/(1-_b[D.L.lnct]))
*Vii) Cointegration test 
reg lnct lnYt
predict r1, res
tsline r1
dfuller r1, nocons
disp"critical value (5% significance level) -3.37"
* viii) Error correction model estimation 
reg lnct lnYt
predict ec1, res
tsline ec1
reg D.lnct D.lnct D.L.lnYt L.ec1
estat bgodfrey
** Determining the information criteria 
* for P=4 K=1
reg D.lnct D.L(1/4).lnct D.L(0/1).lnYt L.ec1
estat bgodfrey, lags(1/4)
* For P=3 K=1
reg D.lnct D.L(1/3).lnct D.L(0/1).lnYt L.ec1
estat bgodfrey, lags(1/3)
* Calculation of half life 
nlcom(half_life:ln(.5)/ln(1+_b[L.ec1]) )

////////////////////////////////////////////////////////////

*Excercise 2 SVAR model on GDP and unemployemnt 
** Data preparation 
clear all 
use"http://web.sgh.waw.pl/~jmuck/AAE/Datasets/VAR_UK.dta"
sort time 
gen t=_n
tsset t
*ii) SVAR Estimation with 4 lags  
gen lnGDP=ln( GDP)
gen dlngdp= d.lnGDP
matrix lr=(.,0\.,.)
svar dlngdp UNRATE, lag(1/4) lreq(lr)
* iii) Serial correlation and stability test 
varlmar ,mlag(4)
varstable
*iv) Impuls response of demand and supply shock 
irf creat var, set( myifr) replace step(40)
* to see the effect of demand shock on GDP
irf graph sirf, impulse(UNRATE) response(dlngdp) 
* to see the effect of supply shock on GDP 
irf graph sirf, impulse(dlngdp) response(dlngdp) 
* forcast variance decomposition to see the effect of supply shock UNRATE and GDP
irf graph sfevd, impulse(dlngdp)
* forcast variance decomposition to see the effect of Demand shock UNRATE and GDP
irf graph sfevd, impulse(UNRATE)
* Implus response reaction to both supply and demand function in one panel. 
irf graph sirf 

*vi) SVAR estimation with 8 lags 
matrix C=(.,0\.,.)
svar dlngdp UNRATE, lag(1/8) lreq(C)
varlmar ,mlag(8)
varstable
irf creat var, set( myifr) replace step(40)
* To see the effect of demand shock on GDP
irf graph sirf, impulse(UNRATE) response(dlngdp) 
* To see the effect of supply shock on GDP 
irf graph sirf, impulse(dlngdp) response(dlngdp) 
* Forcast varaince decomposition to see the effect of supply shock on UNRATE and GDP
irf graph sfevd, impulse(dlngdp)
* Forcast varaince decomposition to see the effect of Demand shock on UNRATE and GDP
irf graph sfevd, impulse(UNRATE)
* Implus response reaction to both supply and demand function
irf graph sirf 

//////////////////////////////////////////////////////////

*Exercise 3 Gravity model -panel dataset 
** Data preparation 
clear all
use"http://web.sgh.waw.pl/~jmuck/AAE/Datasets/Gravity.dta
sort Year 
egen pan_ID=group( ReporterISO3 PartnerISO3 )
xtset pan_ID Year
*i) Data description and estimation 
xtdescribe
gen lnEXijt = ln(Exports)
gen lngdpit = ln(GDPReporter)
gen lngdpjt = ln(GDPPartner)
gen lndistij = ln(dist)
reg lnEXijt lngdpit lngdpjt lndistij
estimates store pooled
* Heteroskdacticty test 
* graphically 
predict ehat, res
gen ehat2=ehat^2
scatter ehat2 lngdpit
scatter ehat2 lngdpjt
scatter ehat2 lndistij
* IM test 
 estat imtest, white
 
*ii) clustered estimation 
*a) cluseted by exporting country 
reg lnEXijt lngdpit lngdpjt lndistij, vce(cluster ReporterISO3)
estimates store Cluster1 
*b) Clusterd by importing country 
reg lnEXijt lngdpit lngdpjt lndistij, vce(cluster PartnerISO3)
estimates store Cluster2
*Clustered by pair of exporting and importing country 
reg lnEXijt lngdpit lngdpjt lndistij, vce(cluster pan_ID)
estimates store Cluster3

*iv) Random effect estimation 
xtreg lnEXijt lngdpit lngdpjt lndistij, re
predict Reeffect, u
estimate store re
* Heteroskdacticty test 
predict ehatre, e 
gen ehatre2=ehatre^2
scatter ehatre2 lngdpit
scatter ehatre2 lngdpjt
scatter ehatre2 lndistij

* v) LM test for variance 
xtreg lnEXijt lngdpit lngdpjt lndistij, re
xttest0
*Random effect clustered estimation 
xtreg lnEXijt lngdpit lngdpjt lndistij, re vce( cluster pan_ID)
estimate store re_cluster
 xttest0
* vi) Fixed effect estimation 
xtreg lnEXijt lngdpit lngdpjt lndistij, fe
predict feeffect , u
estimate store fe
*heteroskfacticty test 
predict ehatfe, e 
gen ehatfe2=ehatfe^2
scatter ehatfe2 lngdpit
scatter ehatfe2 lngdpjt
scatter ehatfe2 lndistij
* Fixed effect Clustered 
xtreg lnEXijt lngdpit lngdpjt lndistij, fe vce( cluster pan_ID)
predict effects_fecluster, u
estimate store fe_cluster
* In one tabel  
estimates table pooled fe fe_cluster re re_cluster Cluster1 Cluster2 Cluster3 , b se t p

*viii) Hausman test 
hausman fe re

*ix) Regression with dummy variable 
**a) pooled regression with dummmy 
reg lnEXijt lngdpit lngdpjt lndistij i.Year, vce(cluster pan_ID)
estimate store pooledD
**b) Random effect with Year dummy 
xtreg lnEXijt lngdpit lngdpjt lndistij i.Year, re 
estimate store reT
*heteroskdacticity test
predict ehatreT, e 
gen ehatreT2=ehatreT^2
scatter ehatreT2 lngdpit
scatter ehatreT2 lngdpjt
scatter ehatreT2 lndistij
** Robust cluserted estimation 
xtreg lnEXijt lngdpit lngdpjt lndistij i.Year, re vce(cluster pan_ID)
estimate store reD

**** LM test for variance 
xtreg lnEXijt lngdpit lngdpjt lndistij i.Year, re vce(cluster pan_ID)
xttest0
*c) fixed effect with dummy 
xtreg lnEXijt lngdpit lngdpjt lndistij i.Year, fe
estimate store feT
* Heteroskdacticty test
predict ehatfeT, e 
gen ehatfeT2=ehatfeT^2
scatter ehatfeT2 lngdpit
scatter ehatfeT2 lngdpjt
scatter ehatfeT2 lndistij
* Robust clustered estimation 
xtreg lnEXijt lngdpit lngdpjt lndistij i.Year, fe vce(cluster pan_ID)
estimate store feD

***On one table 
estimates table pooledD feD reD, b se t p
*** Hausman test 
hausman reT feT

*x) Joint significance test 
**pooled regerssion 
reg lnEXijt lngdpit lngdpjt lndistij i.Year, vce(cluster pan_ID)
testparm i.Year
**Fixed effect regerssion 
xtreg lnEXijt lngdpit lngdpjt lndistij i.Year, fe vce(cluster pan_ID)
testparm i.Year
predict fixed, u 
**Random effect model 
xtreg lnEXijt lngdpit lngdpjt lndistij i.Year, re vce(cluster pan_ID)
testparm i.Year
predict random, u
**Trend effect 
scatter fixed random 

*xi) Variation in individual effect
**Correlation between individual effect and the logged distance 
xtreg lnEXijt lngdpit lngdpjt lndistij i.Year, fe vce(cluster pan_ID)
predict FE, u
corr FE lndistij
*xii) Random effect Reggression using EU trade a new dummy varible 
foreach v of varlist PartnerName ReporterName {
gen byte eu_`v' = inlist(`v',15, 12, 22, 19, 38, 30, 60, 46, 63, 48, 64, 49, 66, 51, 76, 60, 82, 65, 85, 67, 93, 73, 96, 75, 110, 86, 116, 92, 118, 94, 130, 105,135, 109, 136, 110, 144, 118, 163, 134, 187, 152, 188, 153, 191, 156, 207, 167, 208, 168, 215, 171, 224, 179)
}
gen byte eu_trade = ReporterName & PartnerName
replace eu_trade =0 if eu_PartnerName == 0 | eu_ReporterName == 0
*Generating time dummy 
tab Year, gen(TD)
** Random effect estimate  with EU dummy varaible 
xtreg lnEXijt lngdpit lngdpjt lndistij eu_trade TD2-TD30, re
* Heteroskdacticty test
predict ehatreE, e 
gen ehatreE2=ehatreE^2
scatter ehatreE2 lngdpit
scatter ehatreE2 lngdpjt
scatter ehatreE2 lndistij
* Robust clustered estimation
xtreg lnEXijt lngdpit lngdpjt lndistij eu_trade TD2-TD30, re vce(cluster pan_ID) 
estimate store reEU
*xiii) Husman Tylor Estimation 
xthtaylor lnEXijt lngdpit lngdpjt lndistij eu_trade TD2-TD30, endo(eu_trade) 
* Heteroskdacticty test
predict ehathtE, e 
gen ehathtE2=ehathtE^2
scatter ehathtE2 lngdpit
scatter ehathtE2 lngdpjt
scatter ehathtE2 lndistij
* Robust clustered estimation 
xthtaylor lnEXijt lngdpit lngdpjt lndistij eu_trade TD2-TD30, endo(eu_trade) vce(cluster pan_ID)
estimates store ht
estimates table reEU ht, b se p
