
*** SGH Advanced Economietrics Assignemnt 
****Name: Fasika Mulu Nega
*****ID: fn112715
* Excercise 1
* Data preparation 
use http://web.sgh.waw.pl/~jmuck/AAE/Datasets/nkpc.dta
gen MC = log( LS)
gen Pdeflator= log(GDPDEF)
gen inf = D.Pdeflator
xtset

* iii) GMM estimation 
gmm (({theta}*(inf))-(1-{theta})*(1-({beta}*{theta}))*(MC)-({beta}*{theta})*(f.inf)), instruments(L(1/4).inf L(1/4).MC) from(beta=0.99 theta=.75) vce(robust) twostep wmat(hac nw 12)
* overidentfication test
estat overid 

* iv) Statinarity test 
dfuller MC
* futher tested stationarity for marginal cost 
dfuller MC, lag(4)
dfuller MC, trend lags(1)
dfuller MC, trend lags(4)
* infaltion 
dfuller inf
* scatter plot to see if they are stationary 
twoway (tsline inf)
twoway (tsline inf MC)
twoway (tsline MC)

* v) GMM estimation with new proxy output gap
gmm (({theta}*(inf))-(1-{theta})*(1-({beta}*{theta}))*(output_gap)-({beta}*{theta})*(f.inf)), instruments(L(1/4).inf L(1/4).output_gap) from(beta=0.99 theta=.75) vce(robust) twostep wmat(hac nw 12)
estimate store outputgap
estat overid 
* And logged labor share detrend 
gen DLS=ln(LS)
gen T=t^2
reg DLS T t
predict DLSR, residual
gmm (({theta}*(inf))-(1-{theta})*(1-({beta}*{theta}))*(DLSR)-({beta}*{theta})*(f.inf)), instruments(L(1/4).inf L(1/4).DLSR) from(beta=0.99 theta=.75) vce(robust) twostep wmat(hac nw 12)
estimates store Dtlabor
estat overid
estimates table outputgap Dtlabor, b se p t

* vi) Hybrid EStimation 
*With marginal cost as a proxy
gmm (({theta}+({omega}*[1-{theta}*(1-{beta})]))*(inf)-(1-{omega})*(1-{theta})*(1-({beta}*{theta}))*(MC)-({beta}*{theta})*(f.inf)-{omega}*(L.inf)),instruments(L(1/4).inf L(1/4).MC) from(beta=0.99 theta=.75 omega=0.2) vce(robust) twostep wmat(hac nw 12)
estat overid
estimates store NPK1
* With output gap as a proxy
gmm (({theta}+({omega}*[1-{theta}*(1-{beta})]))*(inf)-(1-{omega})*(1-{theta})*(1-({beta}*{theta}))*(output_gap)-({beta}*{theta})*(f.inf)-{omega}*(L.inf)),instruments(L(1/4).inf L(1/4).output_gap) from(beta=0.99 theta=.75 omega=0.2) vce(robust) twostep wmat(hac nw 12)
estat overid
estimates store NPK2
* With detrended labor share as a proxy
gmm (({theta}+({omega}*[1-{theta}*(1-{beta})]))*(inf)-(1-{omega})*(1-{theta})*(1-({beta}*{theta}))*(DLSR)-({beta}*{theta})*(f.inf)-{omega}*(L.inf)),instruments(L(1/4).inf L(1/4).DLSR) from(beta=0.99 theta=.75 omega=0.2) vce(robust) twostep wmat(hac nw 12)
estat overid
estimates store NPK3
estimates table NPK1 NPK2 NPK3, b se p t

*(viii) Replicate with more additional momment condition
* With Marginal cost 
gmm (({theta}+({omega}*[1-{theta}*(1-{beta})]))*(inf)-(1-{omega})*(1-{theta})*(1-({beta}*{theta}))*(MC)-({beta}*{theta})*(f.inf)-{omega}*(L.inf)),instruments(L(1/4).inf L(1/4).MC L(1/4).output_gap) from(beta=0.99 theta=.75 omega=0.2) vce(robust) twostep wmat(hac nw 12)
estimates store IVGAP
estat overid
* With Output gap  
gmm (({theta}+({omega}*[1-{theta}*(1-{beta})]))*(inf)-(1-{omega})*(1-{theta})*(1-({beta}*{theta}))*(output_gap)-({beta}*{theta})*(f.inf)-{omega}*(L.inf)),instruments(L(1/4).inf L(1/4).MC L(1/4).output_gap) from(beta=0.99 theta=.75 omega=0.2) vce(robust) twostep wmat(hac nw 12)
estimates store IVMC
estat overid
* With labor share 
gmm (({theta}+({omega}*[1-{theta}*(1-{beta})]))*(inf)-(1-{omega})*(1-{theta})*(1-({beta}*{theta}))*(DLSR)-({beta}*{theta})*(f.inf)-{omega}*(L.inf)),instruments(L(1/4).inf L(1/4).DLSR L(1/4).MC L(1/4).output_gap) from(beta=0.99 theta=.75 omega=0.2) vce(robust) twostep wmat(hac nw 12)
estimate store IVMCL
estat overid 
estimates table IVGAP IVMC IVMCL, b se p t

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

*Excercise 2 Logit and Probit 
* Data preparation 
clear all
use http://web.sgh.waw.pl/~jmuck/AAE/Datasets/birthweight.dta
gen lbwit=1 if bwght<2500
replace lbwit=0 if bwght>=2500 
tab lbwit
bysort lbwit: sum smoke
bysort lbwit: sum male

* OLS regression and heteroskdasticity test 
reg bwght age npvis omaps male smoke
estat hettest

*i) logit estimation 
logit lbwit age npvis omaps male smoke
estimates store logit
* Calculation of marginal effects 
margins, dydx(*)

*ii) calculating the odds ratio
* Smoke 
nlcom (OR_smoke:exp(_b[smoke]))
nlcom (OR_smoke:(exp(_b[smoke])-1)*100)
* Omap 
nlcom (OR_omaps:exp(_b[omaps]))
nlcom (OR_omaps:(exp(_b[omaps])-1)*100)
*(iii) marginal effect
margins, dydx (smoke omaps)
tab omaps
margins, dydx (smoke) at(omaps=(3 8 9))
margins, dydx (smoke omaps) at(omaps=(3 8 9))

*(v) classification test 
logit lbwit age npvis omaps male smoke
predict phat 
scatter phat lbwit
tab lbwit
tabulate lbwit lbweight
 disp ( 1247+ 51)/1832
 * using stata built in command 
 estat classification
*(vi) multinomial 
gen bwit=3 if bwght < 2000 & bwght !=.
replace bwit=2 if bwght >= 2000 & bwght<=2500 & bwght !=.
replace bwit=1 if bwght >= 2500 & bwght !=.
label define normal 2 "2=very low" 1 "1=low" 0 "0=normal"
mlogit bwi age npvis omaps male smoke
estimates store mlogit
estimates table logit mlogit, b se p t 

 *(vii) relative risk ratio
mlogit bwit age npvis omaps male smoke, rrr

 *(viii) standard linear regression
gen lnbwit=ln(bwght)
reg lnbwit age npvis omaps male smoke, vce(robust)
estimates store ols
estimates table ols mlogit logit, b se p t

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
 
* Excercise 3 : Daynamic Panel regression
* Data Preparation 
clear all 
import delimited https://covid.ourworldindata.org/data/owid-covid-data.csv
egen panelID=group( location)
gen Date=date(date, "YMD")
format Date %td
xtset panelID Date 

* iii) Static panel estimation 
* pooled regression
reg new_deaths_per_million stringency_index total_vaccinations_per_hundred
estimate store pooled 
* Random effect estimation 
xtreg new_deaths_per_million stringency_index total_vaccinations_per_hundred, re
estimate store RE
xttest0
* heteroskdasticity test 
predict ehatre, e 
gen ehatre2=ehatre^2
scatter ehatre2 stringency_index
scatter ehatre2 total_vaccinations_per_hundred
* Using robust estimation 
xtreg new_deaths_per_million stringency_index total_vaccinations_per_hundred, re vce(cluster iso_code)
estimate store VRE
* Fixed effect estimation
xtreg new_deaths_per_million stringency_index total_vaccinations_per_hundred, fe
estimate store FE
* Heteroskdastity test 
predict ehatfe, e 
gen ehatfe2=ehatfe^2
scatter ehatfe2 stringency_index
scatter ehatfe2 total_vaccinations_per_hundred
* Using Robust estimation 
xtreg new_deaths_per_million stringency_index total_vaccinations_per_hundred, fe vce(cluster iso_code)
estimate store VFE 
* to compare the results 
 estimate table pooled VRE VFE, b se p t
 * hausman test 
 hausman FE RE
 
 *iv) Testing serial correlation using fixed effects 
xtreg new_deaths_per_million stringency_index total_vaccinations_per_hundred, fe
predict ehat, e 
corr ehat L.ehat
scatter ehat L.ehat 
*using daynamic estimation 
xtreg new_deaths_per_million L.new_deaths_per_million stringency_index total_vaccinations_per_hundred, fe

******* V) Dynamic panel data estimation  
** Unit root test Fisher-ADF test
xtunitroot fisher new_cases_per_million, dfuller trend lags(0)
xtunitroot fisher new_cases_per_million, dfuller trend lags(4)

* a). Anderson Hsiao
ivregress 2sls d.new_deaths_per_million (L.d.new_deaths_per_million=L2.new_deaths_per_million) d.stringency_index d.total_vaccinations_per_hundred 
ivregress 2sls d.new_deaths_per_million (L.d.new_deaths_per_million=L2.d.new_deaths_per_million) d.stringency_index d.total_vaccinations_per_hundred 
estimate store AH

* b) Arellano-Bond 
xtabond new_deaths_per_million stringency_index total_vaccinations_per_hundred, twostep vce(robust) maxldep(10)
estimate store AB
*serial correlation test 
estat abond 
* overidentfication test
xtabond new_deaths_per_million stringency_index total_vaccinations_per_hundred, twostep maxldep(10)
estat sargan 
* short run and longrun multipliers 
nlcom(b_sr:_b[stringency_index]) (b_lr:_b[stringency_index]/(1-_b[L.new_deaths_per_million]))
nlcom(b_sr:_b[total_vaccinations_per_hundred]) (b_lr:_b[total_vaccinations_per_hundred]/(1-_b[L.new_deaths_per_million]))
*c) System GMM estimator 
xtdpdsys new_deaths_per_million stringency_index total_vaccinations_per_hundred, twostep vce(robust) maxldep(10)
estimate store GMM
* serial correlation
estat abond 
*overidentfication test 
xtdpdsys new_deaths_per_million stringency_index total_vaccinations_per_hundred, twostep maxldep(10)
estat sargan 

* short run and long run multipier 
nlcom(b_sr:_b[stringency_index]) (b_lr:_b[stringency_index]/(1-_b[L.new_deaths_per_million]))
nlcom(b_sr:_b[total_vaccinations_per_hundred]) (b_lr:_b[total_vaccinations_per_hundred]/(1-_b[L.new_deaths_per_million]))

estimates table AB GMM, b se p t
 
*Vi) extentions of (V) with 7 lags 
 
* a) Arellano-Bond 
xtabond new_deaths_per_million L7.new_deaths_per_million stringency_index total_vaccinations_per_hundred, twostep vce(robust) maxldep(10)
estimate store AB7
* serial correlation test
estat abond 
* overidentfication test
xtabond new_deaths_per_million L7.new_deaths_per_million stringency_index total_vaccinations_per_hundred,twostep maxldep(10)
estat sargan 

* shortrun and long run multiplier.
nlcom(b_sr:_b[stringency_index]) (b_lr:_b[stringency_index]/(1-_b[L7.new_deaths_per_million]))
nlcom(b_sr:_b[total_vaccinations_per_hundred]) (b_lr:_b[total_vaccinations_per_hundred]/(1-_b[L7.new_deaths_per_million]))

*b). System GMM estimator 
xtdpdsys L7.new_deaths_per_million stringency_index total_vaccinations_per_hundred, twostep vce(robust) maxldep(10)
estimate store GMM7
*serial correlation 
estat abond
*overidentfication test  
xtdpdsys L7.new_deaths_per_million stringency_index total_vaccinations_per_hundred, twostep maxldep(10)
estat sargan 

* short run and long run multiplier 
nlcom(b_sr:_b[stringency_index]) (b_lr:_b[stringency_index]/(1-_b[L8.new_deaths_per_million]))
nlcom(b_sr:_b[total_vaccinations_per_hundred]) (b_lr:_b[total_vaccinations_per_hundred]/(1-_b[L8.new_deaths_per_million]))

* endogenity test: 
xtdpdsys L7.new_deaths_per_million stringency_index total_vaccinations_per_hundred, twostep vce(robust) maxldep(10)
predict ehats, e
corr stringency_index ehats
corr total_vaccinations_per_hundred  ehats

xtabond L7.new_deaths_per_million stringency_index total_vaccinations_per_hundred, twostep vce(robust) maxldep(10)
predict ehat2, e
corr stringency_index ehat2
corr total_vaccinations_per_hundred  ehat2
*graphically 
scatter stringency_index ehat2
scatter ehat2 total_vaccinations_per_hundred 

* All estimate result of v and vi in one tabel 

estimates table AB AB7 GMM GMM7, b se p t
 
*vii) Estimation with ednogenous varaible as total_vaccinations_per_hundred 
*Arellano-Bond 
xtabond L7.new_deaths_per_million stringency_index total_vaccinations_per_hundred, twostep vce(robust) maxldep(10) endog(total_vaccinations_per_hundred) maxlag(10)
estimate store ABendo
* serial correlation 
estat abond 
*overidentfication test 
xtabond L7.new_deaths_per_million stringency_index total_vaccinations_per_hundred, twostep maxldep(10) endog(total_vaccinations_per_hundred) maxlag(10)
estat sargan
* short run and longrun multipier
nlcom(b_sr:_b[stringency_index]) (b_lr:_b[stringency_index]/(1-_b[L8.new_deaths_per_million]))
nlcom(b_sr:_b[total_vaccinations_per_hundred]) (b_lr:_b[total_vaccinations_per_hundred]/(1-_b[L8.new_deaths_per_million]))

* system GMM estimator
xtdpdsys L7.new_deaths_per_million stringency_index total_vaccinations_per_hundred, twostep vce(robust) maxldep(10) endog(total_vaccinations_per_hundred) maxlag(10)
estimate store GMMendo
*serial correlation
estat abond
* overidentfication test
xtdpdsys L7.new_deaths_per_million stringency_index total_vaccinations_per_hundred, twostep maxldep(10) endog(stringency_index) maxlag(10)
estat sargan
* short run and longrun multipier
nlcom(b_sr:_b[stringency_index]) (b_lr:_b[stringency_index]/(1-_b[L8.new_deaths_per_million]))
nlcom(b_sr:_b[total_vaccinations_per_hundred]) (b_lr:_b[total_vaccinations_per_hundred]/(1-_b[L8.new_deaths_per_million]))

estimates table AB AB7 GMM GMM7 ABendo GMMendo, b se p t

*vii) Aggregation by monthly frequency 

* Plots of varibles on daily , weekly and monthly frquency 
* Daily
tsline new_deaths_per_million
tsline stringency_index
tsline  total_vaccinations_per_hundred
* Weekly 
clear all 
import delimited https://covid.ourworldindata.org/data/owid-covid-data.csv
egen panelID=group( location)
gen Date=date(date, "YMD")
format Date %td
xtset panelID Date 
gen weekly=wofd(Date)
format weekly %tw
collapse (mean) new_deaths_per_million stringency_index total_vaccinations_per_hundred, by(weekly panelID)
sort panelID weekly
xtset panelID weekly
tsline new_deaths_per_million
tsline stringency_index
tsline  total_vaccinations_per_hundred

* replicating previous estimation by weekly frequency
* Arellano-Bond
xtabond L7.new_deaths_per_million stringency_index total_vaccinations_per_hundred, twostep vce(robust) maxldep(10) endog(total_vaccinations_per_hundred) maxlag(10)
estimate store ABwk
* serial correlation
estat abond 
*overidentfication test 
xtabond L7.new_deaths_per_million stringency_index total_vaccinations_per_hundred, twostep maxldep(10) endog(total_vaccinations_per_hundred) maxlag(10)
estat sargan
* short run and longrun multipier
nlcom(b_sr:_b[stringency_index]) (b_lr:_b[stringency_index]/(1-_b[L8.new_deaths_per_million]))
nlcom(b_sr:_b[total_vaccinations_per_hundred]) (b_lr:_b[total_vaccinations_per_hundred]/(1-_b[L8.new_deaths_per_million]))

* system GMM estimator 
xtdpdsys L7.new_deaths_per_million stringency_index total_vaccinations_per_hundred, twostep vce(robust) maxldep(10) endog(total_vaccinations_per_hundred) maxlag(10)
estimate store GMMwk
* serial correlation
estat abond 
*overidentfication test 
xtdpdsys L7.new_deaths_per_million stringency_index total_vaccinations_per_hundred, twostep maxldep(10) endog(total_vaccinations_per_hundred) maxlag(10)
estat sargan 

* shortrun and longrun multiplier 
nlcom(b_sr:_b[stringency_index]) (b_lr:_b[stringency_index]/(1-_b[L8.new_deaths_per_million]))
nlcom(b_sr:_b[total_vaccinations_per_hundred]) (b_lr:_b[total_vaccinations_per_hundred]/(1-_b[L8.new_deaths_per_million]))

estimates table ABwk GMMwk, b se p t 

* monthly 
clear all 
import delimited https://covid.ourworldindata.org/data/owid-covid-data.csv
egen panelID=group( location)
gen Date=date(date, "YMD")
format Date %td
xtset panelID Date
gen monthly=mofd(Date)
format monthly %tm
collapse (mean) new_deaths_per_million stringency_index total_vaccinations_per_hundred, by(monthly panelID)
sort panelID monthly
xtset panelID monthly
tsline new_deaths_per_million
tsline stringency_index
tsline  total_vaccinations_per_hundred

*ix) replicating previous estimation monthly 
* Arellano-Bond
xtabond L7.new_deaths_per_million stringency_index total_vaccinations_per_hundred, twostep vce(robust) maxldep(10) endog(total_vaccinations_per_hundred) maxlag(10)
estimate store ABmnth
* serial correlation
estat abond 
*overidentfication test 
xtabond L7.new_deaths_per_million stringency_index total_vaccinations_per_hundred, twostep maxldep(10) endog(total_vaccinations_per_hundred) maxlag(10)
estat sargan
* short run and longrun
nlcom(b_sr:_b[stringency_index]) (b_lr:_b[stringency_index]/(1-_b[L8.new_deaths_per_million]))
nlcom(b_sr:_b[total_vaccinations_per_hundred]) (b_lr:_b[total_vaccinations_per_hundred]/(1-_b[L8.new_deaths_per_million]))

* system GMM estimator 
xtdpdsys L7.new_deaths_per_million stringency_index total_vaccinations_per_hundred, twostep vce(robust) maxldep(10) endog(total_vaccinations_per_hundred) maxlag(10)
estimate store GMMmnth
* serial correlation
estat abond 
*overidentfication test 
xtdpdsys L7.new_deaths_per_million stringency_index total_vaccinations_per_hundred, twostep maxldep(10) endog(total_vaccinations_per_hundred) maxlag(10)
estat sargan

* short run and long run multiplier 
nlcom(b_sr:_b[stringency_index]) (b_lr:_b[stringency_index]/(1-_b[L8.new_deaths_per_million]))
nlcom(b_sr:_b[total_vaccinations_per_hundred]) (b_lr:_b[total_vaccinations_per_hundred]/(1-_b[L8.new_deaths_per_million]))



estimates table ABmnth GMMmnth, b se p t 

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////