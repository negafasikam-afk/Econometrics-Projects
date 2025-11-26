
*==============================================================*
 *Advanced Applied Econometrics : Homework 2 
 * Fasika Mulu Nega 
 * ID: fn112715
*Exercise 1 : Gravity model  
use"http://web.sgh.waw.pl/~jmuck/AAE/Datasets/Gravity.dta"
**Generating logged varibles
gen lnexij=log( Exports)
gen lngdpi=log( GDPReporter)
gen lngdpj=log( GDPPartner)
gen lndisij=log( dist)
* (ii) OLS regression anaysis 
reg lnexij lngdpi lngdpj lndisij
estimates store ols

* (iii)calcuating critical value for the above models for 95% degrees of freedom ( forHa: b>0)
disp invt(467956, 1-0.05/2)

* (v) Testing collinearity between explanatory variables 
reg lnexij lngdpi lngdpj lndisij
vif
*pairwise correlation ( correlation matrix)
corr(lngdpi lngdpj lndisij)

* (vi) Testing normality by histograms
reg lnexij lngdpi lngdpj lndisij 
predict residuals, res
estimate store model
histogram residuals, normal bin(50)
* and Jaque berra sktest of Skewness and kurtosis  
sktest residuals*, noadj

*(vii) Model specification, RESET test 
reg lnexij lngdpi lngdpj lndisij 
estat ovtest 
** Additionally plotting the two way scatter to test the functionl form 
reg lnexij lngdpi lngdpj lndisij 
predict yhat1
twoway scatter lnexij lngdpi||line yhat1 lngdpi
twoway scatter lnexij lngdpj||line yhat1 lngdpj
twoway scatter lnexij lndisij||line yhat1 lndisij

*(vii) Testing for Heteroskedasticty by plotting the squared residuals vs the explanatory variable 
reg lnexij lngdpi lngdpj lndisij 
predict ehat, res 
gen ehat2=ehat^2
scatter ehat2 lngdpi
scatter ehat2 lngdpj
scatter ehat2 lndisij

*(ix) Regression for each reporting country, estimation of the variance of the error term 

levelsof ReporterName, local(countries)
gen rss = .
gen df_m = .  

foreach cnt of local countries {
regress lnexij lngdpi lngdpj lndisij if ReporterName == `cnt' 
replace rss=e(rss) if ReporterName == `cnt'
replace df_m=e(df_r) if ReporterName ==`cnt'
}
 
gen sigma = rss/df_m
** to check the correlation between estimated variance of the error term and GDP of country i 
corr(sigma lngdpi)
* or by taking the average or gdp of exporting/reporting country 
bysort ReporterName: egen lngdpi_mean=mean(lngdpi)
corr(sigma lngdpi_mean)
** to check if the error term varies among reporting countries and see if the variance of the error term is contant ( using average gdp and distance)
tabstat  sigma, stat(var)
scatter sigma lngdpi_mean
bysort ReporterName: egen lngdpj_mean=mean(lngdpj)
scatter sigma lngdpj_mean
bysort ReporterName: egen lndisij_mean=mean(lndisij) 
scatter sigma lndisij_mean

* (x) a. White test of the error term(squares only)
gen lngdpi2=lngdpi^2
gen lngdpj2=lngdpj^2
gen lndisij2=lndisij^2
reg ehat2 lngdpi lngdpj lndisij lngdpi2 lngdpj2 lndisij2
estimate store white1
scalar LM=e(r2)*e(N)
scalar crit=invchi2(6,0.95)
scalar pval=chi2tail(6,LM)
disp "The White test stratistics:	" LM
disp "The 5% critical value:" crit
disp "P-value: " pval
* b. The White test (squares and interactions)
gen lngdpi_lngdpj=lngdpi*lngdpj 
gen lngdpi_lndisij=lngdpi*lndisij
gen lngdpj_lndisij=lngdpj*lndisij
reg ehat2 lngdpi lngdpj lndisij lngdpi2 lngdpj2 lndisij2 lngdpi_lndisij lngdpi_lngdpj lngdpj_lndisij 
estimate store white2
scalar LM=e(r2)*e(N)
scalar crit=invchi2(9,0.95)
scalar pval=chi2tail(9,LM)
disp "The White test stratistics:	" LM
disp "The 5% critical value:" crit
disp "P-value: " pval
estimates table white1 white2, b t se p 

* And using IM white test with interactions
reg lnexij lngdpi lngdpj lndisij
 estat imtest, white 
 
*(xi) a. Conducting regression with HC/white robust 
reg lnexij lngdpi lngdpj lndisij, vce(robust)
estimates store ols_hc 

* b. weighted least squares - where the variance of the error term varies with logged distance between countries 
gen weight2=1/lndisij^.5
reg lnexij lngdpi lngdpj lndisij [w=weight2]
estimates store wls11

* c. weighted least squares - feasible approach
gen lehat2=ln(ehat2)
reg lehat2 lngdpi lngdpj lndisij
predict lehat4_hat 
gen ehat4_hat=exp(lehat4_hat)
gen weight3=1/(ehat4_hat^.5)
reg lnexij lngdpi lngdpj lndisij [w=weight3]
estimates store wls2 
estimates table ols ols_hc wls11 wls2, b t se p  

* (xii) Applying clustur for exporting and importing countries 

* a. clustered by ReporterISO3
reg lnexij lngdpi lngdpj lndisij, vce(cluster ReporterISO3)
* or 
reg lnexij lngdpi lngdpj lndisij,cluster (ReporterISO3)
estimates store ols_cluster1
* b. Clustered by PartnerISO3
reg lnexij lngdpi lngdpj lndisij, vce(cluster PartnerISO3)
*or 
reg lnexij lngdpi lngdpj lndisij, cluster (PartnerISO3)
estimates store ols_cluster2
estimates table ols ols_hc wls11 wls2 ols_cluster1 ols_cluster2, b t p se

* Test in (xiii) (xiv) (xv) for each regression results 
** For OLS regression
* Testing beta 1 and 2 are above unity 
reg lnexij lngdpi lngdpj lndisij
test _b[lngdpi]=1
local sign_lngdpi = sign(_b[lngdpi]) 
display "Ho: coef > 1  p-value = " ttail(r(df_r),`sign_lngdpi'*sqrt(r(F)))
test _b[lngdpj]=1
local sign_lngdpj = sign(_b[lngdpj]) 
display "Ho: coef > 1  p-value = " ttail(r(df_r),`sign_lngdpj'*sqrt(r(F)))
* Testing Beta3 is less than -1 
reg lnexij lngdpi lngdpj lndisij
test _b[lndisij]=-1
local sign_lndisij = sign(_b[lndisij]) 
display "Ho: coef < -1  p-value = " ttail(r(df_r),`sign_lndisij'*sqrt(r(F)))
*Testing hypothesis for b1=b2=1
reg lnexij lngdpi lngdpj lndisij
test (_b[lngdpi]=_b[lngdpj]=1)
local sign_lngdp = sign(_b[lngdpi]-_b[lngdpj])
display "H_0: lngdp coef > 0 p-value = " normal(`sign_lngdp'*sqrt(r(F)))
** Testing hypotheis for b1>b2 
reg lnexij lngdpi lngdpj lndisij
test lngdpi-lngdpj = 0
local sign_lngdp = sign(_b[lngdpi]-_b[lngdpj])
display "H_0: lngdpi coef > lngdpj coef p-value = " normal(`sign_lngdp'*sqrt(r(F)))

* For OlS Hc white 
* Testing beta 1 and 2 are above unity 
reg lnexij lngdpi lngdpj lndisij, vce(robust)
test _b[lngdpi]=1
local sign_lngdpi = sign(_b[lngdpi]) 
display "Ho: coef > 1  p-value = " ttail(r(df_r),`sign_lngdpi'*sqrt(r(F)))
test _b[lngdpj]=1
local sign_lngdpj = sign(_b[lngdpj]) 
display "Ho: coef > 1  p-value = " ttail(r(df_r),`sign_lngdpj'*sqrt(r(F)))
* Testing Beta3 is less than -1 
reg lnexij lngdpi lngdpj lndisij, vce(robust)
test _b[lndisij]=-1
local sign_lndisij = sign(_b[lndisij]) 
display "Ho: coef < -1  p-value = " ttail(r(df_r),`sign_lndisij'*sqrt(r(F)))
*Testing hypothesis for b1=b2=1
reg lnexij lngdpi lngdpj lndisij, vce(robust)
test (_b[lngdpi]=_b[lngdpj]=1)
local sign_lngdp = sign(_b[lngdpi]-_b[lngdpj])
display "H_0: lngdp coef > 0 p-value = " normal(`sign_lngdp'*sqrt(r(F)))
* Testing hypotheis for b1>b2 
reg lnexij lngdpi lngdpj lndisij, vce(robust)
test lngdpi-lngdpj = 0
local sign_lngdp = sign(_b[lngdpi]-_b[lngdpj])
display "H_0: lngdpi coef > lngdpj coef p-value = " normal(`sign_lngdp'*sqrt(r(F)))

* Weighted least squares - variance of the error term depends on logged distance between countries
* Testing beta 1 and 2 are above unity 
reg lnexij lngdpi lngdpj lndisij [w=weight2]
test _b[lngdpi]=1
local sign_lngdpi = sign(_b[lngdpi]) 
display "Ho: coef > 1  p-value = " ttail(r(df_r),`sign_lngdpi'*sqrt(r(F)))
test _b[lngdpj]=1
local sign_lngdpj = sign(_b[lngdpj]) 
display "Ho: coef > 1  p-value = " ttail(r(df_r),`sign_lngdpj'*sqrt(r(F)))
* Testing Beta3 is less than -1 
reg lnexij lngdpi lngdpj lndisij [w=weight2]
test _b[lndisij]=-1
local sign_lndisij = sign(_b[lndisij]) 
display "Ho: coef < -1  p-value = " ttail(r(df_r),`sign_lndisij'*sqrt(r(F)))
*Testing hypothesis for b1=b2=1
reg lnexij lngdpi lngdpj lndisij [w=weight2]
test (_b[lngdpi]=_b[lngdpj]=1)
local sign_lngdp = sign(_b[lngdpi]-_b[lngdpj])
display "H_0: lngdp coef > 0 p-value = " normal(`sign_lngdp'*sqrt(r(F)))
** Testing hypotheis for b1>b2 
reg lnexij lngdpi lngdpj lndisij [w=weight3] 
test lngdpi-lngdpj = 0
local sign_lngdp = sign(_b[lngdpi]-_b[lngdpj])
display "H_0: lngdpi coef > lngdpj coef p-value = " normal(`sign_lngdp'*sqrt(r(F)))

** Testing for Weighted least squares - feasible approach 
* Testing beta 1 and 2 are above unity 
reg lnexij lngdpi lngdpj lndisij [w=weight3]
test _b[lngdpi]=1
local sign_lngdpi = sign(_b[lngdpi]) 
display "Ho: coef > 1  p-value = " ttail(r(df_r),`sign_lngdpi'*sqrt(r(F)))
test _b[lngdpj]=1
local sign_lngdpj = sign(_b[lngdpj]) 
display "Ho: coef > 1  p-value = " ttail(r(df_r),`sign_lngdpj'*sqrt(r(F)))
* Testing Beta3 is less than -1 
reg lnexij lngdpi lngdpj lndisij [w=weight3]
test _b[lndisij]=-1
local sign_lndisij = sign(_b[lndisij]) 
display "Ho: coef < -1  p-value = " ttail(r(df_r),`sign_lndisij'*sqrt(r(F)))
*Testing hypothesis for b1=b2=1
reg lnexij lngdpi lngdpj lndisij [w=weight3]
test (_b[lngdpi]=_b[lngdpj]=1)
local sign_lngdp = sign(_b[lngdpi]-_b[lngdpj])
display "H_0: lngdp coef > 0 p-value = " normal(`sign_lngdp'*sqrt(r(F)))
** Testing hypotheis for b1>b2 
reg lnexij lngdpi lngdpj lndisij [w=weight2] 
test lngdpi-lngdpj = 0
local sign_lngdp = sign(_b[lngdpi]-_b[lngdpj])
display "H_0: lngdpi coef > lngdpj coef p-value = " normal(`sign_lngdp'*sqrt(r(F)))

* For cluster regression: ReporterISO3
* Testing beta 1 and 2 are above unity 
reg lnexij lngdpi lngdpj lndisij, vce (cluster ReporterISO3) 
test _b[lngdpi]=1
local sign_lngdpi = sign(_b[lngdpi]) 
display "Ho: coef > 1  p-value = " ttail(r(df_r),`sign_lngdpi'*sqrt(r(F)))
test _b[lngdpj]=1
local sign_lngdpj = sign(_b[lngdpj]) 
display "Ho: coef > 1  p-value = " ttail(r(df_r),`sign_lngdpj'*sqrt(r(F)))
* Testing Beta3 is less than -1 
reg lnexij lngdpi lngdpj lndisij , vce (cluster ReporterISO3) 
test _b[lndisij]=-1
local sign_lndisij = sign(_b[lndisij]) 
display "Ho: coef < -1  p-value = " ttail(r(df_r),`sign_lndisij'*sqrt(r(F)))
* Testing hypothesis for b1=b2=1
reg lnexij lngdpi lngdpj lndisij , vce (cluster ReporterISO3) 
test (_b[lngdpi]=_b[lngdpj]=1)
local sign_lngdp = sign(_b[lngdpi]-_b[lngdpj])
display "H_0: lngdp coef > 0 p-value = " normal(`sign_lngdp'*sqrt(r(F)))
* Testing hypotheis for b1>b2 
reg lnexij lngdpi lngdpj lndisij , vce (cluster ReporterISO3) 
test lngdpi-lngdpj = 0
local sign_lngdp = sign(_b[lngdpi]-_b[lngdpj])
display "H_0: lngdpi coef > lngdpj coef p-value = " normal(`sign_lngdp'*sqrt(r(F)))

** For PartnerISO3
* Testing if b1 and b1 are above unity 

reg lnexij lngdpi lngdpj lndisij , vce (cluster PartnerISO3) 
test _b[lngdpi]=1
local sign_lngdpi = sign(_b[lngdpi]) 
display "Ho: coef > 1  p-value = " ttail(r(df_r),`sign_lngdpi'*sqrt(r(F)))
test _b[lngdpj]=1
local sign_lngdpj = sign(_b[lngdpj]) 
display "Ho: coef > 1  p-value = " ttail(r(df_r),`sign_lngdpj'*sqrt(r(F)))
* Testing Beta3 is less than -1 
reg lnexij lngdpi lngdpj lndisij , vce (cluster PartnerISO3) 
test _b[lndisij]=-1
local sign_lndisij = sign(_b[lndisij]) 
display "Ho: coef < -1  p-value = " ttail(r(df_r),`sign_lndisij'*sqrt(r(F)))
*Testing hypothesis for b1=b2=1
reg lnexij lngdpi lngdpj lndisij , vce (cluster PartnerISO3) 
test (_b[lngdpi]=_b[lngdpj]=1)
local sign_lngdp = sign(_b[lngdpi]-_b[lngdpj])
display "H_0: lngdp coef > 0 p-value = " normal(`sign_lngdp'*sqrt(r(F)))
* Testing hypotheis for b1>b2 
reg lnexij lngdpi lngdpj lndisij , vce (cluster PartnerISO3) 
test lngdpi-lngdpj = 0
local sign_lngdp = sign(_b[lngdpi]-_b[lngdpj])
display "H_0: lngdpi coef > lngdpj coef p-value = " normal(`sign_lngdp'*sqrt(r(F)))



*============================================================*
*** Excercsise 2: Economic developemnt and institution 
clear all
use"http://web.sgh.waw.pl/~jmuck/AAE/Datasets/AcemogluEtAl2001.dta"
*(ii) OLS regression 
reg logpgp95 avexpr
* IM Heteroskedasticty
reg logpgp95 avexpr
 estat imtest, white 
 * Using robust regresion since there is heteroskdastcity 
 reg logpgp95 avexpr, vce(robust)
estimates store ols_1
predict ehat, res
* Critical value 
disp invt(109, 1-0.05/2)

*(v) Testing relevance of European settlers mortality rate as an instrumental varaible 
reg avexpr logem4, vce(robust)
test (logem4=0)
ivregress 2sls logpgp95 (avexpr=logem4)
estat first

*(vi) Hausman test to test endogenity 
ivregress 2sls logpgp95 (avexpr=logem4)
estimates store iv1
estat endogenous 

*(viii) IV regression  with both instrument variables 

*First stage 
reg avexpr logem4
predict ave_hat1
* and 
reg avexpr euro1900
predict ave_hat2
* Second stage 
reg logpgp95 ave_hat1 ave_hat2

*or with built-in command IVregress 
ivregress 2sls logpgp95 (avexpr=logem4 euro1900)
estimates store iv2
estimates table ols_1 iv1 iv2 ,b t se p 
* Critical value 
disp invt(109, 1-0.05/2)
** Testing relevance and endogenity(repeating v and vi)
ivregress 2sls logpgp95 (avexpr=logem4 euro1900), vce(robust)
estat endogenous 
estat first

*(ix) Overidetification test
estat overid

*(xii) Extending the baseline regression 
reg logpgp95 avexpr lat_abst
estimates store ols_2
* IM Heteroskedasticty
reg logpgp95 avexpr lat_abst
 estat imtest, white 
 * Using robust regresion 
reg logpgp95 avexpr lat_abs, vce(robust)

*(xiv) IV regression with pervous instrumental variables
ivregress 2sls logpgp95 lat_abst (avexpr=logem4 euro1900), vce(robust)
estimates store iv3
estimates table ols_1 ols_2 iv1 iv2 iv3, b t se p
* Hausman test and overidentification test 
ivregress 2sls logpgp95 lat_abst (avexpr=logem4 euro1900),vce(robust)
estat endogenous 
estat first
estat overid

*(v) Actualize database with new recent data
* merging the data set 
merge m:m shortnam using "C:\Users\fasik\OneDrive\Documents\QEM Second Semester\Econometrics\NNDATA 2019 IV and GDP.dta"
*generating log GDP per capita 
gen lngdp2019= log(gdp2019)

* critical value 
disp invt(215, 1-0.05/2)

**** Findings

** Using available proxy varibles within intial data set 
* Using democarcy as proxy of instution :  this proxy is not endogenous varible 
reg lngdp2019 democ1
reg lngdp2019 lat_abst democ1
ivregress 2sls lngdp2019 ( democ1 = logem4 euro1900 )
ivregress 2sls lngdp2019 lat_abst ( democ1 = logem4 euro1900 )
estat endogenous
estat first
estat overid
*Constraint on excutive: This proxy is not endogenous variable
reg lngdp2019 cons00a
reg lngdp2019 cons00a lat_abst
ivregress 2sls lngdp2019 lat_abst ( cons00a = logem4 euro1900 )
estat endogenous
estat first
estat overid

** Using Recent proxy with TWO IV from the 1900
reg lngdp2019 Transpa 
estimate store ols1
** extended Ols 
reg lngdp2019 transpa lat_abst
estimate store ols2
*IV regresion with one IV 
ivregress 2sls lngdp2019 ( Transpa = logem4 )
estimate store Iv3
estat endogenous
estat first
*IV regression with Two IV
ivregress 2sls lngdp2019 lat_abst ( transpa = logem4 euro1900)
estimate store IV4
estat endogenous
estat first
estat overid
** comparision 
estimates table ols_1 ols_2 ols1 ols2 iv1 iv2 iv3 Iv3 IV4, b t se p

** Used two new recent IV variables: 
* Merging two new data set from CIPA and Governance indicator 
clear all
use "C:\Users\fasik\OneDrive\Documents\QEM Second Semester\Econometrics\GDP2019 and IV proxy Econ.dta"
merge m:m shortnam using "C:\Users\fasik\OneDrive\Documents\QEM Second Semester\Econometrics\DATA 2019 IV and GDP.dta"
gen lngdp2019=log(gdp)
** OLS rgression 
reg lngdp2019 Transpa 
estimate store olsN
** extended Ols 
reg lngdp2019 Transpa lat_abst
estimate store olsN2
*IV regresion with one IV 
ivregress 2sls lngdp2019 ( Transpa = eq )
estimate store IvN1
estat endogenous
estat first
*IV regression with Two IV
ivregress 2sls lngdp2019 lat_abst ( Transpa = eq corr)
estimate store IVN2
estat endogenous
estat first
estat overid
estimates table olsN olsN2 IvN1 IVN2, b t se p 


