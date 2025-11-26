** Econometrics Homework 1
**** Name Fasika Mulu 
******Id  112715



*Excercse 1 do file House prices 

*uploading the data and regression 
use "C:\Users\fasik\Downloads\utown.dta"
gen lnprice=ln( price)
reg price sqft age utown pool fplace
estimates store m1
reg lnprice sqft age utown pool fplace
estimates store m2
gen lnsqft=ln( sqft)
reg lnprice lnsqft age utown pool fplace
estimates store m3
estimates table m*, b se p stats(r2)

** Marginal effect and elasticty of model 1
reg price sqft age utown pool fplace
gen mf_sqft1=_b[sqft]
dis mf_sqft1
gen el_sqft2=_b[sqft]*sqft/price
dis el_sqft2

**Marginal effect and elasticty of Model 2
reg lnprice sqft age utown pool fplace
gen mf_sqft3=_b[sqft]*lnprice
dis mf_sqft3
gen el_sqft4=_b[sqft]*sqft
dis el_sqft4

** Marginal effect and elasticty of Model 3
reg lnprice lnsqft age utown pool fplace
gen mf_sqftn=_b[lnsqft]*lnprice/lnsqft
dis mf_sqftn
gen el_sqftm=_b[lnsqft]
dis el_sqftm

* calcuating critical value for the above models for 95% degrees of freedom 
disp invt(994, 1-0.05/2)

*to compare the R2 between models 
estimates table m*, stats(r2)
*to show why we cant compare the r2 of the three models ?
reg price sqft age utown pool fplace
estimates store models1
reg lnprice sqft age utown pool fplace
estimates store models2
reg lnprice lnsqft age utown pool fplace
estimates store models3
reg lnprsqft lnsqft age utown pool fplace
estimates store models4
tabstat lnprice price, stat(var)

*testing colinearity using variance inflation factor 
reg price sqft age utown pool fplace
vif
reg lnprice sqft age utown pool fplace
vif
reg lnprice lnsqft age utown pool fplace
vif
reg lnprsqft lnsqft age utown pool fplace
vif
* pair wise correlation test 
corr(sqft age utown pool fplace)

*Normality test
*by plotting  
reg price sqft age utown pool fplace
predict residuals1, res
estimate store model1
histogram residuals1
reg lnprice sqft age utown pool fplace
predict residuals2, res
estimate store model2
histogram residuals2
reg lnprice lnsqft age utown pool fplace
predict residuals3, res
estimate store model3
histogram residuals3

* Jaque Berra test 
sktest residuals*, noadj

*model specification test Ramsey RESET test  
reg price sqft age utown pool fplace
estat ovtest
reg lnprice sqft age utown pool fplace
estat ovtest
reg lnprice lnsqft age utown pool fplace
estat ovtest

*Functional form testing, TWO way scatter plot 
*Model 1
reg price sqft age utown pool fplace
predict yhat1
twoway scatter price sqft||line yhat1 sqft 
twoway scatter price age||line yhat1 age
twoway scatter price utown||line yhat1 utown
twoway scatter price pool||line yhat1 pool
twoway scatter price fplace||line yhat1 fplace

*Model 2
reg lnprice sqft age utown pool fplace
predict yhat2 
twoway scatter lnprice sqft||line yhat2 sqft 
twoway scatter lnprice age||line yhat2 age
twoway scatter lnprice utown||line yhat2 utown
twoway scatter lnprice pool||line yhat2 pool
twoway scatter lnprice fplace||line yhat2 fplace

*Model 3
reg lnprice lnsqft age utown pool fplace
predict yhat3 
twoway scatter lnprice lnsqft||line yhat3 lnsqft 
twoway scatter lnprice age||line yhat3 age
twoway scatter lnprice utown||line yhat3 utown
twoway scatter lnprice pool||line yhat3 pool
twoway scatter lnprice fplace||line yhat3 fplace

*to compare Model 4 and comparison with model 3
** point estimates and r-2
reg lnprice lnsqft age utown pool fplace
estimates store m3
gen lnprsqft=ln( price/ sqft)
reg lnprsqft lnsqft age utown pool fplace
estimates store m4
estimates table m*, stats(r2)
tabstat lnprsqft lnprice, stat(var)
estimates table m*, b se p stats(r2)

*to comapre the normality of model 3 and 4
reg lnprice lnsqft age utown pool fplace
predict residuals3, res
estimate store model3
histogram residuals3
reg lnprsqft lnsqft age utown pool fplace
predict residuals4, res
estimate store model4
histogram residuals4
* Jaque Berra test 
sktest residuals*, noadj


* Excersie 2 do file COVID-19 cases 

* importing data 
clear all 
use "C:\Users\fasik\Downloads\COVID2020 (1).dta"
*regression of the different models 
reg total_cases_per_million stringency_index
estimat store m1
*critical value 
disp invt(170, 1-0.05/2)

gen lngdpcapita = ln( gdp_per_capita)
reg total_cases_per_million stringency_index hospital_beds_per_thousand days lngdpcapita
estimat store m2
* critical value 
disp invt(144, 1-0.05/2)

reg reproduction_rate stringency_index hospital_beds_per_thousand days lngdpcapita
estimat store m3
* critical value 
disp invt(139, 1-0.05/2)

reg total_deaths_per_million stringency_index hospital_beds_per_thousand days lngdpcapita
estimat store m4
* critical value 
disp invt(137, 1-0.05/2)

** all point estimates in one table with
estimates table m*, b se p stats(r2)

** correlation test 
* pairwise correlation
corr (stringency_index hospital_beds_per_thousand days lngdpcapita)

*VIF test 
reg total_cases_per_million stringency_index hospital_beds_per_thousand days lngdpcapita
vif
reg reproduction_rate stringency_index hospital_beds_per_thousand days lngdpcapita
vif 
reg total_deaths_per_million stringency_index hospital_beds_per_thousand days lngdpcapita
vif 

** Jarque Berra test to test for normality 
reg total_cases_per_million stringency_index
predict residuals1, res
estimate store model1
histogram residuals1
reg total_cases_per_million stringency_index hospital_beds_per_thousand days lngdpcapita
predict residuals2, res
estimate store model2
histogram residuals2
reg reproduction_rate stringency_index hospital_beds_per_thousand days lngdpcapita
predict residuals3, res
estimate store model3
histogram residuals3
reg total_deaths_per_million stringency_index hospital_beds_per_thousand days lngdpcapita
predict residuals4, res
estimate store model4
histogram residuals3
sktest residuals*, noadj

*** to test model specification 
reg total_cases_per_million stringency_index
estat ovtest 
reg total_cases_per_million stringency_index hospital_beds_per_thousand days lngdpcapita
estat ovtest 
reg reproduction_rate stringency_index hospital_beds_per_thousand days lngdpcapita
estat ovtest
reg total_deaths_per_million stringency_index hospital_beds_per_thousand days lngdpcapita
estat ovtest

*** testing for existece of hetroskedacticity using residual chart 
reg total_cases_per_million stringency_index hospital_beds_per_thousand days lngdpcapita
predict ehat 
gen ehat2=ehat^2
scatter ehat2 stringency_index
scatter ehat2 hospital_beds_per_thousand
Scatter ehat2 days 
scatter ehat2 lngdpcapita
reg reproduction_rate stringency_index hospital_beds_per_thousand days lngdpcapita
predict ehat3
gen ehat32=ehat^2
scatter ehat32 stringency_index
scatter ehat32 hospital_beds_per_thousand
Scatter ehat32 days 
scatter ehat32 lngdpcapita
reg total_deaths_per_million stringency_index hospital_beds_per_thousand days lngdpcapita
predict ehat4 
gen ehat42=ehat4^2
scatter ehat42 stringency_index
scatter ehat42 hospital_beds_per_thousand
Scatter ehat42 days 
scatter ehat42 lngdpcapita

