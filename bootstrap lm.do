cap log close
clear all
set more off
cd "C:\Users\Gebruiker\Documents\Stata\AAE\Final Paper"
log using "Bootstrap LM test Stijn Vermeulen.log", replace

/************************************************/
/***************NORMALITY TEST*******************/
/************************************************/
use "C:\Users\Gebruiker\Documents\Stata\AAE\Final Paper\reelect_data.dta", replace
/*Probit: equation 1*/
probit reelect2 ddef3_n ddef1 gdppc_gr2 i.dev i.nd i.maj
predict eq1, xb

* Construct the generalized residuals
gen gr1=(reelect2-normprob(eq1))* normalden(eq1) / (normprob(eq1)*(1-normprob(eq1)))

*Construct generalized residuals*regressors
gen const = 1
for var const ddef3_n ddef1 gdppc_gr2 dev nd maj: gen gr1_X = gr1 * X

*Construct squared and cubic terms of the prediction * generalized residuals
gen sq1 = gr1 * eq1^2
gen cu1 = gr1 * eq1^3

*Construct LM Statistic
reg const gr1_* sq1 cu1, nocons
gen LM1=e(N)*e(r2)
di in g "LM statistic:       "  LM1
di in g "p-value of LM Test: " chiprob(2,LM1)

*Bootstrap the critical values of the LM statistic
set seed 12345
tempfile bootstrap 
local j = 1
while `j' <= 999 {
	quietly{
	preserve
	bsample
	noisily di in yellow "." _continue
	probit reelect2 ddef3_n ddef1 gdppc_gr2 i.dev i.nd i.maj
	predict xbn, xb
	gen grn=(reelect2-normprob(xbn))* normalden(xbn) / (normprob(xbn)*(1-normprob(xbn)))
	for var const ddef3_n ddef1 gdppc_gr3 dev nd maj: gen grn_X = grn * X
	gen nsq = grn * xbn^2
	gen ncu = grn * xbn^3
	reg const grn_* nsq ncu, nocons
	gen LM = e(N)*e(r2)
	collapse (mean) LM
	if `j' > 1 {
	append using `bootstrap'
	}
	save `bootstrap', replace
	restore
	local j = `j' + 1
	}
	}


use `bootstrap', clear

sum LM

cap program drop sub
program define sub
	sum `1', d
	sort `1'
	di " "
	di in g "lower critical value: " in y `1'[25]
	di " "
	di in g "upper critical value: " in y `1'[975]
	di " "
	end

sub LM


/*Probit: equation 2*/
use "C:\Users\Gebruiker\Documents\Stata\AAE\Final Paper\reelect_data.dta", replace

probit reelect2 ddef3_n ddef1 gdppc_gr2 i.dev i.nd i.maj if dev==1
predict eq2, xb

* Construct the generalized residuals
gen gr2=(reelect2-normprob(eq2))* normalden(eq2) / (normprob(eq2)*(1-normprob(eq2)))

*Construct generalized residuals*regressors
gen const = 1
for var const ddef3_n ddef1 gdppc_gr2 dev nd maj: gen gr2_X = gr2 * X

*Construct squared and cubic terms of the prediction * generalized residuals
gen sq2 = gr2 * eq2^2
gen cu2 = gr2 * eq2^3

*Construct LM Statistic
reg const gr2_* sq2 cu2, nocons
gen LM2=e(N)*e(r2)
di in g "LM statistic:       "  LM2
di in g "p-value of LM Test: " chiprob(2,LM2)

*Bootstrap the critical values of the LM statistic /*GIVES NO OBSERVATIONS ERROR*/
/*set seed 12345
tempfile bootstrap 
local j = 1
while `j' <= 999 {
	quietly{
	preserve
	bsample
	noisily di in yellow "." _continue
	probit reelect2 ddef3_n ddef1 gdppc_gr2 i.dev i.nd i.maj if dev==1
	predict xbn, xb
	gen grn=(reelect2-normprob(xbn))* normalden(xbn) / (normprob(xbn)*(1-normprob(xbn)))
	for var const ddef3_n ddef1 gdppc_gr3 dev nd maj: gen grn_X = grn * X
	gen nsq = grn * xbn^2
	gen ncu = grn * xbn^3
	reg const grn_* nsq ncu, nocons
	gen LM = e(N)*e(r2)
	collapse (mean) LM
	if `j' > 1 {
	append using `bootstrap'
	}
	save `bootstrap', replace
	restore
	local j = `j' + 1
	}
	}


use `bootstrap', clear

sum LM

cap program drop sub
program define sub
	sum `1', d
	sort `1'
	di " "
	di in g "lower critical value: " in y `1'[25]
	di " "
	di in g "upper critical value: " in y `1'[975]
	di " "
	end

sub LM*/


/*Probit: equation 3*/
use "C:\Users\Gebruiker\Documents\Stata\AAE\Final Paper\reelect_data.dta", replace

probit reelect2 ddef3_n ddef1 gdppc_gr2 i.dev i.nd i.maj if dev==0
predict eq3, xb

* Construct the generalized residuals
gen gr3=(reelect2-normprob(eq3))* normalden(eq3) / (normprob(eq3)*(1-normprob(eq3)))

*Construct generalized residuals*regressors
gen const = 1
for var const ddef3_n ddef1 gdppc_gr2 dev nd maj: gen gr3_X = gr3 * X

*Construct squared and cubic terms of the prediction * generalized residuals
gen sq3 = gr3 * eq3^2
gen cu3 = gr3 * eq3^3

*Construct LM Statistic
reg const gr3_* sq3 cu3, nocons
gen LM3=e(N)*e(r2)
di in g "LM statistic:       "  LM3
di in g "p-value of LM Test: " chiprob(2,LM3)

*Bootstrap the critical values of the LM statistic /
set seed 12345
tempfile bootstrap 
local j = 1
while `j' <= 999 {
	quietly{
	preserve
	bsample
	noisily di in yellow "." _continue
	probit reelect2 ddef3_n ddef1 gdppc_gr2 i.dev i.nd i.maj if dev==0
	predict xbn, xb
	gen grn=(reelect2-normprob(xbn))* normalden(xbn) / (normprob(xbn)*(1-normprob(xbn)))
	for var const ddef3_n ddef1 gdppc_gr3 dev nd maj: gen grn_X = grn * X
	gen nsq = grn * xbn^2
	gen ncu = grn * xbn^3
	reg const grn_* nsq ncu, nocons
	gen LM = e(N)*e(r2)
	collapse (mean) LM
	if `j' > 1 {
	append using `bootstrap'
	}
	save `bootstrap', replace
	restore
	local j = `j' + 1
	}
	}


use `bootstrap', clear

sum LM

cap program drop sub
program define sub
	sum `1', d
	sort `1'
	di " "
	di in g "lower critical value: " in y `1'[25]
	di " "
	di in g "upper critical value: " in y `1'[975]
	di " "
	end

sub LM

/*Probit: equation 4*/
use "C:\Users\Gebruiker\Documents\Stata\AAE\Final Paper\reelect_data.dta", replace

probit reelect3 ddef3_n ddef1 gdppc_gr3 i.dev i.nd i.maj
predict eq4, xb

* Construct the generalized residuals
gen gr4=(reelect3-normprob(eq4))* normalden(eq4) / (normprob(eq4)*(1-normprob(eq4)))

*Construct generalized residuals*regressors
gen const = 1
for var const ddef3_n ddef1 gdppc_gr3 dev nd maj: gen gr4_X = gr4 * X

*Construct squared and cubic terms of the prediction * generalized residuals
gen sq4 = gr4 * eq4^2
gen cu4 = gr4 * eq4^3

*Construct LM Statistic
reg const gr4_* sq4 cu4, nocons
gen LM4=e(N)*e(r2)
di in g "LM statistic:       "  LM
di in g "p-value of LM Test: " chiprob(2,LM4)

*Bootstrap the critical values of the LM statistic /
set seed 12345
tempfile bootstrap 
local j = 1
while `j' <= 999 {
	quietly{
	preserve
	bsample
	noisily di in yellow "." _continue
	probit reelect3 ddef3_n ddef1 gdppc_gr2 i.dev i.nd i.maj
	predict xbn, xb
	gen grn=(reelect2-normprob(xbn))* normalden(xbn) / (normprob(xbn)*(1-normprob(xbn)))
	for var const ddef3_n ddef1 gdppc_gr3 dev nd maj: gen grn_X = grn * X
	gen nsq = grn * xbn^2
	gen ncu = grn * xbn^3
	reg const grn_* nsq ncu, nocons
	gen LM = e(N)*e(r2)
	collapse (mean) LM
	if `j' > 1 {
	append using `bootstrap'
	}
	save `bootstrap', replace
	restore
	local j = `j' + 1
	}
	}


use `bootstrap', clear

sum LM

cap program drop sub
program define sub
	sum `1', d
	sort `1'
	di " "
	di in g "lower critical value: " in y `1'[25]
	di " "
	di in g "upper critical value: " in y `1'[975]
	di " "
	end

sub LM

/*Probit: equation 5*/
use "C:\Users\Gebruiker\Documents\Stata\AAE\Final Paper\reelect_data.dta", replace

probit reelect3 ddef3_n ddef1 gdppc_gr3 i.dev i.nd i.maj if dev==1
predict eq5, xb

* Construct the generalized residuals
gen gr5=(reelect3-normprob(eq5))* normalden(eq5) / (normprob(eq5)*(1-normprob(eq5)))

*Construct generalized residuals*regressors
gen const = 1
for var const ddef3_n ddef1 gdppc_gr3 dev nd maj: gen gr5_X = gr5 * X

*Construct squared and cubic terms of the prediction * generalized residuals
gen sq5 = gr5 * eq5^2
gen cu5 = gr5 * eq5^3

*Construct LM Statistic
reg const gr5_* sq5 cu5, nocons
gen LM5=e(N)*e(r2)
di in g "LM statistic:       "  LM
di in g "p-value of LM Test: " chiprob(2,LM5)

*Bootstrap the critical values of the LM statistic /*GIVES NO OBSERVATIONS ERROR*/
/*set seed 12345
tempfile bootstrap 
local j = 1
while `j' <= 999 {
	quietly{
	preserve
	bsample
	noisily di in yellow "." _continue
	probit reelect2 ddef3_n ddef1 gdppc_gr2 i.dev i.nd i.maj if dev==1
	predict xbn, xb
	gen grn=(reelect2-normprob(xbn))* normalden(xbn) / (normprob(xbn)*(1-normprob(xbn)))
	for var const ddef3_n ddef1 gdppc_gr3 dev nd maj: gen grn_X = grn * X
	gen nsq = grn * xbn^2
	gen ncu = grn * xbn^3
	reg const grn_* nsq ncu, nocons
	gen LM = e(N)*e(r2)
	collapse (mean) LM
	if `j' > 1 {
	append using `bootstrap'
	}
	save `bootstrap', replace
	restore
	local j = `j' + 1
	}
	}


use `bootstrap', clear

sum LM

cap program drop sub
program define sub
	sum `1', d
	sort `1'
	di " "
	di in g "lower critical value: " in y `1'[25]
	di " "
	di in g "upper critical value: " in y `1'[975]
	di " "
	end

sub LM*/

/*Probit: equation 6*/
use "C:\Users\Gebruiker\Documents\Stata\AAE\Final Paper\reelect_data.dta", replace

probit reelect3 ddef3_n ddef1 gdppc_gr3 i.dev i.nd i.maj if dev==0
predict eq6, xb

* Construct the generalized residuals
gen gr6=(reelect3-normprob(eq6))* normalden(eq6) / (normprob(eq6)*(1-normprob(eq6)))

*Construct generalized residuals*regressors
gen const = 1
for var const ddef3_n ddef1 gdppc_gr3 dev nd maj: gen gr6_X = gr6 * X

*Construct squared and cubic terms of the prediction * generalized residuals
gen sq6 = gr6 * eq6^2
gen cu6 = gr6 * eq6^3

*Construct LM Statistic
reg const gr6_* sq6 cu6, nocons
gen LM6=e(N)*e(r2)
di in g "LM statistic:       "  LM
di in g "p-value of LM Test: " chiprob(2,LM6)

*Bootstrap the critical values of the LM statistic /
set seed 12345
tempfile bootstrap 
local j = 1
while `j' <= 999 {
	quietly{
	preserve
	bsample
	noisily di in yellow "." _continue
	probit reelect2 ddef3_n ddef1 gdppc_gr2 i.dev i.nd i.maj if dev==0
	predict xbn, xb
	gen grn=(reelect2-normprob(xbn))* normalden(xbn) / (normprob(xbn)*(1-normprob(xbn)))
	for var const ddef3_n ddef1 gdppc_gr3 dev nd maj: gen grn_X = grn * X
	gen nsq = grn * xbn^2
	gen ncu = grn * xbn^3
	reg const grn_* nsq ncu, nocons
	gen LM = e(N)*e(r2)
	collapse (mean) LM
	if `j' > 1 {
	append using `bootstrap'
	}
	save `bootstrap', replace
	restore
	local j = `j' + 1
	}
	}


use `bootstrap', clear

sum LM

cap program drop sub
program define sub
	sum `1', d
	sort `1'
	di " "
	di in g "lower critical value: " in y `1'[25]
	di " "
	di in g "upper critical value: " in y `1'[975]
	di " "
	end

sub LM

/********Hetprobit*******/

/*Hetprobit: equation 1*/
use "C:\Users\Gebruiker\Documents\Stata\AAE\Final Paper\reelect_data.dta", replace

hetprobit reelect2 ddef3_n ddef1 gdppc_gr2 i.dev i.nd i.maj, het(ddef1 gdppc_gr2)
predict heq1, xb

* Construct the generalized residuals
gen hgr1=(reelect2-normprob(heq1))* normalden(heq1) / (normprob(heq1)*(1-normprob(heq1)))

*Construct generalized residuals*regressors
gen const = 1
for var const ddef3_n ddef1 gdppc_gr2 dev nd maj: gen hgr1_X = hgr1 * X

*Construct squared and cubic terms of the prediction * generalized residuals
gen hsq1 = hgr1 * heq1^2
gen hcu1 = hgr1 * heq1^3

*Construct LM Statistic
reg const hgr1_* hsq1 hcu1, nocons
gen hLM1=e(N)*e(r2)
di in g "LM statistic:       "  hLM1
di in g "p-value of LM Test: " chiprob(2,hLM1)

*Bootstrap the critical values of the LM statistic 
/*NON-CONVERGENCE OF LIKELIHOOD FUNCTION*/
/*set seed 12345
tempfile bootstrap 
local j = 1
while `j' <= 999 {
	quietly{
	preserve
	bsample
	noisily di in yellow "." _continue
	hetprobit reelect2 ddef3_n ddef1 gdppc_gr2 i.dev i.nd i.maj, het(ddef1 gdppc_gr2)
	predict xbn, xb
	gen grn=(reelect2-normprob(xbn))* normalden(xbn) / (normprob(xbn)*(1-normprob(xbn)))
	for var const ddef3_n ddef1 gdppc_gr3 dev nd maj: gen grn_X = grn * X
	gen nsq = grn * xbn^2
	gen ncu = grn * xbn^3
	reg const grn_* nsq ncu, nocons
	gen LM = e(N)*e(r2)
	collapse (mean) LM
	if `j' > 1 {
	append using `bootstrap'
	}
	save `bootstrap', replace
	restore
	local j = `j' + 1
	}
	}


use `bootstrap', clear

sum LM

cap program drop sub
program define sub
	sum `1', d
	sort `1'
	di " "
	di in g "lower critical value: " in y `1'[25]
	di " "
	di in g "upper critical value: " in y `1'[975]
	di " "
	end

sub LM*/


/*Hetprobit: equation 2*/
use "C:\Users\Gebruiker\Documents\Stata\AAE\Final Paper\reelect_data.dta", replace

hetprobit reelect2 ddef3_n ddef1 gdppc_gr2 i.dev i.nd i.maj if dev==1, het(ddef3_n i.nd)
predict heq2, xb

* Construct the generalized residuals
gen hgr2=(reelect2-normprob(heq2))* normalden(heq2) / (normprob(heq2)*(1-normprob(heq2)))

*Construct generalized residuals*regressors
gen const = 1
for var const ddef3_n ddef1 gdppc_gr2 dev nd maj: gen hgr2_X = hgr2 * X

*Construct squared and cubic terms of the prediction * generalized residuals
gen hsq2 = hgr2 * heq2^2
gen hcu2 = hgr2 * heq2^3

*Construct LM Statistic
reg const hgr2_* hsq2 hcu2, nocons
gen hLM2=e(N)*e(r2)
di in g "LM statistic:       "  hLM2
di in g "p-value of LM Test: " chiprob(2,hLM2)

*Bootstrap the critical values of the LM statistic 
/*NON-CONVERGENCE OF LIKELIHOOD FUNCTION*/
/*set seed 12345
tempfile bootstrap 
local j = 1
while `j' <= 999 {
	quietly{
	preserve
	bsample
	noisily di in yellow "." _continue
	hetprobit reelect2 ddef3_n ddef1 gdppc_gr2 i.dev i.nd i.maj if dev==1, het(ddef3_n i.nd)
	predict xbn, xb
	gen grn=(reelect2-normprob(xbn))* normalden(xbn) / (normprob(xbn)*(1-normprob(xbn)))
	for var const ddef3_n ddef1 gdppc_gr3 dev nd maj: gen grn_X = grn * X
	gen nsq = grn * xbn^2
	gen ncu = grn * xbn^3
	reg const grn_* nsq ncu, nocons
	gen LM = e(N)*e(r2)
	collapse (mean) LM
	if `j' > 1 {
	append using `bootstrap'
	}
	save `bootstrap', replace
	restore
	local j = `j' + 1
	}
	}


use `bootstrap', clear

sum LM

cap program drop sub
program define sub
	sum `1', d
	sort `1'
	di " "
	di in g "lower critical value: " in y `1'[25]
	di " "
	di in g "upper critical value: " in y `1'[975]
	di " "
	end

sub LM*/


/*Hetprobit: equation 3*/
use "C:\Users\Gebruiker\Documents\Stata\AAE\Final Paper\reelect_data.dta", replace

hetprobit reelect2 ddef3_n gdppc_gr2 i.dev i.nd i.maj if dev==0, het(ddef1)
predict heq3, xb

* Construct the generalized residuals
gen hgr3=(reelect2-normprob(heq3))* normalden(heq3) / (normprob(heq3)*(1-normprob(heq3)))

*Construct generalized residuals*regressors
gen const = 1
for var const ddef3_n ddef1 gdppc_gr2 dev nd maj: gen hgr3_X = hgr3 * X

*Construct squared and cubic terms of the prediction * generalized residuals
gen hsq3 = hgr3 * heq3^2
gen hcu3 = hgr3 * heq3^3

*Construct LM Statistic
reg const hgr3_* hsq3 hcu3, nocons
gen hLM3=e(N)*e(r2)
di in g "LM statistic:       "  hLM3
di in g "p-value of LM Test: " chiprob(2,hLM3)

*Bootstrap the critical values of the LM statistic 
/*NON-CONVERGENCE OF LIKELIHOOD FUNCTION*/
/*set seed 12345
tempfile bootstrap 
local j = 1
while `j' <= 999 {
	quietly{
	preserve
	bsample
	noisily di in yellow "." _continue
	hetprobit reelect2 ddef3_n gdppc_gr2 i.dev i.nd i.maj if dev==0, het(ddef1)
	predict xbn, xb
	gen grn=(reelect2-normprob(xbn))* normalden(xbn) / (normprob(xbn)*(1-normprob(xbn)))
	for var const ddef3_n ddef1 gdppc_gr3 dev nd maj: gen grn_X = grn * X
	gen nsq = grn * xbn^2
	gen ncu = grn * xbn^3
	reg const grn_* nsq ncu, nocons
	gen LM = e(N)*e(r2)
	collapse (mean) LM
	if `j' > 1 {
	append using `bootstrap'
	}
	save `bootstrap', replace
	restore
	local j = `j' + 1
	}
	}


use `bootstrap', clear

sum LM

cap program drop sub
program define sub
	sum `1', d
	sort `1'
	di " "
	di in g "lower critical value: " in y `1'[25]
	di " "
	di in g "upper critical value: " in y `1'[975]
	di " "
	end

sub LM*/


/*Hetprobit: equation 4*/
use "C:\Users\Gebruiker\Documents\Stata\AAE\Final Paper\reelect_data.dta", replace

hetprobit reelect3 ddef3_n ddef1 gdppc_gr3 i.dev i.nd i.maj, het(ddef1 gdppc_gr3)
predict heq4, xb

* Construct the generalized residuals
gen hgr4=(reelect3-normprob(heq4))* normalden(heq4) / (normprob(heq4)*(1-normprob(heq4)))

*Construct generalized residuals*regressors
gen const = 1
for var const ddef3_n ddef1 gdppc_gr3 dev nd maj: gen hgr4_X = hgr4 * X

*Construct squared and cubic terms of the prediction * generalized residuals
gen hsq4 = hgr4 * heq4^2
gen hcu4 = hgr4 * heq4^3

*Construct LM Statistic
reg const hgr4_* hsq4 hcu4, nocons
gen hLM4=e(N)*e(r2)
di in g "LM statistic:       "  hLM4
di in g "p-value of LM Test: " chiprob(2,hLM4)

*Bootstrap the critical values of the LM statistic 
/*NON-CONVERGENCE OF LIKELIHOOD FUNCTION*/
/*set seed 12345
tempfile bootstrap 
local j = 1
while `j' <= 999 {
	quietly{
	preserve
	bsample
	noisily di in yellow "." _continue
	hetprobit reelect3 ddef3_n ddef1 gdppc_gr3 i.dev i.nd i.maj, het(ddef3_n i.nd)
	predict xbn, xb
	gen grn=(reelect2-normprob(xbn))* normalden(xbn) / (normprob(xbn)*(1-normprob(xbn)))
	for var const ddef3_n ddef1 gdppc_gr3 dev nd maj: gen grn_X = grn * X
	gen nsq = grn * xbn^2
	gen ncu = grn * xbn^3
	reg const grn_* nsq ncu, nocons
	gen LM = e(N)*e(r2)
	collapse (mean) LM
	if `j' > 1 {
	append using `bootstrap'
	}
	save `bootstrap', replace
	restore
	local j = `j' + 1
	}
	}


use `bootstrap', clear

sum LM

cap program drop sub
program define sub
	sum `1', d
	sort `1'
	di " "
	di in g "lower critical value: " in y `1'[25]
	di " "
	di in g "upper critical value: " in y `1'[975]
	di " "
	end

sub LM*/

/*Hetprobit: equation 5*/
use "C:\Users\Gebruiker\Documents\Stata\AAE\Final Paper\reelect_data.dta", replace

hetprobit reelect3 ddef3_n ddef1 gdppc_gr3 i.dev i.nd i.maj if dev==1, het(i.nd)
predict heq5, xb

* Construct the generalized residuals
gen hgr5=(reelect3-normprob(heq5))* normalden(heq5) / (normprob(heq5)*(1-normprob(heq5)))

*Construct generalized residuals*regressors
gen const = 1
for var const ddef3_n ddef1 gdppc_gr3 dev nd maj: gen hgr5_X = hgr5 * X

*Construct squared and cubic terms of the prediction * generalized residuals
gen hsq5 = hgr5 * heq5^2
gen hcu5 = hgr5 * heq5^3

*Construct LM Statistic
reg const hgr5_* hsq5 hcu5, nocons
gen hLM5=e(N)*e(r2)
di in g "LM statistic:       "  hLM5
di in g "p-value of LM Test: " chiprob(2,hLM5)

*Bootstrap the critical values of the LM statistic 
/*NON-CONVERGENCE OF LIKELIHOOD FUNCTION*/
/*set seed 12345
tempfile bootstrap 
local j = 1
while `j' <= 999 {
	quietly{
	preserve
	bsample
	noisily di in yellow "." _continue
	hetprobit reelect3 ddef3_n ddef1 gdppc_gr3 i.dev i.nd i.maj if dev==1, het(i.nd)
	predict xbn, xb
	gen grn=(reelect3-normprob(xbn))* normalden(xbn) / (normprob(xbn)*(1-normprob(xbn)))
	for var const ddef3_n ddef1 gdppc_gr3 dev nd maj: gen grn_X = grn * X
	gen nsq = grn * xbn^2
	gen ncu = grn * xbn^3
	reg const grn_* nsq ncu, nocons
	gen LM = e(N)*e(r2)
	collapse (mean) LM
	if `j' > 1 {
	append using `bootstrap'
	}
	save `bootstrap', replace
	restore
	local j = `j' + 1
	}
	}


use `bootstrap', clear

sum LM

cap program drop sub
program define sub
	sum `1', d
	sort `1'
	di " "
	di in g "lower critical value: " in y `1'[25]
	di " "
	di in g "upper critical value: " in y `1'[975]
	di " "
	end

sub LM*/

/*Hetprobit: equation 6*/
use "C:\Users\Gebruiker\Documents\Stata\AAE\Final Paper\reelect_data.dta", replace

hetprobit reelect3 ddef3_n ddef1 gdppc_gr3 i.dev i.nd i.maj if dev==0, het(gdppc_gr3)
predict heq6, xb

* Construct the generalized residuals
gen hgr6=(reelect3-normprob(heq6))* normalden(heq6) / (normprob(heq6)*(1-normprob(heq6)))

*Construct generalized residuals*regressors
gen const = 1
for var const ddef3_n ddef1 gdppc_gr3 dev nd maj: gen hgr6_X = hgr6 * X

*Construct squared and cubic terms of the prediction * generalized residuals
gen hsq6 = hgr6 * heq6^2
gen hcu6 = hgr6 * heq6^3

*Construct LM Statistic
reg const hgr6_* hsq6 hcu6, nocons
gen hLM6=e(N)*e(r2)
di in g "LM statistic:       "  hLM6
di in g "p-value of LM Test: " chiprob(2,hLM6)

*Bootstrap the critical values of the LM statistic 
/*NON-CONVERGENCE OF LIKELIHOOD FUNCTION*/
/*set seed 12345
tempfile bootstrap 
local j = 1
while `j' <= 999 {
	quietly{
	preserve
	bsample
	noisily di in yellow "." _continue
	hetprobit reelect3 ddef3_n ddef1 gdppc_gr3 i.dev i.nd i.maj if dev==0, het(gdppc_gr3)
	predict xbn, xb
	gen grn=(reelect3-normprob(xbn))* normalden(xbn) / (normprob(xbn)*(1-normprob(xbn)))
	for var const ddef3_n ddef1 gdppc_gr3 dev nd maj: gen grn_X = grn * X
	gen nsq = grn * xbn^2
	gen ncu = grn * xbn^3
	reg const grn_* nsq ncu, nocons
	gen LM = e(N)*e(r2)
	collapse (mean) LM
	if `j' > 1 {
	append using `bootstrap'
	}
	save `bootstrap', replace
	restore
	local j = `j' + 1
	}
	}


use `bootstrap', clear

sum LM

cap program drop sub
program define sub
	sum `1', d
	sort `1'
	di " "
	di in g "lower critical value: " in y `1'[25]
	di " "
	di in g "upper critical value: " in y `1'[975]
	di " "
	end

sub LM*/
log close


