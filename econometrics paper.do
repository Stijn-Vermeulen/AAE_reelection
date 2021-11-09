clear all
set more off
capture log close
cd "C:\Users\Gebruiker\Documents\Stata\AAE\Final Paper"
log using "C:\Users\Gebruiker\Documents\Stata\AAE\Final Paper\Final paper Stijn Vermeulen.log", replace

use "C:\Users\Gebruiker\Documents\Stata\AAE\Final Paper\reelect_data.dta", replace
/************************************************/
/************DESCRIPTIVE STATISTICS**************/
/************************************************/
preserve

*Drop values for narrow sample*
**Drop missing values**
drop if reelect2	== .
drop if ddef1		== .
drop if ddef3_n		== .
drop if gdppc_gr2	== .

**Summarize the narrow sample
count if reelect2   != .
count if reelect2	!= . & nd	== 0
count if reelect2	== 1
count if reelect2	== 1 & nd 	== 0
count if reelect2	== 0
count if reelect2	== 0 & nd	== 0

count if reelect2	!= . & dev	== 1
count if reelect2	!= . & dev	== 1 & nd	== 0
count if reelect2	== 1 & dev 	== 1
count if reelect2	== 1 & dev 	== 1 & nd == 0
count if reelect2	== 0 & dev 	== 1
count if reelect2	== 0 & dev 	== 1 & nd == 0


count if reelect2 	!= . & dev	== 0
count if reelect2 	!= . & dev	== 0 & nd	== 0
count if reelect2	== 1 & dev 	== 0
count if reelect2	== 1 & dev 	== 0 & nd == 0
count if reelect2	== 0 & dev 	== 0
count if reelect2	== 0 & dev 	== 0 & nd == 0

restore

*Expanded Sample*
preserve
drop if reelect3	== .
drop if ddef1		== .
drop if ddef3_n		== .
drop if gdppc_gr3	== .

**Summarize the expanded sample
count if reelect3   != .
count if reelect3	!= . & nd	== 0
count if reelect3	== 1
count if reelect3	== 1 & nd 	== 0
count if reelect3	== 0
count if reelect3	== 0 & nd 	== 0

count if reelect3	!= . & dev	== 1
count if reelect3	!= . & dev	== 1 & nd == 0
count if reelect3	== 1 & dev 	== 1
count if reelect3	== 1 & dev 	== 1 & nd == 0
count if reelect3	== 0 & dev 	== 1
count if reelect3	== 0 & dev 	== 1 & nd == 0

count if reelect3 	!= . & dev	== 0
count if reelect3 	!= . & dev	== 0 & nd == 0
count if reelect3	== 1 & dev 	== 0
count if reelect3	== 1 & dev 	== 0 & nd == 0
count if reelect3	== 0 & dev 	== 0
count if reelect3	== 0 & dev 	== 0 & nd == 0

**Summarize regression variables

*BALCH_term
sum ddef3_n
sum ddef3_n if reelect3 == 1
sum ddef3_n if reelect3 == 0
sum ddef3_n if dev == 1
sum ddef3_n if dev == 1 & reelect3 == 1
sum ddef3_n if dev == 1 & reelect3 == 0
sum ddef3_n if dev == 0 
sum ddef3_n if dev == 0 & reelect3 == 1
sum ddef3_n if dev == 0 & reelect3 == 0

*BALCH_ey
sum ddef1
sum ddef1 if reelect3 == 1
sum ddef1 if reelect3 == 0
sum ddef1 if dev == 1
sum ddef1 if dev == 1 & reelect3 == 1
sum ddef1 if dev == 1 & reelect3 == 0
sum ddef1 if dev == 0 
sum ddef1 if dev == 0 & reelect3 == 1
sum ddef1 if dev == 0 & reelect3 == 0

*GDPPC_gr
sum gdppc_gr3
sum gdppc_gr3 if reelect3 == 1
sum gdppc_gr3 if reelect3 == 0
sum gdppc_gr3 if dev == 1
sum gdppc_gr3 if dev == 1 & reelect3 == 1
sum gdppc_gr3 if dev == 1 & reelect3 == 0
sum gdppc_gr3 if dev == 0 
sum gdppc_gr3 if dev == 0 & reelect3 == 1
sum gdppc_gr3 if dev == 0 & reelect3 == 0

*GDPPC_gr_ey
sum dgdppc
sum dgdppc if reelect3 == 1
sum dgdppc if reelect3 == 0
sum dgdppc if dev == 1
sum dgdppc if dev == 1 & reelect3 == 1
sum dgdppc if dev == 1 & reelect3 == 0
sum dgdppc if dev == 0 
sum dgdppc if dev == 0 & reelect3 == 1
sum dgdppc if dev == 0 & reelect3 == 0

*INFCH_ey
sum infch_ey
sum infch_ey if reelect3 == 1
sum infch_ey if reelect3 == 0
sum infch_ey if dev == 1
sum infch_ey if dev == 1 & reelect3 == 1
sum infch_ey if dev == 1 & reelect3 == 0
sum infch_ey if dev == 0 
sum infch_ey if dev == 0 & reelect3 == 1
sum infch_ey if dev == 0 & reelect3 == 0

*Average_INF
sum inf_gr3
sum inf_gr3 if reelect3 == 1
sum inf_gr3 if reelect3 == 0
sum inf_gr3 if dev == 1
sum inf_gr3 if dev == 1 & reelect3 == 1
sum inf_gr3 if dev == 1 & reelect3 == 0
sum inf_gr3 if dev == 0 
sum inf_gr3 if dev == 0 & reelect3 == 1
sum inf_gr3 if dev == 0 & reelect3 == 0

restore

/************************************************/
/*************MODEL ESTIMATION*******************/
/************************************************/
/*Exclude Italy out of the sample*/
preserve
drop if code==31

/*REGULAR PROBIT*/
/*Equation 1: Full narrow sample set*/
probit reelect2 ddef3_n ddef1 gdppc_gr2 i.dev i.nd i.maj
estimates store probit1

*Predictive margins
margins dev, at(ddef3_n = (-0.15(0.01)0.09)) saving(probP_narrow_ddef3_n, replace)
marginsplot, xlabel(-0.15(0.05)0.09) noci
graph save probP_narrow_ddef3_n.gph, replace
margins dev, at(ddef1 = (-0.10(0.01)0.05))  saving(probP_narrow_ddef1, replace)
marginsplot, xlabel(-0.10(0.05)0.05) noci
graph save probP_narrow_ddef1.gph, replace
margins dev, at(gdppc_gr2 = (-0.05(0.01)0.12)) saving(probP_narrow_gdppc_gr2, replace)
marginsplot, xlabel(-0.05(0.05)0.12) noci
graph save probP_narrow_gdppc_gr2.gph, replace
graph combine probP_narrow_ddef3_n.gph probP_narrow_ddef1.gph probP_narrow_gdppc_gr2.gph
graph save eq1_narrow_probit_predicted.gph, replace
*Marginal effects
margins dev, dydx(ddef3_n) at(ddef3_n = (-0.15(0.01)0.09)) saving(probM_narrow_ddef3_n, replace)
marginsplot, xlabel(-0.15(0.05)0.09) noci
graph save probM_narrow_ddef3_n.gph, replace
margins dev, dydx(ddef1) at(ddef1   = (-0.10(0.01)0.05)) saving(probM_narrow_ddef1, replace)
marginsplot, xlabel(-0.10(0.05)0.05) noci
graph save probM_narrow_ddef1.gph, replace
margins dev, dydx(gdppc_gr2) at(gdppc_gr2 = (-0.05(0.01)0.12)) saving(probM_narrow_gdppc_gr2, replace)
marginsplot, xlabel(-0.05(0.05)0.12) noci
graph save probM_narrow_gdppc_gr2.gph, replace
graph combine probM_narrow_ddef3_n.gph probM_narrow_ddef1.gph probM_narrow_gdppc_gr2.gph
graph save eq1_narrow_probit_marginal.gph, replace
*Average Marginal Effects
margins, dydx(*) post
estimates store prob_average_mf1
/*Equation 2: Only developed countries in narrow sample*/
probit reelect2 ddef3_n ddef1 gdppc_gr2 i.nd i.maj if dev == 1
estimates store probit2
margins, dydx(*) post
estimates store prob_average_mf2
/*Equation 3: Only less developed countries in narrow sample*/
probit reelect2 ddef3_n ddef1 gdppc_gr2 i.nd i.maj if dev == 0
estimates store probit3
margins, dydx(*) post
estimates store prob_average_mf3
/*Equation 4: Full expanded sample set*/
probit reelect3 ddef3_n ddef1 gdppc_gr3 i.dev i.nd i.maj
estimates store probit4
*Predictive margins
margins dev, at(ddef3_n = (-0.15(0.01)0.09)) saving(probP_expanded_ddef3_n, replace)
marginsplot, xlabel(-0.15(0.05)0.09) noci
graph save probP_expanded_ddef3_n.gph, replace
margins dev, at(ddef1 = (-0.10(0.01)0.05))  saving(probP_expanded_ddef1, replace)
marginsplot, xlabel(-0.10(0.05)0.05) noci
graph save probP_expanded_ddef1.gph, replace
margins dev, at(gdppc_gr3 = (-0.05(0.01)0.12)) saving(probP_expanded_gdppc_gr3, replace)
marginsplot, xlabel(-0.05(0.05)0.12) noci
graph save probP_expanded_gdppc_gr3.gph, replace
graph combine probP_expanded_ddef3_n.gph probP_expanded_ddef1.gph probP_expanded_gdppc_gr3.gph
graph save eq4_expanded_hprobit_predicted.gph, replace
*Marginal effects
margins dev, dydx(ddef3_n) at(ddef3_n = (-0.15(0.01)0.09)) saving(probM_expanded_ddef3_n, replace)
marginsplot, xlabel(-0.15(0.05)0.09) noci
graph save probM_expanded_ddef3_n.gph, replace
margins dev, dydx(ddef1) at(ddef1   = (-0.10(0.01)0.05)) saving(probM_expanded_ddef1, replace)
marginsplot, xlabel(-0.10(0.05)0.05) noci
graph save probM_expanded_ddef1.gph, replace
margins dev, dydx(gdppc_gr3) at(gdppc_gr3 = (-0.05(0.01)0.12)) saving(hprobM_expanded_gdppc_gr3, replace)
marginsplot, xlabel(-0.05(0.05)0.12) noci
graph save probM_expanded_gdppc_gr3.gph, replace
graph combine probM_expanded_ddef3_n.gph probM_expanded_ddef1.gph probM_expanded_gdppc_gr3.gph
graph save eq4_expanded_probit_marginal.gph, replace
*Average Marginal Effects
margins, dydx(*) post
estimates store prob_average_mf4
/*Equation 5: Only developed countries in expanded sample*/
probit reelect3 ddef3_n ddef1 gdppc_gr3 i.nd i.maj if dev == 1
estimates store probit5
margins, dydx(*) post
estimates store prob_average_mf5
/*Equation 6: Only less developed countries in expanded sample*/
probit reelect3 ddef3_n ddef1 gdppc_gr3 i.nd i.maj if dev == 0
estimates store probit6
margins, dydx(*) post
estimates store prob_average_mf6

/*Table A: original regression with probit*/
esttab probit*, pr2 aic bic scalar(chi2) star(* .10 ** .05 *** .01), using TableA.tex, replace

/*HETPROBIT*/

/*Equation 1: Full narrow sample set*/
hetprobit reelect2 ddef3_n ddef1 gdppc_gr2 i.dev i.nd i.maj, het(ddef1 gdppc_gr2)
estimates store hetprobit1
*Predictive margins
margins dev, at(ddef3_n = (-0.15(0.01)0.09)) saving(hetprobP_narrow_ddef3_n, replace)
marginsplot, xlabel(-0.15(0.05)0.09) noci
graph save hetprobP_narrow_ddef3_n.gph, replace
margins dev, at(ddef1 = (-0.10(0.01)0.05))  saving(hetprobP_narrow_ddef1, replace)
marginsplot, xlabel(-0.10(0.05)0.05) noci
graph save hetprobP_narrow_ddef1.gph, replace
margins dev, at(gdppc_gr2 = (-0.05(0.01)0.12)) saving(hetprobP_narrow_gdppc_gr2, replace)
marginsplot, xlabel(-0.05(0.05)0.12) noci
graph save hetprobP_narrow_gdppc_gr2.gph, replace
graph combine hetprobP_narrow_ddef3_n.gph hetprobP_narrow_ddef1.gph hetprobP_narrow_gdppc_gr2.gph
graph save eq1_narrow_hetprobit_predicted.gph, replace
*Marginal effects
margins dev, dydx(ddef3_n) at(ddef3_n = (-0.15(0.01)0.09)) saving(hetprobM_narrow_ddef3_n, replace)
marginsplot, xlabel(-0.15(0.05)0.09) noci
graph save hetprobM_narrow_ddef3_n.gph, replace
margins dev, dydx(ddef1) at(ddef1   = (-0.10(0.01)0.05)) saving(hetprobM_narrow_ddef1, replace)
marginsplot, xlabel(-0.10(0.05)0.05) noci
graph save hetprobM_narrow_ddef1.gph, replace
margins dev, dydx(gdppc_gr2) at(gdppc_gr2 = (-0.05(0.01)0.12)) nochainrule saving(hetprobM_narrow_gdppc_gr2, replace)
marginsplot, xlabel(-0.05(0.05)0.12) noci
graph save hetprobM_narrow_gdppc_gr2.gph, replace
graph combine hetprobM_narrow_ddef3_n.gph hetprobM_narrow_ddef1.gph hetprobM_narrow_gdppc_gr2.gph
graph save eq1_narrow_hetprobit_marginal.gph, replace
*Average marginal effects
margins , dydx(*) post
estimates store het_average_mf1
/*Equation 2: Only developed countries in narrow sample*/
hetprobit reelect2 ddef3_n ddef1 gdppc_gr2 i.nd i.maj if dev == 1, het(ddef3_n i.nd)
estimates store hetprobit2
margins , at(nd=1 nd=0) post
estimates store het_average_mf2_nd
hetprobit reelect2 ddef3_n ddef1 gdppc_gr2 i.nd i.maj if dev == 1, het(ddef3_n i.nd)
margins , dydx(ddef3_n ddef1 gdppc_gr2 maj) post
estimates store het_average_mf2		
/*Equation 3: Only less developed countries in narrow sample*/
hetprobit reelect2 ddef3_n ddef1 gdppc_gr2 i.nd i.maj if dev == 0, het(ddef1)
estimates store hetprobit3
margins , dydx(*) post 					
estimates store het_average_mf3
/*Equation 4: Full expanded sample set*/
hetprobit reelect3 ddef3_n ddef1 gdppc_gr3 i.dev i.nd i.maj, het(ddef1 gdppc_gr3)
estimates store hetprobit4
*Predictive margins
margins dev, at(ddef3_n = (-0.15(0.01)0.09)) saving(hetprobP_expanded_ddef3_n, replace)
marginsplot, xlabel(-0.15(0.05)0.09) noci
graph save hetprobP_expanded_ddef3_n.gph, replace
margins dev, at(ddef1 = (-0.10(0.01)0.05))  saving(hetprobP_expanded_ddef1, replace)
marginsplot, xlabel(-0.10(0.05)0.05) noci
graph save hetprobP_expanded_ddef1.gph, replace
margins dev, at(gdppc_gr3 = (-0.05(0.01)0.12)) saving(hetprobP_expanded_gdppc_gr3, replace)
marginsplot, xlabel(-0.05(0.05)0.12) noci
graph save hetprobP_expanded_gdppc_gr3.gph, replace
graph combine hetprobP_expanded_ddef3_n.gph hetprobP_expanded_ddef1.gph hetprobP_expanded_gdppc_gr3.gph
graph save eq4_expanded_hetprobit_predicted.gph, replace
*Marginal effects
margins dev, dydx(ddef3_n) at(ddef3_n = (-0.15(0.01)0.09)) saving(hetprobM_expanded_ddef3_n, replace)
marginsplot, xlabel(-0.15(0.05)0.09) noci
graph save hetprobM_expanded_ddef3_n.gph, replace
margins dev, dydx(ddef1) at(ddef1   = (-0.10(0.01)0.05)) saving(hetprobM_expanded_ddef1, replace)
marginsplot, xlabel(-0.10(0.05)0.05) noci
graph save hetprobM_expanded_ddef1.gph, replace
margins dev, dydx(gdppc_gr3) at(gdppc_gr3 = (-0.05(0.01)0.12)) nochainrule saving(hetprobM_expanded_gdppc_gr3, replace)
marginsplot, xlabel(-0.05(0.05)0.12) noci
graph save hetprobM_expanded_gdppc_gr3.gph, replace
graph combine hetprobM_expanded_ddef3_n.gph hetprobM_expanded_ddef1.gph hetprobM_expanded_gdppc_gr3.gph
graph save eq4_expanded_hetprobit_marginal.gph, replace
*Average Marginal Effects
margins , dydx(*) post					
estimates store het_average_mf4
/*Equation 5: Only developed countries in expanded sample*/
hetprobit reelect3 ddef3_n ddef1 gdppc_gr3 i.nd i.maj if dev == 1, het(i.nd)
estimates store hetprobit5
margins , dydx(ddef3_n ddef1 gdppc_gr3 maj) post				
estimates store het_average_mf5
hetprobit reelect3 ddef3_n ddef1 gdppc_gr3 i.nd i.maj if dev == 1, het(i.nd)
margins , at(nd=1 nd=0) post
estimates store het_average_mf5_nd
/*Equation 6: Only less developed countries in expanded sample*/
hetprobit reelect3 ddef3_n ddef1 gdppc_gr3 i.nd i.maj if dev == 0, het(ddef1 gdppc_gr3)
estimates store hetprobit6
margins , dydx(*) post					
estimates store het_average_mf6

/*Table F: Summary of hetprobit results*/
esttab hetprobit*, pr2 aic bic scalar(chi2 p_c) star(* .10 ** .05 *** .01), using TableF.tex, replace

/*Table G: Summary average marginal effect for hetprobit*/
esttab het_average_mf*, star(* .10 ** .05 *** .01), using TableG.tex, replace


/*Table I: Comparison probit-hetprobit in narrow sample*/
esttab probit1 hetprobit1 probit2 hetprobit2 probit3 hetprobit3, star(* .10 ** .05 *** .01), using TableI.tex, replace

/*Table J: Comparison average marginal effect probit-hetprobit in narrow sample*/
esttab prob_average_mf1 het_average_mf1 prob_average_mf2 het_average_mf2 prob_average_mf3 het_average_mf3, star(* .10 ** .05 *** .01), using TableJ.tex, replace

/*Table K: Comparison probit-hetprobit in expanded sample*/
esttab probit4 hetprobit4 probit5 hetprobit5 probit6 hetprobit6, star(* .10 ** .05 *** .01), using TableK.tex, replace

/*Table L: Comparison average marginal effect probit-hetprobit in expanded sample*/
esttab prob_average_mf4 het_average_mf4 prob_average_mf5 het_average_mf5 prob_average_mf6 het_average_mf6, star(* .10 ** .05 *** .01), using TableL.tex, replace

/*Figure 1*/
quietly{
combomarginsplot 	probP_narrow_ddef3_n 		hetprobP_narrow_ddef3_n		,xlabel(-0.15(0.05)0.09) noci saving(Predicted-Margins-Narrow-ddef3_n, replace)
combomarginsplot 	probP_narrow_ddef1 			hetprobP_narrow_ddef1  		,xlabel(-0.10(0.05)0.05) noci saving(Predicted-Margins-Narrow-ddef1, replace)
combomarginsplot 	probP_narrow_gdppc_gr2 		hetprobP_narrow_gdppc_gr2	,xlabel(-0.05(0.05)0.12) noci saving(Predicted-Margins-Narrow-gdppc_gr2, replace)
combomarginsplot 	probM_narrow_ddef3_n 		hetprobM_narrow_ddef3_n		,xlabel(-0.15(0.05)0.09) noci saving(MarginalEffect-Narrow-ddef3_n, replace)
combomarginsplot 	probM_narrow_ddef1 			hetprobM_narrow_ddef1		,xlabel(-0.10(0.05)0.05) noci saving(MarginalEffect-Narrow-ddef1, replace)
combomarginsplot	probM_narrow_gdppc_gr2		hetprobM_narrow_gdppc_gr2	,xlabel(-0.05(0.05)0.12) noci saving(MarginalEffect-Narrow-gdppc_gr2, replace)

combomarginsplot 	probP_expanded_ddef3_n 		hetprobP_expanded_ddef3_n	,xlabel(-0.15(0.05)0.09) noci saving(Predicted-Margins-Expanded-ddef3_n, replace)
combomarginsplot 	probP_expanded_ddef1 		hetprobP_expanded_ddef1		,xlabel(-0.10(0.05)0.05) noci saving(Predicted-Margins-Expanded-ddef1, replace)
combomarginsplot 	probP_expanded_gdppc_gr3 	hetprobP_expanded_gdppc_gr3	,xlabel(-0.05(0.05)0.12) noci saving(Predicted-Margins-Expanded-gdppc_gr3, replace)
combomarginsplot	probM_expanded_ddef3_n      hetprobM_expanded_ddef3_n	,xlabel(-0.15(0.05)0.09) noci saving(MarginalEffect-Expanded-ddef3_n, replace)
combomarginsplot 	probM_expanded_ddef1 		hetprobM_expanded_ddef1		,xlabel(-0.10(0.05)0.05) noci saving(MarginalEffect-Expanded-ddef1, replace)
combomarginsplot	probM_expanded_gdppc_gr3	hetprobM_expanded_gdppc_gr3 ,xlabel(-0.05(0.05)0.12) noci saving(MarginalEffect-Expanded-gdppc_gr3, replace)							
}
/*Panel a*/
graph combine Predicted-Margins-Narrow-ddef3_n.gph Predicted-Margins-Narrow-ddef1.gph Predicted-Margins-Narrow-gdppc_gr2.gph
graph save  "Predicted-Margins-Narrow.gph", replace 
/*Panel c*/
graph combine MarginalEffect-Narrow-ddef3_n.gph MarginalEffect-Narrow-ddef1.gph MarginalEffect-Narrow-gdppc_gr2.gph
graph save  "MarginalEffect-Narrow.gph", replace 
/*Panel b*/
graph combine Predicted-Margins-Expanded-ddef3_n.gph Predicted-Margins-Expanded-ddef1.gph Predicted-Margins-Expanded-gdppc_gr3.gph
graph save  "Predicted-Margins-Expanded.gph", replace
/*Panel d*/
graph combine MarginalEffect-Expanded-ddef3_n.gph MarginalEffect-Expanded-ddef1.gph MarginalEffect-Expanded-gdppc_gr3.gph
graph save  "MarginalEffect-Expanded.gph", replace

/*---------Robustness*----------*/

*Equation 1
hetprobit reelect2 c.ddef3_n#c.dev c.ddef3_n#c.undev c.ddef1#c.dev c.ddef1#c.undev c.gdppc_gr2#c.dev c.gdppc_gr2#c.undev i.dev i.nd i.maj ///
			if drhp1 !=., het(c.ddef3_n#c.dev c.ddef3_n#c.undev c.ddef1#c.undev)
estimates store robusthet1
*Equation 2
hetprobit reelect2 c.ddef3_n#c.dev c.ddef3_n#c.undev c.ddef1#c.dev c.ddef1#c.undev c.gdppc_gr2#c.dev c.gdppc_gr2#c.undev c.dgdppc#c.dev ///
			c.dgdppc#c.undev i.dev i.nd i.maj if drhp1 != ., het(c.ddef3_n#c.dev c.ddef1#c.undev i.nd)
estimates store robusthet2
*Equation 3
hetprobit reelect2 c.ddef3_n#c.dev c.ddef3_n#c.undev c.ddef1#c.dev c.ddef1#c.undev c.gdppc_gr2#c.dev c.gdppc_gr2#c.undev c.drhp1#c.dev ///
			c.drhp1#c.undev i.dev i.nd i.maj, het(c.ddef3_n#c.dev)
estimates store robusthet3
*Equation 4
hetprobit reelect2 c.ddef1#c.dev c.ddef1#c.undev c.gdppc_gr2#c.dev c.gdppc_gr2#c.undev c.ddef3#c.dev c.ddef3#c.undev i.dev i.nd i.maj ///
			if (ddef3_n !=. & drhp1 != .), het(c.ddef1#c.undev)
estimates store robusthet4
*Equation 5
hetprobit reelect2 c.ddef3_n#c.dev c.ddef3_n#c.undev c.ddef1#c.dev c.ddef1#c.undev c.gdppc_gr2#c.dev c.gdppc_gr2#c.undev c.ddef3_n#c.defbegin#c.dev ///
			c.ddef3_n#c.defbegin#c.undev c.ddef1#c.defbegin#c.dev c.ddef1#c.defbegin#c.undev i.dev i.nd i.maj if drhp1 !=. , het(c.ddef3_n#c.dev c.ddef1#c.undev c.ddef1#c.defbegin#c.undev)
estimates store robusthet5

*Equation 6
hetprobit reelect3 c.ddef3_n#c.dev c.ddef3_n#c.undev c.ddef1#c.dev c.ddef1#c.undev c.gdppc_gr3#c.dev c.gdppc_gr3#c.undev i.dev i.nd i.maj, ///
			het(c.ddef1#c.undev c.gdppc_gr3#c.undev)
estimates store robusthet6
*Equation 7
hetprobit reelect3 c.ddef1#c.dev c.ddef1#c.undev c.gdppc_gr3#c.dev c.gdppc_gr3#c.undev c.ddef3#c.dev c.ddef3#c.undev i.dev i.nd i.maj if ddef3_n !=., ///
			het(c.ddef1#c.undev c.gdppc_gr3#c.undev)
estimates store robusthet7
*Equation 8
hetprobit reelect3 c.ddef3_n#c.dev c.ddef3_n#c.undev c.ddef1#c.dev c.ddef1#c.undev c.gdppc_gr3#c.dev c.gdppc_gr3#c.undev  ///
			c.ddef3_n#c.defbegin#c.dev c.ddef3_n#c.defbegin#c.undev c.ddef1#c.defbegin#c.dev c.ddef1#c.defbegin#c.undev i.dev i.nd i.maj, ///
			het(c.ddef1#c.undev c.gdppc_gr3#c.undev c.ddef1#c.defbegin#c.undev)
estimates store robusthet8

/*Table M: Robustness check for hetprobit*/
esttab robusthet*, pr2 aic bic scalar(chi2) star(* .10 ** .05 *** .01), using TableM.tex, replace

restore

log close
