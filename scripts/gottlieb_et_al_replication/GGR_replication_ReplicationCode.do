* GENERAL INFO
	* Project: Replication of "Do Men and Women Have Different Policy Preferences in Africa? Determinants and Implications of Gender Gaps in Policy Prioritization"
	* Original authors: Amanda Robinson, Jessica Gottlieb, Guy Grossman
	* Date created: April 2019

* DO FILE INFO
	* This .do file conducts all analyses and produces figures and tables for the published article.


************************************
* Seemingly unrelated regression
* pooled with country fixed effects
************************************

* Import data
set more off
use "/Users/andrewmccormack/Desktop/africa_age_interviewer_effect/data_clean/gottlieb_clean/GGR_FinalData_agediff.dta", clear

* This makes it so that the variable is categorical
* Otherwise it will not work in a regression
encode coarsened_age, gen(coarsened_age_cat)
encode gender_dyad, gen(gender_dyad_cat)

* Define variables
global outcomes Economy Poverty Infrastructure Health Water Education Agriculture Violence Rights  Services  None
global main Economy Poverty  Health  Water
global min_controls count2
global full_controls age urban primary count2


**************************
* Without AGE DIFFERENCE *
**************************

* Using SV Y/SUEST
svyset URBRUR [pweight=Withinwt], strata(REGION) singleunit(certainty)
svydescribe

foreach y in $outcomes {
svy: reg `y' female $min_controls i.ccodecow i.ROUND, dof(5) 
est sto `y'
}

suest $outcomes, cformat(%9.3f)
test female

*create regression table
#delimit ;
esttab Economy Poverty Infrastructure Water Agriculture using "suest_main1.tex",
 star(* 0.10 ** 0.05 *** 0.01) se b(3) se(3) r2(2) label booktabs alignment(D{.}{.}{-1}) replace 
	/*keep($min_controls)*/ stats(N,layout("\multicolumn{1}{c}{@}") labels("Observations") f(0)) 
	title(Impact of Gender on the Likelihood of Policy Domain Prioritization\label{tab:suest1}) 
	nonotes ///
	addnote("Pooled seemingly unrelated regression analyses with country fixed effects." 
	"Standard errors are in parentheses. $^* p<0.10$, $^{**} p<0.05$, $^{***} p<0.01$"); 

	
esttab Violence Health Rights  Education Services using "Drafts/Tables/suest_main2.tex", 
star(* 0.10 ** 0.05 *** 0.01) se b(3) se(3) r2(2)   label booktabs alignment(D{.}{.}{-1}) replace 
	keep(female $min_controls) stats(N,layout("\multicolumn{1}{c}{@}") labels("Observations") f(0)) 
	title(Impact of Gender on the Likelihood of Policy Domain Prioritization\label{tab:suest2})
	nonotes ///
	sub("Social/Political Rights" "Rights") ///
	addnote("Pooled seemingly unrelated regression analyses with country fixed effects."
	"Standard errors are in parentheses. $^* p<0.10$, $^{**} p<0.05$, $^{***} p<0.01$");
	
#delimit cr

	

***********************
* With AGE DIFFERENCE *
***********************

set more off
* Using SVY/SUEST (with age difference and gender dyad variables)
svyset URBRUR [pweight=Withinwt], strata(REGION) singleunit(certainty)
svydescribe

set more off

foreach y in $outcomes {
svy: reg `y' $min_controls ib2.gender_dyad_cat##ib3.coarsened_age_cat age i.ccodecow i.ROUND, dof(5) 
est sto `y'
}

set matsize 800

set more off
suest $outcomes, cformat(%9.3f)

* Won't compile in LaTeX, must switch #s to /#s. 
* In any case, when the coefficients are labelled nicely, there will be no #s.
esttab Economy Poverty Infrastructure Water Agriculture  using "suest_main1_age_gender_interaction.tex", ///
    star(* 0.10 ** 0.05 *** 0.01) se b(3) se(3) r2(2) nobaselevels label booktabs replace ///
    drop(*.ccodecow *.ROUND) ///
	stats(N,layout("\multicolumn{1}{c}{@}") labels("Observations") f(0)) ///
	title(Impact of Gender on the Likelihood of Policy Domain Prioritization\label{tab:suest1})  ///
	nonotes ///
	addnote("Pooled seemingly unrelated regression analyses with country fixed effects." "Standard errors are in parentheses. $^* p<0.10$, $^{**} p<0.05$, $^{***} p<0.01$")
	
*graph coef plot
preserve
parmest, label format(estimate min95 max95 p %8.2f ) norestore 
keep if  parm=="female"
gen ids=_n

gen outcome =1 if eq=="Economy"
replace outcome =2 if eq=="Poverty"
replace outcome =3 if eq=="Infrastructure"
replace outcome =4 if eq=="Health"
replace outcome =5 if eq=="Water"
replace outcome =6 if eq=="Education"
replace outcome =7 if eq=="Agriculture"
replace outcome =8 if eq=="Violence"
replace outcome =9 if eq=="Rights"
replace outcome =10 if eq=="Services"

lab define outcomes 1 "Economy" 2 "Poverty" 3 "Infrastructure" 4 "Health" 5 "Water" 6 "Education" 7 "Agriculture" 8 "Violence" 9 "Rights" 10 "Services", modify
lab value outcome outcomes

twoway (rspike min95 max95 outcome, hor lc(black) lw(thin) ) ///
	(scatter outcome estimate , msize(medsmall) ms(O) mc(black)) ///
	, ///
	legend(off) ///
	scheme(s2mono) graphregion(fcolor(white)) ///
	yscale(rev) ///
	ylabel(#12, angle(horizontal) valuelabel labs(2.2) nogrid) ///
	xlabel(#6, labs(2.2)) ///
	xline(0, lpattern(shortdash)) ///
    xtitle("Impact of Gender (Female) on Likelihood of Policy Domain Prioritization" "SUR Pooled OLS Analysis with Country Fixed Effects, 95% CIs" , size(2.2)) ytitle("")
graph save "DATA/Afrobarometer/Graphs/suest_main", replace
graph export "Drafts/Figures/suest_main.pdf", replace
restore

*graph predicted probabilities
postfile pp str15 var str15 female pp pp_se pooled str15 sample str15 controls using "DATA/Afrobarometer/Graphs/pp.dta", replace
foreach y in $outcomes {
est restore `y'
qui: margins, at(female=(0 1)) atmeans 
matrix A=r(table)
post pp ("`y'") ("Men") (A[1,1]) (A[2,1]) (1) ("Pooled") ("Min")
post pp ("`y'") ("Women") (A[1,2]) (A[2,2]) (1) ("Pooled") ("Min")
}
postclose pp

preserve
use "DATA/Afrobarometer/Graphs/pp.dta", clear
gen min95=pp-(pp_se*1.96)
gen max95=pp+(pp_se*1.96)
gen id=_n
gen axis=id*-1
sort axis
sencode var, gen(outcome)
replace outcome=outcome + 0.25 if female=="Men"

twoway (rspike min95 max95 outcome if female=="Men" & sample=="Pooled" & controls=="Min", hor lc(black) lw(thin)) ///
	(scatter outcome pp if female=="Men" & sample=="Pooled" & controls=="Min", msize(medsmall) ms(O) mfc(white) mlc(black)) ///
	(rspike min95 max95 outcome if female=="Women" & sample=="Pooled" & controls=="Min", hor lc(black) lw(thin)) ///
	(scatter outcome pp if female=="Women" & sample=="Pooled" & controls=="Min", msize(medsmall) ms(O) mc(black)) ///
, ///
	legend(order(2 "Men" 4 "Women") size (vsmall) row(2) region(lw(none) fc(none)) ring(0) pos(5) ) ///
	scheme(s2mono) graphregion(fcolor(white)) ///
	ylabel(#10, angle(horizontal) valuelabel labs(2.2) nogrid) ///
	xlabel(#6, labs(2.2)) ///
    xtitle("Predicted Probability of Policy Domain Prioritization" "SUR Pooled OLS Analysis with Country Fixed Effects, 95% CIs", size(2.2)) ytitle("")
graph save "DATA/Afrobarometer/Graphs/pooled_pp", replace
graph export "Drafts/Figures/suest_pp.pdf", replace
restore


*********************************************
			* Multi-Level Models *
*********************************************

use "ANALYSES/AB/GGR_FinalData.dta", clear
set more off
global ivs femploy vulA muslimshare 
global controls age urban  
global shortlist1 Water Infrastructure 
global shortlist2 Poverty

*Run analysis of  interactions 
set more off
foreach var of varlist  $shortlist1 $shortlist2 {
mixed `var' i.female##i.employment##c.femploy i.female##i.edgap##c.vulA i.female##i.muslim i.female##c.age i.female##i.urban i.female##c.muslimshare i.female##c.gdp i.ROUND [pw=Withinwt] || country: 
estimates store `var'
}

*graph predicted rate of prioritization by gender, individualIVs, and country IVs
set more off
foreach y in  $shortlist1 $shortlist2{
estimates res `y'
estimates esample: `y' female muslim employment edgap muslimshare femploy vulA $controls gdp ROUND, replace

qui: margins, by(female employment) atmeans vsquish
marginsplot, plotdimension(female) noci  ///
		plot1opts(msymbol(S) mcolor(black) lcolor(black))  /// marker for first line
		plot2opts(msymbol(O) mcolor(gs10) lcolor(gs10)) /// marker for second line
		ytitle ("Predicted Rate of `y' Prioritization",size(small)) title("") ///
		graphregion(fcolor(white) ilcolor(white) lcolor(white)) scale(1.2) name(`y'_pei, replace) ///
		legend (order (1 "Male" 2 "Female") region(lcolor(white)))

qui: margins, at(female=(0 1) employment=(0 1) femploy=(.07(.1).57)) atmeans vsquish
marginsplot, plotdimension(female employment) noci  ///
		plot1opts(msymbol(S) mcolor(black) lcolor(black))  /// marker for first line
		plot2opts(msymbol(O) mcolor(black) lcolor(black))  /// marker for second line
		plot3opts(msymbol(S) mcolor(gs10) lcolor(gs10)) /// marker for third line
		plot4opts(msymbol(O) mcolor(gs10) lcolor(gs10)) /// marker for fourth line
		ytitle ("Predicted Rate of `y' Prioritization",size(small)) title("") ///
		graphregion(fcolor(white) ilcolor(white) lcolor(white)) scale(1.2) name(`y'_pec, replace) ///
		legend (order (1 "Male, unemployed" 2 "Male, employed" 3 "Female, unemployed" 4 "Female, employed") region(lcolor(white)))

qui: margins, by(female edgap) atmeans vsquish
marginsplot, plotdimension(female) noci  ///
		plot1opts(msymbol(S) mcolor(black) lcolor(black))  /// marker for first line
		plot2opts(msymbol(O) mcolor(gs10) lcolor(gs10)) /// marker for second line
		ytitle ("Predicted Rate of `y' Prioritization",size(small)) title("") ///
		graphregion(fcolor(white) ilcolor(white) lcolor(white)) scale(1.2) name(`y'_pvi, replace) ///
		legend (order (1 "Male" 2 "Female") region(lcolor(white)))

qui: margins, at(female=(0 1) edgap=(0 1) vulA=(-1.6(.2)1.2)) atmeans vsquish
marginsplot, plotdimension(female edgap) noci  ///
		plot1opts(msymbol(S) mcolor(black) lcolor(black))  /// marker for first line
		plot2opts(msymbol(O) mcolor(black) lcolor(black))  /// marker for second line
		plot3opts(msymbol(S) mcolor(gs10) lcolor(gs10)) /// marker for third line
		plot4opts(msymbol(O) mcolor(gs10) lcolor(gs10)) /// marker for fourth line
		ytitle ("Predicted Rate of `y' Prioritization",size(small)) title("") ///
		graphregion(fcolor(white) ilcolor(white) lcolor(white)) scale(1.2) name(`y'_pvc, replace) ///
		legend (order (1 "Male, not vulnerable" 2 "Male, vulnerable" 3 "Female, not vulnerable" 4 "Female, vulnerable") region(lcolor(white)))
		
graph combine `y'_pei `y'_pvi `y'_pec `y'_pvc, title("`y'", color(black)) col(2) graphregion(fcolor(white) ilcolor(white) lcolor(white)) ycommon scale(.7)
graph export "Drafts/Figures/`y'_PP.pdf", replace
}

*graph marginal effects of gender by individual and country IVs (done over two loops to remove zero line for poverty)
set more off
foreach y in $shortlist1 {
estimates res `y'
estimates esample: `y' female muslim employment edgap muslimshare femploy vulA $controls gdp ROUND, replace

qui: margins, dydx(female) by(employment) atmeans vsquish 
marginsplot, recast(scatter) yline(0, lcolor(red)) title("") xtitle("Employed") ///
	ytitle ("Marginal Effect of Female on the Pr(`y' Prioritization)",size(small)) ylabel(, nogrid) ///
	plot1opts(msymbol(O) mcolor(black) lcolor(black) msize(small)) ciopts(lcolor(black)recast(. rcap))   ///
	graphregion(fcolor(white) ilcolor(white) lcolor(white))  ///
	xscale(range(-0.5 1.5)) xlabel(0 1) name(`y'_ei, replace) ///
	legend (off)
	
qui: margins, dydx(female) at(femploy=(.07(.1).57)) atmeans vsquish 
marginsplot, recast(scatter) yline(0, lcolor(red)) title("") xtitle("Share female employment") ///
	ytitle ("Marginal Effect of Female on the Pr(`y' Prioritization)",size(small)) ylabel(, nogrid) ///
	plot1opts(msymbol(O) mcolor(black) lcolor(black) msize(small)) ciopts(lcolor(black)recast(. rcap))   ///
	graphregion(fcolor(white) ilcolor(white) lcolor(white)) name(`y'_ec, replace)  ///
	legend (off) 

qui: margins, dydx(female) by(edgap) atmeans vsquish 
marginsplot, recast(scatter) yline(0, lcolor(red)) title("") xtitle("Vulnerability (Education Gap w/ Avg. Male)") ///
	ytitle ("Marginal Effect of Female on the Pr(`y' Prioritization)",size(small)) ylabel(, nogrid) ///
	plot1opts(msymbol(O) mcolor(black) lcolor(black) msize(small)) ciopts(lcolor(black)recast(. rcap))   ///
	graphregion(fcolor(white) ilcolor(white) lcolor(white)) xscale(range(-0.5 1.5))  name(`y'_vi, replace) ///
	legend (off)
	
qui: margins, dydx(female) at(vulA=(-1.6(.2)1.6)) atmeans vsquish 
marginsplot, recast(scatter) yline(0, lcolor(red)) title("") xtitle("Vulnerability Index") ///
	ytitle ("Marginal Effect of Female on the Pr(`y' Prioritization)",size(small)) ylabel(, nogrid) ///
	plot1opts(msymbol(O) mcolor(black) lcolor(black) msize(small)) ciopts(lcolor(black)recast(. rcap))   ///
	graphregion(fcolor(white) ilcolor(white) lcolor(white)) name(`y'_vc, replace)	///
	legend(off) 

graph combine `y'_ei `y'_vi `y'_ec `y'_vc, title("`y'", color(black)) /// 
col(2) graphregion(fcolor(white) ilcolor(white) lcolor(white)) scale(.7) ycommon
graph export "Drafts/Figures/`y'ME.pdf", replace
*graph drop _all
}	

set more off
foreach y in $shortlist2 {
estimates res `y'
estimates esample: `y' female muslim employment edgap muslimshare femploy vulA $controls gdp ROUND, replace

qui: margins, dydx(female) by(employment) atmeans vsquish 
marginsplot, recast(scatter)  title("") xtitle("Employed") ///
	ytitle ("Marginal Effect of Female on the Pr(`y' Prioritization)",size(small)) ylabel(, nogrid) ///
	plot1opts(msymbol(O) mcolor(black) lcolor(black) msize(small)) ciopts(lcolor(black)recast(. rcap))   ///
	graphregion(fcolor(white) ilcolor(white) lcolor(white))  ///
	xscale(range(-0.5 1.5)) xlabel(0 1) name(`y'_ei, replace) ///
	legend (off)
	
qui: margins, dydx(female) at(femploy=(.07(.1).57)) atmeans vsquish 
marginsplot, recast(scatter)  title("") xtitle("Share female employment") ///
	ytitle ("Marginal Effect of Female on the Pr(`y' Prioritization)",size(small)) ylabel(, nogrid) ///
	plot1opts(msymbol(O) mcolor(black) lcolor(black) msize(small)) ciopts(lcolor(black)recast(. rcap))   ///
	graphregion(fcolor(white) ilcolor(white) lcolor(white)) name(`y'_ec, replace)  ///
	legend (off) 

qui: margins, dydx(female) by(edgap) atmeans vsquish 
marginsplot, recast(scatter)  title("") xtitle("Vulnerability (Education Gap w/ Avg. Male)") ///
	ytitle ("Marginal Effect of Female on the Pr(`y' Prioritization)",size(small)) ylabel(, nogrid) ///
	plot1opts(msymbol(O) mcolor(black) lcolor(black) msize(small)) ciopts(lcolor(black)recast(. rcap))   ///
	graphregion(fcolor(white) ilcolor(white) lcolor(white)) xscale(range(-0.5 1.5))  name(`y'_vi, replace) ///
	legend (off)
	
qui: margins, dydx(female) at(vulA=(-1.6(.2)1.6)) atmeans vsquish 
marginsplot, recast(scatter)  title("") xtitle("Vulnerability Index") ///
	ytitle ("Marginal Effect of Female on the Pr(`y' Prioritization)",size(small)) ylabel(, nogrid) ///
	plot1opts(msymbol(O) mcolor(black) lcolor(black) msize(small)) ciopts(lcolor(black)recast(. rcap))   ///
	graphregion(fcolor(white) ilcolor(white) lcolor(white)) name(`y'_vc, replace)	///
	legend(off) 

graph combine `y'_ei `y'_vi `y'_ec `y'_vc, title("`y'", color(black)) /// 
col(2) graphregion(fcolor(white) ilcolor(white) lcolor(white)) scale(.7) ycommon
graph export "Drafts/Figures/`y'ME.pdf", replace
*graph drop _all
}	

*control for individual level poverty
set more off
foreach var of varlist  $shortlist {
mixed `var'  i.female##i.employment##c.femploy i.female##i.edgap##c.vulA i.female##i.muslim i.female##c.age i.female##i.urban i.female##c.muslimshare i.female##c.gdp i.ROUND i.female##c.wealthA [pw=Withinwt] || country: 
estimates store `var'_pov
}

*Produce regression table
esttab Infrastructure Infrastructure_pov  /// 
	using "Drafts/Tables/inf.tex", star(* 0.10 ** 0.05 *** 0.01) se b(3) se(3) r2(2) label booktabs alignment(D{.}{.}{-1}) replace ///
	stats(N N_clust,layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}") labels("Observations (Individual)" "Observations (Country)") f(0))  ///
	title(Impact of Individual and Country Characteristics on Gender Gaps in Policy Domain Prioritization (Top 3) \label{tab:mlminf})  ///
	drop(lnsig_e: lns1_1_1:) eqlab(, none) ///
	varlabels (1.muslim Muslim 1.female#1.muslim "Female $\times$ Muslim" 1.employment Employed 1.female#1.employment "Female $\times$ Employed" /// 
	1.employment#c.femploy "Employed $\times$ Share female employment" 1.edgap "Education Gap w/ Avg. Male" 1.female#1.edgap "Female $\times$ Education Gap" ///
	1.muslimshare "Share muslim" 1.female#1.muslimshare "Female $\times$ Share muslim" c.femploy "Share female employment" /// 
	1.female#c.femploy "Female $\times$ Share female employment" 1.female#1.employment#c.femploy "Female $\times$ Employed $\times$ Share female employment" ///
	vulA "Vulnerability Index" 1.female#c.vulA "Female $\times$ Vulnerability" 1.female#1.edgap#c.vulA "Female $\times$ Education gap $\times$ Vulnerability" /// 
	1.urban "Urban" 1.female#1.urban "Female $\times$ Urban" 1.Round "Round 5" 1.edgap#c.vulA "Education gap $\times$ Vulnerability" ///
	_cons Constant) ///
	nonotes  nobaselevels  nogaps ///
	sub("\begin{tabular}" "\scalebox{0.7}{\begin{tabular}" "\end{tabular}" "\end{tabular}}" "\multicolumn{1}{c}{N}" "Observations") ///
	addnote("Multi-level models in which individuals are nested within countries." "$^* p<0.10$, $^{**} p<0.05$, $^{***} p<0.01$")

esttab Water Water_pov  /// 
	using "Drafts/Tables/water.tex", star(* 0.10 ** 0.05 *** 0.01) se b(3) se(3) r2(2) label booktabs alignment(D{.}{.}{-1}) replace ///
	stats(N N_clust,layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}") labels("Observations (Individual)" "Observations (Country)") f(0))  ///
	title(Impact of Individual and Country Characteristics on Gender Gaps in Policy Domain Prioritization (Top 3) \label{tab:mlmwater})  ///
	drop(lnsig_e: lns1_1_1:) eqlab(, none) ///
	varlabels (1.muslim Muslim 1.female#1.muslim "Female $\times$ Muslim" 1.employment Employed 1.female#1.employment "Female $\times$ Employed" /// 
	1.employment#c.femploy "Employed $\times$ Share female employment" 1.edgap "Education Gap w/ Avg. Male" 1.female#1.edgap "Female $\times$ Education Gap" ///
	1.muslimshare "Share muslim" 1.female#1.muslimshare "Female $\times$ Share muslim" c.femploy "Share female employment" /// 
	1.female#c.femploy "Female $\times$ Share female employment" 1.female#1.employment#c.femploy "Female $\times$ Employed $\times$ Share female employment" ///
	vulA "Vulnerability Index" 1.female#c.vulA "Female $\times$ Vulnerability" 1.female#1.edgap#c.vulA "Female $\times$ Education gap $\times$ Vulnerability" /// 
	1.urban "Urban" 1.female#1.urban "Female $\times$ Urban" 1.Round "Round 5" 1.edgap#c.vulA "Education gap $\times$ Vulnerability" ///
	_cons Constant) ///
	nonotes  nobaselevels  nogaps ///
	sub("\begin{tabular}" "\scalebox{0.7}{\begin{tabular}" "\end{tabular}" "\end{tabular}}" "\multicolumn{1}{c}{N}" "Observations") ///
	addnote("Multi-level models in which individuals are nested within countries." "$^* p<0.10$, $^{**} p<0.05$, $^{***} p<0.01$")

	
	esttab Poverty Poverty_pov  /// 
	using "Drafts/Tables/pov.tex", star(* 0.10 ** 0.05 *** 0.01) se b(3) se(3) r2(2) label booktabs alignment(D{.}{.}{-1}) replace ///
	stats(N N_clust,layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}") labels("Observations (Individual)" "Observations (Country)") f(0))  ///
	title(Impact of Individual and Country Characteristics on Gender Gaps in Policy Domain Prioritization (Top 3) \label{tab:mlmpov})  ///
	drop(lnsig_e: lns1_1_1:) eqlab(, none) ///
	varlabels (1.muslim Muslim 1.female#1.muslim "Female $\times$ Muslim" 1.employment Employed 1.female#1.employment "Female $\times$ Employed" /// 
	1.employment#c.femploy "Employed $\times$ Share female employment" 1.edgap "Education Gap w/ Avg. Male" 1.female#1.edgap "Female $\times$ Education Gap" ///
	1.muslimshare "Share muslim" 1.female#1.muslimshare "Female $\times$ Share muslim" c.femploy "Share female employment" /// 
	1.female#c.femploy "Female $\times$ Share female employment" 1.female#1.employment#c.femploy "Female $\times$ Employed $\times$ Share female employment" ///
	vulA "Vulnerability Index" 1.female#c.vulA "Female $\times$ Vulnerability" 1.female#1.edgap#c.vulA "Female $\times$ Education gap $\times$ Vulnerability" /// 
	1.urban "Urban" 1.female#1.urban "Female $\times$ Urban" 1.Round "Round 5" 1.edgap#c.vulA "Education gap $\times$ Vulnerability" ///
	_cons Constant) ///
	nonotes  nobaselevels  nogaps ///
	sub("\begin{tabular}" "\scalebox{0.7}{\begin{tabular}" "\end{tabular}" "\end{tabular}}" "\multicolumn{1}{c}{N}" "Observations") ///
	addnote("Multi-level models in which individuals are nested within countries." "$^* p<0.10$, $^{**} p<0.05$, $^{***} p<0.01$")

	
	esttab Economy Economy_pov  /// 
	using "Drafts/Tables/econ.tex", star(* 0.10 ** 0.05 *** 0.01) se b(3) se(3) r2(2) label booktabs alignment(D{.}{.}{-1}) replace ///
	stats(N N_clust,layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}") labels("Observations (Individual)" "Observations (Country)") f(0))  ///
	title(Impact of Individual and Country Characteristics on Gender Gaps in Policy Domain Prioritization (Top 3) \label{tab:mlmecon})  ///
	drop(lnsig_e: lns1_1_1:) eqlab(, none) ///
	varlabels (1.muslim Muslim 1.female#1.muslim "Female $\times$ Muslim" 1.employment Employed 1.female#1.employment "Female $\times$ Employed" /// 
	1.employment#c.femploy "Employed $\times$ Share female employment" 1.edgap "Education Gap w/ Avg. Male" 1.female#1.edgap "Female $\times$ Education Gap" ///
	1.muslimshare "Share muslim" 1.female#1.muslimshare "Female $\times$ Share muslim" c.femploy "Share female employment" /// 
	1.female#c.femploy "Female $\times$ Share female employment" 1.female#1.employment#c.femploy "Female $\times$ Employed $\times$ Share female employment" ///
	vulA "Vulnerability Index" 1.female#c.vulA "Female $\times$ Vulnerability" 1.female#1.edgap#c.vulA "Female $\times$ Education gap $\times$ Vulnerability" /// 
	1.urban "Urban" 1.female#1.urban "Female $\times$ Urban" 1.Round "Round 5" 1.edgap#c.vulA "Education gap $\times$ Vulnerability" ///
	_cons Constant) ///
	nonotes  nobaselevels  nogaps ///
	sub("\begin{tabular}" "\scalebox{0.7}{\begin{tabular}" "\end{tabular}" "\end{tabular}}" "\multicolumn{1}{c}{N}" "Observations") ///
	addnote("Multi-level models in which individuals are nested within countries." "$^* p<0.10$, $^{**} p<0.05$, $^{***} p<0.01$")

