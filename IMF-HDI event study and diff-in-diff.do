**********************************************  
*        María Alejandra Torres León         *
*       Advanced Econometrics - 2020-1       *
*          Department of Economics           *
*         Universidad de los Andes           *
*              Final Coursework              *
**********************************************

* This code is part of my final coursework for the Advanced Econometrics course. For this project, I explore the effect of IMF programs on development,
* using the Human Development Index (HDI) as a proxy. For this, I estimate the causal effect of the program using an event study and difference in difference models
* with different specifications (basic model, dynamic model, heterogeneous effects).
* The dataset is "International Political Economy Data Resource" (Graham & Tucker, 2018).

cd ""

use "IMF-HDI_data.dta", clear

** Descriptive statistics **


* Evolution of IMF programs before 2008
preserve
drop if year > 2008

graph bar under_bcg, over(year) graphregion(fcolor(white)) ylabel(, nogrid) title("Average IMF programs") ytitle("# of programs") name(a, replace)
restore
graph export "Programs.png", replace

* Human Development Index: Proxy of development level of a country
hist HDI, graphregion(fcolor(white)) ylabel(, nogrid) title("HDI distribution") ytitle("Density") xtitle("Human Development Index") color(navy) name(b, replace)
graph export "hdi.png", replace

* Average HDI
preserve
drop if year<1980
graph bar HDI, over(year) graphregion(fcolor(white)) ylabel(, nogrid) title("Evolution of Human Development Index") ytitle("Average HDI") name(c, replace)
graph export "idh.png", replace
restore

* Average HDI for countries with and without agreements with the IMF
graph bar HDI, over(under_bcg) graphregion(fcolor(white)) ylabel(, nogrid) title("HDI and IMF agreements") name(d, replace) ytitle("Average HDI") note("1 if there is an agreement and 0 if there is not") 
graph export "idhacuerdos.png", replace

* Combine plots
graph combine a b c d, graphregion(fcolor(white))  title("Descriptive statistics")
graph export "combine.png", replace

* Ttest for countries labeled as democracies
estpost ttest under_bcg, by(democracy_dd)
esttab using "b.tex", label replace nonotes 

* Ttest for OECD countries
estpost ttest under_bcg, by(oecdmem_mem)
esttab using "b.tex", label append nonotes 


* Validate parallel trends assumption
preserve
drop if under_bcg == .
drop if HDI == .
collapse (mean) HDI, by(year under_bcg)
reshape wide HDI, i(year) j(under_bcg)
graph twoway line HDI* year, sort graphregion(fcolor(white)) ylabel(, nogrid) title("PArallel trends assumption") ytitle("Average HDI") note("1 if there is an agreement and 0 if there is not")
graph export "paralleltrends.png", replace
restore


******************************************************************************************************	
* Estimations
******************************************************************************************************

* 1. Event study
* 2. Fixed effects
* 2.1. Basic model
* 2.2. Heterogeneous effects
* 2.3. Dynamic model
* 2.4. OECD dummy
* 2.5. Democracy dummy

* 1. Event Study

* Create group variable - Groups according to HDI classification: Very high, high, middle, low and others.

gen very_high = 1 if HDI >= 0.800
replace very_high = 0 if very_high == .
gen high = 1 if inrange(HDI, 0.700, 0.799)
replace high = 0 if high == .
gen medium = 1 if inrange(HDI, 0.550, 0.699)
replace medium = 0 if medium == .
gen low = 1 if HDI <= 0.550
replace low = 0 if low == .

* Cluster variable
gen clustvar = 1 if very_high == 1
replace clustvar = 2 if high == 1
replace clustvar = 3 if medium == 1
replace clustvar = 4 if low == 1

* Treatment Periods
gen time_treated = year if under_bcg == 1
* We only consider the first time a country was part of the program, this is, the minimum time_treated
bys ifscode: egen first = min(time_treated)
gen time_since_treated = year - first

* Time dummies relative to t = 0
tab time_since_treated, gen(dummy_) m

local j = 1
	forval i = -17(1)28 {
	
	label var dummy_`j' "`i'"
	local j = `j' + 1
	
	}	
drop dummy_47

* Declare panel data
xtset ifscode year
replace dummy_17 = 0


global controles "gdppc_wdi_pw gini_wdi pop_wdi govexp_edu_wdi inflation_wdi"

* Event study model
reghdfe HDI dummy_1 - dummy_46 , nocons a(i.year i.ifscode) vce(cluster ifscode) 
estimates store coef1

* Export estimates
outreg2 using "reg.tex", tex replace nor2 nonotes noobs label dec(2) decmark(.) ctitle("HDI") 

*Coefplot
coefplot coef1,  omitted  xline(17, lcolor(black) lpattern(dash) )vertical label yline(0, lpattern(dash) lwidth(*0.5))    ///
ytitle("Coefficient") xtitle("Time relative to treatment", size(medsmall)) xlabel(, labsize(tiny) nogextend labc(black))  /// 
 ylabel(,nogrid nogextend labc(black) format(%9.0f)) msymbol(O) mlcolor(black)  mfcolor(black) msize(vsmall) levels(95)   ///
  ciopts(lcol(black) recast(rcap) lwidth(*0.8)) plotregion(lcolor(black) fcolor(white)) graphregion(lcolor(black)         ///
  fcolor(white)) yscale(lc(black)) xscale(lc(black)) title("Event Study estimation")                                      ///
  note("Fixed effects by country and year" "We only take the first year of the agreement.")
graph export "coefplot.png", replace


** 2. Fixed effects model
* Post treatment variable
gen post = time_since_treated
replace post = 1 if post >= 1 & time_since_treated != .
replace post = 0 if post <= 0
* 1 for periods after the treatment
* We don't include the agreement dummy to avoid multicollineality 

* Save in matrix
matrix A = J(4,14,.)
matrix significant = J(4,14,0)

* 2.1.  Basic model
xtreg HDI post 1.under_bcg#1.post i.year, fe  vce(cluster ifscode) nonest
matrix A[1,1] = r(table)[1,1]
matrix significant[1,1] = (r(table)[4,1]<0.1)+(r(table)[4,1]<0.05)+(r(table)[4,1]<0.01)
matrix A[1,2] = r(table)[2,1]
matrix A[2,1] = r(table)[1,2]
matrix A[2,2] = r(table)[2,2]
matrix significant[2,1] = (r(table)[4,2]<0.1)+(r(table)[4,2]<0.05)+(r(table)[4,2]<0.01)


* 2.2. Heterogeneous effects model
xtreg HDI  post 1.under_bcg#1.post i.year i.democracy_dd i.oecdmem_mem 1.under_bcg#1.democracy_dd 1.under_bcg#1.oecdmem_mem 1.post#1.democracy_dd 1.post#1.oecdmem_mem 1.under_bcg#1.democracy_dd#1.post 1.under_bcg#1.oecdmem_mem#1.post, fe vce(cluster ifscode) nonest
matrix A[1,3] = r(table)[1,1]
matrix A[1,4] = r(table)[2,1]
matrix significant[1,3] = (r(table)[4,1]<0.1)+(r(table)[4,1]<0.05)+(r(table)[4,1]<0.01)
matrix A[2,3] = r(table)[1,2]
matrix A[2,4] = r(table)[2,2]
matrix significant[2,3] = (r(table)[4,2]<0.1)+(r(table)[4,2]<0.05)+(r(table)[4,2]<0.01)
matrix A[3,3] = r(table)[1,31]
matrix A[3,4] = r(table)[2,31]
matrix significant[3,3] = (r(table)[4,31]<0.1)+(r(table)[4,31]<0.05)+(r(table)[4,31]<0.01)
matrix A[4,3] = r(table)[1,30]
matrix A[4,4] = r(table)[2,30]
matrix significant[4,3] = (r(table)[4,30]<0.1)+(r(table)[4,30]<0.05)+(r(table)[4,30]<0.01)


* 2.3. Dynamic model
xtreg HDI post 1.under_bcg#1.post dummy_1-dummy_44 1.under_bcg#1.post i.year i.democracy_dd i.oecdmem_mem 1.under_bcg#1.democracy_dd 1.under_bcg#1.oecdmem_mem 1.post#1.democracy_dd 1.post#1.oecdmem_mem 1.under_bcg#1.democracy_dd#1.post 1.under_bcg#1.oecdmem_mem#1.post, fe vce(cluster ifscode) nonest
matrix A[1,5] = r(table)[1,1]
matrix significant[1,3] = (r(table)[4,1]<0.1)+(r(table)[4,1]<0.05)+(r(table)[4,1]<0.01)
matrix A[1,6] = r(table)[2,1]
matrix A[2,5] = r(table)[1,2]
matrix A[2,6] = r(table)[2,2]
matrix significant[2,3] = (r(table)[4,2]<0.1)+(r(table)[4,2]<0.05)+(r(table)[4,2]<0.01)
matrix A[3,5] = r(table)[1,75]
matrix A[3,6] = r(table)[2,75]
matrix significant[4,3] = (r(table)[4,75]<0.1)+(r(table)[4,75]<0.05)+(r(table)[4,75]<0.01)
matrix A[4,5] = r(table)[1,74]
matrix A[4,6] = r(table)[2,74]
matrix significant[4,5] = (r(table)[4,74]<0.1)+(r(table)[4,74]<0.05)+(r(table)[4,74]<0.01)

* 2.4. OECD dummy model
xtreg HDI  post 1.under_bcg#1.post i.oecdmem_mem i.year  1.under_bcg#1.oecdmem_mem#1.post
matrix A[1,7] = r(table)[1,1]
matrix significant[1,7] = (r(table)[4,1]<0.1)+(r(table)[4,1]<0.05)+(r(table)[4,1]<0.01)
matrix A[1,8] = r(table)[2,1]
matrix A[2,7] = r(table)[1,2]
matrix A[2,8] = r(table)[2,2]
matrix significant[2,7] = (r(table)[4,2]<0.1)+(r(table)[4,2]<0.05)+(r(table)[4,2]<0.01)
matrix A[3,7] = r(table)[1,24]
matrix A[3,8] = r(table)[2,24]
matrix significant[3,7] = (r(table)[4,24]<0.1)+(r(table)[4,24]<0.05)+(r(table)[4,24]<0.01)


* 2.5. Democracy dummy model
xtreg HDI  post 1.under_bcg#1.post i.democracy_dd i.year 1.under_bcg#1.democracy_dd#1.post
matrix A[1,9] = r(table)[1,1]
matrix significant[1,9] = (r(table)[4,1]<0.1)+(r(table)[4,1]<0.05)+(r(table)[4,1]<0.01)
matrix A[1,10] = r(table)[2,1]
matrix A[2,9] = r(table)[1,2]
matrix A[2,10] = r(table)[2,2]
matrix significant[2,9] = (r(table)[4,2]<0.1)+(r(table)[4,2]<0.05)+(r(table)[4,2]<0.01)
matrix A[3,9] = r(table)[1,24]
matrix A[3,10] = r(table)[2,24]
matrix significant[3,9] = (r(table)[4,24]<0.1)+(r(table)[4,24]<0.05)+(r(table)[4,24]<0.01)


matlist A
matlist significant

* Results table
frmttable using "tabl11.tex", tex replace sdec(3) statmat(A) substat(1) annotate(significant) ///
asymbol (*,**,***) title("Fixed effects model") ///
 ctitles("", "Diff-in-Diff", "Fixed Effects +", "Dynamic Model +", "OECD", "Democracy"\"", "", " Heterogeneous", "Heterogeneous", "", "", "", ""\"","Fixed effects","effects","effects","","","") ///
 rtitles ("Post"\""\"IMFxPost"\""\"IMFxPostx"\"OECD"\"IMFxPostx"\"Democracy") note("Standard Errors in Parenthesis. * p<0.01, ** p<0.05, * p<0.1" "Post means that the country had a program and takes the value of 1 for all the periods after signing the agreement. \\\
  All models have country and year fixed effects. Estimations use country-level clusters. HDI has values between 0 and 1, where 0 is the lowest level and 1 is the highest.")
