********************************************************************************

* Jacaranda Health Evaluation: Facility Randomization 

********************************************************************************

*	REQUIRES: $data/final/facilities.dta
				
*	WRITTEN BY: Wei Chang; Modified by Rajet Vatsa
	
********************************************************************************

* Stata Version
version 15.1

* Clearing
clear all
set more off
cap log close
program drop _all
pause on

* Set local directory and load facilities.dta dataset
if c(username) == "rajvatsa" {
	global data = "/Users/rajvatsa/Dropbox (Harvard University)/JH Eval Data/DHIS2"
}
cd "$data"
use "$data/final/facilities.dta", clear

* Keep 40 facilities in the final randomization list, per the study's inclusion 
* criteria, facility level/volume, Jacaranda Health's county expansion plans,
* geographic considerations, etc.
keep if randomizationlist == "yes"

* Randomize facilities: 
* 1) 1:1 treatment-to-control assignments, balancing on facility-level caesarean 
* birth frequency and perinatal mortality rate
* 2) Iterate until balance p-value >0.7; max 2,000 iterations permitted
* 3) Block on tertile of monthly facility volume of normal vaginal births 
randomize, groups(2) balance(deliver_c_p mortality_perinatal) ///
block(tertile_deliver_nor) jointp(0.7) maxruns(2000) seed(20211021) replace 

* Create intervention arm variable
cap drop treat
cap label drop ltreat
gen treat = _assign - 1
lab def ltreat 0 "Control" 1 "Treated"
lab val treat ltreat
tab treat // Confirm 20 facilities assigned to treatment and control
