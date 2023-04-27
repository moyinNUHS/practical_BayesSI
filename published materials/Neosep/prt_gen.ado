prog def prt_gen
/* 
Simulate data for a personalised randomisation  trial
few or many patterns

16aug2019: change to a uniform distribution of treatment mortalities and drop pattern link to mortality
	(was a normal distribution of treatment effects relative to an intercept)

from prat_gen.do IW 21jan2019

Input: 
	P*(T+1) matrix of patterns (last column = probabilities)
	1*P matrix of treatment effects 
	where P=#patterns
		T=#treatments
Output
	pattern
	canhave*
	randtrt
	y
*/
syntax, Canhave(name) NObs(int) prob(string) BETA(name) 

local ntrt 0
foreach can of varlist `canhave'* {
	local ++ntrt
	local trt = substr("`can'",length("`canhave'")+1,.)
	local trtlist `trtlist' `trt'
}

* treatment subset (pattern)
gen pattern = _n
gen cumprob = sum(`prob')
gen cumn = `nobs' * cumprob / cumprob[_N]
gen n = cumn - cond(_n>1,cumn[_n-1],0)
qui expand n
drop cumn n 

* randomise
egen ncanhave=rsum(`canhave'*)
qui gen randtrt=.
gen rand = runiform()
foreach trt of local trtlist {
	qui replace randtrt = `trt' if randtrt==. & `canhave'`trt' & rand<1/ncanhave
	qui replace rand=rand-1/ncanhave if `canhave'`trt'
}
drop ncanhave rand

* outcome
gen py = .
forvalues j=1/`ntrt' {
	qui replace py = `beta'[1,`j'] if randtrt==`j'
}
gen y = runiform() < py
drop cumprob py
end
