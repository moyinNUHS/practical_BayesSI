/*
simulate and analyse data for PRT

16aug2019: change to a uniform distribution of treatment mortalities and drop pattern link to mortality
	(was a normal distribution of treatment effects relative to an intercept)

18jul2019: improve main graph for WiP talk

Ian 27feb2019
improve graphs 12jun2019

calls prt_gen 
must previously run prt_patterns

To do: 
	consider beta prior instead of log-normal
	explore whether gain as fraction of possible gain is 
		little affected by either priorsd or by patterns

*/

local name prt_all
local patternslist patterns10
local nsims 1000
local nobslist 50 100 200 500 1000 2000 
local pmortlist runiform(0.1,0.2) runiform(0.1,0.3)
local margin 0.001 // amount by which treatment may be inferior to best
local run 0
set seed 578156
pda
set trace off

if `run' {
* patterns

cap postclose ian
postfile ian str20 pmort str10 patterns nobs i str8 method EY success improve using `name'_postfile, replace

foreach pmort of local pmortlist {
foreach patterns of local patternslist {
foreach nobs of local nobslist {
	di as input _new(2) "pmort=`pmort', patterns=`patterns', nobs=`nobs'"

	forvalues i=1/`nsims' {
		dotter `i' `nsims' 	
		qui {

			* count treatments
			use `patterns', clear
			unab vars : canhave*
			local ntrts : word count `vars'

			* state of nature
			drop _all
			set obs `ntrts'
			gen beta = `pmort'
			mkmat beta, matrix(beta)
			mat beta = beta'

			* data
			use `patterns', clear
			prt_gen, canhave(canhave) prob(prob) nobs(`nobs') beta(beta) 

			* pattern (NMA-like) analysis
			cap logit y i.randtrt i.pattern, iter(100)
			if _rc cap logit y ibn.randtrt i.pattern, nocons iter(100)
			if _rc {
				di as error "Non convergence at iteration `i'"
				continue
			}

			* make decisions
			gen EYsample = . // true value of choice
			gen bestpred = . // estimated value of choice
			gen trts=0
			gen EYperfect = .
			gen EYno=0
			gen trueprob = .
			*gen besttrt = .
			*gen truebesttrt = .
			forvalues trt=1/`ntrts' {

				qui replace randtrt = `trt'
				qui predict pred, rules // handles perfect prediction
				qui replace trueprob = beta[1,`trt']
				
				* no information: random choice
				qui replace trts = trts+1 if canhave`trt'
				qui replace EYno = EYno+trueprob if canhave`trt'
				
				* perfect information
				*qui replace truebesttrt = `trt' if trueprob<EYperfect & canhave`trt'
				qui replace EYperfect = min(EYperfect, trueprob) if canhave`trt'

				* sample information
				qui replace EYsample = trueprob if pred<bestpred & canhave`trt' 
				*qui replace besttrt = `trt' if pred<bestpred & canhave`trt' 
				qui replace bestpred = pred if pred<bestpred & canhave`trt' 
				
				drop pred
			}
			qui replace EYno=EYno/trts
			foreach method in no sample perfect {
				summ EY`method', meanonly
				local EYmean = r(mean)
				gen success`method' = EY`method' < EYperfect + `margin'
				summ success`method', meanonly
				local successprob = r(mean)
				gen improve`method' = EY`method' < EYno 
				summ improve`method', meanonly
				local improveprob = r(mean)
				post ian ("`pmort'") ("`patterns'") (`nobs') (`i') ("`method'") ///
					(100*`EYmean') (100*`successprob') (100*`improveprob')
			}
		} 	
	}

} // close the "foreach" loops
} // close the "foreach" loops
} // close the "foreach" loops
postclose ian
} // close "if run"

use `name'_postfile, clear
reshape wide EY success improve, i(i pmort patterns nobs) j(method) string
foreach method in perfect sample {
	gen gain`method' = EYno - EY`method'
}

bysort pmort patterns: ci means EYno EYperfect 
bysort pmort patterns nobs: ci means EYsample

bysort pmort patterns: ci means gainperfect
bysort pmort patterns nobs: ci means gainsample

bysort pmort patterns nobs: ci means success*

bysort pmort patterns nobs: ci means improve*

label var gainsample "Reduction in % mortality from sample information"

histogram gainsample, by(pmort patterns nobs, row(2)) xli(0, lcol(black) lpatt(dash)) ///
	name(hist, replace) 

encode patterns, gen(pattnum)

local nobsmin .
local nobsmax .
foreach nobs of local nobslist {
	local nobsmin= min(`nobsmin',`nobs')
	local nobsmax= max(`nobsmax',`nobs')
}
local nobsmax=1.2*`nobsmax'

table nobs pmort patterns, c(mean gainperfect mean gainsample mean successsample mean improvesample)

foreach pmort of local pmortlist {
local pmortname=strtoname("`pmort'")
foreach pattern of local patternslist {
	summ gainperfect if pmort=="`pmort'" & patterns=="`pattern'", meanonly
	local gainperfect = r(mean)
	meangr8 gainsample nobs if pmort=="`pmort'" & patterns=="`pattern'", ///
		name(g_`pmortname'_`pattern',replace) ///
		saving(g_`pmortname'_`pattern',replace) ///
		xscale(log) xlabel(`nobslist') xscale(range(`nobsmin', `nobsmax')) ///
		ytitle("Average reduction in % mortality" "from sample information") ///
		xtitle(Sample size) color(red) ///
		yli(0 `gainperfect', lcol(blue) lpattern(solid)) lwidth(*2) ///
		yscale(range(0 `gainperfect')) ylabel(#6) scheme(mrc) $PPT ///
		note("Mortality distribution: `pmort'. Pattern file: `pattern'. Reduction from perfect information: `=string(`gainperfect',"%3.1f")'%")

	meangr8 successsample nobs if pmort=="`pmort'" & patterns=="`pattern'", ///
		name(s_`pmortname'_`pattern',replace) ///
		saving(s_`pmortname'_`pattern',replace) ///
		xscale(log) xlabel(`nobslist') xscale(range(`nobsmin', `nobsmax')) ///
		ytitle("% success" "using sample information") ///
		xtitle(Sample size) color(red) ///
		yli(0 100, lcol(blue) lpattern(solid)) lwidth(*2) ///
		yscale(range(0 100)) scheme(mrc) $PPT ///
		note("Mortality distribution: `pmort'. Pattern file: `pattern'. Success: chosen treatment is within `margin ' of best.")

	meangr8 improvesample nobs if pmort=="`pmort'" & patterns=="`pattern'", ///
		name(i_`pmortname'_`pattern',replace) ///
		saving(i_`pmortname'_`pattern',replace) ///
		xscale(log) xlabel(`nobslist') xscale(range(`nobsmin', `nobsmax')) ///
		ytitle("% improving on random choice" "using sample information") ///
		xtitle(Sample size) color(red) ///
		yli(50 100, lcol(blue) lpattern(solid)) lwidth(*2) ///
		yscale(range(50 100)) scheme(mrc) $PPT ///
		note("Mortality distribution: `pmort'. Pattern file: `pattern'. Improve: chosen treatment is better than a random choice.")
}
}

* final summary
collapse (mean) gainperfect gainsample successsample improvesample, by(nobs pmort patterns)
egen gainmax=mean(gainperfect), by(pmort)
drop gainperfect
sort pmort nobs
gen pctgain=gainsample/gainmax
l, sepby(pmort)
