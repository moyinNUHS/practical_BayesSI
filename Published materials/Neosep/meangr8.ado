/*
*! version 1.3.1   Ian White   25 Apr 2017
	corrected bug that led to if/in being ignored
version 1.3   Ian White   3 Oct 2016 

graph mean of y against x with CI

* meangr y x
* meangr y x, by(z)                       -> separate graphs by z
* meangr y x, by(z) spread(.03) overlay   -> single graph

version 1.3   3 Oct 2016 
	by() allows suboptions
26sep2014
	pattern(), lwidth() options; byname inserts "var=" into legend
version 1.2   11 June 2003
	corrects keys and enables c(), s(), p()
version 1.1   25 April 2001 
	uses value labels in key;  uses stata 7

problems: stagger() works best with adjacent levels of by
problems noted 27feb2019: 
	by() without overlay fails to follow legend(off) and note() options

Note:
	with xscale(log), use stagger(*#) 
	otherwise, use stagger(#)

*/
prog def meangr8
version 11

syntax varlist(min=2 max=2) [if] [in] [, stagger(string) by(string) ///
	overlay missing level(cilevel) BYName ///
    Symbol(string) COLor(string) PATtern(string) LWidth(passthru) Connect(string) ///
    clear debug *]
tokenize "`varlist'"
local y `1'
local x `2'
local graphoptions `options'
marksample touse, novarlist

if "`by'"~="" {
	local byby by(`by')
	local 0 `by'
	syntax varname, [*]
	local byvar `varlist'
	local byopts `options'
}
if mi("`color'") local color navy maroon forest_green dkorange teal cranberry ///
	lavender khaki sienna emidblue emerald brown erose gold bluishgray 
	// default for s2color (see http://www.stata.com/statalist/archive/2011-02/msg00692.html)
	
* parse stagger
if mi("`stagger'") local stagger 0
cap confirm number `stagger'
if _rc {
	if substr("`stagger'",1,1) != "*" {
		di as error "stagger(#) or stagger(*#)"
		exit 198
	}
	local stagger = substr("`stagger'",2,.)
	local stagger `stagger'*`x'
}


preserve
qui count if `x'==. & `touse'
if r(N)>0 {
    di in blue "Dropping " r(N) " observations with `x'==."
    qui drop if `x'==.
}
if "`missing'"=="" & "`byvar'"~="" {
    qui count if `byvar'==. & `touse'
    if r(N)>0 {
      di in blue "Dropping " r(N) " observations with `byvar'==."
      qui drop if `byvar'==.
    }
}
collapse (mean) _mean=`y' (sd) _sd=`y' (count) _count=`y' if `touse', by(`byvar' `x')
local zcrit = invnorm(.5+`level'/200)
gen _upper = _mean + `zcrit'*_sd/sqrt(_count)
gen _lower = _mean - `zcrit'*_sd/sqrt(_count)
label var _mean "Mean of `y'"
local graphoptions note(Showing `level'% confidence intervals) `graphoptions'
if "`overlay'"=="" | "`byvar'"=="" {
    if "`connect'"=="" local c l
    else {
        getfirstc `connect'
        local c = r(firstc)
    }
    if "`symbol'"=="" local s o
    else local s =substr("`symbol'",1,1)
    if "`color'"=="" local color navy
    else local color = word("`color'",1)
    local graphcmd twoway (line _mean `x', lcol(`color') `lpattern' `lwidth') ///
        (rspike _upper _lower `x', lcol(`color') `lwidth'), legend(off) `byby' `graphoptions' 
}
else qui {
    local i = 1
    local go = 1
    while `go'==1 {
        if `i'==1 summ `byvar'
        else summ `byvar' if `byvar'>`min' // levelsof should be used here?
        if r(N)>0 {
            local min = r(min)
            gen _mean`i' = _mean if `byvar'==`min'
            local vallab : label (`byvar') `min'
            if !mi("`byname'") label var _mean`i' "`byvar'=`vallab'"
            else label var _mean`i' "`vallab'"
            gen _upper`i' = _upper if `byvar'==`min'
            gen _lower`i' = _lower if `byvar'==`min'
            local thiscolor = word("`color'",`i')
            if "`thiscolor'" != "" local lcolor lcolor(`thiscolor')
            local thispattern = word("`pattern'",`i')
            if "`thispattern'" != "" local lpattern lpattern(`thispattern')
            local twoway `twoway' (line _mean`i' `x', `lcolor' `lpattern' `lwidth') ///
                (rspike _lower`i' _upper`i' `x', `lcolor' `lwidth')
            local thisorder = 2*`i'-1
            local orderlist `orderlist' `thisorder'
            local i = `i' + 1
        }
        else local go 0
    }
    summ `byvar'
	replace `x' = `x' + (`byvar'-r(mean))*`stagger'
	local graphcmd twoway `twoway', `t1title' `t2title' `graphoptions' legend(order(`orderlist'))
}
if !mi("`debug'") di as input `"meangr8 is running the command: `graphcmd'"'

`graphcmd'
if "`clear'"=="clear" {
    restore, not
    global F9 `graphcmd'
    di as text "Graph data loaded into memory: press F9 to recall graph command"
}
end

prog def getfirstc, rclass
if substr("`1'",2,1)=="[" local length=index("`1'","]")
else local length 1
if `length'>0 {
   return local firstc = substr("`1'",1,`length')
   return local rest = substr("`1'",`length'+1,.)
} 
else {
   return local firstc = .
   return local rest = .
} 
end
