/*
Make patterns for use in prt_all.do
*/
drop _all
input canhave1-canhave10 prob alpha
	1 1 1 1 0 0 0 0 0 0  .3  -2
	1 1 1 1 1 1 1 0 0 0  .1  -1.7
	0 0 0 0 1 1 1 0 0 0  .2  -1.5
	0 0 0 0 1 1 1 1 1 0  .1  -1.2
	0 0 0 0 0 0 0 1 1 0  .1  -1.0
	0 0 0 0 0 0 0 1 1 1  .2  -0.8
end
* overall prevalence is 0.1997
replace alpha = alpha - .357175
* overall prevalence is 0.15
save patterns10, replace

gen p=invlogit(alpha)
summ p [aw=prob]




// 2 treatments
drop _all
input canhave1 canhave2 prob alpha
	1 1 1 -1.734601
end
* overall prevalence is 0.15
save patterns2, replace

gen p=invlogit(alpha)
summ p [aw=prob]




// summarise p(mortality) for the two patterns
foreach p in 2 10 {
	foreach beta in -.4 -0.2 0 .2 .4 {
		use patterns`p', clear
		gen p=invlogit(alpha+`beta')
		summ p [aw=prob], meanonly
		di "Data=patterns`p'" _col(22) "beta=`beta'" _col(35) "p(mortality)=" %6.1f 100*r(mean) "%"
	}
}
