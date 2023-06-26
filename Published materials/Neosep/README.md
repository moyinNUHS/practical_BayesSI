# Personalised Randomisation Trial

## Sample size calculation: prt_all.do
inputs:
	number of treatments
	data set of patterns (patterns2 or patterns10, made by prt_patterns.do)
	distribution of p(death|treatment) 
	number of obs
models:
	binary outcome
	random treatment effects
calls:
	prt_gen
my utilities needed:
	pda
	dotter
	meangr8
Note: 
	set -local run 1- at line 28 to run for the first time
	final output variable pctgain is a proportion not a percentage
	
	
	Becky, these should be enough files to run the sample size calculation. Main file is prt_all.do but first read README.md. I call some of my own utilities (e.g. pda.ado) but think I have included them all. Let me know if not clear! I see I did let treatment effects be random at each repetition, that could clearly be changed. Bw Ian
	
	Dear Mo,
 
Thanks for confirming there will be one randomisation allowed for in the sample size calculation. I’m forwarding Ian’s code, because this is much closer to the code you need than the neoSEP1 code. The README file explains what each file does, together with Ian’s message below. One change you will need to make is specifying fixed values for the mortalities under different treatments, because these were generated under a random distribution for the sample size calculations presented in the LID paper. The summary I sent previously shows the mortality values we assumed in neoSEP1.
 
You’re welcome to get in touch with me if anything is unclear.
 
Best wishes,
Becky