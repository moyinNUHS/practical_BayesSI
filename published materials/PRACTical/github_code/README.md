Supporting material for the simulation study in 
“The Personalised Randomised Controlled Trial (PRACTical): evaluation of a new trial design” 
by Lee et al (2022) Stat in Medicine


The “1 PRaCTical_functions.R” contains functions that 
-	generate a data set
-	perform different analysis approaches (methods A, B1, B2, B3, C and D)
-	identify rankings given the estimates of a pattern from an analysis approach
-	simulate a study with replications. The output focuses on 
a) estimands one: the properties of estimated treatment contrasts from methods C and D 
b) estimands two: measures about the treatment rankings


There are three types of scenarios:
1.	Without interaction terms, all subgroups have the same baseline risk. 
The presented scenarios are S1, S1.1, S2, S3.1, S3.2, and S4.

2.	Without interaction terms, subgroups have different baseline risks. 
The presented scenario is S1.2.

3.	With interaction terms, subgroups either have the same or different baseline risks. 
The presented scenarios are S5.1, S5.2, S6.1, and S6.2.


“PRACTical_one data replication” illustrates how the functions work for one replication of scenario 1, 
without averaging over the replications for the main output of interest

To run multiple replications, “1 PRaCTical_functions.R” need to be sourced prior to executing 
the lines in the scenario scripts. Depending on the number of replications, the computation can take hours. 
A high performance computing facility has been used to run the scenarios.


Once the outputs from different scenarios have been obtained, 
the following R scripts are used for plotting the figures in the paper:
•	PLOT figure 2 network.R
•	PLOT figures 1 and 3.R
•	PLOT figure 4.R
