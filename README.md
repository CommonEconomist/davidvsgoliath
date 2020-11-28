Replication material: *"Fundamental patterns and predictions of event size distribution in modern wars and terrorist campaigns"*    
Submitted: 2017.05.19  
Accepted: 2018.09.13  
URL: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0204639    

Data and code to replicate results as reported in the paper. 
Data processing and analysis were carried out in R.
For issues replicating the results contact me via email: vanweezel (at) pm.me.

#### Description
`data` contains the original data used for the analysis. 
* `ged171.Rdata`: conflict event data from [UCDP](ucdp.uu.se/downloads/) (note that this is actually version 17.2)
* `globalterrorismdb_0617dist.RData`: data on terrorist attacks from [GTD](www.start.umd.edu/gtd/)

*NB:* RData file of the GTD terrorism data is included to save space as original excel file had size of ~80Mb. 

`code` includes the R-code to i) process the data, ii) run the analysis, and iii) create the figures.  
First step is to fit a power-law to the data, using the following code

1. `prepare-data.R`: cleans the conflict event data and should be run first;
2. `fit-power-law.R`: fits a power law to data and estimates the alpha parameters along with the xmin value;
3. `fig-results.R`: plots the estimated alpha parameters along with their p-value (figure 2 in paper).

Next step is to cross-validate the results, this is done by using the following code

1. `prepare-cv-data.R`: prepares the data for the cross-validation exercise;
2. `cross-validation.R`: runs the cross validation exercise (produces figure 4 and 5).

A number of extra tests are executed. 
First, a power law model is fitted to data on terrorist attacks, which is done using

1. `prepare-data-terrorism.R`: cleans the terrorist attack data;
2. `fit-power-law-terrorism.R`: fits a power law model to the data and plots the results (figure 3 in paper).

Second, for the conflict event data a log-normal model is fitted
1. `fit-logn.R`: fits the log-normal model;
2. `cross-validation-logn.R`: cross-validation exercise, used in combination with `cross-validation.R` to plot the results (figure 6).  

The example in figure 1 is produced by `fig-eg-power-law.R`.   

`output` contains summary of results produced by the scripts in the *`code`* folder. 
1. `fit-power-law.R`: creates `fitted-values.csv`;
2. `fit-power-law-terrorism.R`: creates `fitted-values_terrorism.csv`.

#### BibTeX
```
@article{spagat2018,
  title={Fundamental patterns and predictions of event size distributions in modern wars and terrorist campaigns},
  author={Spagat, Michael and Johnson, Neil F and van Weezel, Stijn},
  journal={PloS one},
  volume={13},
  number={10},
  pages={e0204639},
  year={2018},
  publisher={Public Library of Science}
}
```