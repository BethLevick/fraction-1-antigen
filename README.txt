## README file for F1 paper data and analysis files
## @BethLevick b.a.levick@liverpool.ac.uk

## Details
Data and R code files for analysis of atypical strains of Y. pestis in a population
of great gerbils in Kazakhstan, to accompany the paper: 
	"The ecology of plague bacteria lacking the fraction 1 (F1) antigen in a wild rodent population."
	

## Data files
strains_14-06-04.csv
Main data file detailing strain types of Y. pestis isolates recovered from gerbils,
1995,1999-2013, pre-Balkhash desert, Kazakhstan

occupancy-data.csv
Average gerbil numbers over the same geographical area, 1995-2013.

traces-full.csv
Full data set of trace element data recovered from R.opimus males.

traces-yearly-deviance.csv
Trace element data converted to deviance from yearly mean of that respective element.

## Analysis files
f1analysis-plots.r
Main R code to perform the analysis and generate the plots as detailed in the F1 manuscript

file-setup.r
R code to reorganise some sections of the data to perform the analysis in f1analysis-plots.r

generate-gifs.r
R code to generate the map images compiled to generate animated .gif files of the outbreak