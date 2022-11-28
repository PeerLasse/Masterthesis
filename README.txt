The structure of this Project is as follows: 

The raw data are stored in "data/raw" 
NOTE: I do not own the rights to the mobility data and am not allowed to share it, thus the mobility data files are not uploaded here.

The source code is  stored in the folder "Source"
	The Complete construction of the data set is executed in the File "Dataset_creation"

	The subfolder "data_wrangling" contains the files responsible for the creation of the dataset.
		-"Distances" contains the relevant files used to form the distance matrix from OSM routing services.
		-"Homeoffice" contains the files used for adjusting the Homeoffice potential data from 401 counties to 400 counties
		-"Infektions" contains the files to make the raw data for infections compatible with the other data sets.
		-"Policy daten" contains the files to make the raw data for restriction policies compatible with the other data sets.
		-"Match OSMID" containes the files used to match the OSM relation IDs supplied by Teralytics to german counties using webscraping.
		-"functions" contains dedicated functions useful during the projects workflow.
	
	The subfolder "Analysis" contains the files responsible for estimation and data analytics
		- The files named "Estimation_2_'Policy/Policies name'" contains the Estimations for the respective policy
		- The suffix "_intra" indicates the estimation for the sample split.
	
		The subfolder Descriptives containes the files responsible for generation of plots and summary statistics
			-"Aggregated_plot" generates a plot of the aggregated time trend of Mobility in Germany
			-"Correlation_spatial_lags" plots the correlation between policies and their spatial lags
			-"Create_descriptives_all" generates summary stats for all variables
			-"General_statistics_density" plots empirical distributions for the dependent  variable
			-"Paralleltrends_assumption" plots the event study graphs.
			-"Mobiltiy_network" plots the mobility networks in Germany before and after policy  implementation.
	
	The subfolder "functions" contains a function to approximate intra county distances.

The "output" folder contains the output of Summary statistics and Regressions
	In "output/Regression Output" All relevant regression results are stored in .tex and .pdf files.
	The Estimations using the whole sample are the files with Names like this:"Estimation_2_'name of policy/policies'"
	
	The subfolder "Sample Split" contains the Regression results for the subsamples, the suffix "_long" indicates results for the sample using only trips with a length greater than 120 min,
	the suffix "_short" indicates results for the subsample using only trips with a lenght of less than 120 min.
	

