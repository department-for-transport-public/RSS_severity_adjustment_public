## Severity adjustments code 

This repo contains R code used to run the severity adjustments model developed by the Office for National Statistics (ONS) and used in producing the Department for Transport's reported road casualty statistics, allowing users of the statistics to reproduce the calculations themselves if they wish.  Full details of the severity adjustments and why they are required are available in the [severity adjustment guidance](https://www.gov.uk/government/publications/guide-to-severity-adjustments-for-reported-road-casualty-statistics).

The code reads in data from the department's [open datasets](https://www.data.gov.uk/dataset/cb7ae6f0-4be6-4935-9277-47e5ce24a11f/road-safety-data) together with an additional file which can be requested from the department which identifies those records where an injury-based reporting ssytem was used. 

### Overview of the regression approach

Full details of the reason for chosen approach are given in the guidance linked above, and in the [methodology paper](https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/820588/severity-reporting-methodology-final-report.odt) and [annex](https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/922708/annex-update-severity-adjustments-methodology.pdf) produced by ONS.  The model has evolved slightly compared with that described in these papers, but the fundamental approach remains unchanged.

There are two potential severity outcomes for a non-fatal casualty - either serious or slight. Therefore a binary outcome a binomial logistic regression was chosen to model the chance of a casualty being serious, depending on the values of a number of explanatory variables.

Many explanatory variables are available in the STATS19 road casualty data, which could be potentially included in the model.  The final variables were selected after testing, to balance the fit of the model with the aim to achieve a parsimonius model (and one which can be reproduced largely using data which is publicly available)

This code: 

- reads and cleans in data for use in the model (scripts 1 and 2)
- runs an initial model to find the 'median police force' (this is then used for estimates for non injury-baseed forces, so that they are in a sense taking the value of the 'average' force)  (script 3a)
- runs a logistic model to generate odds of a non-fatal casualty being serious, based on the values of a number of explanatory variables, and then calculates transition probabilities used to produce severity adjustments (script 3b)  
- produces the severity adjustment probabilities for use in adjusting road casualty statistics (script 4)

The transition probabilities, for slight to serious or serious to slight, are the ultimate output from this model. These transition probabilities are then used to uplift or downlift non-fatal casualties from police forces using non-injury based reporting systems (including historic data for those now using injury-based systems), to estimate what severity would have been reported if an injury-based system were used.  

This means that for each non-fatal casualty there is a column 'adjusted_serious' which gives the probability that the casualty would have been serious were an injury-based system used, and the the total number of serious casualties over a police force or across Great Britain can be estimated by summing the values of this column for a given year.  

In most cases, the overall impact of the adjustment is to increase the number of serious casualties, though this is not guaranteed, and the size of the adjustment can vary by police force as explained in the ONS papers.  For full details of the approach and methodology, and the assumptions involved, please refer to the technical reports.

### How to run the code 

The code reads in data from the department's open datasets.  These do not currently contain an indicator of whether a police force used an injury-based system to record each record, which is required to run the adjustment model.  This information is available from the department's central STATS19 dataset, and available on request.  Before running the code it would be necessary to obtain this file, save it to a specified location and ensure that the code is set up to read the file from the place where it is saved.

The R scripts should then be run in order (step by step rather than all in one go): 

- **1_set_up** reads in the data.  This script requires folders for data inputs and outputs to be specified.
- **2_prepare_data** tidies variables for use in the adjustment model 
- **3a_analysis_find_median** can be run to find the median police force (though the median force is just set in the next script, so this step is not strictly necessary if a particular force is selected)
- **3b_analysis_calc_probs** runs the logistic regression model annd uses the outputs to produce adjustment probabilities. Note that this script specifies injury-based reporting forces, as at 2022.  For running the model with earlier years data, this list may be different so may need to be checked 
- **4_figures** produces the output, which is a file containing the STATS19 record identifiers and the final adjusted figures

Note that within the code. 'CRASH' is often used as a shorthand for 'injury based reporting police forces' - which are all those using CRASH, and the Met police (using COPA, an alternative injury-based system).  Context should be clear from the code, but if not please contact DfT as below for guidance.

It should be emphasised that the resulting figures are **estimates**; and the estimates produced will depend on the approach and specific model chosen.  No model will produce 'true' numbers for adjusted severities, however the adjusted figures will, in aggregate, give a better trend in serious casualties.


### Replicating DfT published data

Outputs from the model (the adjusted probabilities) will change every time new data is added, particularly as new forces adopt injury based reporting systems, as the whole time series of data is used in the model.  In particular, this means that the whole back series of adjustment probabilities will change each year.    

While the fundamental approach and model used does not change, additional of new data and CRASH forces may mean that it is not possible to exactly replicate severity adjustments which have been previously published by DfT.  If this is the aim, then further guidance can be sought via the contact details below. 


## Summary and request for feedback 

This code has been published to allow users of road casualty data and statistics to understand the model used, and in interested, apply the method to their own data.  

The road safety statistics team welcomes any feedback on the clarity or usefulness of the code,  or the model approach itself, this can be provided via email to roadacc.stats@dft.gov.uk.  













