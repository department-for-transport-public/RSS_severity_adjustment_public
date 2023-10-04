################################################################################
# STATS19 SEVERITY ADJUSTMENT: FIGURES FOR PUBLICATION
################################################################################

## This script produces adjusted figures usingprobabilities calculated  in the previous scrip, and saves it to the output folder

################################################################################

## Read in dataset from previous step (if required)

final_dataset <- readr::read_rds(paste0(folder_out,"Pr.RDS"))

################################################################################ 

## Create key figures 

# Create year variable that looks like the accident year variable in STATS19
final_dataset$Year <- paste(20,final_dataset$Year, sep = "")
final_dataset$Year <- as.numeric(final_dataset$Year) #make numeric

# Format in way needed for joining back to STATS19 casualty data
data <- final_dataset %>%
  dplyr::select(Year, accid, accref, Vehicle_Reference, Casualty_Reference, ProbSeriousC, C_Ind) %>%
  dplyr::rename("injury_based" = "C_Ind")


################################################################################  

## Saving results in the ouptut folder

# Save CSV file for use in other analysis
write.csv(data, paste0(folder_out,"adjustment_figures.csv"))
