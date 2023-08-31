################################################################################
## FIGURES FOR PUBLICATION
################################################################################   

## Read in dataset from previous step

final_dataset <- readr::read_rds(paste0(folder_out,"Pr.RDS"))

# Create year variable that looks like accyr in SQL
final_dataset$Year <- paste(20,final_dataset$Year, sep = "")
final_dataset$Year <- as.numeric(final_dataset$Year) #make numeric

# Format in way needed for import into RAS_Statistics
data <- final_dataset %>%
  dplyr::select(Year, accid, accref, Vehicle_Reference, Casualty_Reference, ProbSeriousC, C_Ind) %>%
  dplyr::rename("injury_based" = "C_Ind")

## Save CSV file for loading to SQL [MT: MAKE SURE TO CHANGE THIS FILENAME AS NEEDED]

write.csv(data, paste0(folder_out,"adjustment_figures_2022.csv"))
