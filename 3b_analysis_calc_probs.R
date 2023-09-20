################################################################################
# STATS19 SERVERITY ADJUSTMENT: ANALYSIS - FINAL REGRESSION AND CALCULATION OF PROBABILITIES
################################################################################

## This script to set the median police force (see otuput from 3a_analysis_find_median.R)
## Then run the final regression
## Finally use the regression outputs to calculate the probabilities needed for the severity adjustments

################################################################################   
## Get the read from the output folder

df<- readr::read_rds(paste0(folder_out,"df.RDS"))

# Vector of all the police forces which are usually named (i.e. those using injury based reporting)
# For 2022, seems like we have reliable CRASH data for Nottinghamshire so have added below

all_named_police <- c("Bedfordshire", "Cambridgeshire", "Cumbria", "Devon and Cornwall",
                      "Durham", "Essex", "Gloucestershire", "Greater Manchester", 
                      "City of London", "Humberside", "Hertfordshire", "Kent",
                      "Lancashire", "Metropolitan Police", "Norfolk",
                      "Northumbria", "Nottinghamshire", "Police Scotland",
                      "South Yorkshire", "Staffordshire", "Suffolk", "Sussex",
                      "Surrey", "Warwickshire", "West Mercia", "West Yorkshire",
                      "West Midlands")

# Find and remove the median police force from the vector
median_police <- "Warwickshire" #change this as necessary depending on results of first regression 
keep_pf <- all_named_police[-grep(median_police, all_named_police)]

# Create variable which groups non-CRASH forces with median force, and make them ("Other") the reference 
df[, PFCdiff2 := dplyr::case_when(PFlabel %in% keep_pf ~ as.character(PFlabel),
                                  TRUE ~ "Other")]
df[, PFCdiff2 := relevel(as.factor(PFCdiff2), ref="Other")]


################################################################################   
##  Regression 2 - produce adjustments

f <- as.formula(paste('Severe ~ C16SUMLAB + VM + VLC + Skid + Impact + 
                      Speed + HitOff + HitIn + BusS + WeatherS +
                      RdTypeS + X1RoadS + X2RoadS + SpecialCS + RoadSurfS +
                      PedLocS + JdetailS + DvrSex + DvrAgeBd + JnyPurpS +
                      TimeZone +  CosTim + Cos2Tim + SinTim + Sin2Tim +
                      YearN + YearN*C16SUMLAB + YearN*PFlabel + YearN2  +
                      POattend + VRef + CRef + 
                      Age_Band_of_Casualty + CasAgeN2 + Sex_of_Casualty +
                      NofVsGroup + NofCsGroup +
                      PFlabel + online + C_Ind +  PFCdiff2:C_Ind '))


Bmodel <- glm(data=df, f, binomial)

sink("glm.txt")
print(summary(Bmodel))
sink()

sink("odds2.txt")
print(exp(coef(Bmodel)))
sink()

################################################################################
## Save data [MT: some inefficiency here - as basically the same as previous file]

readr::write_rds(Bmodel,paste0(folder_out,"Bmodel.RDS")) #this is a big file, may not be essential to save it

# Free memory after running model
gc()