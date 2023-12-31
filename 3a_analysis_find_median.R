################################################################################
# STATS19 SEVERITY ADJUSTMENT: ANALYSIS - FIND MEDIAN POLICE FORCE
################################################################################

## This code carries out the regression analysis to get the coefficients from which the median police force is determined
## The code read in data frame for model from the output folder (created by previous 2_prepare_data.R code)
## Note that alternatively, the median force can be specified at the next step and this code then is not required

################################################################################ 

## Read in the data from the output folder

df <- readr::read_rds(paste0(folder_out,"df.RDS"))

################################################################################

## Regression to find median police force

# These are the list of explanatory variables

f <- as.formula(paste('Severe ~ C16SUMLAB + VM + VLC + Skid + Impact + 
                      Speed + HitOff + HitIn + BusS + WeatherS + 
                      RdTypeS + X1RoadS + X2RoadS + SpecialCS + RoadSurfS +
                      PedLocS + JdetailS + DvrSex + DvrAgeBd + JnyPurpS +
                      TimeZone +  CosTim + Cos2Tim + SinTim + Sin2Tim +
                      YearN + YearN*C16SUMLAB + YearN*PFlabel + YearN2  +
                      POattend + VRef + CRef + 
                      Age_Band_of_Casualty + CasAgeN2 + Sex_of_Casualty +
                      NofVsGroup + NofCsGroup + 
                       PFlabel + online + C_Ind +  PFlabel:C_Ind '))

# The binomial logistic response model
Bmodel <- glm(data=df, f, binomial)

# Save outputs - the 'odds' file has the odds ratios which are needed to find the median force
sink("glm.txt")
print(summary(Bmodel))
sink()

sink("odds.txt")
print(exp(coef(Bmodel)))
sink()

# By exponentiating the coefficient values of the model it tells us the expected
# increase in the odds of being a serious casualty when CRASH is introduced
# for this output we are interested in the coefficient value for each police force
# we then choose the median police force coefficient value - whilst adding in Hertfordshire as 1 as the reference
# Also we remove some police forces that have unrealistic values (this can be due to their size)
# The median police force is used in the 3b script as the reference police force for those police forces that have not yet transferred to CRASH


################################################################################

## Save data (if required - all that is needed from this part of the process are the odds from the model)

# Clean environment if needed

rm(df,Bmodel)
gc()
.rs.restartR() #This one fully cleans your memory