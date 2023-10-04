################################################################################
# STATS19 SEVERITY ADJUSTMENT: ANALYSIS - FINAL REGRESSION AND CALCULATION OF PROBABILITIES
################################################################################

## This script to set the median police force (see output from 3a_analysis_find_median.R, or can jsut choose a desired force)
## Then run the final regression
## Finally use the regression outputs to calculate the probabilities needed for the severity adjustments

################################################################################  

## Read in the data from the output folder

df<- readr::read_rds(paste0(folder_out,"df.RDS"))

################################################################################  

# Vector of all the police forces which are usually named (i.e. those using injury based reporting)
# For 2022, we have reliable CRASH data for Nottinghamshire so this could be added below
# CRASH forces will depend on the data years included (refer to the severity adjustment guidance as required)

all_named_police <- c("Bedfordshire", "Cambridgeshire", "Cumbria", "Devon and Cornwall",
                      "Durham", "Essex", "Gloucestershire", "Greater Manchester", 
                      "City of London", "Humberside", "Hertfordshire", "Kent",
                      "Lancashire", "Metropolitan Police", "Norfolk",
                      "Northumbria", "Police Scotland",
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

# This is essentially the same regression model as used previously to find the median force
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

## Predicted values (Pr(serious) & logits)  - for data as is, as if CRASH

# This part of the script uses the outputs of the regression model to produce severity probabilities 
# For further explanation of the process, refer to the technical papers produced by ONS.

# This PrReal datframe uses the confidence intervals to give us uncertainty values (upper, lower and middle) 
# The confidence interval predicts the values in an uncertain range around the mean predictions - also gives standard error of the prediction
PrReal <- predict(Bmodel, newdata=df, interval="confidence", se.fit=TRUE)

# removes residual scale 
PrReal$residual.scale <- NULL

PrReal$p <- 1/(1+exp(-PrReal$fit)) # probability serious 
PrReal$LL <- 1/(1+exp(-(PrReal$fit-(2*PrReal$se.fit)))) # potential lower probability of serious (accounting for uncertainty)
PrReal$UL <- 1/(1+exp(-(PrReal$fit+(2*PrReal$se.fit)))) # potential upper probability of serious (accounting for uncertainty)

# Probabilities adjusted to CRASH

df$C_IndTrue <- df$C_Ind # moving true indicators of crash to new variable 
df$C_Ind <- as.numeric(df$C_Ind) # making numeric
df$C_Ind <- 1 # assign all of them as CRASH forces
df$C_Ind <- as.factor(df$C_Ind) #to avoid error that C_Ind is not a factor

# run predicted value again as all CRASH
PrC <- predict(Bmodel, newdata=df, interval="confidence", se.fit=TRUE) 
PrC$residual.scale <- NULL

PrC$p <- 1/(1+exp(-PrC$fit)) # probability serious 
PrC$LL <- 1/(1+exp(-(PrC$fit-(2*PrC$se.fit)))) # potential lower probability of serious (accounting for uncertainty)
PrC$UL <- 1/(1+exp(-(PrC$fit+(2*PrC$se.fit)))) # potential upper probability of serious (accounting for uncertainty)

# Probs adjusted to Non-CRASH

df$C_Ind <- as.numeric(df$C_Ind) # making numeric again
df$C_Ind <- 0 # assigning all forces as Non-CRASH
df$C_Ind <- as.factor(df$C_Ind)   #DfT added this otherwise error that C_Ind is not a factor
# run predicted value again as all CRASH
PrNC <- predict(Bmodel, newdata=df, interval="confidence", se.fit=TRUE)
PrNC$residual.scale <- NULL

PrNC$p <- 1/(1+exp(-PrNC$fit)) # probability serious 
PrNC$LL <- 1/(1+exp(-(PrNC$fit-(2*PrNC$se.fit)))) # potential lower probability of serious (accounting for uncertainty)
PrNC$UL <- 1/(1+exp(-(PrNC$fit+(2*PrNC$se.fit)))) # potential upper probability of serious (accounting for uncertainty)

# Revert C_Ind to indicating casualty CRASH status (TRUE status)
df$C_Ind <- df$C_IndTrue 
df$C_IndTrue <- NULL

# Pr dataframe is used for calculation of outputs, so save at this stage in case need to reload

Pr <- data.frame(df, PrReal$fit ,PrReal$se.fit ,PrReal$p ,PrReal$LL, PrReal$UL, 
                 PrC$fit ,PrC$se.fit ,PrC$p ,PrC$LL ,PrC$UL,
                 PrNC$fit ,PrNC$se.fit ,PrNC$p ,PrNC$LL ,PrNC$UL  ) 

readr::write_rds(Pr, paste0(folder_out,"Pr.RDS"))
gc()

###############################################################################

## Probabilities

# Percentage change probability: (CRASH prob serious - real prob serious)/ real prob serious
Pr$ChPct <- 100*(Pr$PrC.p - Pr$PrReal.p)/Pr$PrReal.p

# Group variable of prob serious if CRASH PrC.p probabilities to 0.1 points 
Pr$PrCGrp <- NULL
Pr$PrCGrp <- ifelse(Pr$PrC.p < 0.1, 1, ifelse(Pr$PrC.p < 0.2, 2, 
                                              ifelse(Pr$PrC.p < 0.3, 3, ifelse(Pr$PrC.p < 0.4, 4, 
                                                                               ifelse(Pr$PrC.p < 0.5, 5, ifelse(Pr$PrC.p < 0.6, 6,
                                                                                                                ifelse(Pr$PrC.p < 0.7, 7, ifelse(Pr$PrC.p < 0.8, 8, 
                                                                                                                                                 ifelse(Pr$PrC.p < 0.9, 9, 10)))))))))
# Group variable of prob serious if REAL PrREAL.p probabilities to 0.1 points
Pr$PrRealGrp <- NULL
Pr$PrRealGrp <- ifelse(Pr$PrReal.p < 0.1, 1, ifelse(Pr$PrReal.p < 0.2, 2, 
                                                    ifelse(Pr$PrReal.p < 0.3, 3, ifelse(Pr$PrReal.p < 0.4, 4, 
                                                                                        ifelse(Pr$PrReal.p < 0.5, 5, ifelse(Pr$PrReal.p < 0.6, 6,
                                                                                                                            ifelse(Pr$PrReal.p < 0.7, 7, ifelse(Pr$PrReal.p < 0.8, 8, 
                                                                                                                                                                ifelse(Pr$PrReal.p < 0.9, 9, 10)))))))))

################################################################################

##   Generate N datasets as if all casualty severities assessed similarly
##  (ie adjusting NC data to be 'as if' CRASH, or adjusting CRASH to be 'as if'NC)
##  (NB in remainder of this explanation I assume adjusting from NC to C.  Adjustment from 
##    C to NC is similar )
##  We need some basis for re-assigning casualties from slight to serious or vice versa.

##  If we choose to ignore the known Severity-status (ie in NC data as this is what we are adjusting)
##  then the adjustment would simply use the model calculated probabilities of being
##  serious under CRASH, for all the NC data.  NB the probabilies vary from casualty to casualty
##  depending on the model & casualty characteristics like sex, age, vehicle-type, speed-limit etc.

##  However, if we wish to maximise the information from the known Severity-status, by minimising
##  the (expected) number of casualties that have status re-assigned in the adjustment process, then
##  examination of 2x2 table makes it clear that this implies, for each casualty:

##  a. Determine if DeltaP = p'-p is positive (where p' is P(serious\C)
##     and p is P(serious\NC)) on an individual casualty basis,

##  b. If DeltaP is positive, use (p'-p)/(1-p) as basis for determining whether slight\NC casualty is
##     re-assigned to serious\C.  Leave serious\NC as serious\C.

##  c. If DeltaP is negative, use (p-p')/p as basis for determining whether serious\NC casualty is
##     re-assigned to slight\C.  Leave slight\NC as slight\C

################################################################################ 

## Converting to CRASH (ie adjust Non-CRASH data only) 

# Sum DeltaPc for NonCrash (NB DeltaPc=0 for CRASH)
Pr$DeltaPc <- (Pr$PrC.p - Pr$PrReal.p) # CRASH proab serious - real prob serious - will = 0 if already CRASH as they will be the same
Pr$SevereN <- as.numeric(Pr$Severe) # numeric serious variable

## Calc "adjusted (p'-p) " 
# NB pos & neg values of adjusted (p'-p) to be applied to slight & serious casualties respectively only (so set=0 elsewhere)

Pr$AdjDPc <- 0
# if DeltaPc positive e.g. more likely to be serious under CRASH than real assignment & not a serious casualty
i <- (Pr$DeltaPc > 0 & Pr$SevereN == 0 )
# then DeltaPc/1-the probabilty under non-CRASH - generally means if already serious stays the same but if slight cas it becomes a serious casualty
Pr$AdjDPc[i] <- Pr$DeltaPc[i]/(1-Pr$PrNC.p[i])

# if DeltaPc negative e.g. more likely to be serious under real assignment than under CRASH & a serious casualty
j <- (Pr$DeltaPc < 0 & Pr$SevereN == 1) 
# then Delat PC/not Crash probability - generally means if slight it stays as slight but if serious it is reassinged to slight casualty 
Pr$AdjDPc[j] <- Pr$DeltaPc[j]/Pr$PrNC.p[j]


# minor correction to get total of adjustments to sum(DeltaPc)
Pr$AdjDPc2 <- Pr$AdjDPc * (sum(Pr$DeltaPc)/sum(Pr$AdjDPc))

##  Generate adjusted to CRASH dataset(s)  

#  NB Pr$AdjDPc2 is zero in CRASH data, and in pos DeltaP serious\NC & neg DeltaP slight\NC

#getting number of rows (number of casualties)
rows <- length(Pr$DeltaPc)
#generating number of columns
cols <- 10

# create matrix with cols random vectors (uniform [0,1])
set.seed(12345) # random generation
Pr$Rdm <- NULL
Pr$Rdm <- replicate(cols, runif(rows,0,1))  # replicate 10 times, runif generates random number for each row between 0 and 1 

Pr$AdjSevC <- NULL
Pr$AdjSevC <- replicate(cols, replicate(rows,0)) # replicate 10 times, runif assigning 0 for each row

#if random number less than abs(Pr$AdjDPc2) then switch Severity status
for (i in 1:cols) {
  Pr$AdjSevC[,i] <- Pr$SevereN    # Observed Severity status 
  Pr$AdjSevC[Pr$Rdm[,i] < abs(Pr$AdjDPc2),i] <- (1-Pr$SevereN[Pr$Rdm[,i] < abs(Pr$AdjDPc2)]) # if the matric value is less than the value of adjusted value for CRASH then switch severity status
}


## Produce data yearly adjusted & non-adjusted serious & slight plus total CRASH (inc COPA), & Non-CRASH casualties 

# Create new variable, containing the prob of case being serious in one of the random datasets
# ie SevereN + Pr$AdjDPc2.  Then aggregate to give adjusted serious.
# Unadjusted just from aggregating SevereN (& slight from differences)
# CRASH numbers from aggregating C_Ind (NC from differences)

Pr$ProbSeriousC <- Pr$SevereN + Pr$AdjDPc2


################################################################################

## Save data 

## Save output files
readr::write_rds(Pr, paste0(folder_out,"Pr.RDS"))


# Clean environment 
rm(Bmodel, df, Pr, PrC, PrNC, PrReal) #these are dataframes 
rm(cols, f, i, j, rows) #these are values

# Free memory after running model
gc()
