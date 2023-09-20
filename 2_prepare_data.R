################################################################################
# STATS19 SERVERITY ADJUSTMENT: PEPRARING THE DATA
################################################################################

## This scripts cleans the DB data file into a format helpful for the later modelling and interpretation 
## The cleaned is then saved data is also saved in the output folder

################################################################################ 
## Data cleaning - police force / CRASH indicator

# Actions include:
# Amend the indicator for CRASH data 
# Set to 1 for Met Police (using COPA, rather than CRASH)
# Use difference in days as flag, where there is a difference after 01/11/2016 (when COPA introduced)
# Where PF is Met and diff in days is 0 or more (i.e. after 01/11/2016) make COPA true, and set CRASH indicator to 1
# Make all data for Kent from Jan 2016 onwards, as not all data correctly classed as CRASH in STATS19
# Make variables factors for the model, and set the reference force as Herts (as initially chosen by ONS)

DB <- DB %>%
  dplyr::mutate(diff_in_days = difftime( as.Date(date,"%d/%m/%Y"),
                                         as.Date("2016-11-01"), units = "days"),
                COPA = (PFlabel =='Metropolitan Police') & (diff_in_days >= 0),
                C_Ind = ifelse(COPA == TRUE, 1, C_Ind),
                C_Ind = ifelse(Police_Force == 46 & accyr >= 2016, 1, C_Ind),
                C_Ind = as.factor(C_Ind),
                PFlabel = relevel(as.factor(PFlabel), ref="Hertfordshire")) %>%
  
  # Police officer attendance
  dplyr::mutate(Did_Police_Officer_Attend_Scene_of_Accident = dplyr::case_when(
    is.na(Did_Police_Officer_Attend_Scene_of_Accident) ~ 0L,
    TRUE ~ Did_Police_Officer_Attend_Scene_of_Accident),
    Did_Police_Officer_Attend_Scene_of_Accident = as.factor(Did_Police_Officer_Attend_Scene_of_Accident),
    POattend = car::recode(Did_Police_Officer_Attend_Scene_of_Accident, "c(1)='Did Attend';
                                        c(2,-1,0,9,6)='No';c(3)='Self-Report'")) %>%
  dplyr::mutate(Date1 = as.Date(date, "%d/%m/%Y")) %>% 
  
  # Create online reporting flag
  # Forces moved to online reporting at dates below:
  # Essex - Apr 16, Met - Oct 16, City of London - Oct 16, Thames Valley - Jan 18, Hampshire - Jan 18, Derbyshire - Aug 18, Merseyside - Oct 18, Surrey - Dec 18
  dplyr::mutate(online = dplyr::case_when(
    #If a police officer is not 2 or 3, it's always 0
    !Did_Police_Officer_Attend_Scene_of_Accident %in% c(2,3) ~ 0,
    Police_Force == 42 & Date1 >= "2016-04-01" ~ 1,
    Police_Force == c(1, 48) & Date1 >= "2016-10-01" ~ 1,
    Police_Force %in% c(43, 44, 30) & Date1 >= "2018-01-01" ~ 1,
    Police_Force == 5 & Date1 >= "2018-10-01" ~ 1,
    Police_Force == 45 & Date1 >= "2018-12-01" ~ 1,
    Police_Force %in% c(7, 17, 21) & Date1 >= "2019-11-01"~ 1,
    Police_Force %in% c(47, 53) & Date1 >= "2019-12-01" ~ 1,
    Police_Force %in% c(6, 33, 34) & Date1 >= "2020-02-01" ~ 1,
    Police_Force == 22 & Date1 >= "2020-06-01" ~ 1,
    Police_Force == 23 & Date1 >= "2020-09-01" ~ 1,
    Police_Force %in% c(12, 54) & Date1 >= "2021-02-01" ~ 1,
    Police_Force %in% c(35, 40, 41) & Date1 >= "2021-09-01" ~ 1,
    Police_Force %in% c(60, 61, 62, 63) & Date1 >= "2020-11-01" ~ 1,
    TRUE ~ 0),
    online = as.factor(online)) %>%
  
  # Recode PO attend variable
  # Make numeric - did attend = 1, did not attend = 2, self report = 3
  dplyr::mutate(POattend = 
                  dplyr::case_when(
                    Did_Police_Officer_Attend_Scene_of_Accident %in% c("2", "-1", "0", "9", "6") ~ 2,
                    Did_Police_Officer_Attend_Scene_of_Accident == "1" ~ 1,
                    Did_Police_Officer_Attend_Scene_of_Accident == "3" ~ 3,
                    TRUE ~ as.numeric(Did_Police_Officer_Attend_Scene_of_Accident))) 


#######################################################################################
## Model preparation - casualty and driver age and sex

#This section to recode / re-reference variables for speed of running and ease of model interpretation

DB <- DB %>%
  dplyr::mutate(#Recode Casualty_Severity to Severe (TRUE or FALSE)
    Severe = ifelse(c8 == "2", TRUE, FALSE), 
    # Create age band of casualty variable and set 26-35 as reference (as it is the largest category)
    Age_Band_of_Casualty = dplyr::case_when(Age_of_Casualty >= 0 & Age_of_Casualty <= 5 ~ 1,
                                            Age_of_Casualty >= 6 & Age_of_Casualty <= 10 ~ 2,
                                            Age_of_Casualty >= 11 & Age_of_Casualty <= 15 ~ 3,
                                            Age_of_Casualty >= 16 & Age_of_Casualty <= 20 ~ 4,
                                            Age_of_Casualty >= 21 & Age_of_Casualty <= 25 ~ 5,
                                            Age_of_Casualty >= 26 & Age_of_Casualty <= 35 ~ 6,
                                            Age_of_Casualty >= 36 & Age_of_Casualty <= 45 ~ 7,
                                            Age_of_Casualty >= 46 & Age_of_Casualty <= 55 ~ 8,
                                            Age_of_Casualty >= 56 & Age_of_Casualty <= 65 ~ 9,
                                            Age_of_Casualty >= 66 & Age_of_Casualty <= 75 ~ 10,
                                            Age_of_Casualty >= 75 ~ 11,
                                            TRUE ~ -1),
    Age_Band_of_Casualty = relevel(as.factor(Age_Band_of_Casualty), ref="6"),
    # Center the variable to make the regression more stable (should not alter conclusions)
    CasAgeN2 = (Age_of_Casualty-25) ^ 2) %>%
  
  # Similarly, create age band of driver variable
  dplyr::mutate(Age_Band_of_Driver = dplyr::case_when(Age_of_Driver >= 0 & Age_of_Driver <= 5 ~ 1,
                                                      Age_of_Driver >= 6 & Age_of_Driver <= 10 ~ 2,
                                                      Age_of_Driver >= 11 & Age_of_Driver <= 15 ~ 3,
                                                      Age_of_Driver >= 16 & Age_of_Driver <= 20 ~ 4,
                                                      Age_of_Driver >= 21 & Age_of_Driver <= 25 ~ 5,
                                                      Age_of_Driver >= 26 & Age_of_Driver <= 35 ~ 6,
                                                      Age_of_Driver >= 36 & Age_of_Driver <= 45 ~ 7,
                                                      Age_of_Driver >= 46 & Age_of_Driver <= 55 ~ 8,
                                                      Age_of_Driver >= 56 & Age_of_Driver <= 65 ~ 9,
                                                      Age_of_Driver >= 66 & Age_of_Driver <= 75 ~ 10,
                                                      Age_of_Driver >= 75 ~ 11,
                                                      TRUE ~ -1),
                Age_Band_of_Driver = relevel(as.factor(Age_Band_of_Driver), ref="6"), 
                # Create driver age band relabelling
                DvrAgeBd = car::recode(Age_Band_of_Driver, 
                                       "c(1,2,3,6,7,8,9)='a others'; c(4,5)='b 16-25';
                                       c(10,11)='c 66+';-1='d missing'")) %>%
  
  # Make casualty and driver sex 1 'male' the reference; recode missing data (-1) as unknown (3)
  dplyr::mutate(Sex_of_Casualty = car::recode(as.factor(Sex_of_Casualty),
                                              "-1=3; 0=3; 9=3"),
                Sex_of_Casualty = relevel(Sex_of_Casualty, ref="1"),
                Sex_of_Driver = relevel(as.factor(Sex_of_Driver), ref="1"),
                DvrSex = car::recode(Sex_of_Driver,"-1=3; 0=3"))


#######################################################################################
## Model preparation - months and seasons
# These variables allow a time effect on P(serious) to vary smoothly from year to year and avoid using year as a factor 

DB <- DB %>%
  dplyr::mutate(# Extract year from date variable
    Year = format(Date1, format = "%y"),
    YearN = as.numeric(accyr)-2016, # define numeric variables - continuous variables in regression by starting from 0 to make interpretation easier
    YearN2 = YearN ^ 2,
    # Extract hour from time, recode missing to 17:00 as most common value
    TimeHrN = as.numeric(sub("...$", "", as.character(time))),  # matches last 3 characters of any string & replaces with empty so only get hour
    # Create time bands
    TimeZone = car::recode(TimeHrN, "c(00,01,02,22,23)='late';
  c(03,04,05,06)='early';c(07,08,09,10)='Commute am';
  c(16,17,18)='Commute pm'; c(11,12,13,14,15)='Middle of day' ;
  c(19,20,21)='evening'"),
  
  # Similar to months-cycle but now with time of day - cos time repeats over 24 hours
  # 8am seems safest time (as P(serious) seems at its lowest at this time)
  CosTim = cospi((TimeHrN+0.5-8)/12),
  Cos2Tim = cospi((TimeHrN+0.5-8)/6),
  SinTim = sinpi((TimeHrN+0.5-8)/12),
  Sin2Tim = sinpi((TimeHrN+0.5-8)/6)) 


#######################################################################################
## Model preparation - Casualty type

DB <- DB %>%
  # Make car occupant the reference category
  dplyr::mutate(C16SUMLAB = relevel(as.factor(C16SUMLAB), ref="Car Occupant")) 

# Accident variables

# Police officer attendance
DB <- DB %>%  dplyr::mutate(
  # Speed limit
  Speed_limit = dplyr::case_when(is.na(Speed_limit) ~ "20",
                                 TRUE ~ Speed_limit),
  Speed = car::recode(as.factor(Speed_limit), 
                      "c(-1,0,10,15,'NULL')='20'",
                      levels=c("30","20","40","50", "60", "70"))) %>%
  
  # Number of vehicles
  dplyr::mutate(NofVsGroup = dplyr::case_when(Number_of_Vehicles >= 3 ~ "xxx+",
                                              Number_of_Vehicles == 2 ~ "xx", 
                                              Number_of_Vehicles == 1 ~ "x", 
                                              TRUE ~ as.character(Number_of_Vehicles)),
                NofVsGroup = as.factor(NofVsGroup)) %>%
  
  # Number of casualties
  dplyr::mutate(NofCsGroup = dplyr::case_when(Number_of_Casualties >= 3 ~ "xxx+",
                                              Number_of_Casualties == 2 ~ "xx", 
                                              Number_of_Casualties == 1 ~ "x", 
                                              TRUE ~ as.character(Number_of_Casualties)),
                NofCsGroup = as.factor(NofCsGroup)) %>%
  # Junction detail
  dplyr::mutate(JdetailS = car::recode(Junction_Detail, 
                                       "c(-1,0) ='a No'; 3='b T Junc'; 5='d slip rd';
                                       1='e roundabout'; else='c other' "),
                # Road class
                X1RoadS = car::recode(X1st_Road_Class, 
                                      "c(1,2)='a Motorway'; c(3,4,5,6,-1)='b Other'"),
                # 2nd road class
                X2RoadS = car::recode(X2nd_Road_Class, 
                                      "c(-1,0,9)='a None'; c(1,2,3)='b Major';c(4,5,6)='c Minor'"),
                # Road type
                RdTypeS = car::recode(Road_Type, 
                                      "c(6,3)='a Normal carriageway'; c(1,2,7,12,9,-1)='b Other'"),
                # Weather condition
                WeatherS = car::recode(Weather_Conditions, 
                                       "c(1,4)='a Fine'; c(2,5)='b Rain';c(3,6)='c Snow';
                                       c(-1,0,8,9)='d missing/other'; 7='e fog' "),
                # Road surface condition
                RoadSurfS = car::recode(Road_Surface_Conditions, "1='a Dry'; 2='b Wet';3='c Snow';
                            c(-1,0,4,5,6,9,7)='d other / missing'"),
                # Special condition at site
                SpecialCS = car::recode(Special_Conditions_at_Site, 
                                        "0='a No'; else='b other'"),
                # Junction location
                JlocS = car::recode(Junction_Location, 
                                    "c(0,2,3)='a Leaving Junc'; 8='b Mid Junc';
                                    c(-1,1,4,5,6,7,9)='c Entering Junc / missing'"))


#######################################################################################
## Model preparation - Vehicle variables

DB <- DB %>%
  # Vehicle_Leaving_Carriageway - make cat 0 (didnt) the reference 
  dplyr::mutate(Vehicle_Leaving_Carriageway =  relevel(as.factor(Vehicle_Leaving_Carriageway), 
                                                       ref="0"),
                VLC = car::recode(Vehicle_Leaving_Carriageway, "c(0,-1)='didnt';c(3,6)='st or cross';else='other'"),
                # Vehicle_Manoeuvre - make cat 18 (going ahead) the reference
                Vehicle_Manoeuvre = relevel(as.factor(Vehicle_Manoeuvre), 
                                            ref="18"),
                VM = car::recode(Vehicle_Manoeuvre, "2='parked';c(3,4,8,10)='stationary slowing';
                     c(1,5,6,7,9,15)='slow'; c(-1,11,12,13,14,18,22,28,40,99,0)='ahead';c(16,17)='bend'"),
                
                # Skidding_and_Overturning - make cat 0 (didnt) the reference 
                Skidding_and_Overturning =  relevel(as.factor(Skidding_and_Overturning), 
                                                    ref="0"),
                Skid = car::recode(Skidding_and_Overturning, 
                                   "c(0,-1)='didnt';else='other'"),
                # Hit object off carriageway - make cat 0 (didnt) the reference
                Hit_Object_off_Carriageway =  relevel(as.factor(Hit_Object_off_Carriageway), 
                                                      ref="0"),
                HitOff = car::recode(Hit_Object_off_Carriageway, 
                                     "c(0,-1)='didnt';c(2,3,4)='tree etc';else='other'"),
                # Hit object in carriageway
                Hit_Object_in_Carriageway = relevel(as.factor(Hit_Object_in_Carriageway), 
                                                    ref="0"),
                HitIn = car::recode(Hit_Object_in_Carriageway, 
                                    "c(0,-1)='didnt';c(1,4,5,6)='substantial';
                                    else='other'"),
                # Point of impact
                Impact = car::recode(X1st_Point_of_Impact,
                                     "c(1,-1, 5, 7 ,9)='front';c(0)='none';c(3,4)='side';c(2)='rear'"),
                # Journey purpose
                JnyPurpS = car::recode(Journey_Purpose_of_Driver, 
                                       "c(1,2)='a work'; c(3,4)='b school';
                                       c(-1,5,6,15,0,8)='c other'"),
                ## Reference variables (vehicle and casualty)
                VRef = car::recode(Vehicle_Reference,
                                   "c(1)='a first'; c(2)='b second'; else='c other'"),
                CRef = car::recode(Casualty_Reference,
                                   "c(1)='a first'; c(2)='b second'; else='c other'"))


#######################################################################################
## Model preparation - Further casualty variables

DB <- DB %>%
  dplyr::mutate(
    # Pedestrian location
    PedLocS = car::recode(Pedestrian_Location, 
                          "c(1,2,3,4)='a at nr Zebra';
                          c(-1,0, 5,7,8,9,10)='b Carriageway DK';6='c Footpath'"),
    # Bus passenger
    BusS = car::recode(Bus_or_Coach_Passenger,
                       "c(-1,0,9)='a No or missing';c(1,2,3)='b not seated';
                       4='c seated'"))




################################################################################   

## Saving results
# Restrict to only slight / serious casualties (ie exclude fatalities)
# Then only include variables used in ONS dataset 

df <- DB %>% 
  dplyr::filter(c8 != "1") %>% 
  dplyr::select(Severe, C16SUMLAB, VM, VLC, Skid, Impact, Speed, HitOff, 
                HitIn, BusS, WeatherS, RdTypeS, X1RoadS, X2RoadS, SpecialCS, 
                RoadSurfS, PedLocS, JdetailS, DvrSex, DvrAgeBd, JnyPurpS, TimeZone, 
                CosTim, Cos2Tim, SinTim, Sin2Tim, YearN, YearN2, PFlabel, POattend,
                VRef, CRef, Age_Band_of_Casualty, CasAgeN2, Sex_of_Casualty, 
                NofVsGroup, NofCsGroup, C_Ind, Year, accid, online,
                accref, Vehicle_Reference, Casualty_Reference, Date1)

# Make factors to match ONS
df$C_Ind <- as.factor(df$C_Ind)
df$PFlabel <- as.factor(df$PFlabel)
df$Impact <- as.factor(df$Impact)
df$BusS <- as.factor(df$BusS)
df$WeatherS <- as.factor(df$WeatherS)
df$RdTypeS <- as.factor(df$RdTypeS)
df$X1RoadS <- as.factor(df$X1RoadS)
df$X2RoadS <- as.factor(df$X2RoadS)
df$SpecialCS <- as.factor(df$SpecialCS)
df$RoadSurfS <- as.factor(df$RoadSurfS)
df$PedLocS <- as.factor(df$PedLocS)
df$JdetailS <- as.factor(df$JdetailS)
df$JnyPurpS <- as.factor(df$JnyPurpS)
df$VRef <- as.factor(df$VRef)
df$CRef <- as.factor(df$CRef)
#df$P_Ind <- as.factor(df$P_Ind)
df$X1RoadS <- as.factor(df$X1RoadS)
df$VM <- as.factor(df$VM)

# Get rid of NAs - setting speed NA to 20
df$Speed[is.na(df$Speed)] <- 20


## Save the file (so don't need to repeat the above and rename)
readr::write_rds(df, file.path(folder_out,"df.RDS"))

# Clean environment
rm(DB,df)
gc() #This one clears up your RAM .rs.restartR() fully cleans your memory
