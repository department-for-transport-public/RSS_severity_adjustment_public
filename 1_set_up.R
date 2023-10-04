################################################################################
# STATS19 SEVERITY ADJUSTMENT: SETUP
################################################################################

## This initial script loads required libraries and specifies folder and file names where data files are located
## A number of pre-specified lookups are loaded in from web-based CSV files

################################################################################

## Libraries and setup

library(data.table) #import data
library(readr) #import data
library(dplyr) #data manipulation
library(car) #used in re-labelling variables (recode function) in data preparation script


################################################################################

## Reading in data from website (filter for 2004 onwards)

# These are links to the datasets on the data.gov.uk pages
# This reads in the full dataset to the latest published year; it may be necessary to filter out some years as desired.

# Accident data
AccDB <- data.table::fread("https://data.dft.gov.uk/road-accidents-safety-data/dft-road-casualty-statistics-collision-1979-latest-published-year.csv") %>% 
  dplyr::filter(accident_year >= 2004)

# Casualty data
CasDB <- data.table::fread("https://data.dft.gov.uk/road-accidents-safety-data/dft-road-casualty-statistics-casualty-1979-latest-published-year.csv") %>% 
  dplyr::filter(accident_year >= 2004)


# Vehicle data
VehDB <- data.table::fread("https://data.dft.gov.uk/road-accidents-safety-data/dft-road-casualty-statistics-vehicle-1979-latest-published-year.csv") %>% 
  dplyr::filter(accident_year >= 2004)


################################################################################

## Reading in road safety data CRASH data

# Load the crash indicator data
# This datafile provides details as whether the relevant force was on the CRASH system when/where the collision occurred
# Note that although the Met police also uses an injury-based system (COPA, rather than CRASH), this is coded later 
# The variable 'crash_indicator' is set to 1 for each record that was produced from an injury-based system. 
# The file 'RSS_crash_records' has records only where 'crash_indicator' = 1 i.e. those records produced via CRASH

# Specify a folder which will contain all the input data required 
# This code assumes that input data will be loaded from flat files rather than (e.g.) directly from databases
folder <- 
  file.path(
    "Data",
    fsep = "/"
  )

# Read in data set that has accident_index, vehicle_reference, casualty_reference and crash_indicator
crash_indicator <- read.csv(paste(folder, "/RSS_crash_records.csv", sep=""))

####

# As an alternative, it is possible to read in the data from the casualty adjustment file included as part of the published open data
# This file is the output from the DfT running of the model, but it includes a flag as to whether an injury based method was used 
# So this could be used as an alternative to reading in the file as above 
# Note that this is NOT used in the code below so that the filename would need to be amended in the join below 
# That is, change crash_indicator to crash_indicator_open
#
# crash_indicator_open <- data.table::fread("https://data.dft.gov.uk/road-accidents-safety-data/dft-road-casualty-statistics-casualty-adjustment-lookup_2004-latest-published-year.csv") %>% 
#   dplyr::mutate(vehicle_reference = Vehicle_Reference, casualty_reference = Casualty_Reference, 
#                 crash_indicator = injury_based) %>% 
#   dplyr::filter(injury_based == 1 & injury_based_severity_code > 0) %>% #this removes the Met police which is coded later, and non-CRASH records
#   dplyr::select(-Vehicle_Reference, -Casualty_Reference, -injury_based, -Adjusted_Serious, -Adjusted_Slight,
#                 -injury_based_severity_code)


################################################################################ 

## Combine datasets (to produce 'DB', the one master data file)

# Add the crash indicator to the casualty data, and rename to C_Ind
CasDB <- CasDB %>%
  left_join(crash_indicator, by = c("accident_index", "vehicle_reference","casualty_reference")) %>% 
  mutate(C_Ind = dplyr::case_when(crash_indicator == 1 ~ 1, TRUE ~ 0)) %>% 
  select(-crash_indicator) #drop as no longer required after recode

# Join accident and vehicle variables on to the casualty records
DB <- CasDB %>%
  dplyr::left_join(AccDB, by = c("accident_reference", "accident_year")) %>%
  dplyr::left_join(VehDB, by =  c("accident_year", "vehicle_reference", "accident_reference")) 


################################################################################ 

## Initial data cleaning
## This section relabels and recodes variables into a format helpful for the later modelling and interpretation 

# Make all NAs -1
DB[is.na(DB)] <- -1

# Rename variables to ease interpretation (this follows the approach adopted by ONS initially)
DB <- DB %>%
  dplyr::select("accid" = "accident_index",
                "accyr" = "accident_year",
                "accref" = "accident_year",
                date,
                time,
                C_Ind, 
                "c8" = "casualty_severity",
                'Age_of_Casualty' = 'age_of_casualty', 
                'Age_of_Driver' = 'age_of_driver', 
                'Number_of_Vehicles' = 'number_of_vehicles',
                'Number_of_Casualties' = 'number_of_casualties', 
                'Sex_of_Casualty' = 'sex_of_casualty', 
                'Sex_of_Driver' = 'sex_of_driver',
                'Casualty_Type' = 'casualty_type',
                'Pedestrian_Location' = 'pedestrian_location', 
                'Bus_or_Coach_Passenger' = 'bus_or_coach_passenger', 
                'Vehicle_Leaving_Carriageway' = 'vehicle_leaving_carriageway', 
                'Vehicle_Manoeuvre' = 'vehicle_manoeuvre', 
                'Skidding_and_Overturning' = 'skidding_and_overturning', 
                'Hit_Object_off_Carriageway' = 'hit_object_off_carriageway', 
                'Hit_Object_in_Carriageway' = 'hit_object_in_carriageway', 
                'X1st_Point_of_Impact' = 'first_point_of_impact', 
                'Journey_Purpose_of_Driver' = 'journey_purpose_of_driver',
                'Junction_Location' = 'junction_location', 
                'Police_Force' = 'police_force', 
                'Did_Police_Officer_Attend_Scene_of_Accident' = 'did_police_officer_attend_scene_of_accident',
                'Speed_limit' = 'speed_limit', 
                'Junction_Detail' = 'junction_detail', 
                'X1st_Road_Class' = 'first_road_class', 
                'X2nd_Road_Class' = 'second_road_class', 
                'Road_Type' = 'road_type', 
                'Weather_Conditions' = 'weather_conditions',
                'Road_Surface_Conditions' = 'road_surface_conditions', 
                'Special_Conditions_at_Site' = 'special_conditions_at_site',
                'Vehicle_Reference' = 'vehicle_reference', 
                'Casualty_Reference' = 'casualty_reference') %>% 
  mutate(`Police_Force` = case_when(`Police_Force` %in% c(91, 92, 93, 94, 95, 96, 97, 98) ~ 99, 
                                    TRUE ~ `Police_Force`)) %>% #recording for Scottish forces merging into Police Scotland
  mutate(`C16SUMLAB` = case_when(`Casualty_Type` == 0 ~ "Pedestrian",
                                 `Casualty_Type` == 1 ~ "Pedal Cyclist",
                                 `Casualty_Type` %in% c(2,3,4,5,23,97,103,104,105,106) ~ "Motor Cyclist",
                                 `Casualty_Type` %in% c(8,9,10,108,109,110) ~ "Car Occupant",
                                 `Casualty_Type` == 11 ~ "Bus Occupant", 
                                 `Casualty_Type` == 19 ~ "Van Occupant", 
                                 `Casualty_Type` %in% c(20,21,113) ~ "HGV Occupant",
                                 `Casualty_Type` %in% c(-1,16,17,18,22,90,98,99) ~ "Other Veh Occupant", #should include unknowns 
                                 TRUE ~ "Unknown"),
         `PFlabel` = case_when(`Police_Force` == 1 ~ "Metropolitan Police",
                               `Police_Force` == 3 ~ "Cumbria",
                               `Police_Force` == 4 ~ "Lancashire",
                               `Police_Force` == 5 ~ "Merseyside",
                               `Police_Force` == 6 ~ "Greater Manchester",
                               `Police_Force` == 7 ~ "Cheshire",
                               `Police_Force` == 10 ~ "Northumbria",
                               `Police_Force` == 11 ~ "Durham",
                               `Police_Force` == 12 ~ "North Yorkshire",
                               `Police_Force` == 13 ~ "West Yorkshire",
                               `Police_Force` == 14 ~ "South Yorkshire",
                               `Police_Force` == 16 ~ "Humberside",
                               `Police_Force` == 17 ~ "Cleveland",
                               `Police_Force` == 20 ~ "West Midlands",
                               `Police_Force` == 21 ~ "Staffordshire",
                               `Police_Force` == 22 ~ "West Mercia",
                               `Police_Force` == 23 ~ "Warwickshire",
                               `Police_Force` == 30 ~ "Derbyshire",
                               `Police_Force` == 31 ~ "Nottinghamshire",
                               `Police_Force` == 32 ~ "Lincolnshire",
                               `Police_Force` == 33 ~ "Leicestershire",
                               `Police_Force` == 34 ~ "Northamptonshire",
                               `Police_Force` == 35 ~ "Cambridgeshire",
                               `Police_Force` == 36 ~ "Norfolk",
                               `Police_Force` == 37 ~ "Suffolk",
                               `Police_Force` == 40 ~ "Bedfordshire",
                               `Police_Force` == 41 ~ "Hertfordshire",
                               `Police_Force` == 42 ~ "Essex",
                               `Police_Force` == 43 ~ "Thames Valley",
                               `Police_Force` == 44 ~ "Hampshire",
                               `Police_Force` == 45 ~ "Surrey",
                               `Police_Force` == 46 ~ "Kent",
                               `Police_Force` == 47 ~ "Sussex",
                               `Police_Force` == 48 ~ "City of London",
                               `Police_Force` == 50 ~ "Devon and Cornwall",
                               `Police_Force` == 52 ~ "Avon and Somerset",
                               `Police_Force` == 53 ~ "Gloucestershire",
                               `Police_Force` == 54 ~ "Wiltshire",
                               `Police_Force` == 55 ~ "Dorset",
                               `Police_Force` == 60 ~ "North Wales",
                               `Police_Force` == 61 ~ "Gwent",
                               `Police_Force` == 62 ~ "South Wales",
                               `Police_Force` == 63 ~ "Dyfed-Powys",
                               `Police_Force` == 99 ~ "Police Scotland",
                               TRUE ~ "Unknown")) 


################################################################################

## Specify output folder

# This is where the outputted data from the modelling will be saved 
# Due to the large file sizes, for a full model run, this should be changed to a local folder rather than one within GitHub

folder_out <- 
  file.path(
    "Public_SA_output",
    fsep = "/"
  )

################################################################################   

## Clean environment

rm(AccDB, VehDB, CasDB, crash_indicator)

