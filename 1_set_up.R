################################################################################
## Libraries and setup

library(DBI) #connect to SQL
library(odbc) #connect to SQL
library(dplyr) #data manipulation
library(car) #used in re-labelling variables (recode function)
library(readr) #import data
library(pROC) #for ROC curve (in analysis section)
library(data.table)

## reading in data from website

AccDB <- data.table::fread("https://data.dft.gov.uk/road-accidents-safety-data/dft-road-casualty-statistics-accident-1979-2021.csv")
CasDB <- data.table::fread("https://data.dft.gov.uk/road-accidents-safety-data/dft-road-casualty-statistics-casualty-1979-2021.csv")
VehDB <- data.table::fread("https://data.dft.gov.uk/road-accidents-safety-data/dft-road-casualty-statistics-vehicle-1979-2021.csv")

CasDB <- CasDB %>% 
  dplyr::filter(accident_year >= 2004)
AccDB <- AccDB %>% 
  dplyr::filter(accident_year >= 2004)
VehDB <- VehDB %>% 
  dplyr::filter(accident_year >= 2004)


### Need to contact road safety stats team for C-Ind variable dataset - this requires requesting the c8crash variable 

con <- DBI::dbConnect(odbc::odbc(), 
                      .connection_string = "driver={ODBC Driver 17 for SQL Server};
                             Trusted_Connection=yes;
                             server=GCP-TS01; 
                             database=RAS_Statistics")

c8crash <- dbGetQuery(con,
                      paste0("select
                        [accident_index],[accyr],[accref],[vehref],[casref],[c8crash]
                     FROM cas
                     WHERE accyr between 2004 and 2021")) %>%
  dplyr::mutate(C_Ind = ifelse(c8crash==-1 , 0, 1))

CasDB <- CasDB %>%
  left_join(c8crash, by = c("accident_index", "vehicle_reference" = "vehref", "casualty_reference" = "casref"))

################################################################################   

## Combine datasets (to produce 'DB', the one master data file)

DB <- CasDB %>%
  dplyr::left_join(AccDB, by = c("accident_reference", "accident_year")) %>%
  dplyr::left_join(VehDB, by =  c("accident_year", "vehicle_reference", "accident_reference")) 
#rm(AccDB, CasDB, VehDB)

################################################################################   

## Data cleaning - misc.

# Make all NAs -1 like ONS method
DB[is.na(DB)] <- -1


# Rename variables so they are the same as ONS list - also easier to interpret than STATS19 naming
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
                                    TRUE ~ `Police_Force`)) %>% 
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

## read in data set that has c8crash, accident_index, accident_year, vehicle_reference, casualty_reference

## SPECIFY FOLDER 

#Specify a folder which will contain all the input data required 
#This code assumes that input data will be loaded from flat files rather than (e.g.) directly from databases

folder_in <-  "~/g/AFP/RLTDAll/STS/007 ROAD SAFETY STATISTICS/002 PUBLICATION/0003 Reported Road Casualties Great Britain/RRCGB22/Severity_adjustment/Public_2021_run/"
folder_out <- "~/g/AFP/RLTDAll/STS/007 ROAD SAFETY STATISTICS/002 PUBLICATION/0003 Reported Road Casualties Great Britain/RRCGB22/Severity_adjustment/Public_2021_run/"
data_year <- 2021

################################################################################



## SPECIFY DATA FILENAMES

#These input files should all be located within the Data folder (unless otherwise specified)
#Within this project, sample/dummy files have been loaded to show the format of the data - these should allow the code to run but will not produce meaningful results

# cas_data <- paste0("Cas_2004_", data_year, ".csv")
# acc_data <- paste0("Acc_2004_", data_year, ".csv") 
# veh_data <- paste0("Veh_2004_", data_year, ".csv")