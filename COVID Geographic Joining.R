##--------------------------------
# Rural Internet Panel Data Model
##--------------------------------

## Load Packages ----------------------------------------------------------------------------------

library(tidyverse)
library(lubridate)

## Load Data --------------------------------------------------------------------------------------

# https://www.alberta.ca/data/stats/covid-19-alberta-statistics-data.csv
COVID_Cases_by_Local_Area <- read_csv("C:\\Users\\Keenan Viney\\Desktop\\Project Folder\\Rural Internet Access\\COVID\\covid-19-alberta-statistics-map-data.csv")

# Spatial Union between Alberta Local Health Regions: https://www.alberta.ca/data/stats/covid-19-alberta-statistics-map-data.csv
# and the Census Division digital bountry file: https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2016-eng.cfm
Geographic_Union <- read_csv("C:\\Users\\Keenan Viney\\Desktop\\Project Folder\\Rural Internet Access\\COVID\\Geographic Joining.csv")


## Check the differences between location names

COVID_Locations <- sort(unique(COVID_Cases_by_Local_Area$`Region name`))

COVID_Geo_Union <- sort(unique(Geographic_Union$LOCAL_NAME)) 

Diff <- setdiff(COVID_Locations,COVID_Geo_Union) %>% names("Counties") # Geographic union includes county aggregation, we don't need these

names(Diff) <- "Counties"

## Create a mapping dataframe between Census Division and Local Health Region

`%notin%` <- Negate(`%in%`)

COVID_by_Census_Division <- Geographic_Union %>% 
  select(CDUID, CDNAME, LOCAL_NAME, pct_Health, pct_Division, URBAN, Div_Area, Area_H) %>% 
  filter(LOCAL_NAME %notin% Diff) %>% 
  right_join(., COVID_Cases_by_Local_Area, by = c("LOCAL_NAME" = "Region name")) %>% 
  mutate(Cases_Allocated = `Active cases`*pct_Division,
         Population_Allocated = Population*pct_Division,
         Urban = case_when(URBAN %in% c("URBAN", "MODERATE URBAN INFLUENCE", "MODERATE METRO INFLUENCE", "METRO") ~ Population_Allocated,
                           TRUE ~ 0),
         Rural = case_when(URBAN %in% c("RURAL", "RURAL CENTRE AREA", "RURAL REMOTE") ~ Population_Allocated,
                           TRUE ~ 0)) %>% 
  group_by(CDUID, CDNAME, Date) %>% 
  summarize(Active_Cases = sum(Cases_Allocated),
            Population = sum(Population_Allocated),
            Urban_Population = sum(Urban),
            Rural_Population = sum(Rural)) %>% 
  mutate(Urban_Percentage = Urban_Population/(Urban_Population + Rural_Population),
         Report_Date = mdy(Date)) %>% 
  select(-Date)

## Write Out Covid statistics by Census Division

write_csv(COVID_by_Census_Division, "C:\\Users\\Keenan Viney\\Desktop\\Project Folder\\Rural Internet Access\\COVID\\COVID_by_Census_Division.csv")
