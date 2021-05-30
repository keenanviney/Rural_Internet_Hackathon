##-----------------------------
# Rural Internet EDA - May 12
##-----------------------------

library(tidyverse)
library(lubridate)
#library(maps)
library(sf)
library(cansim)
library(rgdal)
#library(sp)

## Load data

RMA_Data <- read_csv("C:\\Users\\Keenan Viney\\Desktop\\Project Folder\\Rural Internet Access\\RMADataForHackathon21-04-23.csv")

Employment_Insurance <- get_cansim("14-10-0323-01") %>% 
  mutate(Report_Date = ymd(paste0(REF_DATE,"-01"))) 

Census_Divisions <- readOGR("C:\\Users\\Keenan Viney\\Desktop\\Project Folder\\Rural Internet Access\\lcd_000b16a_e.shp")

AB_Oogle <- readOGR("C:\\Users\\Keenan Viney\\Desktop\\Project Folder\\Rural Internet Access\\AB_ookla_data_2020.shp")

## Create Spatial Point from the RMA data (sf approach)

RMA_Data_sf <- st_as_sf(RMA_Data, coords = c("GEO LATITUDE", "GEO LONGITUDE"))

st_crs(RMA_Data_sf) <- 4326

st_write(RMA_Data_sf, "C:\\Users\\Keenan Viney\\Desktop\\Project Folder\\Rural Internet Access\\RMA_Data_sf", 
         driver = "ESRI Shapefile", 
         delete_layer = TRUE)

## Select Only Alberta Extent

Alberta_Census_Divisions <- subset(Census_Divisions, PRNAME=="Alberta")

plot(Alberta_Census_Divisions)


## Employment Insurance visualization

Alberta_All_Time <- Employment_Insurance %>% 
  filter(GEO == "Alberta") 

# graph of the benefits for all Albertans
ggplot(data = filter(Alberta_All_Time, 
                     `Beneficiary detail` == "All types of income benefits",
                     `Hierarchy for Sex` == 1,
                     `Hierarchy for Age group` == 1), 
       mapping = aes(x = Report_Date, y = VALUE)) + 
  geom_line() +
  ylab("Total Recipients") +
  ggtitle("Alberta Employment Insurance") + 
  theme(plot.title = element_text(hjust = 0.5))

# graph of Alberta EI by type

ggplot(data = filter(Alberta_All_Time, 
                     `Beneficiary detail` %in% c("Regular benefits with declared earnings","Regular benefits without declared earnings"),
                     `Hierarchy for Sex` == 1,
                     `Hierarchy for Age group` == 1), 
       mapping = aes(x = Report_Date, y = VALUE, color = `Beneficiary detail`)) + 
  geom_line() +
  ylab("Total Recipients") +
  ggtitle("Alberta EI by Type") + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.2, 0.8))

# graph of Alberta EI by age cohort

ggplot(data = filter(Alberta_All_Time, 
                     `Beneficiary detail` == "All types of income benefits",
                     `Hierarchy for Sex` == 1,
                     `Age group` %in% c("15 to 24 years","25 to 54 years","55 to 64 years")), 
       mapping = aes(x = Report_Date, y = VALUE, color = `Age group`)) + 
  geom_line() +
  ylab("Total Recipients") +
  ggtitle("Alberta EI by Age Group") + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.2, 0.8))

# graph of Alberta EI by sex

ggplot(data = filter(Alberta_All_Time, 
                     Beneficiary detail` == "All types of income benefits",
                     `Hierarchy for Sex` %in% c("Males", "Females"),
                     `Hierarchy for Age group` == 1), 
       mapping = aes(x = Report_Date, y = VALUE, color = `Sex`)) + 
  geom_line() +
  ylab("Total Recipients") +
  ggtitle("Alberta EI by Type") + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.2, 0.8))

##------------------------------------------------------------------------------------------

## sp method

#coordinates(RMA_Data) <- c("GEO LATITUDE", "GEO LONGITUDE")

#proj4string(RMA_Data) <- CRS("+init=epsg:4326")

#is.projected(RMA_Data)



## Map the test points

# Canada <- map_data("World", region = "Canada") %>% select(lon = long, lat, group, id = subregion)
# 
# ggplot(Canada, aes(lon, lat, group = group)) +
#   geom_polygon(fill = "white", colour = "grey50") + 
#   coord_quickmap()
# 
# ggplot() + 
#   geom_sf(data = Canada) + 
#   geom_point(data = RMA_Data, mapping = aes(x = `GEO LONGITUDE`, y = `GEO LATITUDE`), colour = "red") + 
#   coord_sf()
# 
# ggplot(Canada) + 
#   geom_sf() + 
#   coord_sf()
# 
# Canada2 <- map_data("world", "canada")
