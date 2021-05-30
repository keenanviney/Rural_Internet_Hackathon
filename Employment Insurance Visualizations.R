##------------------------------------
# Employment Insurance Visualizations
##------------------------------------

library(tidyverse)
library(lubridate)
library(cansim)
library(scales)

## Load data

Employment_Insurance <- get_cansim("14-10-0323-01") %>% 
  mutate(Report_Date = ymd(paste0(REF_DATE,"-01"))) 

## All Time Graphs

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
  xlab("Report Date") +
  scale_y_continuous(labels = scales::comma) +
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
  xlab("Report Date") +
  scale_y_continuous(labels = scales::comma) +
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
  xlab("Report Date") +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Alberta EI by Age Group") + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.2, 0.8))

# graph of Alberta EI by sex

ggplot(data = filter(Alberta_All_Time, 
                     `Beneficiary detail` == "All types of income benefits",
                     Sex %in% c("Males", "Females"),
                     `Hierarchy for Age group` == 1), 
       mapping = aes(x = Report_Date, y = VALUE, color = Sex)) + 
  geom_line() +
  ylab("Total Recipients") +
  xlab("Report Date") +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Alberta EI by Type") + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.2, 0.8))

## Only 2020 forward----------------------------------------------------------------

Alberta_2020_Forward <- Employment_Insurance %>% 
  filter(GEO == "Alberta", Report_Date >= "2020-01-01") 

# graph of the benefits for all Albertans
ggplot(data = filter(Alberta_2020_Forward, 
                     `Beneficiary detail` == "All types of income benefits",
                     `Hierarchy for Sex` == 1,
                     `Hierarchy for Age group` == 1), 
       mapping = aes(x = Report_Date, y = VALUE)) + 
  geom_line() +
  ylab("Total Recipients") +
  xlab("Report Date") +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Alberta Employment Insurance") + 
  theme(plot.title = element_text(hjust = 0.5))

# graph of Alberta EI by type

ggplot(data = filter(Alberta_2020_Forward, 
                     `Beneficiary detail` %in% c("Regular benefits with declared earnings","Regular benefits without declared earnings"),
                     `Hierarchy for Sex` == 1,
                     `Hierarchy for Age group` == 1), 
       mapping = aes(x = Report_Date, y = VALUE, color = `Beneficiary detail`)) + 
  geom_line() +
  ylab("Total Recipients") +
  xlab("Report Date") +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Alberta EI by Type") + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.2, 0.8))

# graph of Alberta EI by age cohort

ggplot(data = filter(Alberta_2020_Forward, 
                     `Beneficiary detail` == "All types of income benefits",
                     `Hierarchy for Sex` == 1,
                     `Age group` %in% c("15 to 24 years","25 to 54 years","55 to 64 years")), 
       mapping = aes(x = Report_Date, y = VALUE, color = `Age group`)) + 
  geom_line() +
  ylab("Total Recipients") +
  xlab("Report Date") +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Alberta EI by Age Group") + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.2, 0.8))

# graph of Alberta EI by sex

ggplot(data = filter(Alberta_2020_Forward, 
                     `Beneficiary detail` == "All types of income benefits",
                     Sex %in% c("Males", "Females"),
                     `Hierarchy for Age group` == 1), 
       mapping = aes(x = Report_Date, y = VALUE, color = Sex)) + 
  geom_line() +
  ylab("Total Recipients") +
  xlab("Report Date") +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Alberta EI by Sex") + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.2, 0.8))

# graph of Alberta EI by region

ggplot(data = filter(Employment_Insurance, 
                     `Beneficiary detail` == "All types of income benefits",
                     `Hierarchy for Sex` == 1,
                     `Hierarchy for Age group` == 1,
                     Report_Date >= "2020-01-01",
                     between(GeoUID, 4801, 4819),
                     GeoUID != 4806,
                     GeoUID != 4811), 
       mapping = aes(x = Report_Date, y = VALUE, color = GeoUID)) + 
  geom_line() +
  ylab("Total Recipients") +
  xlab("Report Date") +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Alberta EI by Region, excluding Calgary & Edmonton") + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
