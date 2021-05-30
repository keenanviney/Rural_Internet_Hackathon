##---------------------------------------
# CERB Estimation by Census Division
##---------------------------------------

## Load Packages ----------------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(cansim)

## Load Data --------------------------------------------------------------------------------------

# CERB 
# http://www.edsc-esdc.gc.ca/ouvert-open/cerb/PCU_total_des_candidats_uniques_PT_groupe_age-CERB_total_unique_applicants_PT_Age_group.csv
CERB <- read_csv("~\\CERB\\CERB_total_unique_applicants_PT_Age_group.csv") %>% 
  select(End_of_week_date = `pcu_date_des_donnees_inclus-cerb_week_ending_date`,
         Province_abbreviation = `code_de_la_subdivision_canadienne-canadian_subdivision_code`,
         Age_Group = `code_de_groupe_dage-age_group_code`,
         Unique_Applicant_Count = `compte_unique_du_demandeur-unique_applicant_count`)

# Employment Insurance
Employment_Insurance <- get_cansim("14-10-0323-01") %>% 
  mutate(GeoUID = as.numeric(GeoUID))

## Visualization ----------------------------------------------------------------------------------

ggplot(data = filter(CERB, 
                     Province_abbreviation == "AB"), 
       mapping = aes(x = End_of_week_date, y = Unique_Applicant_Count, group = Age_Group)) + 
  geom_line() +
  ylab("Unique CERB Applicants") +
  xlab("End of week date") +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Alberta CERB Applicants") + 
  theme(plot.title = element_text(hjust = 0.5))

## Change in Employment Insurance when CERB ended -------------------------------------------------


CERB_Alberta_By_Age <- CERB %>% filter(Province_abbreviation == "AB") %>% 
  mutate(EI_Age_Group = case_when(Age_Group == 24 ~ "15 to 24 years",
                                  Age_Group == 25 ~ "25 to 54 years",
                                  Age_Group == 35 ~ "25 to 54 years",
                                  Age_Group == 45 ~ "25 to 54 years",
                                  Age_Group == 55 ~ "55 years and over",
                                  Age_Group == 65 ~ "55 years and over"),
         REF_DATE = case_when(month(End_of_week_date) < 10 ~ paste0(year(End_of_week_date),"-0", month(End_of_week_date)),
                              month(End_of_week_date) > 9 ~ paste0(year(End_of_week_date),"-", month(End_of_week_date)))) %>% 
  group_by(REF_DATE, EI_Age_Group) %>% 
  summarise(Unique_CERB_Applicants = mean(Unique_Applicant_Count))
  
EI_Discontinuity <- Employment_Insurance %>% 
  filter(`Age group` %in% c("15 to 24 years","25 to 54 years","55 years and over"),
         REF_DATE %in% c("2020-09","2020-10"),
         `Beneficiary detail` == "All types of income benefits",
         `Hierarchy for Sex` == 1,
         between(GeoUID, 4801, 4819)) %>% 
  spread(key = REF_DATE, value = VALUE) %>% 
  select(GEO, GeoUID, `Age group`, `2020-09`, `2020-10`) %>% 
  mutate(Level_Increase = `2020-10`-`2020-09`)

Alberta_Total_Difference_by_Age <- EI_Discontinuity %>% 
  group_by(`Age group`) %>% 
  summarise(Total_Difference = sum(Level_Increase))

EI_CERB_Percentages <- left_join(EI_Discontinuity, Alberta_Total_Difference_by_Age, by = c("Age group" = "Age group")) %>% 
  mutate(Percentage_Division_Age = Level_Increase / Total_Difference) %>% 
  select(GEO, GeoUID, `Age group`, Percentage_Division_Age) 

Assertion <- EI_CERB_Percentages %>% group_by(`Age group`) %>% 
  summarise(Adds_to_One = sum(Percentage_Division_Age))

## Adding unique CERB applicants to EI benificiaries ----------------------------------------------

Employment_Insurance_with_CERB <- Employment_Insurance %>% 
  mutate(Report_Date = ymd(paste0(REF_DATE,"-01"))) %>%
  filter(`Age group` %in% c("15 to 24 years","25 to 54 years","55 years and over"),
         `Beneficiary detail` == "All types of income benefits",
         `Hierarchy for Sex` == 1,
         GeoUID %in% c("4801", "4802", "4803", "4804", "4805", "4806", "4807", 
                       "4808", "4809", "4810", "4811", "4812", "4813", "4814", 
                      "4815", "4816", "4817", "4818", "4819")) %>%
  left_join(., EI_CERB_Percentages, by = c("GeoUID" = "GeoUID", "Age group" = "Age group")) %>% 
  left_join(., CERB_Alberta_By_Age, by = c("REF_DATE"="REF_DATE" , "Age group" = "EI_Age_Group")) %>%
  mutate(Region_Age_CERB = Percentage_Division_Age.y * Unique_CERB_Applicants) %>%  
  group_by(GeoUID, Report_Date) %>% 
  summarise(EI_Recipients_Count = sum(VALUE),
            CERB_Count = sum(Region_Age_CERB, na.rm = TRUE)) %>% 
  mutate(Value_EI_and_CERB = (EI_Recipients_Count + CERB_Count))


## Write out new EI Benefits with CERB dataset ----------------------------------------------------

write.csv(Employment_Insurance_with_CERB, "~\\CERB\\Employment_Insurance_with_CERB.csv")
