##--------------------------------
# Rural Internet Panel Data Model
##--------------------------------

## Load Packages ----------------------------------------------------------------------------------

library(tidyverse)
library(plm)
library(lubridate)
library(cansim)


## Load Data --------------------------------------------------------------------------------------

MLab <- read_csv("C:\\Users\\Keenan Viney\\Desktop\\Project Folder\\Rural Internet Access\\Joined_Attributes\\MLab_Joined_Attributes.csv")

Ookla <- read_csv("C:\\Users\\Keenan Viney\\Desktop\\Project Folder\\Rural Internet Access\\Joined_Attributes\\Ookla_Joined_Attributes.csv")

RMA <- read_csv("C:\\Users\\Keenan Viney\\Desktop\\Project Folder\\Rural Internet Access\\Joined_Attributes\\RMA_Joined_Attributes.csv") %>% 
  mutate(Test_Date = mdy(`TEST DATE / UTC HOUR`))

Employment_Insurance <- get_cansim("14-10-0323-01") %>% 
  mutate(Report_Date = ymd(paste0(REF_DATE,"-01")),
         GeoUID = as.numeric(GeoUID))

Population_Census <- get_cansim("17-10-0139-01") %>% 
  filter(`Hierarchy for Age group` == 1,
         `Hierarchy for Sex` == 1,
         GeoUID %in% c("4801", "4802", "4803", "4804", "4805", "4806", "4807", 
                       "4808", "4809", "4810", "4811", "4812", "4813", "4814", 
                       "4815", "4816", "4817", "4818", "4819")) %>% 
  mutate(Report_Date = ymd(paste0(REF_DATE,"-01-01")),
         GeoUID = as.numeric(GeoUID))

Working_Age_Population <- get_cansim("17-10-0139-01") %>% 
  filter(`Age group` == "15 to 64 years",
         `Hierarchy for Sex` == 1,
         GeoUID %in% c("4801", "4802", "4803", "4804", "4805", "4806", "4807", 
                       "4808", "4809", "4810", "4811", "4812", "4813", "4814", 
                       "4815", "4816", "4817", "4818", "4819")) %>% 
  mutate(Report_Date = ymd(paste0(REF_DATE,"-01-01")),
         GeoUID = as.numeric(GeoUID))

# https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/dt-td/Rp-eng.cfm?TABID=4&LANG=E&A=R&APATH=3&DETAIL=0&DIM=0&FL=A&FREE=0&GC=48&GL=-1&GID=1261445&GK=1&GRP=1&O=D&PID=111840&PRID=10&PTYPE=109445&S=0&SHOWALL=0&SUB=0&Temporal=2017&THEME=123&VID=0&VNAMEE=&VNAMEF=&D1=0&D2=0&D3=0&D4=0&D5=0&D6=0
Educational_Attainment <- read_csv("C:\\Users\\Keenan Viney\\Desktop\\Project Folder\\Rural Internet Access\\98-400-X2016261_ENG_CSV\\98-400-X2016261_English_CSV_data.csv") %>% 
  filter(GEO_LEVEL == 2,
         `DIM: Age (10)` == "Total - Age",
         `DIM: Work activity during the reference year (5)` == "Total - Work activity during the reference year",
         `DIM: Sex (3)` == "Total - Sex",
         `DIM: Employment income statistics (7)` == "Total - Employment income statistics",
         `GEO_CODE (POR)` %in% c("4801", "4802", "4803", "4804", "4805", "4806", "4807", 
                                 "4808", "4809", "4810", "4811", "4812", "4813", "4814", 
                                 "4815", "4816", "4817", "4818", "4819")) %>% 
  mutate(Bachelors_or_Above = `Dim: Highest certificate, diploma or degree (11): Member ID: [1]: Total - Highest certificate, diploma or degree (Note: 4)`/
           `Dim: Highest certificate, diploma or degree (11): Member ID: [11]: University certificate, diploma or degree above bachelor level (Note: 8)`,
         GeoUID = as.numeric(`GEO_CODE (POR)`),
         Report_Date = ymd(paste0(CENSUS_YEAR,"-01-01")))

# Covid Metrics
COVID_by_Census_Division <- read_csv("C:\\Users\\Keenan Viney\\Desktop\\Project Folder\\Rural Internet Access\\COVID\\COVID_by_Census_Division.csv")

# CERB 
EI_with_CERB <- read_csv("C:\\Users\\Keenan Viney\\Desktop\\Project Folder\\Rural Internet Access\\CERB\\Employment_Insurance_with_CERB.csv")

## Explore Internet Union Columns -----------------------------------------------------------------

max(MLab$TestTimeDownload, na.rm = TRUE)
min(MLab$TestTimeDownload, na.rm = TRUE)

max(RMA$Test_Date)
min(RMA$Test_Date)

sample(RMA$`TEST DATE / UTC HOUR`, 5)

MLab_UD <- MLab %>% group_by(CDUID) %>% 
  filter(TestTimeDownload <= ymd("2021-04-14")) %>% 
  summarise(MLab_Avg_Down = mean(TestThroughputMbpsDownload), 
            MLab_Avg_Upload = mean(TestThroughputMbpUpoad))
  
RMA_UD <- RMA %>% group_by(CDUID) %>% 
  filter(Test_Date >= ymd("2020-07-01")) %>% 
  summarise(RMA_Avg_Down = mean(`DOWNLOAD SPEED`), 
            RMA_Avg_Upload = mean(`UPLOAD SPEED`))

Comparison_between_samples <- left_join(MLab_UD, RMA_UD, by = c("CDUID"="CDUID")) 


## Has speed increased over time? -----------------------------------------------------------------

ggplot(data = RMA, 
       mapping = aes(x = Test_Date, y = `DOWNLOAD SPEED`)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("MB/s") +
  xlab("Test Date") +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("RMA Download Speed Across Time") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = RMA, 
       mapping = aes(x = Test_Date, y = `UPLOAD SPEED`)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("MB/s") +
  xlab("Test Date") +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("RMA Upload Speed Across Time") + 
  theme(plot.title = element_text(hjust = 0.5))


## Preparing Panel data (Uncontrolled Model)  -----------------------------------------------------

EI_Panel_Input <- Employment_Insurance %>% 
  filter(`Beneficiary detail` == "All types of income benefits",
         `Hierarchy for Sex` == 1,
         `Hierarchy for Age group` == 1,
          GeoUID %in% c("4801", "4802", "4803", "4804", "4805", "4806",
                        "4807", "4808", "4809", "4810", "4811", "4812",
                        "4813", "4814", "4815", "4816", "4817", "4818", "4819")) %>% 
  group_by(GeoUID, Report_Date) %>% 
  summarize(EI_Recipients_Count = VALUE)

RMA_Panel_Input <- RMA %>% 
  mutate(Report_Date = ymd(paste0(year(Test_Date), "-", month(Test_Date), "-01"))) %>% 
  group_by(CDUID, Report_Date) %>% 
  summarize(Download_Speed = mean(`DOWNLOAD SPEED`, na.rm = TRUE),
            Upload_Speed = mean(`UPLOAD SPEED`, na.rm = TRUE),
            Jitter = mean(JITTER, na.rm = TRUE),
            Latency = mean(LATENCY, na.rm = TRUE),
            Packet_Loss = mean(`PACKET LOSS`, na.rm = TRUE),
            Contracted_Down_Speed = mean(`CONTRACTED DOWNLOAD SPEED`, na.rm = TRUE),
            Contracted_Upload_Speed = mean(`CONTRACTED UPLOAD SPEED`, na.rm = TRUE),
            User_Satisfaction = mean(`USER COMMENTS`, na.rm = TRUE))


MLab_Panel_Input <- MLab %>%  
  mutate(Report_Year = year(TestTimeDownload), Report_Month = month(TestTimeDownload)) %>% 
  group_by(CDUID, Report_Year, Report_Month) %>% 
  summarize(Download_Speed = mean(TestThroughputMbpsDownload, na.rm = TRUE),
            Upload_Speed = mean(TestThroughputMbpUpoad, na.rm = TRUE),
            Packet_Loss = mean(LossRateDownload, na.rm = TRUE),
            Minimum_Round_Trip_Time = min(MinRoundTripTime, na.rm = TRUE)) %>% 
  drop_na(Report_Year, Report_Month) %>% 
  mutate(Report_Date = ymd(paste0(Report_Year, "-", Report_Month, "-01")))

Population_Panel_Input <- Population_Census %>% 
  select(Report_Date, GeoUID, Population = VALUE) %>% 
  mutate(Report_Year = year(Report_Date))

Education_Panel_Input <- Educational_Attainment %>% 
  select(Report_Date, GeoUID, Bachelors_or_Above) %>% 
  mutate(Report_Year = year(Report_Date))

Joined_Data_RMA <- left_join(EI_Panel_Input, RMA_Panel_Input, 
                             by = c("GeoUID" = "CDUID", "Report_Date" = "Report_Date")) %>% 
  left_join(., Population_Panel_Input, by = c("GeoUID" = "GeoUID", "Report_Date" = "Report_Date")) %>% 
  left_join(., Education_Panel_Input, by = c("GeoUID" = "GeoUID", "Report_Date" = "Report_Date")) %>% 
  filter(Report_Date >= min(RMA$Test_Date))

Joined_Data_MLab <- left_join(EI_Panel_Input, MLab_Panel_Input, 
                              by = c("GeoUID" = "CDUID", "Report_Date" = "Report_Date")) %>% 
  left_join(., Population_Panel_Input, by = c("GeoUID" = "GeoUID", "Report_Date" = "Report_Date")) %>% 
  left_join(., Education_Panel_Input, by = c("GeoUID" = "GeoUID", "Report_Date" = "Report_Date")) %>% 
  filter(Report_Date >= min(MLab$TestTimeDownload, na.rm = TRUE)) 

## Running the Uncontrolled Panel Regression ------------------------------------------------------

Panel_Data <- pdata.frame(Joined_Data_MLab, index=c("GeoUID","Report_Date"))

# Pooled OLS estimator
Pooling_OLS <- plm(EI_Recipients_Count ~ Download_Speed + Upload_Speed + Packet_Loss + Minimum_Round_Trip_Time, 
                   data = Panel_Data, model= "pooling")

summary(Pooling_OLS)

# Fixed effects or within estimator
Fixed_Effects_Model <- plm(EI_Recipients_Count ~ Download_Speed + Upload_Speed + Packet_Loss + Minimum_Round_Trip_Time, 
                           data = Panel_Data, model= "within")

summary(Fixed_Effects_Model)

# Random effects estimator
Random_Effects_Model <- plm(EI_Recipients_Count ~ Download_Speed + Upload_Speed + Packet_Loss + Minimum_Round_Trip_Time,
                            data = Panel_Data, model= "random")

summary(Random_Effects_Model)

# LM test for random effects versus OLS
plmtest(Pooling_OLS)

# LM test for fixed effects versus OLS
pFtest(Fixed_Effects_Model, Pooling_OLS)

# Hausman test for fixed versus random effects model
phtest(Random_Effects_Model, Fixed_Effects_Model)


## Full Model Input Preparation -------------------------------------------------------------------

Covid_Panel_Input <- COVID_by_Census_Division %>% 
  mutate(Report_Year = year(Report_Date), 
         Report_Month = month(Report_Date)) %>% 
  group_by(CDUID, CDNAME, Report_Year, Report_Month) %>% 
  summarize(Min_Report_Date = min(Report_Date, na.rm = TRUE),
            Rural_Percentage = mean(Rural_Percentage, na.rm = TRUE),
            Average_Monthly_Active_Cases = mean(Active_Cases)) %>% 
  mutate(Report_Date = ymd(Min_Report_Date))


Full_Model <- left_join(EI_with_CERB, MLab_Panel_Input, 
                        by = c("GeoUID" = "CDUID", "Report_Date" = "Report_Date")) %>% 
  left_join(., Population_Panel_Input, by = c("GeoUID" = "GeoUID", "Report_Year" = "Report_Year")) %>% 
  left_join(., Education_Panel_Input, by = c("GeoUID" = "GeoUID")) %>% 
  filter(Report_Date.x >= min(MLab$TestTimeDownload, na.rm = TRUE)) %>% 
  fill(Population, .direction = "down") %>% 
  left_join(., Covid_Panel_Input, by = c("GeoUID" = "CDUID", "Report_Date.x" = "Report_Date")) %>% 
  mutate(Rural_Internet_Interaction = (Download_Speed + Upload_Speed)*Rural_Percentage)

## Full Model Panel Regression --------------------------------------------------------------------

Panel_Data_Full_Model <- pdata.frame(Full_Model, index=c("GeoUID","Report_Date.x"))

# Pooled OLS estimator
Pooling_OLS_Full_Model <- plm(Value_EI_and_CERB ~ Download_Speed + Upload_Speed + Packet_Loss + Minimum_Round_Trip_Time + Bachelors_or_Above + Rural_Percentage + Average_Monthly_Active_Cases + Rural_Internet_Interaction, 
                              data = Panel_Data_Full_Model, model= "pooling")

summary(Pooling_OLS_Full_Model)

# Fixed effects or within estimator
Fixed_Effects_Model_Full_Model <- plm(Value_EI_and_CERB ~ Download_Speed + Upload_Speed + Packet_Loss + Minimum_Round_Trip_Time + Bachelors_or_Above + Rural_Percentage + Average_Monthly_Active_Cases + Rural_Internet_Interaction, 
                                      data = Panel_Data_Full_Model, model= "within")

summary(Fixed_Effects_Model_Full_Model)

# Random effects estimator
Random_Effects_Model_Full_Model <- plm(Value_EI_and_CERB ~ Download_Speed + Upload_Speed + Packet_Loss + Minimum_Round_Trip_Time + Bachelors_or_Above + Rural_Percentage + Average_Monthly_Active_Cases + Rural_Internet_Interaction,
                                       data = Panel_Data_Full_Model, model= "random")

summary(Random_Effects_Model_Full_Model)

# LM test for random effects versus OLS
plmtest(Pooling_OLS_Full_Model)

# LM test for fixed effects versus OLS
pFtest(Fixed_Effects_Model_Full_Model, Pooling_OLS_Full_Model)

# Hausman test for fixed versus random effects model
phtest(Random_Effects_Model_Full_Model, Fixed_Effects_Model_Full_Model)
