### Calculates forecast for Raven model and writes it to .rvt file for next model run
### written by SAG for 3004430 on 22-APR-2019
### contact: sgrass@nhweb.com

##Uses observed data to calc Lake inflows, will only work for historic period not for projections

library(tidyverse)
library(lubridate)
library(reshape2)
library(tidyhydat) #need to update for projections
library(RavenR)
library(data.table)

## Make time series
#cgvd offsets
kal.cgvd = 0.252 #value updated wsp survey
ok.cgvd = 0.215 #value at 08NM083
skaha.cgvd = 0.275 #value at 08NM084
vaseux.cgvd = 0.256 #value at 08NM243

##Read in Data
result.path <- ("./model/output/")


snow.gauge <- c("2F08P", "2F05P", "2F10P", "2F12", "2F19")

########################################################
##                                                    ##
## Part 1 - Process Data and Calculate Forecast Vol   ##
##                                                    ##
########################################################

#### Process SWE Data ####

#Table with HRUs for each gauge station
swe.met <- read.csv("./model_tables/snow_metadata_hrus.csv", header = TRUE, stringsAsFactors = FALSE) %>%
  select(Gauge = ID, HRU = HRUID) %>% 
  filter(Gauge %in% snow.gauge)

#read in snow model data, coerce to tibble, select only HRUs we care about
swe.model.xts <- custom.read(file.path(result.path, "SNOW_Daily_Average_ByHRUGroup.csv"))

swe.model <- as_tibble(swe.model.xts) %>%
  mutate(Date = as.Date(index(swe.model.xts))) %>%
  mutate_if(is.character, as.numeric) %>%
  select(Date, snow.gauge)

swe.all <- swe.model %>% 
  gather('Gauge', 'SWE', -Date) %>% 
  mutate(Year = year(Date), Month = month(Date), Day = day(Date)) %>%
  filter(Day == 1)

#################3
#option apply bias correction on swe (to observation data)
models <- read_csv('./model_tables/20200110_rfc_model_biascorrect.csv')

swe.all <- swe.all %>% left_join(models, by = c('Gauge', 'Month')) %>%
  mutate(SWE = ifelse(is.na(multiplier), SWE, SWE * multiplier)) %>%
  as.data.table
############################################################


model.years <- unique(swe.all$Year)

#### Process Precip Data ####
precip.model.xts <- custom.read(file.path(result.path, "PRECIP_Daily_Average_ByHRUGroup.csv"))
                                
precip.model.select <- as_tibble(precip.model.xts) %>%
  mutate(Date = as.Date(index(precip.model.xts))) %>%
  mutate_if(is.character, as.numeric) %>%
  select(Date, Precip = MSW0) %>% 
  mutate(Year = year(Date), Month = month(Date), Day = day(Date),
         WY = ifelse(Month >= 10, Year + 1, Year)) %>%
  group_by(WY) %>%
  mutate(Cumulative.pp = cumsum(Precip))

precip.model.select <- as.data.table(precip.model.select)

#annual summary

#inflows from model
#calculate Cumulative by water year
qnet <- read_csv(file.path(result.path, 'Hydrographs.csv')) %>%
  select(Date = date, KalamalkaLake = `WoodandKalamalkaLake (res. inflow) [m3/s]`, OkanaganLake = `OkanaganLake (res. inflow) [m3/s]`) %>%
  gather(Name, qnet, ends_with('Lake')) %>%
  mutate(Month = month(Date),
         Year = year(Date),
         WY = ifelse(Month >= 10, Year + 1, Year),
         qnet = qnet *86400) %>%
  group_by(Name, WY) %>%
  mutate(Cumulative.in.mod = cumsum(qnet)/1e6)

qnet <- as.data.table(qnet)


#### Calc Forecast for each month ####

#Bring in coeff from template - used Month - Jul coeff - confirmed by Sean Reimer
all.coeff <- bind_rows(tibble(Month = 2,
                        Lake = "OkanaganLake",
                        Gauge = c("Precip", "2F08P", "2F05P", "2F12", "2F10P", "Qnet"),
                        Coeff = c(1.30162, 0.69437, 0.39819, 0.18809, 0.29859, 2.40352),
                        Y_incpt = -154.16964),
                  tibble(Month = 2,
                         Lake = "KalamalkaLake",
                         Gauge = c("2F05P", "2F19", "Precip"),
                         Coeff = c(0.05229, 0.12234, 0.13436),
                         Y_incpt = -18.7671),
                  tibble(Month = 3,
                        Lake = "OkanaganLake",
                        Gauge = c("Precip", "2F08P", "2F05P", "2F12", "2F10P", "Qnet"),
                        Coeff = c(1.1236, 0.79319, 0.32842, 0, 0.26618, 1.95999),
                        Y_incpt = -222.17026),
                  tibble(Month = 3,
                         Lake = "KalamalkaLake",
                         Gauge = c("2F05P", "2F10P", "2F19"),
                         Coeff = c(0.04377, 0.03297, 0.08354),
                         Y_incpt = -18.89802),
                  tibble(Month = 4,
                        Lake = "OkanaganLake",
                        Gauge = c("Precip", "2F08P", "2F05P", "2F12", "2F10P", "Qnet"),
                        Coeff = c(1.11105, 0.70733, 0.26748, 0, 0.18596, 1.21223),
                        Y_incpt = -264.54632),
                  tibble(Month = 4,
                         Lake = "KalamalkaLake",
                         Gauge = c("2F05P", "2F10P", "Precip"),
                         Coeff = c(0.0208, 0.0171, 0.18307),
                         Y_incpt = -27.49682),
                  tibble(Month = 5,
                        Lake = "OkanaganLake",
                        Gauge = c("Precip", "2F08P", "2F05P", "2F12", "2F10P", "Qnet"),
                        Coeff = c(0.85276, 0.48948, 0.3215, 0, 0.26311, 0.28253),
                        Y_incpt = -277.85093),
                  tibble(Month = 5,
                         Lake = "KalamalkaLake",
                         Gauge = c("2F05P", "2F10P", "Precip"),
                         Coeff = c(0.03008, 0.02148, 0.06765),
                         Y_incpt = -24.80208))

#no targets for may, so ignore
#add in may targets just to view (jwt dec 2019)
all.coeff2 <- all.coeff %>% 
  #filter(Month !=5) %>% 
  as.data.table

OBWB.forecast <- function(month, lake, year){
  #print(paste(month, lake, year))
  coeff <- all.coeff2[Lake == lake & Month == month]
  y <- coeff$Y_incpt[1]
  
  swe <- swe.all[Month == month & Year == year & Day == 1]
  
  forecast.date <- ymd(paste(year, month, 1, sep = '-'))

  #get cumulative pp  
  precip.sum <- precip.model.select[Date == forecast.date - days(1), Cumulative.pp]
  
  #get cumulative inflows
  inflow.sum <- qnet[Name == lake & Date == forecast.date - days(1), Cumulative.in.mod]
  
  ######################
  #optional
  #bias correct inflow .sum
  correction <- models %>%
    mutate(Gauge = ifelse(Gauge == 'KalWoodLake', 'KalamalkaLake', Gauge)) %>%
    filter(Gauge == lake, Month == month) %>%
    pull(multiplier)
  ##########################
  
  inflow.sum.use <- inflow.sum*correction
  
  forecast <- merge(coeff, swe, by = "Gauge", all = TRUE)[Lake == lake, .(Gauge, Value = SWE, Coeff, Y_incpt)]
    
  forecast$Value <- with(forecast, case_when(Gauge == 'Precip' ~ precip.sum,
                                             Gauge == 'Qnet'~ inflow.sum.use,
                                             TRUE ~ Value))
  

  forecast$Product = with(forecast, Value * Coeff)
  
  forecast.vol <- sum(forecast$Product) + y
  
  df <- tibble(Year = year,
                   Month = month,
                   Lake = lake,
                   Forecast.Volume = forecast.vol)
  return(df)
}

Months <-  rep(unique(all.coeff2$Month), times = length(model.years), each = 2)
Lake <-  rep(c("OkanaganLake", "KalamalkaLake"), times = length(Months)/2, each = 1)
Year <-  rep(model.years, times = 1, each = 8) 

forecast.volumes <- pmap_df(tibble(month = Months, lake = Lake, year = Year), OBWB.forecast)

########################################################
##                                                    ##
## Part 2 - Determine Target Levels from Forecast     ##
##                                                    ##
########################################################

start_date <- swe.model$Date[1]
end_date <- swe.model$Date[nrow(swe.model)]

target.okanagan.df <- tibble(Date = seq.Date(from = start_date, to = end_date, by = 1)) %>% 
  mutate(Year = year(Date),
         Month = month(Date), 
         Day = day(Date), 
         Target.Elevation = case_when(
    Month == 1 ~ 341.74 + ok.cgvd,
    Month == 5 ~ 342.48 + ok.cgvd,
    Month == 6 ~ 342.44 + ok.cgvd,
    Month == 7 ~ 342.24 + ok.cgvd,
    Month == 8 ~ 342.04 + ok.cgvd,
#    Month == 9 & Day < 23 & Day > 7 ~ 341.94  + ok.cgvd, #just use end of sept goals
    Month == 9 ~ 341.89 + ok.cgvd,
    TRUE ~ 341.84 + ok.cgvd)) 


forecast.vol.okanagan <- forecast.volumes %>% filter(Lake == "OkanaganLake")

okanagan.targets <- left_join(target.okanagan.df, forecast.vol.okanagan, by = c("Month", "Year"), all = TRUE) %>%
  mutate(Lake = "OkanaganLake", Target.Elevation = case_when(
    Month ==2 & Forecast.Volume < 430 ~ 342.04 + ok.cgvd, #this is the interpretation of as high as possible (same as august)
    #Month ==2 & Forecast.Volume < 430 ~ 341.74 + ok.cgvd, #this is instead just the same as january
    Month == 2 & Forecast.Volume >= 430 ~ 341.54 + ok.cgvd,
    Month == 3 & Forecast.Volume < 620 ~ 342.04 + ok.cgvd, #this is the interpretation of as high as possible (same august)
    #Month == 3 & Forecast.Volume < 620 ~ 341.74 + ok.cgvd, #this is instead just the same as january
    Month == 3 & Forecast.Volume >= 620 ~ 341.49 + ok.cgvd,
    Month == 4 & Forecast.Volume < 250 ~ 342.48 + ok.cgvd, #this is the interpretation of as high as possible (same as may)
    Month == 4 & Forecast.Volume >= 250 & Forecast.Volume < 500 ~ 341.44 + ok.cgvd,
    Month == 4 & Forecast.Volume >= 500 ~ 341.34 + ok.cgvd,
    TRUE ~ Target.Elevation )) 

####################################
#option, delay forecast action til 8th of the month, still using info from the 1st thoguh
#closer simulation of reality
date.end.month <- c(start_date, seq.Date(from = start_date, to = end_date, by = 'months') - days(1), end_date)

date.end.month <- date.end.month[!(month(date.end.month) %in% c(2:3))]

okanagan.targets <- okanagan.targets %>% 
  mutate(Target.Elevation = ifelse((Day == 8 & Month %in% c(2:4)) | Date %in% date.end.month, Target.Elevation, NA)) #%>%
#  fill(Target.Elevation)
####################################
         
# #########################################
#optional, convert all okanagan data to gradual targets, centered at he month

#inflill targets with smoothed values
ok.xts <- xts(okanagan.targets$Target.Elevation, order.by = okanagan.targets$Date)
ok.xts <- na.approx(ok.xts, na.rm = F)

okanagan.targets <- tibble(Date = index(ok.xts),
                           Target.Elevation = ok.xts) %>%
  mutate(Month = month(Date),
         Target.Elevation = ifelse(is.na(Target.Elevation), -1.2345, Target.Elevation))
# ###########################################################


# #optional, convert all data from august 1 to dec 31 to na, use flow targets instead
# okanagan.targets <- okanagan.targets %>%
#   mutate(Target.Elevation = ifelse(Month >=8, -1.2345, Target.Elevation))

#default low inflow forecast
target.kalamalka.df <- tibble(Date = seq.Date(from = start_date, to = end_date, by = 1)) %>% 
  mutate(Year = year(Date),Month = month(Date), 
         Day = day(Date), 
         Target.Elevation = case_when(
    Month == 1  ~ 391.2 + kal.cgvd, 
    Month == 2  ~ 391.4 + kal.cgvd, #interpretation of keep it as high as possible
    Month == 3  ~ 391.4 + kal.cgvd, #interpretation of keep it as high as possible
    Month == 4  ~ 391.5 + kal.cgvd,             #april forecast decisiont
    Month == 5  ~ 391.6 + kal.cgvd,
    Month == 6  ~ 391.7 + kal.cgvd,
    Month == 7  ~ 391.6 + kal.cgvd,
    Month == 8  ~ 391.5 + kal.cgvd,
    Month == 9  ~ 391.4 + kal.cgvd,
    Month == 10 ~ 391.35 + kal.cgvd,
    Month == 11 ~ 391.3 + kal.cgvd,
    Month == 12 ~ 391.25 + kal.cgvd,
    TRUE ~ NA_real_))
    
 
forecast.vol.kalamalka <- forecast.volumes %>% filter(Lake == "KalamalkaLake")

kalamalka.targets <- left_join(target.kalamalka.df, forecast.vol.kalamalka, by = c("Month", "Year"), all = TRUE) %>%
  mutate(Lake = "KalamalkaLake", 
         Target.Elevation = case_when(Month == 2 & Forecast.Volume > 15 ~ 391.2 + kal.cgvd,
                                      Month == 3 & Forecast.Volume > 15 ~ 391.2 + kal.cgvd,
                                      Month == 4 & Forecast.Volume > 30 ~ 391.4 + kal.cgvd,
                                      TRUE ~ Target.Elevation))


####################################
#option, delay forecast action til 8th of the month, still using info from the 1st thoguh
#closer simulation of reality
####################################
#option, delay forecast action til 8th of the month, still using info from the 1st thoguh
#closer simulation of reality


kalamalka.targets <- kalamalka.targets %>% 
  mutate(Target.Elevation = ifelse((Day == 8 & Month %in% c(2:4)) | Date %in% date.end.month, Target.Elevation, NA)) #%>%
#  fill(Target.Elevation)
####################################

# #########################################
#optional, convert all okanagan data to gradual targets, centered at he month

#inflill targets with smoothed values
kal.xts <- xts(kalamalka.targets$Target.Elevation, order.by = kalamalka.targets$Date)
kal.xts <- na.approx(kal.xts, na.rm = F)

kalamalka.targets <- tibble(Date = index(kal.xts),
                           Target.Elevation = kal.xts) %>%
  mutate(Month = month(Date),
         Target.Elevation = ifelse(is.na(Target.Elevation), -1.2345, Target.Elevation))

####################################
#optional write max flow constraints based on flow forecast

####################################

okanagan.max.flow <- tibble(Date = seq.Date(from = start_date, to = end_date, by = 1)) %>% 
  mutate(Year = year(Date),Month = month(Date), 
         Day = day(Date), 
         Target.Max.Flow = case_when(
           Month == 1  ~ 28.3, 
           Month == 2  ~ 28.3,
           Month == 3  ~ 28.3,
           Month == 4  ~ 28.3,          
           Month == 5  ~ 78,
           Month == 6  ~ 78,
           Month == 7  ~ 78,
           #Month == 8  ~ 28.3, 
           Month == 8 ~ 78,  #breaking outflow rule to draw down farther
           #Month == 9  ~ 28.3,
           Month == 9 ~ 78,  #breaking outflow rule to draw down farther
           Month == 10 ~ 15.6,
           Month == 11 ~ 28.3,
           Month == 12 ~ 28.3,
           TRUE ~ NA_real_))

okanagan.max.flow.targets <- left_join(okanagan.max.flow, forecast.vol.okanagan, by = c("Month", "Year"), all = TRUE) %>%
  mutate(Lake = "OkanaganLake", 
         Target.Max.Flow = case_when(Month == 4 & Forecast.Volume > 620 ~ 78,
                                     Month == 9 & Day >14 ~ 15.6,
                                     TRUE ~ Target.Max.Flow))




#also need: if april forecast is over 620, min flow in april, may, june of 45 cms
#not a rule, but will be a more realistic representation
okanagan.min.flow <- tibble(Date = seq.Date(from = start_date, to = end_date, by = 1)) %>% 
  mutate(Year = year(Date),Month = month(Date), 
         Day = day(Date), 
         Target.Min.Flow = case_when(
           Month == 1  ~ 3.75, 
           Month == 2  ~ 3.75,
           Month == 3  ~ 3.75,
           Month == 4  ~ 3.75,          
           Month == 5  ~ 4.875,
           Month == 6  ~ 4.95,
           Month == 7  ~ 6.15,
           Month == 8  ~ 7.95, 
           Month == 9  ~ 6.9, 
           Month == 10 ~ 7.425,
           Month == 11 ~ 3.75,
           Month == 12 ~ 3.75,
           TRUE ~ NA_real_))

okanagan.min.flow.targets <- left_join(okanagan.min.flow, forecast.vol.okanagan, by = c("Month", "Year"), all = TRUE) %>%
  mutate(Lake = "OkanaganLake", 
         Target.Min.Flow = case_when(Month == 4 & Forecast.Volume > 620 ~ 45,
                                     Month == 5 & Forecast.Volume > 620 ~ 45, #this addition still fails in some years such as 1974
                                     TRUE ~ Target.Min.Flow))


########################################################
##                                                    ##
## Part 3 - Write rvt for model                       ##
##                                                    ##
########################################################

cat(file ="./model/Reservoirs/Okanagan_Reservoir_Targets.rvt", append=F, sep="",
    ":ReservoirTargetStage 41","\n",
   format(okanagan.targets$Date[1], format = "%Y-%m-%d"), " 00:00:00.0", ' 1 ', nrow(okanagan.targets),"\n")
   write.table(okanagan.targets$Target.Elevation,"./model/Reservoirs/Okanagan_Reservoir_Targets.rvt",append=T,col.names=F,row.names=F,sep=" ",quote=F)
cat(file="./model/Reservoirs/Okanagan_Reservoir_Targets.rvt", append=T, sep="",    ":EndReservoirTargetStage","\n")

cat(file ="./model/Reservoirs/Okanagan_Reservoir_MaxFlow.rvt", append=F, sep="",
    ":ReservoirMaxFlow 41","\n",
    format(okanagan.max.flow.targets$Date[1], format = "%Y-%m-%d"), " 00:00:00.0", ' 1 ', nrow(okanagan.max.flow.targets),"\n")
write.table(okanagan.max.flow.targets$Target.Max.Flow,"./model/Reservoirs/Okanagan_Reservoir_MaxFlow.rvt",append=T,col.names=F,row.names=F,sep=" ",quote=F)
cat(file="./model/Reservoirs/Okanagan_Reservoir_MaxFlow.rvt", append=T, sep="",    ":EndReservoirMaxFlow","\n")

cat(file ="./model/Reservoirs/Okanagan_Reservoir_MinFlow.rvt", append=F, sep="",
    ":ReservoirMinFlow 41","\n",
    format(okanagan.min.flow.targets$Date[1], format = "%Y-%m-%d"), " 00:00:00.0", ' 1 ', nrow(okanagan.min.flow.targets),"\n")
write.table(okanagan.min.flow.targets$Target.Min.Flow,"./model/Reservoirs/Okanagan_Reservoir_MinFlow.rvt",append=T,col.names=F,row.names=F,sep=" ",quote=F)
cat(file="./model/Reservoirs/Okanagan_Reservoir_MinFlow.rvt", append=T, sep="",    ":EndReservoirMinFlow","\n")


cat(file ="./model/Reservoirs/Kalamalka_Reservoir_Targets.rvt", append=F, sep="",
    ":ReservoirTargetStage 42","\n",
    format(kalamalka.targets$Date[1], format = "%Y-%m-%d"), " 00:00:00.0", ' 1 ', nrow(kalamalka.targets),"\n")
write.table(kalamalka.targets$Target.Elevation,"./model/Reservoirs/Kalamalka_Reservoir_Targets.rvt",append=T,col.names=F,row.names=F,sep=" ",quote=F)
cat(file="./model/Reservoirs/Kalamalka_Reservoir_Targets.rvt", append=T, sep="",    ":EndReservoirTargetStage","\n")


