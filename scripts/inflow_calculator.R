#calculate the inflows for okanagan lake and kalamalka lake
#no evaporation loss is included

library(tidyhydat)
library(tidyverse)
library(lubridate)
library(RavenR)
library(RcppRoll)
library(dygraphs)
library(hydroGOF)

locs <- tibble(Name = c('KalWoodLake', 'OkanaganLake'),
               Level = c('08NM143', '08NM083'),
               Out = c('08NM065', '08NM050'),
               Area = c(34.85, 349.93))
               
#get the data
stage <- hy_daily_levels(locs$Level) %>% 
  select(ID = STATION_NUMBER, Date, Stage = Value)
of <- hy_daily_flows(locs$Out) %>% 
  select(ID = STATION_NUMBER, Date, Flow = Value)

#join data together
dat <- stage %>% 
  left_join(locs, by = c('ID' = 'Level')) %>%
  full_join(of, by = c('Date', 'Out' = 'ID')) %>%
  filter(is.na(Name) == F) %>%
  group_by(Name) %>%
  mutate(Qlag = lag(Flow, 1),
         HGlag = lag(Stage, 1))

#calculated evaporation from scherzer and taylor
#temp data
temps.xts <- custom.read('./model/output/TEMP_AVE_Daily_Average_ByHRUGroup.csv')
temps <- as_tibble(temps.xts) %>%
  mutate(Date = as.Date(index(temps.xts))) %>%
  select(Date, KalWood_temp = WoodandKalamalka, Okanagan_temp = Okanagan)

#direct precip data
pp.xts <- custom.read('./model/output/PRECIP_Daily_Average_ByHRUGroup.csv')
pp <- as_tibble(pp.xts) %>%
  mutate(Date = as.Date(index(pp.xts))) %>%
  select(Date, KalWood_pp = WoodandKalamalka, Okanagan_pp = Okanagan)



#should we also include direct precip?
losses <- temps %>% 
  full_join(pp, by = 'Date') %>%
  mutate(OkanaganEvap = 0.0052*Okanagan_temp^2 - 0.0551 * Okanagan_temp + 1.0256,
         KalWoodEvap = 0.0027*KalWood_temp^2 - 0.0086 * KalWood_temp + 0.4075,
         OkanaganEvap = OkanaganEvap/1000*locs$Area[2]*1000^2/3600/24,
         KalWoodEvap = KalWoodEvap/1000*locs$Area[1]*1000^2/3600/24,
         Okanagan_pp = Okanagan_pp/1000*locs$Area[2]*1000^2/3600/24,
         KalWood_pp = KalWood_pp/1000*locs$Area[1]*1000^2/3600/24) 
  
evap.flow <- losses %>%
  select(Date, OkanaganLake = OkanaganEvap, KalWoodLake = KalWoodEvap) %>%
  gather(Name, Evap_cms, -Date)

pp.flow <- losses %>% 
  select(Date, OkanaganLake = Okanagan_pp, KalWoodLake = KalWood_pp) %>%
  gather(Name, pp_cms, -Date)

q_wsc <- dat %>%  
  #as_tsibble(key = id(Name), index = Date) %>%
  group_by(Name) %>%
  mutate(q_in = (Stage - HGlag)*Area + Flow * 0.0864, # million m3/d
         q_in = q_in*1e6/86400) %>%  # convert to cms 
  filter(q_in < 1000) #datum shift in kalamalka data


#include evaporation
qnet <- q_wsc %>%
  select(Date, Name, q_in) %>%
  left_join(evap.flow, by = c('Name', 'Date')) %>%
  left_join(pp.flow, by = c('Name', 'Date')) %>%
  mutate(q_in_all = q_in + Evap_cms - pp_cms)

#

  
#need to run temps through end of 2017  
qnet %>% ggplot(aes(x = Date, y = q_in_all)) + 
  geom_line() +
  facet_wrap(~Name, scales = 'free_y', ncol = 1) +
  theme_bw()
#ggsave('./scratch/20190918_Ok_Kal_calc_inflows.png')


#read in simulations
sim <- read_csv('./model/output/ReservoirMassBalance.csv') %>%
  select(Date = date, OkanaganLake = `OkanaganLake inflow [m3]`, KalWoodLake = `WoodandKalamalkaLake inflow [m3]`) %>%
  gather(Name, Value, -Date) %>%
  mutate(sim = Value/3600/24) %>%
  select(Date, Name, sim)

obs <- qnet %>%
  mutate(obs = q_in + Evap_cms - pp_cms) %>%
  select(Date, Name, obs) %>%
  group_by(Name) %>%
  mutate(Filt = roll_mean(obs, n = 5, align = 'center', fill = NA))
write_csv(obs, './scratch/20200115_Ok_Kal_calc_inflows.csv')

# #rfc values, very similar answers
# obs.rfc <- read_csv("/RFC/20190919 full length inflows/2019-09-19allDates_inflows.csv") %>%
#     select(Date, obs.rfc = Inflow_m3s) %>%
#     mutate(Name = 'OkanaganLake')
# 
# #compare rfc and ours
# obs.comp <- obs %>% filter(Name == 'OkanaganLake') %>%
#   inner_join(obs.rfc, by = c("Date", 'Name'))
# 
# obs.comp %>% ggplot(aes(x = obs, y = obs.rfc)) + 
#   geom_point(size = 0.1)  +
#   geom_abline(slope = 1) +
#   labs(x = 'NHC (with evap and ppt)', y = 'RFC') +
#   theme_bw() +
#   coord_equal()
# ggsave('./scratch/20190923_inflow_compare.png')


#write the obs into an RVH (for okanagan and kal, only calibrate to kal though)

dat <- obs %>% filter(Name == 'OkanaganLake') %>% ungroup

dat.xts <- xts(select(dat, obs, Filt), order.by = dat$Date)

dygraph(dat.xts) %>% dyRangeSelector()

dat.reg <- data.frame(Date = seq(dat$Date[1], dat$Date[nrow(dat)], by = 'day')) %>%
  left_join(dat, by = 'Date') %>%
  mutate(Filt = ifelse(is.na(Filt) | Filt < 0, -1.2345, Filt))

#Okanagan
rvt <- './model/obs/Okanagan_Inflows.rvt'

cat(file=rvt, append=F, sep="",
    
    ":ObservationData RESERVOIR_INFLOW ", 41, " m3/s #5 day moving average filter","\n",
    format(dat.reg$Date[1], format = '%Y-%m-%d 00:00:00.0'), ' 1 ', nrow(dat.reg),"\n")

write.table(select(dat.reg, Filt),rvt,append=T,col.names=F,row.names=F,sep=" ",quote=F)

cat(file=rvt, append=T, sep="",    ":EndObservationData","\n")

#######################################################
#kalwood
dat.kal <- obs %>% filter(Name == 'KalWoodLake') %>% ungroup

dat.reg.kal <- data.frame(Date = seq(dat.kal$Date[1], dat.kal$Date[nrow(dat.kal)], by = 'day')) %>%
  left_join(dat.kal, by = 'Date') %>%
  mutate(Filt = ifelse(is.na(Filt) | Filt < 0, -1.2345, Filt))


rvt <- './model/obs/KalWood_Inflows.rvt'

cat(file=rvt, append=F, sep="",
    
    ":ObservationData RESERVOIR_INFLOW ", 42, " m3/s #5 day moving average filter","\n",
    format(dat.reg.kal$Date[1], format = '%Y-%m-%d 00:00:00.0'), ' 1 ', nrow(dat.reg.kal),"\n")

write.table(select(dat.reg.kal, Filt),rvt,append=T,col.names=F,row.names=F,sep=" ",quote=F)

cat(file=rvt, append=T, sep="",    ":EndObservationData","\n")



