###Osyooos Rule Curves
##Written by SAG for 3004430 on 23 APR 2019
## Contact sgrass@nhcweb.com


library(tidyverse)
library(lubridate)
library(xts)
library(data.table)

## Make time series
start.date <- as.Date("1944-10-01", format = "%Y-%m-%d")
end.date <- as.Date("2018-11-01", format = "%Y-%m-%d")

dates <-  seq.Date(from = start.date, to = end.date, by = 1)

osoyoos.max.stage <- data.frame(Date = dates,
                                Month = month(dates),
                                Day = day(dates)) %>% mutate(Max_WSE = case_when(
                                  Month <= 2 ~ 277.8,
                                  Month ==3 & Day == 1 ~ 277.8,
                                  Month == 5 & Day == 1 ~ 277.98,
                                  Month == 9 & Day == 15 ~ 277.98,
                                  Month >= 11 ~ 277.8,
                                  TRUE ~ 0)) 

osoyoos.min.stage <- data.frame(Date = dates,
                                Month = month(dates),
                                Day = day(dates)) %>% mutate(Min_WSE = case_when(
                                  Month < 3 ~ 277.1,
                                  Month == 3 ~ 277.1,
                                  Month ==4 & Day == 1 ~ 277.4,
                                  Month == 6 & Day == 1 ~ 277.7,
                                  Month == 9 & Day == 15 ~ 277.7,
                                  Month == 11 & Day ==1 ~ 277.4,
                                  Month == 11 & Day >1 ~ 277.1,
                                  Month == 12 ~ 277.1,
                                  TRUE ~ 0))

osoyoos.max.stage.interp <- osoyoos.max.stage %>% mutate(Max = ifelse(Max_WSE == 0, NA, Max_WSE))%>% select(Date, Max) %>%
  filter(Date > "1949-10-31")

osoyoos.min.stage.interp <- osoyoos.min.stage %>% mutate(Min = ifelse(Min_WSE == 0, NA, Min_WSE))%>% select(Date, Min) %>%
  filter(Date > "1949-10-31")

max.stage.xts <- as.xts(osoyoos.max.stage.interp$Max, order.by = osoyoos.max.stage.interp$Date)
max.stage.filled <- na.approx(max.stage.xts, na.rm = FALSE)
osoyoos.max.stage <- osoyoos.max.stage %>% filter(Date > "1949-10-31") %>% mutate(Max = max.stage.filled[,1])

min.stage.xts <- as.xts(osoyoos.min.stage.interp$Min, order.by = osoyoos.min.stage.interp$Date)
min.stage.filled <- na.approx(min.stage.xts, na.rm = FALSE)
osoyoos.min.stage <- osoyoos.min.stage %>% filter(Date > "1949-10-31") %>% mutate(Min = min.stage.filled[,1])


# ##remove leap years
# 
# osoyoos.max.stage <- osoyoos.max.stage %>% mutate(Keep = ifelse(Month ==2 & Day == 29, "Loose", "Keep")) %>%
#   filter(Keep == "Keep") %>% select(-one_of("Keep"))
# 
# osoyoos.min.stage <- osoyoos.min.stage %>% mutate(Keep = ifelse(Month ==2 & Day == 29, "Loose", "Keep")) %>%
#   filter(Keep == "Keep") %>% select(-one_of("Keep"))

##write time series
#Max Stage
cat(file = "../R_output/Osoyoos_MaxStage.rvt", append=F, sep="",
    ":ReservoirMaxStage 39","\n",
    format(start.date, format = "%Y-%m-%d"), " 00:00:00.0", ' 1 ', nrow(osoyoos.max.stage),"\n")
write.table(osoyoos.max.stage$Max,"../R_output/Osoyoos_MaxStage.rvt",append=T,col.names=F,row.names=F,sep=" ",quote=F)

cat(file="../R_output/Osoyoos_MaxStage.rvt", append=T, sep="", ":EndReservoirMaxStage","\n")

#Min Stage
cat(file = "../R_output/Osoyoos_MinStage.rvt", append=F, sep="",
    ":ReservoirMinStage 39","\n",
    format(start.date, format = "%Y-%m-%d"), " 00:00:00.0", ' 1 ', nrow(osoyoos.min.stage),"\n")
write.table(osoyoos.min.stage$Min,"../R_output/Osoyoos_MinStage.rvt",append=T,col.names=F,row.names=F,sep=" ",quote=F)

cat(file="../R_output/Osoyoos_MinStage.rvt", append=T, sep="", ":EndReservoirMinStage","\n")



