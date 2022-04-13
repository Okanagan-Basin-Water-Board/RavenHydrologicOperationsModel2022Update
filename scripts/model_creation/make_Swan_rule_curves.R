###Swan min flow and target levels
##Written by SAG for 3004430 on 09-Jan-2020
## Contact sgrass@nhcweb.com


library(tidyverse)
library(lubridate)
library(xts)
library(data.table)

## Make time series
out.path <- "./model/reservoirs/"

start.date <- as.Date("1945-01-01", format = "%Y-%m-%d")
end.date <- start.date + 24837

dates <-  seq.Date(from = start.date, to = end.date, by = 1)

swan.targets <- data.frame(Date = dates,
                                Month = month(dates),
                                Day = day(dates)) %>% 
  mutate(Target_WSE = case_when(
                                  Month ==3  ~ 389.189,
                                  Month == 4 & Day < 15 ~ 389.089,
                                  Month == 4 & Day <29 & Day > 14 ~ 389.139,
                                  Month == 4 & Day >28 ~389.239,
                                  Month == 5 & Day < 13 ~ 389.239,
                                  Month == 5 & Day > 12 & Day < 20 ~ 389.289,
                                  Month == 5 & Day > 19 & Day < 27 ~ 389.339,
                                  Month == 5 & Day > 26 ~ 389.389,
                                  Month == 6 & Day < 24 ~ 389.389,
                                  Month == 6 & Day > 23 ~ 389.339,
                                  Month == 7 & Day < 8 ~ 389.339,
                                  TRUE ~ 389.289)) 

swan.min.Q <- data.frame(Date = dates,
                                Month = month(dates),
                                Day = day(dates)) %>% 
  mutate(Min_Q = case_when(Month == 4 & Day < 29 ~ 0.259,
                           Month == 4 & Day > 28 ~ 0.517,
                           Month == 5 ~ 0.517,
                           Month == 6 & Day < 3 ~ 0.517,
                           Month == 6 & Day > 2 ~ 0.259,
                           Month == 7 & Day < 8 ~ 0.103,
                           Month == 7 & Day > 7 & Day < 15 ~ 0.078,
                           Month == 7 & Day > 14 & Day < 22 ~ 0.065,
                           TRUE ~ 0.052))

#variable weir heights (0 = 3 logs)
swan.var.weir <- data.frame(Date = dates,
                            Month = month(dates),
                            Day = day(dates)) %>% 
  mutate(weir = case_when(Month %in% c(6,7,8,9,10,11,12) ~ 0.612-0.308 , # 5 logs
                          Month %in% c(1,2,5) ~ 0.460-0.308, # 4 logs
                          TRUE ~ 0)) #feb, march, 3 logs


##remove leap years

# swan.targets<- swan.targets %>% mutate(Keep = ifelse(Month ==2 & Day == 29, "Lose", "Keep")) %>%
#   filter(Keep == "Keep") %>% select(-one_of("Keep"))
# 
# swan.min.Q <- swan.min.Q %>% mutate(Keep = ifelse(Month ==2 & Day == 29, "Lose", "Keep")) %>%
#   filter(Keep == "Keep") %>% select(-one_of("Keep"))

# ##write time series
# #Max Stage
# cat(file = paste0(out.path, "Swan_Target.rvt"), append=F, sep="",
#     ":ReservoirTargetStage 63","\n",
#     format(start.date, format = "%Y-%m-%d"), " 00:00:00.0", ' 1 ', nrow(swan.targets),"\n")
# write.table(swan.targets$Target_WSE,paste0(out.path, "Swan_Target.rvt"),append=T,col.names=F,row.names=F,sep=" ",quote=F)
# 
# cat(file=paste0(out.path, "Swan_Target.rvt"), append=T, sep="", ":EndReservoirTargetStage","\n")
# 
# #Min Q
# cat(file = paste0(out.path, "Swan_MinQ.rvt"), append=F, sep="",
#     ":ReservoirMinFlow 63","\n",
#     format(start.date, format = "%Y-%m-%d"), " 00:00:00.0", ' 1 ', nrow(swan.min.Q),"\n")
# write.table(swan.min.Q$Min_Q,paste0(out.path, "Swan_MinQ.rvt"),append=T,col.names=F,row.names=F,sep=" ",quote=F)
# 
# cat(file=paste0(out.path, "Swan_MinQ.rvt"), append=T, sep="", ":EndReservoirMinFlow","\n")

#variableweirheight
cat(file = paste0(out.path, "Swan_VariableWeir.rvt"), append=F, sep="",
    ":VariableWeirHeight 63","\n",
    format(start.date, format = "%Y-%m-%d"), " 00:00:00.0", ' 1 ', nrow(swan.var.weir),"\n")
write.table(swan.var.weir$weir,paste0(out.path, "Swan_VariableWeir.rvt"),append=T,col.names=F,row.names=F,sep=" ",quote=F)

cat(file=paste0(out.path, "Swan_VariableWeir.rvt"), append=T, sep="", ":EndVariableWeirHeight","\n")



