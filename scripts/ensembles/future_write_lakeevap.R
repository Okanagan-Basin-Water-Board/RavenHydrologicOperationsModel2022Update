#pre-processing step to compute reservoir evaporation based on temperature
#using equations from Schertzer and Taylor, 2009 (https://www.obwb.ca/fileadmin/docs/okanagan_evaporation.pdf)
#JWT Sept 2019

library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(RavenR)

#hrudata for mainstem lakes and swan lake
resall <- read_csv('./model_tables/lake_name_lookup.csv')
resmain <- resall %>%
  filter(LakeName %in% c('Osoyoos', 'Skaha', 'Okanagan', 'WoodandKalamalka', 'Vaseux', 'Ellison', 'Swan'))


##Read in Data
args <- commandArgs(TRUE)
ensemble = args[1]


##Read in Data
path.in <- file.path(ensemble, "/output/")
path.out <- file.path(ensemble, "/resevap/")

#tempdata
temp.xts <- custom.read(paste0(path.in, '/TEMP_AVE_Daily_Average_ByHRUGroup.csv'))
temps <- as_tibble(temp.xts) %>%
  mutate(Date = as.Date(index(temp.xts)))


evapo <- function(reservoir){

  if(reservoir != 'dummy'){
  
    ##########################
    #Step 1 pull reservoir temps from model output (daily average values) and hru
    tdat <- temps %>% select(Date, Airtemp = !!reservoir)
    
    hru <- resall %>% filter(LakeName == reservoir) %>% 
      pull(HRUID)
    ###########################
    
    
    #############################
    #Step 2 Calculate evaporation for HRU based on equations
    # for lakes that freeze (not ok, skaha, vaseux, osoyoos, kalwood), set evap to 0 at temps < 0
    if(reservoir == 'Skaha' | reservoir == 'Okanagan'){
      tdat <- tdat %>% mutate(evap = 0.0052*Airtemp^2 - 0.0551 * Airtemp + 1.0256)
    } else {
      tdat <- tdat %>% mutate(evap = 0.0027*Airtemp^2 - 0.0086 * Airtemp + 0.4075)
    }
    
    #temporary turn off evap in winter
    #if(reservoir %in% c('Ellison', 'Swan')) {
      tdat <- tdat %>% mutate(evap = ifelse(Airtemp <= 0, 0, evap)) %>%
        #filter out leap days
        filter(!(month(Date) == 2 & day(Date) == 29))
      
    #}
    ################################
    
    ############################
    #Step 3 write RVT with evaporation values
    rvt <- paste0(path.out, '/', reservoir, '.rvt')
    
    cat(file=rvt, append=F, sep="",
        ":Data OW_PET mm\n",
        format(tdat$Date[1], format = '%Y-%m-%d %H:%M:%S.0'), ' 1 ', nrow(tdat),"\n")
    
    write.table(select(tdat, evap), rvt, append=T, col.names=F, row.names=F, sep=" ", quote=F)
    
    cat(file=rvt, append=T, sep="",    ":EndData","\n")
  } else {
    #make a dummy empty dataset to use for the lakes which arent calculated
    tdat <- temps %>% select(Date) %>% 
      mutate(evap = -1.2345)
    
    rvt <- paste0(path.out, '/', reservoir, '.rvt')
    
    cat(file=rvt, append=F, sep="",
        ":Data OW_PET mm\n",
        format(tdat$Date[1], format = '%Y-%m-%d %H:%M:%S.0'), ' 1 ', nrow(tdat),"\n")
    
    write.table(select(tdat, evap), rvt, append=T, col.names=F, row.names=F, sep=" ", quote=F)
    
    cat(file=rvt, append=T, sep="",    ":EndData","\n")
  }
  
  ##############################
}

#apply to each reservoir
map(c(resmain$LakeName, 'dummy'), evapo)
# 
# 
# # # #one time application, make gauge file for every lake hru, assign to empty gauge data when you want to override
# rvtgauge <- './model/Okanagan.rvt'
# 
# hrus <- rvh.read('./model/Okanagan.rvh')
# #
# # #write gauge names for gauges with actual data
# # writegauge <- function(reservoir){
# #   hru <- resall %>% filter(LakeName == reservoir) %>%
# #     pull(HRUID)
# #
# #   hrudat <- hrus$HRUtable %>% filter(ID == hru)
# #
# #   cat(file=rvtgauge, append=T, sep="",
# #       ":Gauge ", reservoir, "\n",
# #       "  :Latitude ", hrudat$Latitude,"\n",
# #       "  :Longitude ", hrudat$Longitude,"\n",
# #       "  :Elevation ", hrudat$Elevation,"\n",
# #       "  :RedirectToFile ",'resevap/', reservoir, '.rvt\n',
# #       ":EndGauge\n\n")
# # }
# #
# # map(resmain$LakeName, writegauge)
# #
# gauges <- tibble(lakename = c(resmain$LakeName, 'dummy'),
#                  ID = seq(1:8))
# 
# gaugeweights = matrix(data = 0, nrow = nrow(hrus$HRUtable), ncol = 8)
# 
# #match the HRUID with its row in the hru file
# resmain <- resmain %>%
#   rowwise %>%
#   mutate(HRUROW = which(HRUID == hrus$HRUtable$ID))
# 
# #loop through
# for(i in 1:nrow(gaugeweights)){
#   if(i %in% resmain$HRUROW){
#     name = resmain %>% filter(HRUROW == i) %>%
#       pull(LakeName)
#     col <- gauges %>% filter(lakename == name) %>%
#       pull(ID)
#     gaugeweights[i,col] = 1
#   } else
#     gaugeweights[i,8] = 1
# }
# 
# gaugeweights <- as.data.frame(gaugeweights)
# 
# gweights <- './model/GaugeWeights.txt'
# cat(file=gweights, append=F, sep="",
#     ":GaugeWeightTable\n",
#     ncol(gaugeweights), ' ', nrow(gaugeweights), '\n')
# 
# write.table(gaugeweights,gweights,append=T,col.names=F,row.names=F,sep=" ",quote=F)
# 
# cat(file=gweights, append=T, sep="",    ":EndGaugeWeightTable","\n")

