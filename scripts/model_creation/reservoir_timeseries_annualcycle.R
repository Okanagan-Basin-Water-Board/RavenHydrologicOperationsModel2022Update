## Make time-series for reservoirs
##Written by SAG for 3004430 on 23 APR 2019
## Contact sgrass@nhcweb.com

library(tidyverse)
library(lubridate)

## Make time series
#cgvd offsets
kal.cgvd = 0.252 #value at gauge
ok.cgvd = 0.264 #value at dam
skaha.cgvd = 0.33 #value at dam
vaseux.cgvd = 0.256 #value at 08nm243

kalamalka.min.q <-  rep(0.85,12)

kalamalka.max.q.delta <- rep(0.11, 12)


skaha.elevations <- c(337.8, 337.8, 337.8, 337.8, 337.85, 337.9, 337.9, 337.9, 337.85, 337.8, 337.8, 337.8) + skaha.cgvd

skaha.max.q.delta <-rep(1.4,12)


Vaseux.elevations <- c(327.4, 327.4, 327.4, 327.4, 327.5, 327.6, 327.6, 327.6, 327.5, 327.4, 327.4, 327.4) + vaseux.cgvd

vasuex.max.q.delta <- rep(1.8, 12)


okanagan.max.stage <- rep(342.75,12) + ok.cgvd

okanagan.min.stage <- rep(341.5,12) + ok.cgvd

okanagan.min.q <- rep(5.0, 12)

okanagan.max.q.delta <- rep(2, 12)

##write time series

#Kalamalka Min Q
cat(file = "./model/reservoirs/Annualcycles.rvt", append=F, sep="",
    ":ReservoirMinStageFlow 42","\n",
    ":AnnualCycle ", paste(kalamalka.min.q, collapse=",", sep = " "), "\n",
    ":EndReservoirMinStageFlow","\n",
    "\n")

#Kalamalka Max Q Delta
cat(file = "./model/reservoirs/Annualcycles.rvt", append=T, sep="",
    ":ReservoirMaxQDelta 42","\n",
    ":AnnualCycle ", paste(kalamalka.max.q.delta, collapse=",", sep = " "), "\n",
    ":EndReservoirMaxQDelta","\n",
    "\n")

#Skaha Target Elevations
cat(file = "./model/reservoirs/Annualcycles.rvt", append=T, sep="",
    ":ReservoirTargetStage 40","\n",
    ":AnnualCycle ", paste(skaha.elevations, collapse=",", sep = " "), "\n",
    ":EndReservoirTargetStage","\n",
    "\n")

#Skaha Max Q Delta
cat(file = "./model/reservoirs/Annualcycles.rvt", append=T, sep="",
    ":ReservoirMaxQDelta 40","\n",
    ":AnnualCycle ", paste(skaha.max.q.delta, collapse=",", sep = " "), "\n",
    ":EndReservoirMaxQDelta","\n",
    "\n")

#Vaseux Target Elevations
cat(file = "./model/reservoirs/Annualcycles.rvt", append=T, sep="",
    ":ReservoirTargetStage 43","\n",
    ":AnnualCycle ", paste(Vaseux.elevations, collapse=",", sep = " "), "\n",
    ":EndReservoirTargetStage","\n",
    "\n")

#Vasuex Max Q Delta
cat(file = "./model/reservoirs/Annualcycles.rvt", append=T, sep="",
    ":ReservoirMaxQDelta 43","\n",
    ":AnnualCycle ", paste(vasuex.max.q.delta, collapse=",", sep = " "), "\n",
    ":EndReservoirMaxQDelta","\n",
    "\n")

# #Okanagan Max Stage
# cat(file = "./model/reservoirs/Annualcycles.rvt", append=T, sep="",
#     ":ReservoirMaxStage 41","\n",
#     ":AnnualCycle ", paste(okanagan.max.stage, collapse=",", sep = " "), "\n",
#     ":EndReservoirMaxStage","\n",
#     "\n")
# 
# #Okanagan Min Stage
# cat(file = "./model/reservoirs/Annualcycles.rvt", append=T, sep="",
#     ":ReservoirMinStage 41","\n",
#     ":AnnualCycle ", paste(okanagan.min.stage, collapse=",", sep = " "), "\n",
#     ":EndReservoirMinStage","\n",
#     "\n")
# 
# #Okanagan Min Q - temporary requires updating!!
# cat(file = "./model/reservoirs/Annualcycles.rvt", append=T, sep="",
#     ":ReservoirMinStageFlow 41 ","#Requires Updating!!", "\n",
#     ":AnnualCycle ", paste(okanagan.min.q, collapse=",", sep = " "), "\n",
#     ":EndReservoirMinStageFlow","\n",
#     "\n")
# 
# #Okanagan Max Delta Q
# cat(file = "./model/reservoirs/Annualcycles.rvt", append=T, sep="",
#     ":ReservoirMaxQDelta 41","\n",
#     ":AnnualCycle ", paste(okanagan.max.q.delta, collapse=",", sep = " "), "\n",
#     ":EndReservoirMaxQDelta","\n",
#     "\n")
