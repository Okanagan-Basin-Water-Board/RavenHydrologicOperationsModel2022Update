#wrapper script to take an ensemble name as an input
#set up model template to run with that model
#do intermediate calculations (lake evap and forecast target timeseries)
#run model, both level priority and flow priority
#summarize output

#####################################
#JWT + SAG  January 2020
#####################################

library(tidyverse)
library(here)
library(furrr)

####################################
#                                  #
# Model Run 1 - Setup              #
#                                  #
####################################

#to start we have a model template
#needed in template:
#rvh,rvp,rvi,rvc
#rvt.tpl (adjusted by model)
#reservoir stuff
#not needed: overrides, obs

path <- "C:/OkanaganGIS/Raven_ensembles/"

localpath <- here()

#ensemble names
index <- sprintf("%02d", c(1:50))

#index = c('01', '02')

#fill model data into each folder
modelfiller <- function(index){
  out.path <- file.path(path, index)
  
  cmd <- paste0('mkdir ', out.path, '/reservoirs/')
  system(cmd)
  
  cmd <- paste0('mkdir ', out.path, '/resevap/')
  system(cmd)
  
  cmd <- cmd <- paste0('mkdir ', out.path, '/output/')
  system(cmd)

  cmd <- paste0('cp -r ', localpath, '/scripts/ensembles/Annualcycles.rvt ', out.path, '/reservoirs/')
  system(cmd)

  cmd <- paste0('cp -r ', localpath, '/scripts/ensembles/Reservoirs_wstage.rvh ', out.path, '/reservoirs/')
  system(cmd)

  cmd <- paste0('cp -r ', localpath, '/model/*.rvh ', out.path)
  system(cmd)

  cmd <- paste0('cp -r ', localpath, '/model/Okanagan.rvc ', out.path)
  system(cmd)

  cmd <- paste0('cp -r ', localpath, '/model/Okanagan.rvp ', out.path)
  system(cmd)

  cmd <- paste0('cp -r ', localpath, '/model/Streamprofiles.rvp ', out.path)
  system(cmd)

  cmd <- paste0('cp -r ', localpath, '/model/Gaugeweights.txt ', out.path)
  system(cmd)
  cmd <- paste0('cp -r ', localpath, '/model/Raven.exe ', out.path)
  system(cmd)
}

map(index, modelfiller)

#Overwrite .rvt.tmpl file and save in model directory
overwrite.rvt1 <- function(index){
 i <- index

 pr <-  paste0("pr_", i, "_qdm.rvt")
 tasmax <- paste0("tasmax_", i, "_qdm.rvt")
 tasmin <- paste0("tasmin_", i, "_qdm.rvt")

 #don't keep templates in batch_testing
 df <- readLines("./scripts/ensembles/Okanagan.rvt.tmpl")

 replaced <- str_replace_all(df, '%PR%', pr) %>%
   str_replace_all('%TASMAX%', tasmax) %>%
   str_replace_all('%TASMIN%', tasmin)

 out.path <- file.path(path, index)

 writeLines(replaced, paste0(out.path, "/Okanagan.rvt"))
 return(out.path)
}

map(index, overwrite.rvt1)

overwrite.rvi1 <- function(index){
  i <- index

  evap <- "PET_HARGREAVES_1985"

  df <- readLines("./scripts/ensembles/Okanagan.rvi.tmpl", -1)

  replaced <- str_replace_all(df, "%OWET%", evap)

  out.path <- file.path(path, index)

  writeLines(replaced, paste0(out.path, "/Okanagan.rvi"))
  return(out.path)
}

map(index, overwrite.rvi1)


####################################
#                                  #
# Run Raven Model for first time   #
#                                  #
####################################

#run Raven for Run 1 of each ensmeble

raven1 <- function(index){
  i <- as.character(index)
  df <- readLines("./scripts/ensembles/Raven1_tmp.bat")
  template.comment <- "REM Template for batch file that will run Raven for the first Run to get forecast and evap data"
  final.comment <- "REM Run Raven for the first Run to get forecast and evap data - Generated from template file; overwritten for each Ensemble run"
  
  replaced <- str_replace_all(df, '%INDEX%', i) %>%
    str_replace_all(template.comment, final.comment)
  
  #need to name it based on its index number
  writeLines(replaced, file.path(path, index, 'Raven.bat'))
  
  system(file.path(path, index, 'Raven.bat'), show.output.on.console = FALSE)
}

#parallelize
plan(multiprocess)
future_map(index, raven1)

####################################
#                                  #
# Calc Forecast & Evap             #
#                                  #
####################################

#run the scripts, save output to Run 2 model
# index.path <- file.path(path, index)
# 
# ##############################################
# forecast <- function(index.path){
#   #print(index.path)
#   system(paste0('Rscript ', localpath, "/scripts/ensembles/Future_OBWB_Forecast.R ", index.path))
# }
# 
# plan(multiprocess)
# future_map(index.path, forecast)
# 
# 
# evap <- function(index.path){
#   system(paste0('Rscript ', localpath, "/scripts/ensembles/future_write_lakeevap.R ", index.path))
# }
# 
# #plan(multiprocess)
# future_map(index.path, evap)
# 
# calc.swan <- function(index.path){
#   system(paste0('Rscript ', localpath, "/scripts/ensembles/future_make_Swan_rule_curves.R ", index.path))
# }
# 
# #plan(multiprocess)
# future_map(index.path, calc.swan)
# ################################################
# 
# 
# 
# ####################################
# #                                  #
# # Setup Run 2                      #
# #                                  #
# ####################################
# 
# #update .rvt for run 2
# overwrite.rvt2 <- function(index){
#   i <- index
# 
#   pr <- paste0("pr_", i, "_qdm.rvt")
#   tasmax <- paste0("tasmax_", i, "_qdm.rvt")
#   tasmin <- paste0("tasmin_", i, "_qdm.rvt")
# 
#   df <- readLines("./scripts/ensembles/Okanagan.rvt.tmpl")
# 
#   replaced <- str_replace_all(df, '%PR%', pr) %>%
#     str_replace_all('%TASMAX%', tasmax) %>%
#     str_replace_all('%TASMIN%', tasmin) %>%
#     str_replace_all("#:RedirectToFile", ":RedirectToFile") %>%
#     str_replace_all("#  :RedirectToFile", "  :RedirectToFile")
# 
#   out.path <- file.path(path, i)
# 
#   writeLines(replaced, paste0(out.path, "/Okanagan.rvt"))
# }
# 
# map(index, overwrite.rvt2)
# 
# #update .rvi for run 2 including outputs required
# overwrite.rvi2 <- function(index){
#   i <- index
# 
#   evap <- "PET_DATA PET_HARGREAVES_1985"
# 
#   df <- readLines("./scripts/ensembles/Okanagan.rvi.tmpl", -1)
# 
#   replaced <- str_replace_all(df, "%OWET%", evap) %>%
#     str_replace_all(":CustomOutput", "#:CustomOutput") #%>%
#     #str_replace_all(":StartDate 1950-01-01 00:00:00", ":StartDate 1950-10-01 00:00:00") %>%
#     #str_replace_all(":Duration 55115", ":Duration 54841")
# 
#   out.path <- file.path(path, i)
# 
#   writeLines(replaced, paste0(out.path, "/Okanagan.rvi"))
# }
# 
# map(index, overwrite.rvi2)


####################################
#                                  #
# Run Raven model for second time  #
#                                  #
####################################

# ptm <- proc.time()
# future_map(index, raven1)
# proc.time() - ptm
# 
# #make the last plots too
# source('./scripts/ensembles/ensemble_explorer.R')