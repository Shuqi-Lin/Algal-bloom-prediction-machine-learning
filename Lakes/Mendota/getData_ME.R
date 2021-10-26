# remove everything from workspace
rm(list = ls())

# set wd to current dir of script
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load all functions (big mess)
source('ME_data/functions-glm.R')
# read all packages
library(tidyverse)
library(glmtools)
library(lubridate)

# location of output
out <- 'ME_data//output.nc'

# GLM v3.1
os = 'gfort'

sim_vars(out)

oxy.df <- read_csv('ME_data/field_mendota-timestamp.csv')
oxy.df <-  oxy.df$OXY_oxy * 1000/32

chem.df <- read_csv('ME_data/field_chemistry-timestamp.csv')
chem.df$CAR_dic = chem.df$CAR_dic * 1000/12
chem.df$SIL_rsi = chem.df$SIL_rsi * 1000/28
chem.df$NIT_nit = chem.df$NIT_nit * 1000/14
chem.df$PHS_frp = chem.df$PHS_frp * 1000/31
chem.df$NIT_amm = chem.df$NIT_amm * 1000/31

chla.df <-  read_csv("ME_data/chlorophyll_madison-timestamp.csv")

eg_nml <- read_nml(nml_file = 'ME_data/glm3.nml')
eg_nml$morphometry$H
H <- abs(eg_nml$morphometry$H - max(eg_nml$morphometry$H))
A <- eg_nml$morphometry$A

meteo_data <- read_csv('ME_data/NLDAS2_Mendota_1979_2016_cell_5_GLMReady_cut_timezonecorr.csv')
daily.agg.meteo.dat <- meteo_data %>%
  mutate(year = lubridate::year(Date)) %>%
  mutate(month = lubridate::month(Date)) %>%
  mutate(day = format(Date, '%Y-%m-%d')) %>%
  mutate(doy = yday(Date)) %>%
  group_by(day) %>%
  summarise_all(mean)

# use rLakeAnalyzer to calculate physical derivatives, e.g. thermocline depth
wtr_data <- get_var(file = out,
                    var_name = 'temp',
                    reference = 'surface')
str(wtr_data)
# transform data into rLakeAnalyzer format
wtr_df <- data.frame('datetime' = wtr_data$DateTime,
                     as.matrix(wtr_data[, 2:ncol(wtr_data)]))
colnames(wtr_df) <- c('datetime',paste("wtr_", round(as.numeric(sub(".*_", "", colnames(wtr_df[-1]))),1), sep=""))


meteo.dates <- match(format(wtr_df$datetime, '%Y-%m-%d'), daily.agg.meteo.dat$day)
min.date <- (daily.agg.meteo.dat$day[min(meteo.dates)])
max.date  <- (daily.agg.meteo.dat$day[max(meteo.dates)])

meteo.df <- daily.agg.meteo.dat %>%
  filter(day >= min.date & day <= max.date)

thermocline <- rLakeAnalyzer::ts.thermo.depth(wtr = wtr_df, Smin = 0.1, na.rm = TRUE)

sp.wtr <- load.ts('ME_data/wtr.csv')
sp.wnd <- load.ts('ME_data/wind.csv')
sp.bath <- load.bathy('ME_data/bathy.csv')
wedderburn <- rLakeAnalyzer::ts.wedderburn.number (wtr= sp.wtr , wnd = sp.wnd, wnd.height = 5,
                                   bathy = sp.bath, Ao = rev(eg_nml$morphometry$A)[1],
                                   seasonal = T)

yahara.inflow <- read.csv('ME_data/Mendota_yahara_30year2xP.csv')
pheasant.inflow <- read.csv('ME_data/Mendota_pheasant_branch_30year2xP.csv')
outflow <- read.csv('ME_data/Mendota_outflow_30year.csv')

# SST-- surface water temperature (°C) 
# delT -- temperature difference between surface and bottom water (°C) 
# U -- wind speed (m/s) 
# AirT -- air temperature (°C) 
# Humidity -- (0-100 %) 
# CC -- cloud cover fraction (0-1) 
# Prec -- precipitation (mm/day) 
# swr -- short wave radiation (w/m2) 
# inflow -- river inflow (m3/s) 
# outflow -- river outflow (m3/s) 
# Ice_d -- ice duration in the previous winter (days) 
# days from iceoff -- the number of days from previous ice-off date (days) 
# MLD -- mixing layer depth (m) 
# W -- Wedderburn number thermD -- themocline depth
model.df <- data.frame(
  'DateTime' = wtr_data$DateTime,
  "SST" = wtr_data$temp_0,
  'delT' = wtr_data$temp_0 - wtr_data$temp_23.8400303091291,
  'U' = meteo.df$WindSpeed,
  'AirT' = meteo.df$AirTemp,
  'CC' = NA,
  'Prec' = meteo.df$Rain * 1000,
  "swr" = meteo.df$ShortWave,
  'inflow' =  yahara.inflow$FLOW + pheasant.inflow$FLOW,
  'outflow' =  outflow$FLOW,
  "Ice_d" = ,
  'days_from_iceoff' = ,
  'MLD' = thermocline$thermo.depth,
  'W' = wedderburn$wedderburn.number
)
