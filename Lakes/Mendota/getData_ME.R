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
oxy.df$OXY_oxy <-  oxy.df$OXY_oxy * 1000/32

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

mendota.summary <- read.csv('ME_data//lake.csv')
mendota.summary$time <- as.POSIXct(mendota.summary$time)
ice_data <- mendota.summary %>%
  mutate(year = lubridate::year(time)) %>%
  mutate(month = lubridate::month(time)) %>%
  dplyr::select(c(time, year, month, Blue.Ice.Thickness))
ice_data$doy <- yday(ice_data$time)

ice_start <- c()
ice_end <- c()
for (ii in 2:nrow(ice_data)-1){
  if (ice_data$Blue.Ice.Thickness[ii] != 0. && ice_data$Blue.Ice.Thickness[ii+1] != 0. && ice_data$Blue.Ice.Thickness[ii-1] == 0.){
    ice_start <- append(ice_start,ice_data$time[ii] )
  } else if (ice_data$Blue.Ice.Thickness[ii] != 0. && ice_data$Blue.Ice.Thickness[ii+1] == 0. && ice_data$Blue.Ice.Thickness[ii-1] != 0.){
    ice_end <- append(ice_end,ice_data$time[ii] )
  }
}
ice_dur <- data.frame('year' = year(ice_end), 'ice_dur'=ice_end - ice_start, 'ice_end' = ice_end)
double_ice = which(duplicated(ice_dur$year))
year_double_ice <- which(ice_dur$year[double_ice] == ice_dur$year)
ice.df <- c()
if (ice_dur$ice_dur[double_ice] < ice_dur$ice_dur[year_double_ice[which(double_ice != year_double_ice)]] ){
  ice.df$ice_dur = as.numeric(ice_dur$ice_dur[-c(double_ice)])
  ice.df$ice_end = yday(ice_dur$ice_end[-c(double_ice)])
} else {
  ice.df$ice_dur = as.numeric(ice_dur$ice_dur[-c(year_double_ice[which(double_ice != year_double_ice)])])
  ice.df$ice_end = yday(ice_dur$ice_end[-c(year_double_ice[which(double_ice != year_double_ice)])])
}

ice.df$year = unique(year(wtr_data$DateTime))


idx <- match(as.Date(wtr_data$DateTime), as.Date(yahara.inflow$time))
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
  'inflow' =  yahara.inflow$FLOW[na.omit(idx)] + pheasant.inflow$FLOW[na.omit(idx)],
  'outflow' =  outflow$FLOW[na.omit(idx)],
  'MLD' = thermocline$thermo.depth,
  'W' = wedderburn$wedderburn.number,
  'doy' = yday(wtr_data$DateTime),
  'month' = month(wtr_data$DateTime)
)
model.df$Ice_d <- ice.df$ice_dur[match(year(model.df$DateTime), ice.df$year)]
model.df$days_from_iceoff = model.df$doy - ice.df$ice_end[match(year(model.df$DateTime), ice.df$year)] 
model.df$DateTime <- as.Date(model.df$DateTime)

write_csv(x = model.df, file = 'Training data/ME_Daily_Observation_df.csv', col_names = T)


# Date,
# NOX(mmole/m3),
# NH4(mmole/m3),
# PO4(mmole/m3),
# TotP(mmole/m3),
# Si(mmole/m3),
# Chl(mg/m3),
# O2(mmole/m3),

temp <- oxy.df %>%
  rename(DateTime = datetime) %>%
  select(DateTime, depth, temp)

temp$ustar <- meteo.df$WindSpeed[match(as.Date((temp$DateTime)), as.Date(meteo.df$Date))]
  
temp.df <- data.frame(
  'DateTime' = unique(temp$DateTime),
  "SST" = as.numeric(unlist(temp %>% group_by(DateTime) %>% summarise(surfT = temp[which.min(depth)]) %>% select(surfT))),
  'delT' = as.numeric(unlist(temp %>% group_by(DateTime) %>% summarise(delT =  temp[which.min(depth)] - temp[which.max(depth)]) %>% select(delT))),
  'U' = meteo.df$WindSpeed[match(as.Date(unique(temp$DateTime)), as.Date(meteo.df$Date))],
  'AirT' = meteo.df$AirTemp[match(as.Date(unique(temp$DateTime)), as.Date(meteo.df$Date))],
  'CC' = NA,
  'Prec' = meteo.df$Rain[match(as.Date(unique(temp$DateTime)), as.Date(meteo.df$Date))] * 1000,
  "swr" = meteo.df$ShortWave[match(as.Date(unique(temp$DateTime)), as.Date(meteo.df$Date))],
  'inflow' =  NA, 
  'outflow' = NA, 
  'MLD' = as.numeric(unlist(temp %>% group_by(DateTime) %>% summarise(MLD = thermo.depth(temp, depth)) %>% select(MLD))),
  'doy' = yday(unique(temp$DateTime)),
  'month' = month(unique(temp$DateTime))
)
Wnum =  temp %>% group_by(DateTime) %>% mutate(delta_rho = water.density(wtr = temp[which.min(depth)]) -
                                                                  water.density(wtr = temp[which.max(depth)]),
                                                                metaTop = meta.depths(wtr = temp,
                                                                                      depths = depth)[1],
                                                                metaBot = meta.depths(wtr = temp,
                                                                                      depths = depth)[2],
                                                                uSt = uStar(wndSpeed = ustar,wndHeight = 5,averageEpiDense = water.density(wtr = temp[which.min(depth)])) ,
                                                                Ao = max(A),
                                                                Area = approx(H, A, depth)$y,
                                                                AvHyp_rho = water.density(wtr = temp[which.max(depth)])) %>% 
                           group_by(DateTime) %>%
                           summarise(W = wedderburn.number(delta_rho = abs(delta_rho), metaT = metaTop, uSt = uSt, Ao = Ao, AvHyp_rho = AvHyp_rho)) %>%   
                           select(W)
temp.df$W = Wnum$W[which(duplicated(Wnum$DateTime) == F)]


ice.data = read_csv('ME_data/ntl33_v7.csv')
ice.df = ice.data %>% mutate(soi = sub('.*(\\d{4}).*', '\\1', ice.data$season)) %>%
  group_by(soi) %>%
  mutate(doy = yday(ice_off)) %>%
  select(soi, doy, ice_duration)

idx = match( year(temp.df$DateTime),ice.df$soi)
temp.df$Ice_d <- ice.df$ice_duration[idx]
temp.df$days_from_iceoff = temp.df$doy - ice.df$doy[idx]
temp.df$DateTime <- as.Date(temp.df$DateTime)

oxygen <- oxy.df %>%
  filter(depth == 0) %>%
  rename(DateTime = datetime) %>%
  select(DateTime, OXY_oxy)
wq <- chem.df %>%
  filter(depth == 0) %>%
  rename(DateTime = datetime) %>%
  select(DateTime, CAR_dic, SIL_rsi, NIT_nit, PHS_frp, NIT_amm)
chla <- chla.df  %>%
  filter(depth_range_m == "0-2") %>%
  rename(DateTime = sampledate) %>%
  select(DateTime, correct_chl_fluor)
oxygen <- oxygen[complete.cases(oxygen),]
wq <- wq[complete.cases(wq),]
chla <- chla[complete.cases(chla),]

df <- merge(oxygen, wq, by = 'DateTime')
df <- merge(df, chla, by = 'DateTime')
df$DateTime <- as.Date(df$DateTime)
df <- merge(df, temp.df, by = 'DateTime')

df= df %>%
  rename(O2 = OXY_oxy, Si = SIL_rsi, N_Nit = NIT_nit, 
         PO4 = PHS_frp, N_Amm = NIT_amm, Chl = correct_chl_fluor)

df$DateTime <- as.Date(df$DateTime)

idx <- match(df$DateTime, model.df$DateTime)
df.all <- merge(df, model.df, by = 'DateTime')
write_csv(x = df, file = 'Training data/ME_Observation_df.csv', col_names = T)

