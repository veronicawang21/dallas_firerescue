###################################
######## Cleaning data ############
###################################

rm(list = ls())

library(tidyverse)
library(lubridate)
library(weathermetrics) 

###################################
######## Prep outcome #############
###################################
setwd("C:\\Users\\veron\\OneDrive - Harvard University\\smartsurfaces\\Dallas_911calls\\outcome")

# combine yearly call/dispatch files into one dataset
temp = list.files(pattern="\\.csv$")
df <- data.frame()

for (i in temp) {
  df <- rbind(df, read.csv(i))
}

names(df) <- c("row", "response_date", "length_sec", "postal", "type", "amb_num", "fire_num", "tot_num")

df <- df %>%
  select(-c("row")) %>%
  mutate(date=strptime(response_date, format = "%m/%d/%y %I:%M %p")) %>%
  mutate(yr=year(date),
         mo=month(date)) %>%
  mutate(day_mo=mday(date),
         date_only=make_date(year=yr, month=mo, day=day_mo),
         date=as.Date(date))

# saveRDS(df, "df.rds")

# format dataset for time series analysis
df_tot <- readRDS("C:\\Users\\veron\\OneDrive - Harvard University\\smartsurfaces\\Dallas_911calls\\outcome\\df.rds") %>%
  filter(fire_num!="NULL") %>%
  group_by(date) %>%
  summarise(fire=sum(as.numeric(fire_num)), amb=sum(as.numeric(amb_num)), tot=sum(as.numeric(tot_num)), call=n()) %>%
  ungroup() %>%
  mutate(date_num = as.numeric(date)) %>%
  mutate(yr=year(date),
         mo=month(date)) %>%
  filter(mo %in% c(5:10)) %>%
  mutate(doy=yday(date),
         harm_d=(2*pi*doy)/365,
         tot_here=fire + amb)

# saveRDS(df_tot, "C:\\Users\\veron\\OneDrive - Harvard University\\smartsurfaces\\Dallas_911calls\\outcome\\df_tot.rds")

###################################
######## Prep heat exposure #######
###################################
setwd("C:\\Users\\veron\\OneDrive - Harvard University\\smartsurfaces\\Dallas_911calls\\temp\\")

# PRISM exposure
prism_tmax <- read.csv("PopWeightPRISM_Dallas\\PopWeightPRISM_Dallas_tmax.csv")
prism_vapmax <- read.csv("PopWeightPRISM_Dallas\\PopWeightPRISM_Dallas_vpdmax.csv") %>% select(vpdmax)
prism_tmin <- read.csv("PopWeightPRISM_Dallas\\PopWeightPRISM_Dallas_tmin.csv") %>% select(tmin)
prism_vapmin <- read.csv("PopWeightPRISM_Dallas\\PopWeightPRISM_Dallas_vpdmin.csv") %>% select(vpdmin)

#vapd: vapor pressure deficit

prism <- prism_tmax %>%
  mutate(date=seq(as.Date('2010-12-01'),as.Date('2023-12-31'), 'day')) %>%
  select(date, tmax) %>%
  cbind(prism_vapmax, prism_tmin, prism_vapmin) %>%
  mutate(satvp_max=6.1094*exp((17.625 * tmax) / (243.04 + tmax)),  # using August-Roche-Magnus equation
         satvp_min=6.1094*exp((17.625 * tmin) / (243.04 + tmin)),
         vp_max=satvp_max-vpdmax,
         vp_min=satvp_min-vpdmin,
         rh_max=100*(vp_max/satvp_max),
         rh_min=100*(vp_min/satvp_min),
         himax=heat.index(t=tmax, rh=rh_max, temperature.metric = 'celsius'),
         himin=heat.index(t=tmin, rh=rh_min, temperature.metric = 'celsius'))

# create exposure lags
prism <- prism %>% 
  mutate(tmax_lag0=tmax,
         tmax_lag1=lag(tmax, n=1),
         tmax_lag2=lag(tmax, n=2),
         tmax_lag3=lag(tmax, n=3),
         tmax_lag4=lag(tmax, n=4),
         tmax_lag5=lag(tmax, n=5),
         tmax_lag6=lag(tmax, n=6),
         tmin_lag0=tmin,
         tmin_lag1=lag(tmin, n=1),
         tmin_lag2=lag(tmin, n=2),
         tmin_lag3=lag(tmin, n=3),
         tmin_lag4=lag(tmin, n=4),
         tmin_lag5=lag(tmin, n=5),
         tmin_lag6=lag(tmin, n=6),
         himax_lag0=himax,
         himax_lag1=lag(himax, n=1),
         himax_lag2=lag(himax, n=2),
         himax_lag3=lag(himax, n=3),
         himax_lag4=lag(himax, n=4),
         himax_lag5=lag(himax, n=5),
         himax_lag6=lag(himax, n=6),
         himin_lag0=himin,
         himin_lag1=lag(himin, n=1),
         himin_lag2=lag(himin, n=2),
         himin_lag3=lag(himin, n=3),
         himin_lag4=lag(himin, n=4),
         himin_lag5=lag(himin, n=5),
         himin_lag6=lag(himin, n=6))

# saveRDS(prism, "prism_dallas.rds")
prism <- readRDS("prism_dallas.rds")

###################################
### Link exposure with outcome ####
###################################
df_all <- prism %>%
  left_join(df_tot) %>%
  drop_na(fire)

# saveRDS(df_all, "df_all.rds")
