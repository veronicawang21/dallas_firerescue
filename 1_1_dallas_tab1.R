###################################
######## Table 1 ##################
###################################

rm(list = ls())

setwd("C:\\Users\\veron\\OneDrive - Harvard University\\smartsurfaces\\Dallas_911calls\\")

library(tidyverse)
library(lubridate)

# read in data
df_all <- readRDS("temp\\df_all.rds")


# entire study period
df <- df_all %>%
  select(himax, himin, call, tot, tot_here, fire, amb)

round(sapply(df, sum),1)
round(sapply(df, mean),1)
round(sapply(df, sd),1)

# by year group
df_yr <- df_all %>%
  mutate(y_group=case_when(
    yr %in% c(2011:2015) ~ 1,
    yr %in% c(2016:2020) ~ 2,
    TRUE ~ 3)) %>%
  select(himax, himin, call, tot, tot_here, fire, amb, y_group)

for (i in c(1:3)){
  df_yr_s <- df_yr %>% filter(y_group==i)
  print(paste("y_group=", i))
  print(round(sapply(df_yr_s, sum),1))
  print(round(sapply(df_yr_s, mean),1))
  print(round(sapply(df_yr_s, sd),1))
}
