###################################
###### Main analysis: DLNM ########
###### linear expo-resp ###########
###################################

rm(list = ls())

setwd("C:\\Users\\veron\\OneDrive - Harvard University\\smartsurfaces\\Dallas_911calls\\")

library(tidyverse)
library(lubridate)
library(dlnm)
library(mgcv)
library(splines)
library(tsModel)

# read in dataset
df <- readRDS("temp\\df_all.rds") %>%
  mutate(wkday=weekdays(date))

# choose: exposure ################################################################
exposure <- "maximum"
# exposure <- "minimum"

expo <- substr(exposure, 1, 3)
out_list <- c("call", "tot", "tot_here", "fire", "amb")

for (i in c(1:5)){
  out <- out_list[i]
  
  # set up figure parameters/names
  if (exposure=="maximum") {
    temp <- df$himax
    refer=round(median(temp))
    ind <- 34-3
  } else if (exposure=="minimum") {
    temp <- df$himin
    refer=round(median(temp))
    ind <- 23+3
  } else {
    print("error")
  }
  
  
  ################################################################################
  # GAM WITH PENALTY ON THE LAG
  
  # DEFINE THE CROSS-BASIS
  cbgam2 <- crossbasis(temp,lag=6,argvar=list(fun="lin"),
                       arglag=list(fun="ps",df=5))
  cbgam2Pen <- cbPen(cbgam2)
  
  gam2 <- gam(as.formula(paste(out,"~ cbgam2+sin(harm_d)+cos(harm_d)+as.factor(yr)+as.factor(wkday)")),family=quasipoisson(),df,
              paraPen=list(cbgam2=cbgam2Pen), method='REML')
  
  # get cross prediction
  predslgam2 <- crosspred(cbgam2,gam2,at=seq(ceiling(min(temp)),floor(max(temp)),by=1),bylag=1,cen=refer, cumul = T)
  
  # save relevant coefficients
  res <- data.frame(expo=exposure,
                    out=out,
                    lag=c(0:6, "cum0", "cum01", "cum06"),
                    est=c(predslgam2$matRRfit[ind, 1:7], exp(predslgam2$cumfit[ind, 1]), exp(predslgam2$cumfit[ind, 2]), predslgam2$allRRfit[ind]),
                    lo=c(predslgam2$matRRlow[ind, 1:7], exp(predslgam2$cumfit[ind, 1]-1.96*predslgam2$cumse[ind, 1]), exp(predslgam2$cumfit[ind, 2]-1.96*predslgam2$cumse[ind, 2]), predslgam2$allRRlow[ind]),
                    hi=c(predslgam2$matRRhigh[ind, 1:7], exp(predslgam2$cumfit[ind, 1]+1.96*predslgam2$cumse[ind, 1]), exp(predslgam2$cumfit[ind, 2]+1.96*predslgam2$cumse[ind, 2]), predslgam2$allRRhigh[ind]))
  
  saveRDS(res, paste("results_lin\\", expo, "_", out, ".rds", sep=""))
}
