###################################
###### Main analysis: DLNM ########
###### nonlinear expo-resp ########
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
    per <- round(quantile(temp, probs = c(0.95, 0.99)),2)
    per_lab <- c("[p95]", "[p99]")
    fig_lim <- c(0.9,1.25)
    refer=5
    cum_lim <- c(0.8, 1.5)
    zlim_lab <- c(0.9,1.25)
  } else if (exposure=="minimum") {
    temp <- df$himin
    per <- round(quantile(temp, probs = c(0.95, 0.99)),2)
    per_lab <- c("[p95]", "[p99]")
    fig_lim <- c(0.9,1.25)
    refer=-1
    cum_lim <- c(0.8, 1.5)
    zlim_lab <- c(0.9,1.25)
  } else {
    print("error")
  }
  
  
  if(out=="call"){
    name <- paste("figs\\", expo, "_call_overall.pdf", sep="")
    name_sp <- paste("figs\\", expo, "_call_sp.pdf", sep="")
    desc <- "Calls dispatched"
  } else if (out=="tot"){
    name <- paste("figs\\", expo, "_tot_overall.pdf", sep="")
    name_sp <- paste("figs\\", expo, "_tot_sp.pdf", sep="")
    desc <- "Vehicles dispatched"
  } else if (out=="tot_here"){
    name <- paste("figs\\", expo, "_tothere_overall.pdf", sep="")
    name_sp <- paste("figs\\", expo, "_tothere_sp.pdf", sep="")
    desc <- "Fire trucks and ambulances dispatched"
  } else if (out=="fire"){
    name <- paste("figs\\", expo, "_fire_overall.pdf", sep="")
    name_sp <- paste("figs\\", expo, "_fire_sp.pdf", sep="")
    desc <- "Fire trucks dispatched"
  } else if (out=="amb"){
    name <- paste("figs\\", expo, "_amb_overall.pdf", sep="")
    name_sp <- paste("figs\\", expo, "_amb_sp.pdf", sep="")
    desc <- "Ambulances dispatched"
  }
  
  ################################################################################
  # GAM WITH DOUBLY VARYING PENALTY ON THE LAG
  
  # DEFINE THE CROSS-BASIS
  cbgam2 <- crossbasis(temp,lag=6,argvar=list(fun="ps",df=5),
                       arglag=list(fun="ps",df=5))
  summary(cbgam2)
  cbgam2Pen <- cbPen(cbgam2)
  
  gam2 <- gam(as.formula(paste(out,"~ cbgam2+sin(harm_d)+cos(harm_d)+as.factor(yr)+as.factor(wkday)")),family=quasipoisson(),df,
              paraPen=list(cbgam2=cbgam2Pen), method='REML')
  
  # graph parameters
  border <- NULL
  col3d <- "slategray1"
  col <- 2
  
  # get cross prediction
  pred3dgam2 <- crosspred(cbgam2,gam2,at=ceiling(min(temp)):floor(max(temp)),cen=refer)
  predslgam2 <- crosspred(cbgam2,gam2,at=seq(ceiling(min(temp)),floor(max(temp)),by=0.01),bylag=1,cen=refer)
  
  # plot supplemental figures
  pdf(name,height=5,width=9)
  layout(matrix(1:2,ncol=2,byrow=TRUE))
  
  plot(pred3dgam2,xlab="Maximum heat index (C)",zlab=paste("RR (", refer, "C ref)", sep=""),zlim=zlim_lab,xlim=c(min(temp), max(temp)),
       theta=140, phi=20, lphi=30,shade=0.75,cex.axis=0.7,cex.lab=1,
       border=border,col=col3d,main="Exposure-lag-response")
  mtext(desc,cex=1)
  
  plot(predslgam2,"overall",ylab=paste("RR (", refer, "C ref)", sep=""),xlab=paste("M", substr(expo,2,3), "imum heat index (C)", sep=""),xlim=c(min(temp), max(temp)),
       ylim=cum_lim,lwd=1.5,col=col,main="Overall cumulative exposure-response")
  mtext(desc,cex=1)
  dev.off()
  
  pdf(name_sp,height=7,width=10)
  layout(matrix(1:2,ncol=2,byrow=TRUE))
  
  plot(predslgam2,var=as.character(per[1]),xlab="Lag (days)",ylab=paste("RR (", refer, "C ref)", sep=""),ylim=fig_lim,
       lwd=1.5,col=col,main=paste("Lag-response at ", exposure, " heat index ", per[1], "C ", per_lab[1], sep=""))
  mtext(desc,cex=0.6)
  
  plot(predslgam2,var=as.character(per[2]),xlab="Lag (days)",ylab=paste("RR (", refer, "C ref)", sep=""),ylim=fig_lim,
       lwd=1.5,col=col,main=paste("Lag-response at ", exposure, " heat index ", per[2], "C", per_lab[2], sep=""))
  mtext(desc,cex=0.6)
  dev.off()
  
}
