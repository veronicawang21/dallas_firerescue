###################################
######## Process results ##########
###################################

rm(list = ls())

setwd("C:\\Users\\veron\\OneDrive - Harvard University\\smartsurfaces\\Dallas_911calls\\results_lin\\")

library(tidyverse)
library(ggh4x)

expo <- c("max", "min")
out_list <- c("call", "tot", "tot_here", "fire", "amb")

# load in linear results
res <- readRDS(paste(expo[1], "_", out_list[1], ".rds", sep="")) %>%
  rbind(readRDS(paste(expo[1], "_", out_list[2], ".rds", sep=""))) %>%
  rbind(readRDS(paste(expo[1], "_", out_list[3], ".rds", sep=""))) %>%
  rbind(readRDS(paste(expo[1], "_", out_list[4], ".rds", sep=""))) %>%
  rbind(readRDS(paste(expo[1], "_", out_list[5], ".rds", sep=""))) %>%
  rbind(readRDS(paste(expo[2], "_", out_list[1], ".rds", sep=""))) %>%
  rbind( readRDS(paste(expo[2], "_", out_list[2], ".rds", sep=""))) %>%
  rbind(readRDS(paste(expo[2], "_", out_list[3], ".rds", sep=""))) %>%
  rbind(readRDS(paste(expo[2], "_", out_list[4], ".rds", sep=""))) %>%
  rbind(readRDS(paste(expo[2], "_", out_list[5], ".rds", sep="")))

# estimates to report in results section of manuscript
rep <- res %>%
  filter(lag %in% c("cum01", "cum06"),
         out %in% c("call")) %>%
  mutate(est5=round(est^5,3),
         lo5=round(lo^5,3),
         hi5=round(hi^5,3))

res_sec <- readRDS(paste(expo[1], ".rds", sep="")) %>%
  rbind(readRDS(paste(expo[2], ".rds", sep="")))

###################################
######## Plot #####################
###################################

### by lag
pdf("fig1.pdf", width = 14, height = 6)
res %>%
  filter(!(lag %in% c("cum0", "cum01", "cum06"))) %>%
  mutate(out_nest=ifelse(out %in% c("call", "tot"), "All", "Specific vehicle types"),
         out=factor(out, levels=c("call", "tot", "tot_here", "fire", "amb")),
         out=recode(out,
                    "call" = "Dispatched calls",
                    "tot" = "Total emergency vehicles",
                    "tot_here" = "Fire trucks and ambulances",
                    "fire" = "Fire trucks only",
                    "amb" = "Ambulances only"),
         expo=recode(expo,
                     "maximum" = "Maximum heat index",
                     "minimum" = "Minimum heat index")) %>%
  ggplot(aes(x=lag, y=est^5, group=expo, color=expo, fill=expo)) +
  geom_hline(yintercept = 1, linetype="dashed", color="grey") +
  geom_line() +
  geom_ribbon(aes(ymax=hi^5, ymin=lo^5), alpha=0.1, color=NA) +
  geom_point(aes(shape=expo), size=2.5) +
  labs(x="Exposure lags", y="IRR per 5°C") +
  facet_nested(~out_nest+out, scales = "free_x") +
  theme_test() +
  scale_color_manual(values = c("Maximum heat index"="black", "Minimum heat index"="grey60")) +
  scale_fill_manual(values = c("Maximum heat index"="black", "Minimum heat index"="grey60")) +
  scale_x_continuous(breaks = c(0:6)) +
  scale_y_continuous(breaks = seq(0.99, 1.04, by=0.01), limits = c(0.987, 1.04)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom")
dev.off()

### cumulative
pdf("fig2.pdf", width = 12, height = 6)
res %>%
  filter(lag %in% c("cum0", "cum01", "cum06")) %>%
  mutate(out_nest=ifelse(out %in% c("call", "tot"), "All", "Specific vehicle types"),
         out=factor(out, levels=c("call", "tot", "tot_here", "fire", "amb")),
         out=recode(out,
                    "call" = "Dispatched calls",
                    "tot" = "Total emergency vehicles",
                    "tot_here" = "Fire trucks and ambulances",
                    "fire" = "Fire trucks only",
                    "amb" = "Ambulances only"),
         expo=recode(expo,
                     "maximum" = "Maximum heat index",
                     "minimum" = "Minimum heat index"),
         lag=recode(lag,
                    "cum0"="0",
                    "cum01"="0-1",
                    "cum06"="0-6")) %>%
  ggplot(aes(x=lag, y=est^5, color=expo)) +
  geom_hline(yintercept = 1, linetype="dashed", color="grey") +
  geom_errorbar(aes(ymax=hi^5, ymin=lo^5), position = position_dodge(width = 0.5), width=0) +
  geom_point(aes(shape=expo), position = position_dodge(width = 0.5), size=2.5) +
  labs(x="Cumulative exposure lags", y="IRR per 5°C") +
  facet_nested(~out_nest+out, scales = "free_x") +
  scale_y_continuous(breaks = seq(0.99, 1.04, by=0.01), limits = c(0.987, 1.04)) +
  theme_test() +
  scale_color_manual(values = c("Maximum heat index"="black", "Minimum heat index"="grey60")) +
  theme(legend.title = element_blank(),
        legend.position = "bottom")
dev.off()

### over time
pdf("figs1.pdf", width = 12, height = 10)
res_sec %>%
  filter(lag %in% c("cum0", "cum01", "cum06")) %>%
  mutate(out_nest=ifelse(out %in% c("call", "tot"), "All", "Specific vehicle types"),
         out=factor(out, levels=c("call", "tot", "tot_here", "fire", "amb")),
         out=recode(out,
                    "call" = "Dispatched calls",
                    "tot" = "Total emergency vehicles",
                    "tot_here" = "Fire trucks and ambulances",
                    "fire" = "Fire trucks only",
                    "amb" = "Ambulances only"),
         expo=recode(expo,
                     "maximum" = "Maximum heat index",
                     "minimum" = "Minimum heat index"),
         lag=recode(lag,
                    "cum0"="0",
                    "cum01"="0-1",
                    "cum06"="0-6"),
         years=case_when(
           years==1 ~ "2011-2015",
           years==2 ~ "2016-2020",
           TRUE ~ "2021-2023")) %>%
  ggplot(aes(x=lag, y=est^5, color=expo)) +
  geom_hline(yintercept = 1, linetype="dashed", color="grey") +
  geom_errorbar(aes(ymax=hi^5, ymin=lo^5), position = position_dodge(width = 0.5), width=0) +
  geom_point(aes(shape=expo), position = position_dodge(width = 0.5), size=2.5) +
  labs(x="Cumulative exposure lags", y="IRR per 5°C") +
  facet_nested(years~out_nest+out, scales = "free_x") +
  # scale_y_continuous(breaks = seq(0.99, 1.04, by=0.01), limits = c(0.987, 1.04)) +
  theme_test() +
  scale_color_manual(values = c("Maximum heat index"="black", "Minimum heat index"="grey60")) +
  theme(legend.title = element_blank(),
        legend.position = "bottom")
dev.off()
