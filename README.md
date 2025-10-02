# dallas_firerescue
Investigating the association between heat and dispatched calls and vehicles for Dallas Fire-Rescue services

## data files
PopWieghtPRISM_Dallas: folder contains temperature exposure files
\
  PopWeightPRISM_Dallas_tmax: Daily populated-weighted maximum temperature exposure averaged across Dallas city boundaries
  \
  PopWeightPRISM_Dallas_tmin: Daily populated-weighted minimum temperature exposure averaged across Dallas city boundaries
  \
  PopWeightPRISM_Dallas_vpdmax: Daily maximum vapor pressure deficit averaged across Dallas city boundaries
  \
  PopWeightPRISM_Dallas_vpdmin: Daily minimum vapor pressure deficit averaged across Dallas city boundaries
  \

## analyses R code
1_0_dallas_prep.R: Clean and merge all exposure and outcome files and calculate heat index
\
1_1_dallas_tab1.R: Summary statistics for Table 1
\
2_0_dallas_main_nonlin.R: Association between heat index and the number of dispatched calls and vehicles with penalized exposure-response and lag-response crossbases for entire study period (and create supplemental figures)
\
2_1_dallas_main_lin.R: Association between heat index and the number of dispatched calls and vehicles with penalized lag-response and linear exposure-response crossbases for entire study period
\
2_2_dallas_sec_lin.R: Association between heat index and the number of dispatched calls and vehicles with penalized lag-response and linear exposure-response crossbases for by time periods (2011-2015, 2016-2020, 2021-2023) (Secondary analyses)
\
3_0_process_results.R: Process results and create main figures
