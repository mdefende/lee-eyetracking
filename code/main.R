# load libraries and source functions
library(janitor)
library(rmarkdown)
library(tidyverse)

# remove current objects from workspace
rm(list=ls()) 

source('code/methods.R')

################   Inputs   #####################

# List files to be loaded and the total number of trials
et_file <- 'data/TReD_2818/raw/TReD_2818_et.csv'
stim_file <- 'data/TReD_2818/raw/TReD_2818_stim.csv'

# list the participant ID and output directory for preprocessing reports
participant <- 'TReD_2818'
report_dir <- 'reports'

################ Pipeline #######################

combined <- match_et_with_stim(et_file,stim_file)
num_trials <- max(combined$remember_loop_this_trial_n) + 1


### Preprocess eyetracking

## Calculate Fixation
wl <- 200
fix_df <- calc_fixation(combined,
                        fix_l = wl)

## Normalize Raw
combined_n <- normalize_raw(combined, 
                            fix = fix_df,
                            trials = num_trials)
## Calculate Stimulus
stim_df <- calc_stimulus(combined_n)

## Generate Preprocessing Report
render_report(combined_n, stim_df, participant, wl, report_dir)
