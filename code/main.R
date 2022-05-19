# load libraries and source functions
library(janitor)
library(rmarkdown)
library(tidyverse)

# remove current objects from workspace
rm(list=ls()) 

source('code/methods.R')

################   Inputs   #####################

# List files to be loaded and the total number of trials
et_file <- 'data/Pranav_pilot_new/raw/Pranav1EyeTrackerData.csv'
stim_file <- 'data/Pranav_pilot_new/raw/Pranav1_with_correct.csv'

# list the participant ID and output directory for preprocessing reports
participant <- 'PranavNew'
report_dir <- 'reports'

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
render('code/generate_report.Rmd', 
       output_format = html_document(title = participant), 
       output_dir = report_dir,
       output_file = paste0(participant,'.html'))
