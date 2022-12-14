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

#################  Pipeline ######################

# align eyetracking and stimulus timepoints and calculate the total number of trials
combined <- match_et_with_stim(et_file, stim_file)
num_trials <- max(combined$remember_loop_this_trial_n) + 1

### Preprocess eyetracking

## Calculate Fixation
window_length <- 200 # the last window_length samples in each fixation block are used for data normalization
fix_df <- calc_fixation(combined,
                        fix_l = window_length)

## Normalize Raw
combined_n <- normalize_raw(combined, 
                            fix = fix_df,
                            trials = num_trials)
## Calculate Stimulus
stim_df <- calc_stimulus(combined_n)

## Save preprocessed data for later and for use in generating the report
save_data(combined, combined_n, stim_df, fix_df, window_length, participant)

## Generate Preprocessing Report
render_report(participant)
