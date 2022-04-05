# load libraries and source functions
library(janitor)
library(rstatix)
library(rmarkdown)
library(tidyverse)

# remove current objects from workspace
rm(list=ls()) 

source('code/methods.R')

################   Inputs   #####################

# List files to be loaded and the total number of trials
et_file <- 'data/bp_wander/raw/eyetracking_raw.csv'
stim_file <- 'data/bp_wander/raw/stimulus.csv'
num_trials <- 35

# list the participant ID and output directory for preprocessing reports
participant <- 'BP_Wander'
report_dir <- 'reports'

combined <- match_et_with_stim(et_file,stim_file)


### Preprocess eyetracking

## Calculate Prefixation

# Prefixation is calculated using the texttime block prior to the fixation block
# for the next trial. The only exception is for the first trial where the last
# prefix_l samples of the instructions will be used instead

prefix_df <- calc_prefixation(combined)

## Calculate Fixation

fix_df <- calc_fixation(combined)

## Normalize Raw

combined_n <- normalize_raw(combined, 
                            prefix = prefix_df, 
                            fix = fix_df,
                            trials = num_trials)
## Calculate Stimulus

stim_df <- calc_stimulus(combined_n)

## Generate Preprocessing Report
render('code/generate_report.Rmd', 
       output_format = html_document(title = participant), 
       output_dir = report_dir,
       output_file = paste0(participant,'.html'))
