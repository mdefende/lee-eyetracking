# load libraries and source functions
library(janitor)
library(rstatix)
library(tidyverse)

source('code/methods.R')

## Matching eyetracking and Stimulus timepoints

# List files to be loaded and the output file
et_file <- 'data/hf_focus/raw/eyetracking_raw.csv'
stim_file <- 'data/hf_focus/raw/stimulus.csv'

combined <- match_et_with_stim(et_file,stim_file)


### Preprocess eyetracking

## Calculate Prefixation

# Prefixation is calculated using the texttime block prior to the fixation block
# for the next trial. The only exception is for the first trial where the last
# prefix_l samples of the instructions will be used instead

prefix_df <- calc_prefixation(combined)

## Calculate Fixation

fix_df <- calc_fixation(combined)

## Calculate Stimulus

stim_df <- calc_stimulus(combined)

## Normalize stimulus

stim_n <- normalize_stimulus(stim_df, 
                             prefix = prefix_df, 
                             fix = fix_df, 
                             outfile = 'data/hf_focus/preproc/stimulus_averaged_eyetracking_outputs.csv')

render('code/generate_report.Rmd', output_format = 'html_document', output_file = 'test.html')
