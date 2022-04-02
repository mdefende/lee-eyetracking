# Load libraries
library(tidyverse)

# set time frames as window lengths
file <- 'data/hf_wander/raw/raw_combined.csv'
prefix_l <- 200 # number of samples (ms) in the pre-fixation window
fix_l <- 100 # window size for the fixation time block
stim_w <- 200 #set stimulus block averaging window (i.e. every stim_w samples will be averaged together)
min_pupil_size <- 100 # set the minimum average pupil size to keep for each sample. 
fix_norm <- TRUE # if true, normalizes stimulus data to fixation values. If false, normalizes to prefixation values

# read in data
raw <- read_csv(file) %>%
  mutate(block = replace_na(block,'instructions'),
         trials_this_n = replace_na(trials_this_n,0)) %>%
  mutate(across(average_acceleration_x:timestamp, as.numeric), # coerce eyetracking data to numeric.
         across(contains(c('in_saccade','in_blink')), as.logical)) 

############# Prefixation ################

# Prefixation is calculated using the texttime block prior to the fixation block
# for the next trial. The only exception is for the first trial where the last
# prefix_l samples of the instructions will be used instead

prefix <- raw %>%
  filter(block %in% c('instructions','texttime')) %>%
  select(average_pupil_size,left_pupil_size,right_pupil_size,average_gaze_x,trials_this_n,block) %>%
  mutate(trials_this_n = ifelse(block == 'instructions',0,trials_this_n + 1)) %>%
  filter(trials_this_n != max(trials_this_n)) # remove the last prefixation block before the experiment ends

# grab last prefix_l samples per trial, give mean, median, max, min, and sd
prefix_s <- prefix %>%
  mutate(trials_this_n = as.factor(trials_this_n)) %>%
  group_by(trials_this_n) %>%
  slice_tail(n = prefix_l) %>%
  filter(!is.na(average_gaze_x)) %>%
  summarize(across(contains('pupil_size'),
                   .fns = list(mean = ~mean(., na.rm = TRUE), 
                               median = ~median(., na.rm = TRUE),
                               min = ~min(., na.rm = TRUE),
                               max = ~max(., na.rm = TRUE),
                               sd = ~sd(., na.rm = TRUE)),
                   .names = 'pf_{.fn}_{.col}'),
            pf_n = sum(!is.na(average_pupil_size)))

rm(prefix)

############ Fixation ################

fix <- raw %>%
  filter(block == 'fixationtime') %>%
  select(average_pupil_size,left_pupil_size,right_pupil_size,average_gaze_x,trials_this_n,block)

# grab last prefix_l samples per trial, give mean, median, and sd
fix_s <- fix %>%
  mutate(trials_this_n = as.factor(trials_this_n)) %>%
  group_by(trials_this_n) %>%
  slice_tail(n = fix_l) %>%
  filter(!is.na(average_gaze_x)) %>%
  summarize(across(contains('pupil_size'),
                   .fns = list(mean = ~mean(., na.rm = TRUE), 
                               median = ~median(., na.rm = TRUE),
                               min = ~min(., na.rm = TRUE),
                               max = ~max(., na.rm = TRUE),
                               sd = ~sd(., na.rm = TRUE)),
                   .names = 'fix_{.fn}_{.col}'),
            fix_n = sum(!is.na(average_pupil_size)))

rm(fix)

########### Stimulus ################

# divide each stimulus block into epochs determined by the window size set at
# the beginning of the script.
stim <- raw %>%
  filter(block == 'stimulustime') %>%
  group_by(trials_this_n) %>% 
  mutate(epoch = cut(timestamp,
                     seq(min(timestamp),max(timestamp),stim_w),
                     include.lowest = TRUE, 
                     right = FALSE, 
                     labels = FALSE))

# count the number of samples with NA assigned as the epoch value. then
# determine the number of the last assigned epoch. For the samples with NA
# assigned as the epoch, if the number of those samples is greater than half the
# window size, they will be assigned to a new epoch, otherwise they will be
# grouped with the last epoch of the block
nas <- stim %>% 
  group_by(trials_this_n) %>% 
  summarize(num_na = sum(is.na(epoch)),
            max_epoch = max(epoch, na.rm = TRUE)) %>%
  mutate(na_rep = ifelse(num_na > stim_w/2, max_epoch + 1, max_epoch))


# add the NA contingent epoch information back to the stimulus set, and replace
# the NA epoch values with the correct epoch assignment
stim <- left_join(stim, nas, by = "trials_this_n") %>%
  mutate(epoch = ifelse(is.na(epoch),na_rep,epoch)) %>%
  select(-num_na,-max_epoch,-na_rep) %>%
  ungroup()

rm(nas)


stim_s <- stim %>%
  filter(between(average_gaze_x, 540, 1380), between(average_gaze_y, 270,810)) %>%
  group_by(trials_this_n,epoch) %>% 
  summarize(across(c(stimuli:group,key_resp_rt,key_resp_corr), unique),
            across(.cols = contains(c('pupil_size','gaze_x','gaze_y')),
                   .fns = list(mean = ~mean(.x, na.rm = TRUE),
                               med = ~median(.x, na.rm = TRUE),
                               sd = ~sd(.x, na.rm = TRUE)),
                   .names = 'stim_{.fn}_{.col}'),
            stim_n = n()) %>%
  relocate(correct_ans, .after = key_resp_corr) %>%
  mutate(trials_this_n = as.factor(trials_this_n))

############## Combine Prefixation, Fixation, and Stimulus Together ##############

df <- left_join(stim_s,prefix_s,by = "trials_this_n") %>%
  left_join(fix_s, by = "trials_this_n")

# normalize the left and right pupil sizes to the prefixation and fixation pupil sizes. Using medians for all here for now
df <- df %>%
  mutate(stim_med_average_pupil_norm_fix = stim_med_average_pupil_size/fix_median_average_pupil_size,
         stim_med_average_pupil_norm_pf = stim_med_average_pupil_size/pf_median_average_pupil_size,
         stim_med_left_pupil_norm_fix = stim_med_left_pupil_size/fix_median_left_pupil_size,
         stim_med_left_pupil_norm_pf = stim_med_left_pupil_size/pf_median_left_pupil_size,
         stim_med_right_pupil_norm_fix = stim_med_right_pupil_size/fix_median_right_pupil_size,
         stim_med_right_pupil_norm_pf = stim_med_right_pupil_size/pf_median_right_pupil_size) %>%
  select(-starts_with(c('fix','pf')))

outpath <- dirname(file) %>% str_replace(.,'raw','preprocessed')
dir.create(outpath, showWarnings = FALSE)

write_csv(df,file.path(outpath,'stimulus_averaged_eyetracking_outputs.csv'))
write_csv(fix_s,file.path(outpath,'fixation_summary.csv'))
write_csv(prefix_s,file.path(outpath,'prefixation_summary.csv'))
