###################### Load Libraries #######################
library(rstatix)
library(tidyverse)

###################### Set Functions ########################

prep_raw <- function(file) {
  file %>%
    read_csv() %>%
    mutate(block = replace_na(block,'instructions'),
           trials_this_n = replace_na(trials_this_n,0)) %>%
    mutate(across(average_acceleration_x:timestamp, as.numeric), # coerce eyetracking data to numeric.
           across(contains(c('in_saccade','in_blink')), as.logical), # change logical data to logical format
           across(contains('gaze_x'), ~ . - 1920/2), # center x and y gaze coordinates based on a 1920x1080 monitor size
           across(contains('gaze_y'), ~ . - 1080/2),
           average_gaze_dist = sqrt(average_gaze_x^2 + average_gaze_y^2)) %>% # add gaze distance from center
    relocate(average_gaze_dist, .after = average_gaze_y)
}

plot_gaze_position <- function(df) {
  df %>%
    filter(block != 'instructions', !is.na(average_gaze_x)) %>%
    select(average_gaze_x, average_gaze_y) %>%
    pivot_longer(cols = everything(), names_to = 'type', values_to = 'location') %>%
    ggplot(aes(x = location)) +
    geom_density() +
    facet_wrap(~type) +
    theme_minimal()
}


######################## BP Focus ###########################

# set the files to load
bp_focus_raw <- prep_raw('G:/My Drive/Work/Eyetracking/data/bp_focus/raw/raw_combined.csv')
bp_focus_preproc <- read_csv('G:/My Drive/Work/Eyetracking/data/bp_focus/preprocessed/stimulus_averaged_eyetracking_outputs.csv')

## Checking gaze location and how often gaze was found to be within the boundaries of the screen
bp_focus_raw %>%
  filter(block != 'instructions', !is.na(average_gaze_x)) %>%
  summarize(avg_within = sum(between(average_gaze_x,-960,960) & between(average_gaze_y,-540,540))/n())

bp_focus_raw %>%
  plot_gaze_position()

## Check correlations between gaze distance from center of screen and pupil size

bp_focus_raw %>%
  filter(!is.na(average_gaze_x), !is.na(average_pupil_size), between(average_gaze_x,-960,960), between(average_gaze_y,-540,540)) %>%
  cor_test(average_gaze_dist, average_pupil_size)

bp_focus_raw %>%
  filter(!is.na(average_gaze_x), !is.na(average_pupil_size), between(average_gaze_x,-960,960), between(average_gaze_y,-540,540)) %>%
  ggplot(aes(x = average_gaze_dist, y = average_pupil_size)) +
  geom_smooth(method = 'lm') +
  theme_minimal()

## Plot average pupil size by epoch, facet by trial number, and save to an image

p <- bp_focus_preproc %>%
  filter(!is.na(stim_med_average_pupil_norm_fix)) %>%
  ggplot(aes(x = epoch, y = stim_med_average_pupil_norm_fix)) +
  geom_point() +
  facet_wrap(~trials_this_n) +
  labs(x = 'Epoch', y = 'Normalized Median Pupil Size') +
  theme_bw()

ggsave('C:/Users/mkdef/Desktop/bp_focus_pupil_plots.png',p)

## check pupil size differences between left and right eye
bp_focus_raw %>%
  filter(block != 'instructions', !is.na(average_pupil_size)) %>%
  ggplot() + 
  geom_density(aes(x = left_pupil_size), color = 'red') + 
  geom_density(aes(x = right_pupil_size), color = 'blue') + 
  theme_bw()

######################## BP Wander ###########################

# set the files to load
bp_wander_raw <- prep_raw('G:/My Drive/Work/Eyetracking/data/bp_wander/raw/raw_combined.csv')
bp_wander_preproc <- read_csv('G:/My Drive/Work/Eyetracking/data/bp_wander/preprocessed/stimulus_averaged_eyetracking_outputs.csv')

## Checking gaze location and how often gaze was found to be within the boundaries of the screen
bp_wander_raw %>%
  filter(block != 'instructions', !is.na(average_gaze_x)) %>%
  summarize(avg_within = sum(between(average_gaze_x,-960,960) & between(average_gaze_y,-540,540))/n())

bp_wander_raw %>%
  plot_gaze_position()

## Check correlations between gaze distance from center of screen and pupil size

bp_wander_raw %>%
  filter(!is.na(average_gaze_x), !is.na(average_pupil_size), between(average_gaze_x,-960,960), between(average_gaze_y,-540,540)) %>%
  cor_test(average_gaze_dist, average_pupil_size)

bp_wander_raw %>%
  filter(!is.na(average_gaze_x), !is.na(average_pupil_size), between(average_gaze_x,-960,960), between(average_gaze_y,-540,540)) %>%
  ggplot(aes(x = average_gaze_dist, y = average_pupil_size)) +
  geom_smooth(method = 'lm') +
  theme_minimal()

## Plot average pupil size by epoch, facet by trial number, and save to an image

p <- bp_wander_preproc %>%
  filter(!is.na(stim_med_average_pupil_norm_fix)) %>%
  ggplot(aes(x = epoch, y = stim_med_average_pupil_norm_fix)) +
  geom_point() +
  facet_wrap(~trials_this_n) +
  labs(x = 'Epoch', y = 'Normalized Median Pupil Size') +
  theme_bw()

ggsave('C:/Users/mkdef/Desktop/bp_wander_pupil_plots.png',p)

## check pupil size differences between left and right eye
bp_focus_raw %>%
  filter(block != 'instructions', !is.na(average_pupil_size)) %>%
  ggplot() + 
  geom_density(aes(x = left_pupil_size), color = 'red') + 
  geom_density(aes(x = right_pupil_size), color = 'blue') + 
  theme_bw()

######################## HF Wander ###########################

# set the files to load
hf_wander_raw <- prep_raw('G:/My Drive/Work/Eyetracking/data/hf_wander/raw/raw_combined.csv')
hf_wander_preproc <- read_csv('G:/My Drive/Work/Eyetracking/data/hf_wander/preprocessed/stimulus_averaged_eyetracking_outputs.csv')

## Checking gaze location and how often gaze was found to be within the boundaries of the screen
hf_wander_raw %>%
  filter(block != 'instructions', !is.na(average_gaze_x)) %>%
  summarize(avg_within = sum(between(average_gaze_x,-420,420) & between(average_gaze_y,-270,270))/n())

hf_wander_raw %>%
  plot_gaze_position()

## Check correlations between gaze distance from center of screen and pupil size

hf_wander_raw %>%
  filter(!is.na(average_gaze_x), !is.na(average_pupil_size), between(average_gaze_x,-960,960), between(average_gaze_y,-540,540)) %>%
  cor_test(average_gaze_dist, average_pupil_size)

hf_wander_raw %>%
  filter(!is.na(average_gaze_x), !is.na(average_pupil_size), between(average_gaze_x,-960,960), between(average_gaze_y,-540,540)) %>%
  ggplot(aes(x = average_gaze_dist, y = average_pupil_size)) +
  geom_smooth(method = 'lm') +
  theme_minimal()

## Plot average pupil size by epoch, facet by trial number, and save to an image

p <- hf_wander_preproc %>%
  filter(!is.na(stim_med_average_pupil_norm_fix)) %>%
  ggplot(aes(x = epoch, y = stim_med_average_pupil_norm_fix)) +
  geom_point() +
  facet_wrap(~trials_this_n) +
  labs(x = 'Epoch', y = 'Normalized Median Pupil Size') +
  theme_bw()

ggsave('C:/Users/mkdef/Desktop/hf_wander_pupil_plots.png',p)


hf_wander_raw %>%
  filter(block == 'stimulustime', between(average_gaze_x,-420,420) & between(average_gaze_y,-270,270)) %>%
  ggplot(aes(x = average_gaze_x, y = -average_gaze_y, z = average_pupil_size)) + 
  stat_summary_hex(fun = 'mean') + 
  lims(x = c(-420,420),y = c ( -270,270))

bp_wander_raw %>%
  filter(block == 'stimulustime', between(average_gaze_x,-960,960) & between(average_gaze_y,-540,540)) %>%
  ggplot(aes(x = average_gaze_x, y = -average_gaze_y)) +
  geom_density_2d_filled() + 
  lims(x = c(-960,960),y = c (-540,540))
