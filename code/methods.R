match_et_with_stim <- function (et_file, stim_file, save_output = FALSE, max_blocks = 35) {

# Load Required Libraries
library(janitor)
library(tidyverse)

######################## Eyetracking File ##################################
# read in the csv file. columns with '.' (and '[]') as the only unique value
# will be removed, as these are empty columns
et <- read_csv(et_file) %>%
  clean_names() %>%
  select(where(~ !all(. %in% c('.','[]'))))

# remove some other seemingly unnecessary columns
et <- et %>%
  select(-data_file, -eye_tracked, -starts_with('ip'), -sample_index, -sample_message, -trial_index, -trial_label, -trial_start_time)

####################### Stimulus File ######################################
# read in stimulus file
stim <- read_csv(stim_file) %>%
  clean_names()

# remove first row as it corresponds to instructions time.
stim <- stim %>%
  filter(!is.na(stimuli))

# remove any completely empty columns
stim <- stim %>%
  select(where(~ !all(is.na(.))))

# remove the timeOff columns. some reporting is off and needs to be looked at.
# also remove response time, it's being merged with the texttime block
stim <- stim %>%
  select(-contains('time_off'), -et_response)

stim <- stim %>%
  select(stimuli:trials_this_n, contains('et_'),key_resp_rt,key_resp_keys,key_resp_corr) %>%
  pivot_longer(starts_with('et_'), names_to = 'block', values_to = 'timestamp') %>%
  mutate(block = str_remove(block,'et_'))


#################### Combine and Export ###################################
v <- left_join(et,stim, by = 'timestamp') %>%
  relocate(stimuli:block, .after = recording_session_label) %>%
  fill(stimuli:block, .direction = 'down') %>%
  mutate(block = replace_na(block,'instructions'),
         block = str_remove(block,'time'),
         block = factor(block),
         trials_this_n = replace_na(trials_this_n,0),
         trials_this_n = factor(trials_this_n, levels = seq(0,max_blocks-1,1))) %>%
  mutate(across(average_acceleration_x:timestamp, as.numeric), # coerce eyetracking data to numeric.
         across(contains(c('in_saccade','in_blink')), as.logical)) 

if (save_output){
  outdir <- dirname(et_file)
  write_csv(v, file.path(outdir,'raw_combined.csv'))
}

return(v)
}

calc_prefixation <- function(raw, prefix_l = 200){
  
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
  
  return(prefix_s)
}

calc_fixation <- function(raw, fix_l = 100){
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
  
  return(fix_s)
}

calc_stimulus <- function (raw, stim_w = 200){
  
  # divide each stimulus block into epochs determined by the window size stim_w
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

  return(stim_s)
}

normalize_stimulus <- function(stim, prefix = NULL, fix = NULL, outfile = NULL){
  
  # combine stim with prefixation data, fixation, or both
  if (!is.null(prefix)){
    stim <- left_join(stim,prefix,by = "trials_this_n") %>%
      mutate(stim_med_average_pupil_norm_pf   = stim_med_average_pupil_size/pf_median_average_pupil_size,
             stim_med_left_pupil_norm_pf      = stim_med_left_pupil_size/pf_median_left_pupil_size,
             stim_med_right_pupil_norm_pf     = stim_med_right_pupil_size/pf_median_right_pupil_size,
             stim_mean_average_pupil_norm_pf  = stim_mean_average_pupil_size/pf_mean_average_pupil_size,
             stim_mean_left_pupil_norm_pf     = stim_mean_left_pupil_size/pf_mean_left_pupil_size,
             stim_mean_right_pupil_norm_pf    = stim_mean_right_pupil_size/pf_mean_right_pupil_size) %>%
      select(-starts_with('pf'))
  }
  
  if (!is.null(fix)){
    stim <- left_join(stim,fix,by = "trials_this_n") %>%
      mutate(stim_med_average_pupil_norm_fix  = stim_med_average_pupil_size/fix_median_average_pupil_size,
             stim_med_left_pupil_norm_fix     = stim_med_left_pupil_size/fix_median_left_pupil_size,
             stim_med_right_pupil_norm_fix    = stim_med_right_pupil_size/fix_median_right_pupil_size,
             stim_mean_average_pupil_norm_fix = stim_mean_average_pupil_size/fix_mean_average_pupil_size,
             stim_mean_left_pupil_norm_fix    = stim_mean_left_pupil_size/fix_mean_left_pupil_size,
             stim_mean_right_pupil_norm_fix   = stim_mean_right_pupil_size/fix_mean_right_pupil_size) %>%
      select(-starts_with('fix'))
  }
  
  # normalize pupil sizes to the prefixation and fixation pupil sizes, depending on what is passed in
    if (!is.null(outfile)){
    if (dirname(outfile) != '.'){
      dir.create(dirname(outfile), showWarnings = FALSE)
    }
    
    write_csv(stim, outfile)
  }
  
  return(stim)
}