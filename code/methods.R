check_stim_times <- function(stim_file) {
  inorder <- read_csv(stim_file, col_types = cols()) %>%
    clean_names() %>%
    select(starts_with('et_')) %>%
    remove_empty('rows') %>%
    pivot_longer(everything(), names_to = 'block',values_to = 'time') %>%
    mutate(sort_time = sort(time)) %>%
    summarize(inorder = all(time == sort_time))
  
  if(inorder$inorder){
    print('All reported block times are in order')
  } else {
    print('Reported eyetracking block times are not in order')
  }
}

match_et_with_stim <- function (et_file, stim_file,save_output = FALSE) {

# Load Required Libraries
library(janitor)
library(tidyverse)

######################## Eyetracking File ##################################
# read in the csv file. columns with '.' (and '[]') as the only unique value
# will be removed, as these are empty columns
et <- read_csv(et_file) %>%
  clean_names() %>%
  remove_empty('cols')

# remove some other seemingly unnecessary columns
et <- et %>%
  select(-data_file, -eye_tracked, -starts_with('ip'), -sample_index, -sample_message, -trial_index, -trial_label, -trial_start_time)

####################### Stimulus File ######################################
# read in stimulus file
stim <- read_csv(stim_file) %>%
  clean_names()

stim <- stim %>%
  filter(!is.na(remember_loop_this_trial_n)) %>%
  remove_empty('cols') %>%
  select(stimuli:remember_loop_this_trial_n, contains(c('et_','key','_rt','correct'))) %>%
  pivot_longer(starts_with('et_'), names_to = 'block', values_to = 'timestamp') %>%
  mutate(block = str_remove(block,'et_'))


#################### Combine and Export ###################################
v <- left_join(et,stim, by = 'timestamp') %>%
  relocate(stimuli:block, .after = recording_session_label) %>%
  fill(stimuli:timestamp, .direction = 'down') %>%
  mutate(block = str_remove(block,'time'),
         block = factor(block),
         across(average_acceleration_x:last_col(), as.numeric), # coerce eyetracking data to numeric.
         across(contains(c('in_saccade','in_blink')), as.logical)) %>%
  filter(!is.na(remember_loop_this_trial_n),!str_detect(block,'off')) %>% # remove timepoints pre stim 1 and during the off blocks (sim_off, fix_off, etc.)
  mutate(block = fct_drop(block)) %>%
  rename_with(~str_replace(.,'average','comb'),contains('average'))

if (save_output){
  outdir <- dirname(et_file)
  write_csv(v, file.path(outdir,'raw_combined.csv'))
}

return(v)
}

calc_fixation <- function(raw, fix_l = 100){
  ############ Fixation ################
  
  fix <- raw %>%
    filter(block == 'fixation') %>%
    select(comb_gaze_x,comb_gaze_y,comb_pupil_size,left_pupil_size,right_pupil_size,comb_gaze_x,remember_loop_this_trial_n,block) %>%
    group_by(remember_loop_this_trial_n) %>%
    mutate(remember_loop_this_trial_n = as.factor(remember_loop_this_trial_n),
           sample = row_number())
  
  # get the max sample number before filtering for calculating the number of missing samples
  ms <- fix %>%
    summarize(max_sample = max(sample))
  
  # grab last fix_l samples per trial, give mean, median, and sd
  fix_s <- fix %>%
    filter(between(comb_gaze_x,0,1920),between(comb_gaze_y,0,1080)) %>%
    slice_tail(n = fix_l) %>%
    summarize(across(contains('pupil_size'),
                     .fns = list(mean = ~mean(., na.rm = TRUE), 
                                 median = ~median(., na.rm = TRUE),
                                 min = ~min(., na.rm = TRUE),
                                 max = ~max(., na.rm = TRUE),
                                 sd = ~sd(., na.rm = TRUE)),
                     .names = 'fix_{.fn}_{.col}'),
              min_sample = min(sample))
  
  fix_s <- fix_s %>%
    left_join(ms, by = 'remember_loop_this_trial_n') %>%
    mutate(missing_samples = max_sample - (min_sample-1) - fix_l) %>%
    select(-max_sample, -min_sample)
  
  return(fix_s)
}

normalize_raw <- function(raw, fix, trials, outfile = NULL){
  
  # check if raw$remember_loop_this_trial_n is a factor, change to it if not
  if (!is.factor(raw$remember_loop_this_trial_n)){
    raw <- raw %>%
      mutate(remember_loop_this_trial_n = factor(remember_loop_this_trial_n, levels = seq(0,trials-1,1)))
  }
  
  raw <- raw %>%
      left_join(fix,by = "remember_loop_this_trial_n") %>%
      mutate(med_comb_pupil_norm_fix  = comb_pupil_size/fix_median_comb_pupil_size,
             med_left_pupil_norm_fix     = left_pupil_size/fix_median_left_pupil_size,
             med_right_pupil_norm_fix    = right_pupil_size/fix_median_right_pupil_size,
             mean_comb_pupil_norm_fix = comb_pupil_size/fix_mean_comb_pupil_size,
             mean_left_pupil_norm_fix    = left_pupil_size/fix_mean_left_pupil_size,
             mean_right_pupil_norm_fix   = right_pupil_size/fix_mean_right_pupil_size) %>%
      select(-starts_with('fix'))
 
  raw <- raw %>%
    relocate(contains('comb_pupil_norm'), .after = 'comb_pupil_size') %>%
    relocate(contains('left_pupil_norm'), .after = 'left_pupil_size') %>%
    relocate(contains('right_pupil_norm'), .after = 'right_pupil_size')
  
  if (!is.null(outfile)){
    if (dirname(outfile) != '.'){
      dir.create(dirname(outfile), showWarnings = FALSE)
    }
    
    write_csv(raw, outfile)
  }
  
  return(raw)
}

calc_stimulus <- function (raw, stim_w = 200){
  
  # divide each stimulus block into epochs determined by the window size stim_w
  stim <- raw %>%
    filter(block == 'stimulus') %>%
    group_by(remember_loop_this_trial_n) %>% 
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
    group_by(remember_loop_this_trial_n) %>% 
    summarize(num_na = sum(is.na(epoch)),
              max_epoch = max(epoch, na.rm = TRUE)) %>%
    mutate(na_rep = ifelse(num_na > stim_w/2, max_epoch + 1, max_epoch))
  
  
  # add the NA contingent epoch information back to the stimulus set, and replace
  # the NA epoch values with the correct epoch assignment
  stim <- left_join(stim, nas, by = "remember_loop_this_trial_n") %>%
    mutate(epoch = ifelse(is.na(epoch),na_rep,epoch)) %>%
    select(-num_na,-max_epoch,-na_rep) %>%
    ungroup()
  
  rm(nas)
  
  
  stim_s <- stim %>%
    filter(between(comb_gaze_x, 540, 1380), between(comb_gaze_y, 0,540)) %>%
    group_by(remember_loop_this_trial_n,epoch) %>% 
    summarize(across(c(stimuli:block), unique),
              across(.cols = c(contains(c('gaze_x','gaze_y')),ends_with('pupil_size')),
                     .fns = list(mean = ~mean(.x, na.rm = TRUE),
                                 med = ~median(.x, na.rm = TRUE),
                                 sd = ~sd(.x, na.rm = TRUE)),
                     .names = 'stim_{.fn}_{.col}'),
              
              across(.cols = starts_with('med'),
                     .fns = ~median(.x, na.rm = TRUE),
                     .names = 'stim_{.col}'),
              across(.cols = starts_with('med'),
                     .fns = ~sd(.x, na.rm = TRUE),
                     .names = 'stim_sd_{.col}'),
  
              across(.cols = starts_with('mean'),
                     .fns = ~median(.x, na.rm = TRUE),
                     .names = 'stim_{.col}'),
              across(.cols = starts_with('mean'),
                     .fns = ~sd(.x, na.rm = TRUE),
                     .names = 'stim_sd_{.col}'),
              
              stim_n = n())
  
  return(stim_s)
}

render_report <- function(combined_n, stim_df, participant, fix_l, report_dir) {
  #rmarkdown::render('code/generate_report.Rmd', 
  #                  params = list(combined_n = combined_n,
  #                                stim_df = stim_df,
  #                                fix_l = fix_l),
  #                  output_format = rmarkdown::html_document(title = participant), 
  #                  output_dir = report_dir,
  #                  output_file = paste0(participant,'.html'))
  
  quarto::quarto_render('code/generate_individual_report.qmd',
                        execute_params = list(combined_n = combined_n,
                                              stim_df = stim_df,
                                              fix_l = fix_l),
                        output_file = paste0('reports/',participant,'.html'))
}