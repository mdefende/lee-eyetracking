# Load Required Libraries
library(janitor)
library(tidyverse)

# List files to be loaded and the output file
et_file <- 'G:/My Drive/Work/Eyetracking/data/hf_focus/raw/eyetracking_raw.csv'
stim_file <- 'G:/My Drive/Work/Eyetracking/data/hf_focus/raw/stimulus.csv'
out_file <- 'G:/My Drive/Work/Eyetracking/data/hf_focus/raw/raw_combined.csv'

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
  fill(stimuli:block, .direction = 'down')

write_csv(v, out_file)
