library(here)
library(tidyverse)
library(jsonlite)

processed_data_directory <- here("..","data","processed_data")
file_name <- "motivated_cues"

#read experiment data
exp_data <- read_csv(here(processed_data_directory,paste0(file_name,"-alldata.csv")))

#code for dealing with atypical participant id storage
exp_data <- exp_data %>%
  #clean up participant ids
  rename(participant_id = participant) %>%
  mutate(
    participant_id = case_when(
      participant_id == "9252" ~ "parrot",
      participant_id == "A18534325" ~ "moose",
      participant_id == "A18552625" ~ "bunny",
      TRUE ~ trimws(tolower(participant_id))
    )
  )

#double check that participant ids are unique
counts_by_random_id <- exp_data %>%
  group_by(random_id,participant_id) %>%
  count()
#output to track participants
write_csv(counts_by_random_id,here(processed_data_directory,paste0(file_name,"-participant-list.csv")))


#filter dataset
exp_data <- exp_data %>%
  filter(!is.na(correct_response))

#filter participant ids
filter_ids <- c()

#identify participants from the experiment group
group_members <- c("beaver","turtle","antelope","otter","bunny","elk")

processed_data <- exp_data %>%
  filter(!(participant_id %in% filter_ids)) %>%
  #flag for group participants
  mutate(participant_is_group_member = case_when(
    participant_id %in% group_members ~ TRUE,
    TRUE ~ FALSE
  
  )) %>%
  #remove unneeded columns
  select(-c(success:failed_video,plugin_version,question_order)) %>%
  #add trial_number
  group_by(participant_id) %>%
  mutate(trial_number = row_number()) %>%
  relocate(trial_number,.after=trial_index)
  
#store processed and prepped data
write_csv(processed_data,here(processed_data_directory,paste0(file_name,"-processed-data.csv")))
