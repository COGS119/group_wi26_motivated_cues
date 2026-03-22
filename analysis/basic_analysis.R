library(here)
library(tidyverse)
library(jsonlite)
library(lme4)
library(lmerTest)
library(car)

processed_data_directory <- here("..","data","processed_data")
file_name <- "motivated_cues"

processed_data <- read_csv(here(processed_data_directory,paste0(file_name,"-processed-data.csv"))) 

#further data prepping
processed_data <- processed_data %>%
  filter(is.na(practice)) %>%
  #remove outliers (fairly arbitrary)
  filter(rt<=5000) %>%
  #add column for label vs. sound
  mutate(
    trial_kind = case_when(
      str_detect(audio,"label") ~ "label",
      str_detect(audio,"sound") ~ "sound",
      TRUE ~ NA_character_
    )
  )

#average subject ratings
avg_subj <- processed_data %>%
  group_by(participant_id,correct_response,trial_kind,congruent) %>%
  summarize(
    N=n(),
    avg_accuracy = mean(correct,na.rm=T),
    avg_rt = mean(rt[correct],na.rm=T)
  ) 

overall_avg <- avg_subj %>%
  group_by(correct_response,trial_kind,congruent) %>%
  summarize(
    N=n(),
    mean_accuracy=mean(avg_accuracy),
    sd_accuracy = sd(avg_accuracy),
    sem_accuracy = sd_accuracy / sqrt(N),
    mean_rt = mean(avg_rt),
    sd_rt = sd(avg_rt),
    sem_rt = sd_rt / sqrt(N)
  ) %>%
  mutate(
    condition_ls=case_when(
      trial_kind == "label" ~ "label",
      trial_kind == "sound" & congruent == "yes" ~ "Congruent Sound",
      trial_kind == "sound" & congruent == "no" ~ "Incongruent Sound"
    )
  )

ggplot(overall_avg,aes(condition_ls,mean_rt,fill=condition_ls))+
  geom_bar(stat="identity",width=0.5)+
  geom_errorbar(aes(ymin=mean_rt-sem_rt,ymax=mean_rt+sem_rt),width=0.1,color="black")+
  facet_wrap(~correct_response)

processed_data <- processed_data %>%
  mutate(
    condition_ls=case_when(
      trial_kind == "label" ~ "label",
      trial_kind == "sound" & congruent == "yes" ~ "Congruent Sound",
      trial_kind == "sound" & congruent == "no" ~ "Incongruent Sound"
    )
  )

m <- lmer(rt ~ condition_ls + (1|participant_id), data=processed_data)
summary(m)
Anova(m,type="III")
 