library(haven)
library(dplyr)
library(missForest)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(forcats)
library(reshape2)
library(readxl)
library(tibble)

#----------------------------------
# 1. Data Loading & Question Setup
#----------------------------------

# load raw data
slvk_questions <- as_tibble(read_sav("~/projects/bustikova/data/raw_data/question/slovakia_raw.sav"))

slvk_questions <- slvk_questions %>% select(q6a_control, q6b_control, q6c_control,
                                      q6a_experiment, q6b_experiment, q6c_experiment)

# Convert response columns to numeric then recode: 1->4, 4->1, 2->3, 3->2
slvk_questions <- slvk_questions %>%
  mutate(across(everything(), ~ as.numeric(.x))) %>%
  mutate(across(everything(), ~ recode(.x,
                                       `1` = 4,
                                       `4` = 1,
                                       `2` = 3,
                                       `3` = 2)))

#-----------------------
# 2. Re-Naming Variables
#-----------------------

# choose more descriptive names for each variable
slvk_questions <- slvk_questions %>%
  rename(
    Control_A = q6a_control,
    Control_B = q6b_control, 
    Control_C = q6c_control,
    Experimental_A = q6a_experiment,
    Experimental_B = q6b_experiment,
    Experimental_C = q6c_experiment
  )

#----------------------------
# 3. Load Other Scrubbed Data
#----------------------------

slvk_vars <- as_tibble(read_sav("~/projects/bustikova/data/raw_data/var/slovakia_raw.sav"))

vars <- c("male", "age", "educ", "capital", "ideology", "income", "DemPolGrievance", "PolicyPolGrievance",
          "DemonstrateTrad", "DemonstrateNational", "PetitionSameSex", "VoteFarRight", "VotePrevFarRight",
          "ideologyLC", "SocialMediaUse", "InternetUse", "SlovakNationality", "FAMincome", "Nationalist",
          "EconGrievenceRetro", "EconGrievenceProspInd", "EconGrievenceProspAgg", "EconGrievenceProspMostFams",
          "NatPride", "RomaPartner", "RomaNeighbor", "GayNeighbor", "GayFamily", "ForNeighbor", "ForPartner", "Ukraine",
          "ChristianSchool", "MaleChauvinism", "LawOrder", "ChurchPolitics", "Abortion", "TradMarriage", "SexbMarriage",
          "ChildHome", "MaleJobs", "NativeJobs", "NativeRights", "Religiosity")

slvk_vars <- slvk_vars[vars]

#------------------------------
# 4. Put the data back together
#------------------------------

final_dataset <- bind_cols(slvk_questions, slvk_vars)

# set all data to factor

final_dataset <- mutate(final_dataset, across(everything(), ~as.factor(.)))

#--------------------------------
# 5. Save the final scrubbed data 
#--------------------------------

# saves final data to 'PATH'
write_sav(final_dataset, "~/projects/bustikova/data/scrubbed_data/slovakia_scrubbed.sav")

# cleans up global environment
rm(list = ls())