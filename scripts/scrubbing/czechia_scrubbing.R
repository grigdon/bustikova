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
czech_questions <- as_tibble(read_sav("~/projects/bustikova/data/raw_data/question/czechia_raw.sav"))

czech_questions <- czech_questions %>% select(Q6AA, Q6AB, Q6AC,
                                          Q6BA, Q6BB, Q6BC)

# Convert response columns to numeric then recode: 1->4, 4->1, 2->3, 3->2
czech_questions <- czech_questions %>%
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
czech_questions <- czech_questions %>%
  rename(
    Control_A = Q6AA,
    Control_B = Q6AB, 
    Control_C = Q6AC,
    Experimental_A = Q6BA,
    Experimental_B = Q6BB,
    Experimental_C = Q6BC
  )

#----------------------------
# 3. Load Other Scrubbed Data
#----------------------------

czech_vars <- as_tibble(read_dta("~/projects/bustikova/data/raw_data/var/czechia_raw.dta"))

vars <- c("Male", "Age", "Education", "Capital", "IdeologyLR", "Income", "FamIncome", "DemPolGrievance", "PolicyPolGrievance",
          "EconGrievanceRetro", "EconGrievanceProspInd", "EconGrievanceProspAgg", "EconGrievanceProspMostFams",
          "GayNeighbor", "GayFamily", "ForNeighbor", "ForPartner", "Ukraine",
          "NativeJobs", "NativeRights", "Religiosity", "VoteFarRight", "DemonstrateNational")

czech_vars <- czech_vars[vars]

#------------------------------
# 4. Put the data back together
#------------------------------

final_dataset <- bind_cols(czech_questions, czech_vars)

# Convert all variables to numeric
final_dataset <- mutate(final_dataset, across(everything(), ~as.factor(.)))

#--------------------------------
# 5. Save the final scrubbed data 
#--------------------------------

# saves final data to 'PATH'
write_sav(final_dataset, "~/projects/bustikova/data/scrubbed_data/czechia_scrubbed.sav")

# cleans up global environment
rm(list = ls())