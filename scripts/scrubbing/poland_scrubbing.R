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
pol_questions <- as_tibble(read_sav("~/projects/bustikova/data/raw_data/question/poland_raw.sav"))

pol_questions <- pol_questions %>% select(Q06A, Q06B, Q06C,
                                          Q06D, Q06E, Q06F)

# Convert response columns to numeric then recode: 1->4, 4->1, 2->3, 3->2
pol_questions <- pol_questions %>%
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
pol_questions <- pol_questions %>%
  rename(
    Control_A = Q06A,
    Control_B = Q06B, 
    Control_C = Q06C,
    Experimental_A = Q06D,
    Experimental_B = Q06E,
    Experimental_C = Q06F
  )

#----------------------------
# 3. Load Other Scrubbed Data
#----------------------------

pol_vars <- as_tibble(read_dta("~/projects/bustikova/data/raw_data/var/poland_raw.dta"))

vars <- c("Male", "Age", "Education", "Capital", "IdeologyLR", "Income",  "DemPolGrievance", "PolicyPolGrievance",
          "EconGrievanceRetro", "EconGrievanceProspInd", "EconGrievanceProspAgg", "EconGrievanceProspMostFams",
          "GayNeighbor", "GayFamily", "GayLesRights", "ForNeighbor", "ForPartner", "Ukraine",
          "NativeJobs", "NativeRights", "DemonstrateNational", "Religiosity", "VoteFarRight"
)
pol_vars <- pol_vars[vars]

#------------------------------
# 4. Put the data back together
#------------------------------

final_dataset <- bind_cols(pol_questions, pol_vars)

# Convert all variables to numeric
final_dataset <- mutate(final_dataset, across(everything(), ~as.factor(.)))

#--------------------------------
# 5. Save the final scrubbed data 
#--------------------------------

# saves final data to 'PATH'
write_sav(final_dataset, "~/projects/bustikova/data/scrubbed_data/poland_scrubbed.sav")

# cleans up global environment
rm(list = ls())