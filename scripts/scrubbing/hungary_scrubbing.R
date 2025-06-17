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
hungary_questions <- as_tibble(read_sav("~/projects/bustikova/data/raw_data/question/hungary_raw.sav"))

hungary_questions <- hungary_questions %>% select(A6A_1, A6A_2, A6A_3,
                                                  A6B_1, A6B_2, A6B_3)

# Convert response columns to numeric then recode: 1->4, 4->1, 2->3, 3->2
hungary_questions <- hungary_questions %>%
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
hungary_questions <- hungary_questions %>%
  rename(
    Control_A = A6A_1,
    Control_B = A6A_2, 
    Control_C = A6A_3,
    Experimental_A = A6B_1,
    Experimental_B = A6B_2,
    Experimental_C = A6B_3
  )

#----------------------------
# 3. Load Other Scrubbed Data
#----------------------------

hungary_vars <- as_tibble(read_dta("~/projects/bustikova/data/raw_data/var/hungary_raw.dta"))

vars <- c("Male", "Age", "Education", "Capital", "IdeologyLR", "FamIncome",  "DemPolGrievance", "PolicyPolGrievance",
          "EconGrievanceRetro", "EconGrievanceProspInd", "EconGrievanceProspAgg", "EconGrievanceProspMostFams",
          "GayNeighbor", "GayFamily", "GayLesRights", "ForNeighbor", "ForPartner", "Ukraine",
          "NativeJobs", "NativeRights", "DemonstrateNational", "Religiosity", "VoteFarRight"
)
hungary_vars <- hungary_vars[vars]

#------------------------------
# 4. Put the data back together
#------------------------------

final_dataset <- bind_cols(hungary_questions, hungary_vars)

# Convert all variables to numeric

final_dataset <- mutate(final_dataset, across(everything(), ~as.factor(.)))

#--------------------------------
# 5. Save the final scrubbed data 
#--------------------------------

# saves final data to 'PATH'
write_sav(final_dataset, "~/projects/bustikova/data/scrubbed_data/hungary_scrubbed.sav")

# cleans up global environment
rm(list = ls())