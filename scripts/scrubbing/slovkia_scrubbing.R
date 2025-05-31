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

#--------------------------------
# 1. Data Loading & Initial Setup
#--------------------------------

# load raw data
slvk_raw <- as_tibble(read_sav("~/projects/bustikova/raw_data/slovakia_raw_data.sav"))

# remove unused columns
slvk_scrubbed <- slvk_raw %>% select(-date, -vzorka, -q1, -q2, -q3, -q5d, -q8a, -q8b, -q9b, -q9g, 
                                     -q9h, -q9i, -q9j, -q10a_control, -q10b_control, -q10c_control,
                                     -q10a_experiment, -q10b_experiment, -q10c_experiment, -r2, -r5,
                                     -r6, -r7a, -r9, -r10, -r11a, -r12, -y3, -y4, -e14, -mm3_1, -mm3_2, -w)

#-------------------------
# 2. Re-Ordering Variables
#-------------------------

# Reverse code responses (1 ↔ 4, 2 ↔ 3) for grievances questions
slvk_scrubbed <- slvk_scrubbed %>%
  mutate(across(2:7, ~ recode(as.numeric(.), 
                               `1` = 4L, 
                               `2` = 3L, 
                               `3` = 2L, 
                               `4` = 1L, 
                               .default = NA_integer_)))  # Keeps NA values intact

# Reverse code responses (1 ↔ 4, 2 ↔ 3) for experimental & control on q6
slvk_scrubbed <- slvk_scrubbed %>%
  mutate(across(8:13, ~ recode(as.numeric(.), 
                              `1` = 4L, 
                              `2` = 3L, 
                              `3` = 2L, 
                              `4` = 1L, 
                              .default = NA_integer_)))  # Keeps NA values intact

# Reverse code responses (1 ↔ 4, 2 ↔ 3) for q7
slvk_scrubbed <- slvk_scrubbed %>%
  mutate(across(14, ~ recode(as.numeric(.), 
                               `1` = 4L, 
                               `2` = 3L, 
                               `3` = 2L, 
                               `4` = 1L, 
                               .default = NA_integer_)))  # Keeps NA values intact

# Reverse code responses (1 ↔ 4, 2 ↔ 3) for q9
slvk_scrubbed <- slvk_scrubbed %>%
  mutate(across(20:27, ~ recode(as.numeric(.), 
                             `1` = 4L, 
                             `2` = 3L, 
                             `3` = 2L, 
                             `4` = 1L, 
                             .default = NA_integer_)))  # Keeps NA values intact

# Reverse code responses (1 ↔ 4, 2 ↔ 3) for r1
slvk_scrubbed <- slvk_scrubbed %>%
  mutate(across(32, ~ recode(as.numeric(.), 
                                `2` = 0L, # female set to 0
                                .default = 1L)))  # else, set male to 1

# Bin the r2 age data into the 6 categories defined in the survey

slvk_scrubbed <- slvk_scrubbed %>%
  mutate(r2pov = as.numeric(r2pov))

slvk_scrubbed <- slvk_scrubbed %>%
  mutate(
    r2pov = cut(r2pov,
                breaks = c(18, 24, 34, 44, 54, 64, Inf),
                labels = 1:6,
                include.lowest = TRUE,
                right = TRUE),
    r2pov = as.integer(as.character(r2pov))  # Convert factor to integer safely
  )

# set r11b '99' values to N/A

slvk_scrubbed <- slvk_scrubbed %>%
  mutate(r11b = as.numeric(r11b))

slvk_scrubbed <- slvk_scrubbed %>%
  mutate(r11b = na_if(r11b, 99))

# set y1 '9' values to N/A

slvk_scrubbed <- slvk_scrubbed %>%
  mutate(y1 = as.numeric(y1))

slvk_scrubbed <- slvk_scrubbed %>%
  mutate(y1 = na_if(y1, 9))


# set all to factors
slvk_scrubbed <- slvk_scrubbed %>%
  mutate(across(everything(), as.factor))

summary(slvk_scrubbed)

#-----------------------
# 3 Re-Naming Variables
#-----------------------

# choose more descriptive names for each variable
slvk_scrubbed <- slvk_scrubbed %>%
  rename(
    DemPolGrievance = q4a, 
    PolicyPolGrievance = q4b,
    EUPolGrievance = q4c,
    EconGrievanceRetro = q5a,
    EconGrievanceProspInd = q5b,
    EconGrievanceProspAgg = q5c,
    Control_A = q6a_control,
    Control_B = q6b_control, 
    Control_C = q6c_control,
    Experimental_A = q6a_experiment,
    Experimental_B = q6b_experiment,
    Experimental_C = q6c_experiment,
    NatPride = q7,
    NativeRights = q9a,
    NativeJobs = q9m,
    LawOrder = q9e,
    MaleChauvinism = q9d,
    ChristianSchool = q9c,
    GayNeighbor = q8c,
    GayPartner = q8d,
    ForNeighbor = q8e,
    ForPartner = q8f,
    Ukraine = q8g,
    ChildHome = q9k,
    MaleJobs = q9l,
    ChurchPolitics = q9f,
    Religiosity = q11,
    DemonstrateNational = q12b,
    DemonstrateTrad = q12a,
    PetitionSameSex = q13,
    Gender = r1,
    Age = r2pov,
    Education = r4,
    Is_Capital = r13,
    Ideology = y1, 
    Income = r11b
  )

#---------------------
# 3. Impute N/A Values
#---------------------

# only select the variables for imputation
slvk_vars <- as.data.frame(slvk_scrubbed %>%
                             select(
                               -id,
                               -Control_A, -Control_B, -Control_C,
                               -Experimental_A, -Experimental_B, -Experimental_C
                             )
)

# getting ~ 7% out-of-the-box error score for imputation
# Impute missing values using random forest
imputed_result <- missForest(slvk_vars)

# Extract the imputed data frame
slvk_vars_imputed <- imputed_result$ximp

#------------------------------
# 4. Put the data back together
#------------------------------

# extract the endorse questions from the original data
slvk_questions <- as.data.frame(slvk_scrubbed %>% 
                                  select(
                                    id, Control_A, Control_B, Control_C,
                                    Experimental_A, Experimental_B, Experimental_C
                                  )
                                )

final_dataset <- bind_cols(slvk_questions, slvk_vars_imputed)

#-----------------------------
# Save the final scrubbed data 
#-----------------------------

# saves final data to 'PATH'
write_sav(final_dataset, "~/projects/bustikova/scrubbed_data/slovakia_scrubbed.sav")

# cleans up global environment
rm(list = ls())