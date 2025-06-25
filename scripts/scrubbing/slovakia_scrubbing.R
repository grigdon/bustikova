library(haven)
library(dplyr)
library(missForest)
library(tibble)
library(flextable)
library(magick)
library(ggplot2)
library(patchwork)

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

#-------------------------
# 2. Processing Covariates
#-------------------------

data_path <- "~/projects/bustikova/data/raw_data/var/slovakia_raw_data.dta"

slvk_vars <- as_tibble(read_dta(data_path))

slvk_vars <- slvk_vars %>% select(
  q9k, q8d, q11, q9d, r2pov, q7, q5b,
  q12a, r4, r1, r11a, r13
)

slvk_vars <- slvk_vars %>%
  rename(
    ChildHome = q9k,
    GayFamily = q8d,
    Religiosity = q11,
    MaleChauvinism = q9d,
    Age = r2pov,
    NatPride = q7,
    EconGrievanceProspInd = q5b,
    DemonstrateTrad = q12a,
    Education = r4,
    Male = r1,
    FamIncome = r11a,
    Capital = r13
  )

slvk_vars <- slvk_vars %>%
  mutate(FamIncome = na_if(FamIncome, 99))


summary(slvk_vars)
slvk_vars <- slvk_vars %>%
  mutate(Male = recode(as.numeric(Male),
                       `1` = 0,
                       `2` = 1))

slvk_vars <- slvk_vars %>%
  mutate(Capital = case_when(
    as.numeric(Capital) == 1 ~ 0,
    as.numeric(Capital) != 1 ~ 1,
  ))

slvk_vars <- slvk_vars %>%
  mutate(across(c("ChildHome", "GayFamily", "MaleChauvinism", "NatPride"),
                ~ recode(as.numeric(.x),
                         `1` = 4,
                         `4` = 1,
                         `2` = 3,
                         `3` = 2)))

slvk_vars <- slvk_vars %>%
  mutate(across("DemonstrateTrad",
                ~ recode(as.numeric(.x),
                         `0` = 1,
                         `1` = 2,
                         `2` = 3,
                         `3` = 4)))

levels_3_pt <- c("1", "2", "3")
levels_4_pt <- c("1", "2", "3", "4")
levels_5_pt <- c("1", "2", "3", "4", "5")
levels_male <- c("0", "1")

slvk_vars <- slvk_vars %>%
  mutate(
    ChildHome = factor(ChildHome, levels = levels_4_pt),
    GayFamily = factor(GayFamily, levels = levels_4_pt),
    Religiosity = factor(Religiosity, levels = levels_5_pt),
    MaleChauvinism = factor(MaleChauvinism, levels = levels_4_pt),
    NatPride = factor(NatPride, levels = levels_4_pt),
    EconGrievanceProspInd = factor(EconGrievanceProspInd, levels = levels_5_pt),
    DemonstrateTrad = factor(DemonstrateTrad, levels = levels_4_pt),
    Education = factor(Education, levels = levels_3_pt),
    Male = factor(Male, levels = levels_male),
    FamIncome = factor(FamIncome, levels = levels_5_pt),
    Capital = factor(Capital, levels = c("0", "1"))
  )

slvk_vars <- slvk_vars %>%
  mutate(across(c("Age"), ~as.numeric(.)))

slovak_vars_for_imputation <- as.data.frame(slvk_vars)

na_proportions <- colMeans(is.na(slovak_vars_for_imputation))

na_proportions_df <- tibble(
  Variable = names(na_proportions),
  NA_Proportion = na_proportions,
  NA_Count = colSums(is.na(slovak_vars_for_imputation))
) %>%
  arrange(desc(NA_Proportion))

na_table_ft <- na_proportions_df %>%
  flextable() %>%
  set_header_labels(
    Variable = "Variable",
    NA_Proportion = "NA Proportion",
    NA_Count = "NA Count"
  ) %>%
  colformat_num(
    j = "NA_Proportion",
    digits = 3
  ) %>%
  bold(part = "header") %>%
  autofit() %>%
  theme_booktabs()

save_as_image(na_table_ft, path = "~/projects/bustikova/output/scrubbing/slovakia/tables/na_proportions_table_flextable.png", webshot = FALSE)

set.seed(23)
imputed_slovak_vars_mf_output <- missForest(slovak_vars_for_imputation)
imputed_slovak_vars_mf <- imputed_slovak_vars_mf_output$ximp

oob_error_data <- imputed_slovak_vars_mf_output$OOBerror

imputation_results_df <- tibble(
  Metric = c("Continuous (NRMSE)", "Categorical (PFC)"),
  Value = c(oob_error_data["NRMSE"], oob_error_data["PFC"]))

imputation_table_ft <- imputation_results_df %>%
  flextable() %>%
  set_header_labels(
    Metric = "Imputation Metric",
    Value = "OOB Error Value"
  ) %>%
  colformat_num(
    j = "Value",
    digits = 4
  ) %>%
  bold(part = "header") %>%
  autofit() %>%
  theme_booktabs() %>%
  set_caption(caption = "MissForest Out-Of-Bag Error Results")

save_as_image(imputation_table_ft, path = "~/projects/bustikova/output/scrubbing/slovakia/tables/missForest_OOB_error_table.png", webshot = FALSE)

for (col_name in names(slovak_vars_for_imputation)) {
  p_before <- ggplot(slovak_vars_for_imputation, aes_string(x = col_name)) +
    labs(title = paste(col_name, "(Before Imputation)"),
         x = col_name, y = "Frequency") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "black"),
          panel.grid.major = element_line(color = "grey80", linetype = "dotted"),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white", colour = "black", linewidth = 0.5),
          axis.line = element_line(colour = "black"),
          axis.text = element_text(color = "black"),
          axis.title = element_text(color = "black"))
  
  if (is.numeric(slovak_vars_for_imputation[[col_name]])) {
    numeric_data <- na.omit(slovak_vars_for_imputation[[col_name]])
    if(length(numeric_data) > 1) {
      h <- 2 * IQR(numeric_data) / (length(numeric_data)^(1/3))
      bins <- if (h == 0 || is.na(h) || is.infinite(h)) 30 else diff(range(numeric_data)) / h
      if (bins < 5) bins <- 5
      if (bins > 50) bins <- 50
    } else {
      bins <- 30
    }
    p_before <- p_before + geom_histogram(bins = round(bins),
                                          fill = "grey70", color = "black", linewidth = 0.2)
  } else if (is.factor(slovak_vars_for_imputation[[col_name]])) {
    p_before <- p_before + geom_bar(fill = "grey70", color = "black", linewidth = 0.2)
  } else {FamIncome
    p_before <- p_before + geom_bar(fill = "grey70", color = "black", linewidth = 0.2)
  }
  
  p_after <- ggplot(imputed_slovak_vars_mf, aes_string(x = col_name)) +
    labs(title = paste(col_name, "(After Imputation)"),
         x = col_name, y = "Frequency") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "black"),
          panel.grid.major = element_line(color = "grey80", linetype = "dotted"),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white", colour = "black", linewidth = 0.5),
          axis.line = element_line(colour = "black"),
          axis.text = element_text(color = "black"),
          axis.title = element_text(color = "black"))
  
  if (is.numeric(imputed_slovak_vars_mf[[col_name]])) {
    numeric_data <- na.omit(imputed_slovak_vars_mf[[col_name]])
    if(length(numeric_data) > 1) {
      h <- 2 * IQR(numeric_data) / (length(numeric_data)^(1/3))
      bins <- if (h == 0 || is.na(h) || is.infinite(h)) 30 else diff(range(numeric_data)) / h
      if (bins < 5) bins <- 5
      if (bins > 50) bins <- 50
    } else {
      bins <- 30
    }
    p_after <- p_after + geom_histogram(bins = round(bins),
                                        fill = "grey70", color = "black", linewidth = 0.2)
  } else if (is.factor(imputed_slovak_vars_mf[[col_name]])) {
    p_after <- p_after + geom_bar(fill = "grey70", color = "black", linewidth = 0.2)
  } else {
    p_after <- p_after + geom_bar(fill = "grey70", color = "black", linewidth = 0.2)
  }
  
  combined_plot <- p_before + p_after
  ggsave(filename = paste0("~/projects/bustikova/output/scrubbing/slovakia/histograms/histogram_", col_name, "_before_and_after_imputation.png"),
         plot = combined_plot, width = 14, height = 5, dpi = 300)
}

#------------------------------
# 4. Put the data back together
#------------------------------

final_dataset <- bind_cols(slvk_questions, imputed_slovak_vars_mf)

# saves final data to 'PATH'
write_sav(final_dataset, "~/projects/bustikova/data/scrubbed_data/slovakia_scrubbed.sav")

# cleans up global environment
rm(list = ls())