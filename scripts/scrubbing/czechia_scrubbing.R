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

czech_questions <- czech_questions %>% mutate(across(everything(), ~as.factor(.)))

#-------------------------
# 2. Processing Covariates
#-------------------------


data_path <- "~/projects/bustikova/data/raw_data/var/czechia_raw_data.dta"

czech_vars <- as_tibble(read_dta(data_path))

czech_vars <- czech_vars %>% select(
  q9kchildh, gaypart8d, religiosity,
  q9dmalechauv, IDE_2, q9lgendcomp,
  Q8G, prideq7, Q4B, Q5B, Q12a,
  t_lepra1, IDE_6b, gender, IDE_10a
)

czech_vars <- czech_vars %>%
  rename(
    ChildHome = q9kchildh,
    GayPartner = gaypart8d,
    Religiosity = religiosity,
    MaleChauvinism = q9dmalechauv,
    Age = IDE_2,
    MaleJobs = q9lgendcomp,
    Ukraine = Q8G,
    NatPride = prideq7,
    PolicyPolGrievance = Q4B,
    EconGrievanceProspInd = Q5B,
    DemonstrateTrad = Q12a,
    Ideology = t_lepra1,
    Education = IDE_6b,
    Gender = gender,
    Income = IDE_10a
  )

czech_vars <- czech_vars %>%
  mutate(across(c(ChildHome, GayPartner, Religiosity, MaleChauvinism, MaleJobs,
                  Ukraine, NatPride, PolicyPolGrievance, EconGrievanceProspInd,
                  DemonstrateTrad),
                ~na_if(., 9)))

czech_vars <- czech_vars %>%
  mutate(Ideology = na_if(Ideology, 99))

czech_vars <- czech_vars %>%
  mutate(Income = na_if(Income, 8))

czech_vars <- czech_vars %>%
  mutate(Gender = recode(as.numeric(Gender),
                         `1` = 0,
                         `2` = 1))

czech_vars <- czech_vars %>%
  mutate(Income = case_when(
    as.numeric(Income) == 7 ~ 0,
    TRUE ~ as.numeric(Income)
  ))

czech_vars <- czech_vars %>%
  mutate(across(-c("Religiosity", "Age", "EconGrievanceProspInd", "Ideology", "Education", "Income", "Gender", "PolicyPolGrievance", "DemonstrateTrad"),
                ~ recode(as.numeric(.x),
                         `1` = 4,
                         `4` = 1,
                         `2` = 3,
                         `3` = 2)))

levels_4_pt <- c("1", "2", "3", "4")
levels_5_pt <- c("1", "2", "3", "4", "5")
levels_gender <- c("0", "1")
levels_education <- as.character(1:10)

czech_vars <- czech_vars %>%
  mutate(
    ChildHome = factor(ChildHome, levels = levels_4_pt),
    GayPartner = factor(GayPartner, levels = levels_4_pt),
    Religiosity = factor(Religiosity, levels = levels_5_pt),
    MaleChauvinism = factor(MaleChauvinism, levels = levels_4_pt),
    MaleJobs = factor(MaleJobs, levels = levels_4_pt),
    Ukraine = factor(Ukraine, levels = levels_4_pt),
    NatPride = factor(NatPride, levels = levels_4_pt),
    PolicyPolGrievance = factor(PolicyPolGrievance, levels = levels_4_pt),
    EconGrievanceProspInd = factor(EconGrievanceProspInd, levels = levels_5_pt),
    DemonstrateTrad = factor(DemonstrateTrad, levels = levels_4_pt),
    Ideology = factor(Ideology, levels = levels_5_pt),
    Education = factor(Education, levels = levels_education),
    Gender = factor(Gender, levels = levels_gender)
  )

czech_vars <- czech_vars %>%
  mutate(across(c("Age", "Income"), ~as.numeric(.)))

czech_vars_for_imputation <- as.data.frame(czech_vars)

na_proportions <- colMeans(is.na(czech_vars_for_imputation))

na_proportions_df <- tibble(
  Variable = names(na_proportions),
  NA_Proportion = na_proportions,
  NA_Count = colSums(is.na(czech_vars_for_imputation))
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

save_as_image(na_table_ft, path = "~/projects/bustikova/output/scrubbing/czechia/tables/na_proportions_table_flextable.png", webshot = FALSE)

set.seed(23)
imputed_czech_vars_mf_output <- missForest(czech_vars_for_imputation)
imputed_czech_vars_mf <- imputed_czech_vars_mf_output$ximp

oob_error_data <- imputed_czech_vars_mf_output$OOBerror

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

save_as_image(imputation_table_ft, path = "~/projects/bustikova/output/scrubbing/czechia/tables/missForest_OOB_error_table.png", webshot = FALSE)

for (col_name in names(czech_vars_for_imputation)) {
  p_before <- ggplot(czech_vars_for_imputation, aes_string(x = col_name)) +
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
  
  if (is.numeric(czech_vars_for_imputation[[col_name]])) {
    numeric_data <- na.omit(czech_vars_for_imputation[[col_name]])
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
  } else if (is.factor(czech_vars_for_imputation[[col_name]])) {
    p_before <- p_before + geom_bar(fill = "grey70", color = "black", linewidth = 0.2)
  } else {
    p_before <- p_before + geom_bar(fill = "grey70", color = "black", linewidth = 0.2)
  }
  
  p_after <- ggplot(imputed_czech_vars_mf, aes_string(x = col_name)) +
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
  
  if (is.numeric(imputed_czech_vars_mf[[col_name]])) {
    numeric_data <- na.omit(imputed_czech_vars_mf[[col_name]])
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
  } else if (is.factor(imputed_czech_vars_mf[[col_name]])) {
    p_after <- p_after + geom_bar(fill = "grey70", color = "black", linewidth = 0.2)
  } else {
    p_after <- p_after + geom_bar(fill = "grey70", color = "black", linewidth = 0.2)
  }
  
  combined_plot <- p_before + p_after
  ggsave(filename = paste0("~/projects/bustikova/output/scrubbing/czechia/histograms/histogram_", col_name, "_before_and_after_imputation.png"),
         plot = combined_plot, width = 14, height = 5, dpi = 300)
}

#------------------------------
# 3. Put the data back together
#------------------------------

final_dataset <- bind_cols(czech_questions, imputed_czech_vars_mf)

new_interaction_vars <- final_dataset %>%
  mutate(
    Gender_numeric = as.numeric(as.character(Gender)),
    Religiosity_numeric = as.numeric(as.character(Religiosity))
  ) %>%
  transmute(
    Age_Male_numeric = Age * Gender_numeric,
    Religiosity_Male_numeric = Religiosity_numeric * Gender_numeric
  )

new_interaction_vars$`AgeMale` <- as.numeric(new_interaction_vars$Age_Male_numeric)
new_interaction_vars$`ReligiosityMale` <- as.factor(new_interaction_vars$Religiosity_Male_numeric)

new_interaction_vars <- new_interaction_vars %>%
  select(`AgeMale`, `ReligiosityMale`)

final_dataset <- bind_cols(final_dataset, new_interaction_vars)

write_sav(final_dataset, "~/projects/bustikova/data/scrubbed_data/czechia_scrubbed.sav")

rm(list = ls())