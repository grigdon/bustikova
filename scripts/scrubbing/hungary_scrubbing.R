library(haven)
library(dplyr)
library(missForest)
library(tibble)
library(flextable)
library(ggplot2)
library(patchwork)

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

#-------------------------
# 2. Processing Covariates
#-------------------------

data_path <- "~/projects/bustikova/data/raw_data/var/hungary_raw_data.dta"

hungary_vars <- as_tibble(read_dta(data_path))

hungary_vars <- hungary_vars %>% select(
  childhome, churchpolit, laworder, gaypartner, q418, children,
  malejobs, age, settl2, marital, income, gndr, ecgrproi, educ, A4_2, votefut
)

hungary_vars <- hungary_vars %>%
  rename(
    ChildHome = childhome,
    ChurchPolitics = churchpolit,
    LawOrder = laworder,
    GayPartner = gaypartner,
    Religiosity = q418,
    Children = children,
    MaleJobs = malejobs,
    Age = age,
    Capital = settl2,
    Married = marital,
    Income = income,
    Gender = gndr,
    EconomicFuture = ecgrproi,
    Education = educ,
    GovDissatisfaction = A4_2,
    Fidesz_vote = votefut
  )

hungary_vars <- hungary_vars %>%
  mutate(
    Capital = na_if(Capital, "9"),
    Capital = case_when(
      Capital == "Budapest" ~ 0,
      !is.na(Capital) ~ 1,
      TRUE ~ NA_real_
    )
  )

hungary_vars <- hungary_vars %>%
  mutate(
    Married = recode(as.numeric(Married), `1` = 0, `2` = 1, `3` = 0, `4` = 0, `5` = 0, `6` = 0, `7` = 0, `8` = 0)
  )

hungary_vars <- hungary_vars %>%
  mutate(
    Income = as.numeric(Income),
    Gender = recode(as.numeric(Gender), `1` = 0, `2` = 1),
    Fidesz_vote = recode(as.numeric(Fidesz_vote), `1` = 1, `2` = 0)
  )

levels_4_pt <- c("1", "2", "3", "4")
levels_5_pt <- c("1", "2", "3", "4", "5")
levels_gender <- c("0", "1")
levels_education <- as.character(1:10)

summary(hungary_vars)

hungary_vars <- hungary_vars %>%
  mutate(
    ChildHome = factor(ChildHome, levels = levels_4_pt),
    ChurchPolitics = factor(ChurchPolitics, levels = levels_4_pt),
    LawOrder = factor(LawOrder, levels = levels_4_pt),
    GayPartner = factor(GayPartner, levels = levels_4_pt),
    Religiosity = factor(Religiosity, levels = levels_5_pt),
    MaleJobs = factor(MaleJobs, levels = levels_4_pt),
    Capital = factor(Capital, levels = c("0", "1")),
    Married = factor(Married, levels = c("0", "1")),
    Gender = factor(Gender, levels = levels_gender),
    EconomicFuture = factor(EconomicFuture, levels = levels_5_pt),
    Education = factor(Education, levels = levels_education),
    GovDissatisfaction = factor(GovDissatisfaction, levels = levels_4_pt),
    Fidesz_vote = factor(Fidesz_vote, levels = c("0", "1"))
  )

hungary_vars <- hungary_vars %>%
  mutate(across(c("Age", "Income"), ~as.numeric(.)))

hungary_vars_for_imputation <- as.data.frame(hungary_vars)

na_proportions <- colMeans(is.na(hungary_vars_for_imputation))

na_proportions_df <- tibble(
  Variable = names(na_proportions),
  NA_Proportion = na_proportions,
  NA_Count = colSums(is.na(hungary_vars_for_imputation))
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

save_as_image(na_table_ft, path = "~/projects/bustikova/output/scrubbing/hungary/tables/na_proportions_table_flextable.png", webshot = FALSE)

set.seed(23)
imputed_hungary_vars_mf_output <- missForest(hungary_vars_for_imputation)
imputed_hungary_vars_mf <- imputed_hungary_vars_mf_output$ximp

oob_error_data <- imputed_hungary_vars_mf_output$OOBerror

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

save_as_image(imputation_table_ft, path = "~/projects/bustikova/output/scrubbing/hungary/tables/missForest_OOB_error_table.png", webshot = FALSE)

for (col_name in names(hungary_vars_for_imputation)) {
  p_before <- ggplot(hungary_vars_for_imputation, aes_string(x = col_name)) +
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

  if (is.numeric(hungary_vars_for_imputation[[col_name]])) {
    numeric_data <- na.omit(hungary_vars_for_imputation[[col_name]])
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
  } else if (is.factor(hungary_vars_for_imputation[[col_name]])) {
    p_before <- p_before + geom_bar(fill = "grey70", color = "black", linewidth = 0.2)
  } else {
    p_before <- p_before + geom_bar(fill = "grey70", color = "black", linewidth = 0.2)
  }

  p_after <- ggplot(imputed_hungary_vars_mf, aes_string(x = col_name)) +
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

  if (is.numeric(imputed_hungary_vars_mf[[col_name]])) {
    numeric_data <- na.omit(imputed_hungary_vars_mf[[col_name]])
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
  } else if (is.factor(imputed_hungary_vars_mf[[col_name]])) {
    p_after <- p_after + geom_bar(fill = "grey70", color = "black", linewidth = 0.2)
  } else {
    p_after <- p_after + geom_bar(fill = "grey70", color = "black", linewidth = 0.2)
  }

  combined_plot <- p_before + p_after
  ggsave(filename = paste0("~/projects/bustikova/output/scrubbing/hungary/histograms/histogram_", col_name, "_before_and_after_imputation.png"),
         plot = combined_plot, width = 14, height = 5, dpi = 300)
}

#------------------------------
# 3. Put the data back together
#------------------------------

final_dataset <- bind_cols(hungary_questions, imputed_hungary_vars_mf)

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

write_sav(final_dataset, "~/projects/bustikova/data/scrubbed_data/hungary_scrubbed.sav")

rm(list = ls())