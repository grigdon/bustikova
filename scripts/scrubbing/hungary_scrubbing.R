library(haven)
library(dplyr)
library(missForest)
library(tibble)
library(flextable)
library(magick)
library(ggplot2)
library(patchwork)

#-------------------------
# 2. Processing Covariates
#-------------------------

data_path <- "~/projects/bustikova/data/raw_data/var/hungary_raw_data.dta"

hungary_vars <- as_tibble(read_dta(data_path))

hungary_vars <- hungary_vars %>% select(
  settl1, gndr, yrbrn, educ, marital, 
  q403, q404, A4_1, A4_2, A4_3,
  A8_1, A8_2, A8_3, A8_4, A8_5, A8_6, A8_7,
  A9_1, A9_2, A9_3, A9_4, A9_5, A9_6, A9_7, 
  A9_8, A9_9, A9_10, A9_11, A9_12, A9_13
)

hungary_vars <- hungary_vars %>%
  rename(
    Capital = settl1,
    Gender = gndr,
    Age = yrbrn,
    Education = educ,
    Married = marital,
    PersonalIncome = q403,
    FamilyIncome = q404,
    DemGrievance= A4_1,
    DemPolGrievance = A4_2, 
    EUPolGrievance = A4_3,
    RomaNeighbor = A8_1, 
    RomaPartner = A8_2, 
    GayNeighbor = A8_3, 
    GayPartner = A8_4, 
    ForeignNeighbor = A8_5, 
    ForeignPartner = A8_6, 
    UkraineNeighbor = A8_7,
    NativeRights = A9_1,
    StateEconomy = A9_2,
    ChristainSchool = A9_3,
    MaleChauvinism = A9_4,
    LawOrder = A9_5, 
    ChurchPolitics = A9_6, 
    Abortion = A9_7,
    TraditionalMarriage = A9_8,
    SexBeforeMarriage = A9_9,
    GayLesRights = A9_10,
    ChildHome = A9_11, 
    MaleJobs = A9_12,
    NativeJobs = A9_13
  )

summary(hungary_vars)

hungary_vars <- hungary_vars %>%
  mutate(across(c(ChildHome, GayPartner, Religiosity, MaleChauvinism, MaleJobs,
                  Ukraine, NatPride, PolicyPolGrievance, EconGrievanceProspInd,
                  DemonstrateTrad),
                ~na_if(., 9)))

hungary_vars <- hungary_vars %>%
  mutate(Ideology = na_if(Ideology, 99))

hungary_vars <- hungary_vars %>%
  mutate(Income = na_if(Income, 8))

hungary_vars <- hungary_vars %>%
  mutate(Gender = recode(as.numeric(Gender),
                         `1` = 0,
                         `2` = 1))

hungary_vars <- hungary_vars %>%
  mutate(Income = case_when(
    as.numeric(Income) == 7 ~ 0,
    TRUE ~ as.numeric(Income)
  ))

hungary_vars <- hungary_vars %>%
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

hungary_vars <- hungary_vars %>%
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

# saves final data to 'PATH'
write_sav(final_dataset, "~/projects/bustikova/data/scrubbed_data/hungary_scrubbed.sav")

# cleans up global environment
rm(list = ls())