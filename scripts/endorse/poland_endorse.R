library(endorse)
library(haven)
library(ggpubr)
library(missForest)
library(forcats)
library(reshape2)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(Cairo)

#====================================================
# 1. Data Loading and Initial Processing for Poland
#====================================================

PLData <- read_sav("~/projects/bustikova/data/scrubbed_data/poland_scrubbed.sav")

# Define questions for the endorsement experiment
questions <- c("Control_A", "Control_B", "Control_C",
               "Experimental_A", "Experimental_B", "Experimental_C")

summary(PLData)

# Select relevant columns for endorsement analysis
data_pol_questions <- PLData[questions]

# Define vars to keep based on the provided list and summary(PLData)
# These are the actual column names from summary(PLData)
vars_to_keep <- c("ChildHome", "Religiosity", "GayPartner", "EUPolGrievance",
                  "NationalPride", "MaleChauvinism", "Vote", "Rural",
                  "Income", "Age", "DemonstrateTrad", "Education", "Gender",
                  "DemPolGrievance")

# Subset variables
data_pol_vars <- PLData[vars_to_keep]

# Convert all variables to numeric (if not already)
data_pol_vars <- mutate(data_pol_vars, across(everything(), ~as.numeric(.)))

# Merge the questions and variables datasets
data_pol <- bind_cols(data_pol_questions, data_pol_vars)

# Standardize 'Age' and 'Income'
# Using scale() function for standardization (mean=0, sd=1)
data_pol$Age_std <- scale(data_pol$Age)
data_pol$Income_std <- scale(data_pol$Income)


# Create named list for response questions
Y <- list(Q1 = c("Control_A", "Experimental_A"),
          Q2 = c("Control_B", "Experimental_B"),
          Q3 = c("Control_C", "Experimental_C"))

#====================================================
# 2. Creating endorse objects for three models
#====================================================

# Model 1: Original model with all main effects (using standardized Age and Income)
endorse_object_model1 <- endorse(Y = Y,
                                 data = data_pol,
                                 identical.lambda = FALSE,
                                 covariates = TRUE,
                                 prop = 0.008,
                                 formula.indiv = formula( ~ ChildHome + Religiosity + GayPartner + EUPolGrievance +
                                                            NationalPride + MaleChauvinism + Vote + Rural +
                                                            Income_std + Age_std + DemonstrateTrad + Education + Gender +
                                                            DemPolGrievance),
                                 omega2.out = TRUE,
                                 hierarchical = FALSE
)

# Model 2: Interaction Age * Gender (using standardized Age)
endorse_object_model2 <- endorse(Y = Y,
                                 data = data_pol,
                                 identical.lambda = FALSE,
                                 covariates = TRUE,
                                 prop = 0.008,
                                 formula.indiv = formula( ~ ChildHome + Religiosity + GayPartner + EUPolGrievance +
                                                            NationalPride + MaleChauvinism + Vote + Rural +
                                                            Income_std + Age_std + DemonstrateTrad + Education + Gender +
                                                            DemPolGrievance + Age_std*Gender),
                                 omega2.out = TRUE,
                                 hierarchical = FALSE
)

# Model 3: Interaction Age * Religiosity (using standardized Age)
endorse_object_model3 <- endorse(Y = Y,
                                 data = data_pol,
                                 identical.lambda = FALSE,
                                 covariates = TRUE,
                                 prop = 0.008,
                                 formula.indiv = formula( ~ ChildHome + Religiosity + GayPartner + EUPolGrievance +
                                                            NationalPride + MaleChauvinism + Vote + Rural +
                                                            Income_std + Age_std + DemonstrateTrad + Education + Gender +
                                                            DemPolGrievance + Age_std*Religiosity),
                                 omega2.out = TRUE,
                                 hierarchical = FALSE
)


#====================================================
# 2.5 Output the acceptance ratio for each question (for Model 1 as example)
#====================================================

# Extract acceptance ratios from the endorse object (using Model 1 for this example)
acceptance_ratios <- data.frame(
  Question = paste("Question", 1:3),
  Ratio = endorse_object_model1$accept.ratio
)

# Create a bar plot of acceptance ratios
acceptance_plot <- ggplot(acceptance_ratios, aes(x = Question, y = Ratio)) +
  geom_bar(stat = "identity", fill = "#4682B4", width = 0.6) +
  geom_text(aes(label = sprintf("%.3f", Ratio)), vjust = -0.5, size = 4) +
  scale_y_continuous(limits = c(0, 1.0), breaks = seq(0, 1, by = 0.1)) +
  labs(
    title = "Poland: Metropolis-Hastings Acceptance Ratios for Endorsement Model (Model 1)",
    x = "Question",
    y = "Acceptance Ratio"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold")
  )

# Save the plot as PNG
ggsave("~/projects/bustikova/output/endorse/metro_plot/poland_acceptance_ratios.png",
       acceptance_plot, width = 8, height = 6)


#====================================================
# 3. Plotting coefficient plots from the delta matrix for each model
#====================================================

# Function to create and save coefficient plots
plot_coefficients <- function(endorse_object, model_name, plot_title_suffix) {
  # Get column names of delta matrix excluding the intercept
  # Updated regex to include all specified variables and interactions for Poland models
  delta_cols <- colnames(endorse_object$delta)[grepl("^(ChildHome|Religiosity|GayPartner|EUPolGrievance|NationalPride|MaleChauvinism|Vote|Rural|Income_std|Age_std|DemonstrateTrad|Education|Gender|DemPolGrievance|Age_std:Gender|Religiosity:Age_std|Age_std:Religiosity)", colnames(endorse_object$delta))]
  
  # Create the dataframe using posterior samples
  delta_matrix_values <- data.frame(
    mean = apply(endorse_object$delta[, delta_cols], 2, mean),
    lower = apply(endorse_object$delta[, delta_cols], 2, quantile, 0.025),
    upper = apply(endorse_object$delta[, delta_cols], 2, quantile, 0.975)
  )
  
  # Add variable names and categories
  delta_matrix_values$variables <- rownames(delta_matrix_values) # Use rownames for variables
  delta_matrix_values$category <- NA
  
  # Define categories based on your provided groups and variable names
  descriptives_socio_economic <- c("Age_std", "Gender", "Education", "Income_std", "Rural", "Religiosity")
  conservatism <- c("ChildHome", "GayPartner", "MaleChauvinism", "DemonstrateTrad")
  performance_economy_and_government <- c("DemPolGrievance", "EUPolGrievance", "NationalPride", "Vote")
  
  # Assign categories
  delta_matrix_values <- delta_matrix_values %>%
    mutate(
      category = case_when(
        variables %in% descriptives_socio_economic ~ "Descriptives Socio-Economic",
        variables %in% conservatism ~ "Conservatism",
        variables %in% performance_economy_and_government ~ "Performance: Economy and Government",
        grepl("Age_std:Gender", variables) ~ "Interaction: Age × Female", # Label based on 'Gender' being 'Female'
        grepl("Age_std:Religiosity|Religiosity:Age_std", variables) ~ "Interaction: Age × Religiosity", # Handle both naming conventions
        TRUE ~ "Other" # Fallback for any variables not caught
      )
    )
  
  # Reorder variables within each category by mean
  delta_matrix_values <- delta_matrix_values %>%
    group_by(category) %>%
    mutate(variables = forcats::fct_reorder(variables, mean)) %>% # Explicitly use forcats::fct_reorder
    ungroup()
  
  # Reorder categories (ensure a consistent order)
  category_order <- c("Descriptives Socio-Economic",
                      "Conservatism",
                      "Performance: Economy and Government",
                      "Interaction: Age × Female", # Reflects Gender label
                      "Interaction: Age × Religiosity",
                      "Other")
  delta_matrix_values$category <- factor(delta_matrix_values$category, levels = intersect(category_order, unique(delta_matrix_values$category)))
  
  # Define custom labels for variables (drawing inspiration from all scripts)
  custom_labels <- c(
    "ChildHome" = "Child Mom Dad", # Simplified
    "Religiosity" = "Religiosity",
    "GayPartner" = "Gay Partner", # Simplified
    "EUPolGrievance" = "EU Dissatisfaction", # Simplified from Poland specific
    "NationalPride" = "National Pride",
    "MaleChauvinism" = "Male Leader", # Simplified from "Male Political Leader"
    "Vote" = "PiS & Konf Vote", # Based on image and context
    "Rural" = "Rural", # Changed from 'Urban' assuming the variable is 'Rural'
    "Income_std" = "Income",
    "Age_std" = "Age",
    "DemonstrateTrad" = "Mobilize Trad Fam", # Consistent with previous script
    "Education" = "Education",
    "Gender" = "Female", # Based on image
    "DemPolGrievance" = "Democracy Dissatisfaction", # Simplified
    "Age_std:Gender" = "Age × Female", # Consistent with Gender label
    "Age_std:Religiosity" = "Age × Religiosity",
    "Religiosity:Age_std" = "Age × Religiosity" # To catch if named this way
  )
  
  # Create the plot
  plot <- ggplot(delta_matrix_values, aes(x = variables, y = mean)) +
    geom_point(size = 1, shape = 10) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.25, size = .25) + # Consistent width
    coord_flip() +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.5) +
    facet_grid(category ~ ., scales = "free_y", space = "free_y") +
    scale_x_discrete(labels = custom_labels) +
    theme_classic() +
    ggtitle(paste("Poland: Posterior Coefficient Estimates ", plot_title_suffix, sep = "")) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      strip.text.y = element_text(angle = 0, hjust = 0, face = "bold"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank()
    ) +
    labs(x = NULL, y = "Coefficient Estimate")
  
  # Save to PNG
  ggsave(paste0("~/projects/bustikova/output/endorse/coef_plot/poland_coef_plot_", tolower(gsub(" ", "_", model_name)), ".png"), plot, width = 12, height = 10)
}

# Plot for Model 1
plot_coefficients(endorse_object_model1, "Model 1 Original", "(Original Model)")
# Plot for Model 2
plot_coefficients(endorse_object_model2, "Model 2 Age_Gender Interaction", "(with Age × Female Interaction)")
# Plot for Model 3
plot_coefficients(endorse_object_model3, "Model 3 Age_Religiosity Interaction", "(with Age × Religiosity Interaction)")


######################################
# Extract and standardize MCMC samples for Model 1 (as example for macro plot)
# --------------------------------------
# Ensure endorse_object_model1 is defined and contains necessary lambda and x components.

lambda_samples_m1 <- as.matrix(endorse_object_model1$lambda)
sigma2_m1 <- (endorse_object_model1$x)^2  # Convert standard deviation to variance

# Calculate the row-wise average of the three lambda parameters
avg_lambda_m1 <- (lambda_samples_m1[, "(Intercept).1.1"] +
                    lambda_samples_m1[, "(Intercept).2.1"] +
                    lambda_samples_m1[, "(Intercept).3.1"]) / 3

# Include this average in the lambda_std list
lambda_std_m1 <- list(
  q1 = lambda_samples_m1[, "(Intercept).1.1"] / sigma2_m1,
  q2 = lambda_samples_m1[, "(Intercept).2.1"] / sigma2_m1,
  q3 = lambda_samples_m1[, "(Intercept).3.1"] / sigma2_m1,
  average = avg_lambda_m1 / sigma2_m1 # Standardize the average lambda just like the others
)

# Calculate summary statistics
# --------------------------------------
# Calculate mean and confidence intervals for each question
question_stats_m1 <- lapply(lambda_std_m1, function(x) {
  list(
    mean = mean(x),
    ci = quantile(x, probs = c(0.025, 0.975))
  )
})

# Create visualizations
# --------------------------------------
# Create dataframe in long format with average
box_data_long_m1 <- data.frame(
  Question = factor(rep(c("Question A", "Question B", "Question C", "Average"),
                        times = c(length(lambda_std_m1$q1), length(lambda_std_m1$q2),
                                  length(lambda_std_m1$q3), length(lambda_std_m1$average))),
                    levels = c("Question A", "Question B", "Question C", "Average")),
  Support = c(lambda_std_m1$q1, lambda_std_m1$q2, lambda_std_m1$q3, lambda_std_m1$average)
)

# Set a professional color palette suitable for publication
journal_colors <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7")

# Create publication-quality box plot
box_plot_m1 <- ggplot(box_data_long_m1, aes(x = Question, y = Support, fill = Question)) +
  geom_boxplot(
    alpha = 0.85,
    outlier.shape = 21,
    outlier.size = 2,
    outlier.alpha = 0.7,
    outlier.color = "black",
    outlier.fill = "white",
    width = 0.6,
    lwd = 0.6
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.5) +
  ylim(-1.5, 1.5) +
  labs(
    title = "Poland: Standardized Support by Question (Model 1)",
    subtitle = "Standardized posterior distributions across survey questions",
    x = "",
    y = "Standardized Support (λ / σ²)",
    caption = "Note: Boxes represent interquartile range; whiskers extend to 1.5 x IQR"
  ) +
  scale_fill_manual(values = journal_colors) +
  theme_minimal(base_family = "Arial") +
  theme(
    plot.title = element_text(hjust = 0, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0, size = 12, color = "gray30"),
    plot.caption = element_text(hjust = 0, size = 9, color = "gray30", margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 11, face = "bold", margin = margin(t = 5)),
    panel.grid.major.y = element_line(color = "gray90", size = 0.3),
    panel.grid.minor.y = element_line(color = "gray95", size = 0.2),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(color = "gray80", fill = NA, size = 0.5),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 25, b = 20, l = 25)
  )

# Save the plot as PNG
ggsave(
  filename = "~/projects/bustikova/output/endorse/macro_plot/poland_macro_model1.png",
  plot = box_plot_m1,
  device = "png",
  width = 8,
  height = 6
)