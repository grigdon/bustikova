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
# 1. Data Loading and Initial Processing for Hungary
#====================================================
HUData <- read_sav("~/projects/bustikova/data/scrubbed_data/hungary_scrubbed.sav")

# Define questions for the endorsement experiment
questions <- c("A6A_1", "A6A_2", "A6A_3",
               "A6B_1", "A6B_2", "A6B_3")

# Select relevant columns for endorsement analysis
data_hu_questions <- HUData[questions]

# Define vars to keep based on the provided list and summary() output
vars_to_keep <- c("ChildHome", "ChurchPolitics", "LawOrder", "GayPartner", "Religiosity",
                  "Children", "MaleJobs", "Age", "Capital", "Married", "Income",
                  "Gender", "EconomicFuture", "Education", "GovDissatisfaction", "Fidesz_vote")

# Subset variables
data_hu_vars <- HUData[vars_to_keep]

# Convert all variables to numeric
data_hu_vars <- mutate(data_hu_vars, across(everything(), ~as.numeric(.)))

# Merge the questions and variables datasets
data_hu <- bind_cols(data_hu_questions, data_hu_vars)

# Standardize 'Age' and 'Income'
data_hu$Age_std <- scale(data_hu$Age)
data_hu$Income_std <- scale(data_hu$Income)

#====================================================
# 2. Creating three separate single-lambda models
#====================================================

# Model for Question 1 (Q1)
Y_Q1 <- list(Q1 = c("A6A_1", "A6B_1"))

endorse_Q1 <- endorse(Y = Y_Q1,
                      data = data_hu,
                      identical.lambda = TRUE,  # Single lambda
                      covariates = TRUE,
                      prop = 0.008,
                      formula.indiv = formula( ~ ChildHome + ChurchPolitics + LawOrder + GayPartner + Religiosity +
                                                 Children + MaleJobs + Age_std + Capital + Married + Income_std +
                                                 Gender + EconomicFuture + Education + GovDissatisfaction),
                      omega2.out = TRUE,
                      hierarchical = FALSE
                      #burn = 200,
                      #MCMC = 1000
)

# Model for Question 2 (Q2)
Y_Q2 <- list(Q1 = c("A6A_2", "A6B_2"))

endorse_Q2 <- endorse(Y = Y_Q2,
                      data = data_hu,
                      identical.lambda = TRUE,  # Single lambda
                      covariates = TRUE,
                      prop = 0.008,
                      formula.indiv = formula( ~ ChildHome + ChurchPolitics + LawOrder + GayPartner + Religiosity +
                                                 Children + MaleJobs + Age_std + Capital + Married + Income_std +
                                                 Gender + EconomicFuture + Education + GovDissatisfaction),
                      omega2.out = TRUE,
                      hierarchical = FALSE

)

# Model for Question 3 (Q3)
Y_Q3 <- list(Q1 = c("A6A_3", "A6B_3"))

endorse_Q3 <- endorse(Y = Y_Q3,
                      data = data_hu,
                      identical.lambda = TRUE,  # Single lambda
                      covariates = TRUE,
                      prop = 0.008,
                      formula.indiv = formula( ~ ChildHome + ChurchPolitics + LawOrder + GayPartner + Religiosity +
                                                 Children + MaleJobs + Age_std + Capital + Married + Income_std +
                                                 Gender + EconomicFuture + Education + GovDissatisfaction),
                      omega2.out = TRUE,
                      hierarchical = FALSE
)

#====================================================
# 3. Generate predictions for marginal effects
#====================================================

# Function to create prediction data for marginal effects
create_marginal_data <- function(data, var_name, n_points = 100) {
  # Get the range of the variable
  var_range <- range(data[[var_name]], na.rm = TRUE)

  # Create sequence of values
  var_seq <- seq(var_range[1], var_range[2], length.out = n_points)

  # Create baseline data (set all other variables to their means/modes)
  baseline_data <- data %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE),
              across(where(is.factor), ~names(sort(table(.), decreasing = TRUE))[1]),
              across(where(is.character), ~names(sort(table(.), decreasing = TRUE))[1])) %>%
    slice(rep(1, n_points))

  # Replace the variable of interest with the sequence
  baseline_data[[var_name]] <- var_seq

  return(baseline_data)
}

# Create marginal effect data for the three key variables
vars_of_interest <- c("Religiosity", "GayPartner", "ChurchPolitics")

#====================================================
# 4. Generate predictions and create plots
#====================================================

# Function to generate marginal effects plot
create_marginal_plot <- function(endorse_model, var_name, question_name, data) {
  # Create prediction data
  pred_data <- create_marginal_data(data, var_name)

  # Generate predictions
  predictions <- predict(endorse_model,
                         newdata = pred_data,
                         type = "prob.support")

  # Calculate summary statistics (mean and credible intervals)
  pred_summary <- data.frame(
    var_value = pred_data[[var_name]],
    mean_prob = apply(predictions, 2, mean),
    lower_ci = apply(predictions, 2, quantile, 0.025),
    upper_ci = apply(predictions, 2, quantile, 0.975)
  )

  # Create plot
  ggplot(pred_summary, aes(x = var_value, y = mean_prob)) +
    geom_line(color = "blue", size = 1) +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.3, fill = "blue") +
    labs(title = paste("Marginal Effect of", var_name, "on Endorser Support"),
         subtitle = paste("Question:", question_name),
         x = var_name,
         y = "Probability of Positive Support") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
}

# Generate plots for all combinations
plots_list <- list()

for (var in vars_of_interest) {
  plots_list[[paste0("Q1_", var)]] <- create_marginal_plot(endorse_Q1, var, "Q1", data_hu)
  plots_list[[paste0("Q2_", var)]] <- create_marginal_plot(endorse_Q2, var, "Q2", data_hu)
  plots_list[[paste0("Q3_", var)]] <- create_marginal_plot(endorse_Q3, var, "Q3", data_hu)
}

# Arrange plots in a grid
# Religiosity across all questions
religiosity_plots <- ggarrange(plots_list[["Q1_Religiosity"]],
                               plots_list[["Q2_Religiosity"]],
                               plots_list[["Q3_Religiosity"]],
                               ncol = 3, nrow = 1)

# GayPartner across all questions
gaypartner_plots <- ggarrange(plots_list[["Q1_GayPartner"]],
                              plots_list[["Q2_GayPartner"]],
                              plots_list[["Q3_GayPartner"]],
                              ncol = 3, nrow = 1)

# ChurchPolitics across all questions
churchpolitics_plots <- ggarrange(plots_list[["Q1_ChurchPolitics"]],
                                  plots_list[["Q2_ChurchPolitics"]],
                                  plots_list[["Q3_ChurchPolitics"]],
                                  ncol = 3, nrow = 1)

# Display the plots
print("Religiosity Effects Across Questions:")
print(religiosity_plots)

print("GayPartner Effects Across Questions:")
print(gaypartner_plots)

print("ChurchPolitics Effects Across Questions:")
print(churchpolitics_plots)

# Create a comprehensive summary plot
all_plots <- ggarrange(religiosity_plots, gaypartner_plots, churchpolitics_plots,
                       ncol = 1, nrow = 3,
                       labels = c("Religiosity", "Gay Partner Acceptance", "Church Politics"))

print("Comprehensive Marginal Effects Summary:")
print(all_plots)

#====================================================
# 5. Save plots (optional)
#====================================================

# Uncomment to save plots
# ggsave("religiosity_marginal_effects.png", religiosity_plots, width = 15, height = 5, dpi = 300)
# ggsave("gaypartner_marginal_effects.png", gaypartner_plots, width = 15, height = 5, dpi = 300)
# ggsave("churchpolitics_marginal_effects.png", churchpolitics_plots, width = 15, height = 5, dpi = 300)
# ggsave("all_marginal_effects.png", all_plots, width = 15, height = 15, dpi = 300)

#====================================================
# 6. Summary statistics for interpretation
#====================================================

# Function to extract coefficient summaries
extract_coef_summary <- function(model, model_name) {
  # Extract lambda coefficients (these are the main effects of interest)
  lambda_posterior <- model$lambda

  # Calculate summary statistics
  coef_summary <- summary(lambda_posterior)

  # Create a cleaner summary focusing on our variables of interest
  vars_indices <- grep("Religiosity|GayPartner|ChurchPolitics", rownames(coef_summary$statistics))

  if(length(vars_indices) > 0) {
    clean_summary <- data.frame(
      Variable = rownames(coef_summary$statistics)[vars_indices],
      Mean = coef_summary$statistics[vars_indices, "Mean"],
      SD = coef_summary$statistics[vars_indices, "SD"],
      Lower_CI = coef_summary$quantiles[vars_indices, "2.5%"],
      Upper_CI = coef_summary$quantiles[vars_indices, "97.5%"],
      Model = model_name
    )
    return(clean_summary)
  }
  return(NULL)
}

# Extract summaries for all models
summary_Q1 <- extract_coef_summary(endorse_Q1, "Q1")
summary_Q2 <- extract_coef_summary(endorse_Q2, "Q2")
summary_Q3 <- extract_coef_summary(endorse_Q3, "Q3")

# Combine all summaries
all_summaries <- rbind(summary_Q1, summary_Q2, summary_Q3)

print("Coefficient Summaries for Key Variables:")
print(all_summaries)

# Create coefficient plot
if(nrow(all_summaries) > 0) {
  coef_plot <- ggplot(all_summaries, aes(x = Variable, y = Mean, color = Model)) +
    geom_point(position = position_dodge(width = 0.3), size = 3) +
    geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI),
                  position = position_dodge(width = 0.3), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = "Coefficient Estimates for Key Variables",
         subtitle = "Effect on Support Parameter (Lambda)",
         x = "Variable",
         y = "Coefficient Estimate") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    coord_flip()

  print("Coefficient Plot:")
  print(coef_plot)
}

cat("\n=== Analysis Complete ===\n")
cat("Three single-lambda models have been fitted for questions Q1, Q2, and Q3.\n")
cat("Marginal effects plots show how Religiosity, GayPartner, and ChurchPolitics\n")
cat("affect the probability of positive support for endorsers in each question.\n")
cat("Use the plots above to interpret the substantive effects of these variables.\n")