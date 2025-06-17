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
# 1. Data Loading and Initial Processing for Czechia
#====================================================

CZdata <- read_sav("~/projects/bustikova/data/scrubbed_data/czechia_scrubbed.sav")

# Define questions for the endorsement experiment
questions <- c("Control_A", "Control_B", "Control_C", 
               "Experimental_A", "Experimental_B", "Experimental_C")

# Select relevant columns for endorsement analysis
data_cz_questions <- CZdata[questions]

# Define vars to keep 
vars <- c("Male", "Age", "Education", "Capital", "IdeologyLR", "Income", "FamIncome", "DemPolGrievance", "PolicyPolGrievance",
          "EconGrievanceRetro", "EconGrievanceProspInd", "EconGrievanceProspAgg", "GayNeighbor", "GayFamily", "ForNeighbor",
          "ForPartner", "Ukraine", "NativeJobs", "NativeRights", "Religiosity", "VoteFarRight", "DemonstrateNational")

# Subset and recode variables
data_cz_vars <- CZdata[vars]

# Merge the questions and standardized variables datasets
data_cz <- bind_cols(data_cz_questions, data_cz_vars)

# Create named list for response questions
Y <- list(Q1 = c("Control_A", "Experimental_A"), 
          Q2 = c("Control_B", "Experimental_B"), 
          Q3 = c("Control_C", "Experimental_C"))

#====================================================
# 2. Creating the endorse object
#====================================================

# Creating an endorse object, excluding all covariates that are in the set { traditionalism }

endorse_object <- endorse(Y = Y, 
                          data = data_cz,
                          identical.lambda = FALSE,
                          covariates = TRUE,
                          prop = 0.008,
                          formula.indiv = formula( ~ Male + Age + Education + Capital + IdeologyLR + Income + FamIncome + DemPolGrievance +
                                                     PolicyPolGrievance + EconGrievanceRetro + EconGrievanceProspInd + EconGrievanceProspAgg +
                                                     GayNeighbor + GayFamily + ForNeighbor + ForPartner + Ukraine + NativeJobs + NativeRights + 
                                                     Religiosity + DemonstrateNational + VoteFarRight
                          ),
                          omega2.out = TRUE,
                          hierarchical = FALSE
)

#====================================================
# 2.5 Output the acceptance ratio for each question
#====================================================

# Extract acceptance ratios from the endorse object
acceptance_ratios <- data.frame(
  Question = paste("Question", 1:3),
  Ratio = endorse_object$accept.ratio
)

# Create a bar plot of acceptance ratios
acceptance_plot <- ggplot(acceptance_ratios, aes(x = Question, y = Ratio)) +
  geom_bar(stat = "identity", fill = "#4682B4", width = 0.6) +
  geom_text(aes(label = sprintf("%.3f", Ratio)), vjust = -0.5, size = 4) +
  scale_y_continuous(limits = c(0, 1.0), breaks = seq(0, 1, by = 0.1)) +
  labs(
    title = "Czechia: Metropolis-Hastings Acceptance Ratios by Question",
    x = NULL,
    y = "Acceptance Ratio"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold")
  )

# Save the plot
ggsave("~/projects/bustikova/output/endorse/metro_plot/czechia_acceptance_ratios.pdf", 
       acceptance_plot, width = 8, height = 6)

#====================================================
# 3. Plotting coefficient plots from the delta matrix
#====================================================

# Create the dataframe using posterior samples
delta_matrix_values <- data.frame(
  mean = apply(endorse_object$delta[, 2:23], 2, mean),
  lower = apply(endorse_object$delta[, 2:23], 2, quantile, 0.025),
  upper = apply(endorse_object$delta[, 2:23], 2, quantile, 0.975)
)

# Add variable names and categories
delta_matrix_values$variables <- colnames(endorse_object$delta)[2:23]
delta_matrix_values$category <- NA

# Define categories
ses_demographics <- c("Age", "Male", "Education", "Capital", "IdeologyLR", "Income", "FamIncome", "Religiosity")
political_economic_grievances <- c("DemPolGrievance", "PolicyPolGrievance", "EconGrievanceRetro", "EconGrievanceProspInd", "EconGrievanceProspAgg")
nationalism <- c( "NativeRights", "NativeJobs", "VoteFarRight", "DemonstrateNational")
boundary_maintenance <- c("GayNeighbor", "GayFamily", "ForNeighbor", "ForPartner", "Ukraine")

# Assign categories
delta_matrix_values <- delta_matrix_values %>%
  mutate(
    category = case_when(
      variables %in% ses_demographics ~ "SES Demographics",
      variables %in% political_economic_grievances ~ "Political & Economic Grievances",
      variables %in% nationalism ~ "Nationalism",
      variables %in% boundary_maintenance ~ "Boundary Maintenance & Prejudice"
    )
  )

# Reorder variables within each category by mean
delta_matrix_values <- delta_matrix_values %>%
  group_by(category) %>%
  mutate(variables = fct_reorder(variables, mean)) %>%
  ungroup()

# Reorder categories
category_order <- c(
  "SES Demographics", 
  "Political & Economic Grievances", 
  "Nationalism", 
  "Boundary Maintenance & Prejudice"
)
delta_matrix_values$category <- factor(delta_matrix_values$category, levels = category_order)

# Define custom labels for variables
custom_labels <- c(
  "Age" = "Age",
  "Male" = "Male",
  "Education" = "Education",
  "Capital" = "Capital",
  "IdeologyLR" = "Political Ideology",
  "Income" = "Personal Income",
  "FamIncome" = "Family Income",
  "DemPolGrievance" = "Political Grievance (Democracy)",
  "PolicyPolGrievance" = "Political Grievance (Policy)",
  "EconGrievanceRetro" = "Economic Grievance (Retro)",
  "EconGrievanceProspInd" = "Economic Grievance (Prospective-Ind)",
  "EconGrievanceProspAgg" = "Economic Grievance (Prospective-Agg)",
  "NativeRights" = "Native Rights",
  "NativeJobs" = "Native Jobs",
  "VoteFarRight" = "Far Right Voter",
  "Religiosity" = "Religiosity",
  "GayNeighbor" = "Anti-Gay Neighbor",
  "GayFamily" = "Anti-Gay Family",
  "ForNeighbor" = "Anti-Foreigner Neighbor",
  "ForPartner" = "Anti-Foreigner Partner",
  "Ukraine" = "Anti-Ukrainian Refugee",
  "DemonstrateNational" = "Demonstrated for National Values"
)

# Create the plot
plot <- ggplot(delta_matrix_values, aes(x = variables, y = mean)) +
  geom_point(size = 1, shape = 10) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 1, size = .25) + 
  coord_flip() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.5) + 
  facet_grid(category ~ ., scales = "free_y", space = "free_y") +
  scale_x_discrete(labels = custom_labels) +
  theme_classic() +
  ggtitle("Czechia: Coefficient Estimates by Explanatory Variable") + 
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

# Save to PDF
ggsave("~/projects/bustikova/output/endorse/coef_plot/czechia_coef_plot.pdf", plot, width = 12, height = 10)

######################################
# Extract and standardize MCMC samples
# --------------------------------------
# Extract MCMC samples for lambda and sigma²
lambda_samples <- as.matrix(endorse_object$lambda)
sigma2 <- (endorse_object$x)^2  # Convert standard deviation to variance

# Calculate the row-wise average of the three lambda parameters
avg_lambda <- (lambda_samples[, "(Intercept).1.1"] + 
                 lambda_samples[, "(Intercept).2.1"] + 
                 lambda_samples[, "(Intercept).3.1"]) / 3

# Include this average in the lambda_std list
lambda_std <- list(
  q1 = lambda_samples[, "(Intercept).1.1"] / sigma2,
  q2 = lambda_samples[, "(Intercept).2.1"] / sigma2,
  q3 = lambda_samples[, "(Intercept).3.1"] / sigma2,
  average = avg_lambda / sigma2  # Standardize the average lambda just like the others
)

# Calculate summary statistics
# --------------------------------------
# Calculate mean and confidence intervals for each question
question_stats <- lapply(lambda_std, function(x) {
  list(
    mean = mean(x),
    ci = quantile(x, probs = c(0.025, 0.975))
  )
})

# Create visualizations
# --------------------------------------
library(ggplot2)
library(reshape2)
library(extrafont)  # For professional fonts
library(gridExtra)  # For adding table below plot

# Load additional fonts if available
# font_import()
# loadfonts(device = "win")  # Adjust for your OS

# Create dataframe in long format with average
box_data_long <- data.frame(
  Question = factor(rep(c("Question A", "Question B", "Question C", "Average"), 
                        times = c(length(lambda_std$q1), length(lambda_std$q2), 
                                  length(lambda_std$q3), length(lambda_std$average))),
                    levels = c("Question A", "Question B", "Question C", "Average")),
  Support = c(lambda_std$q1, lambda_std$q2, lambda_std$q3, lambda_std$average)
)

# Set a professional color palette suitable for publication
# Using a colorblind-friendly palette
journal_colors <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7")

# Create publication-quality box plot
box_plot <- ggplot(box_data_long, aes(x = Question, y = Support, fill = Question)) +
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
  # Add horizontal reference line at 0http://127.0.0.1:33439/graphics/11e2db3c-b487-4933-abbc-13eaaa6d06f1.png
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.5) +
  # Fixed y-axis limits as requested
  ylim(-1.5, 1.5) +
  # Labels
  labs(
    title = "Czechia: Standardized Support by Question",
    subtitle = "Standardized posterior distributions across survey questions",
    x = "",  # No x-axis label needed
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

# Create a table with summary statistics for display below the plot
stats_table <- data.frame(
  Question = c("Question A", "Question B", "Question C", "Average"),
  Mean = sapply(question_stats, function(x) round(x$mean, 3)),
  Lower_CI = sapply(question_stats, function(x) round(x$ci[1], 3)),
  Upper_CI = sapply(question_stats, function(x) round(x$ci[2], 3))
)

# Format table for display
table_theme <- ttheme_minimal(
  core = list(fg_params = list(hjust = 1, x = 0.9, fontface = "plain"),
              bg_params = list(fill = c("white", "gray95"))),
  colhead = list(fg_params = list(fontface = "bold", hjust = 1, x = 0.9),
                 bg_params = list(fill = "white")),
  rowhead = list(fg_params = list(hjust = 0, x = 0.1)))

# save the plot

ggsave(
  filename = "~/projects/bustikova/output/endorse/macro_plot/czechia_macro.pdf",
  plot = box_plot,
  device = Cairo::CairoPDF,
  width = 8,
  height = 6
)