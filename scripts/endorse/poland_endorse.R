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

# Select relevant columns for endorsement analysis
data_pol_questions <- PLData[questions]

# Define variables to keep
vars <- c("Male", "Age", "Education", "Capital", "IdeologyLR", "Income",  "DemPolGrievance", "PolicyPolGrievance",
          "EconGrievanceRetro", "EconGrievanceProspInd", "EconGrievanceProspAgg", "GayNeighbor", "GayFamily", "ForNeighbor", 
          "ForPartner", "Ukraine", "NativeJobs", "NativeRights", "DemonstrateNational", "Religiosity", "VoteFarRight")

# Subset and recode variables
data_pol_vars <- PLData[vars]

# Convert all variables to numeric
data_pol_vars <- mutate(data_pol_vars, across(everything(), ~as.numeric(.)))

# Merge the questions and standardized variables datasets
data_pol <- bind_cols(data_pol_questions, data_pol_vars)

# Create named list for response questions
Y <- list(Q1 = c("Control_A", "Experimental_A"), 
          Q2 = c("Control_B", "Experimental_B"), 
          Q3 = c("Control_C", "Experimental_C"))

#====================================================
# 2. Creating the endorse object
#====================================================

# Creating an endorse object, excluding all covariates that are in the set { traditionalism }

endorse_object <- endorse(Y = Y, 
                          data = data_pol,
                          prop = 0.010,
                          identical.lambda = FALSE,
                          covariates = TRUE,
                          formula.indiv = formula( ~ Male + Age + Education + Capital + IdeologyLR + Income + DemPolGrievance + PolicyPolGrievance +
                                                     EconGrievanceRetro + EconGrievanceProspInd + EconGrievanceProspAgg + GayNeighbor + 
                                                     GayFamily + ForNeighbor + ForPartner + Ukraine + NativeJobs + NativeRights + 
                                                     DemonstrateNational + Religiosity + VoteFarRight
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
    title = "Poland: Metropolis-Hastings Acceptance Ratios by Question",
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
ggsave("~/projects/bustikova/output/endorse/metro_plot/poland_acceptance_ratios.pdf", 
       acceptance_plot, width = 8, height = 6)

#====================================================
# 3. Plotting coefficient plots from the delta matrix 
#====================================================

# Create the dataframe using posterior samples
delta_matrix_values <- data.frame(
  mean = apply(endorse_object$delta[, 2:22], 2, mean),
  lower = apply(endorse_object$delta[, 2:22], 2, quantile, 0.025),
  upper = apply(endorse_object$delta[, 2:22], 2, quantile, 0.975)
)

# Add variable names and categories
delta_matrix_values$variables <- colnames(endorse_object$delta)[2:22]
delta_matrix_values$category <- NA

# Define categories
ses_demographics <- c("Age", "Male", "Education", "Capital", "IdeologyLR", "Income", "Religiosity")
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
  "DemPolGrievance" = "Political Grievance (Democracy)", 
  "PolicyPolGrievance" = "Political Grievance (Policy)", 
  "EconGrievanceRetro" = "Economic Grievance (Retro)", 
  "EconGrievanceProspInd" = "Economic Grievance (Prospective-Ind)", 
  "EconGrievanceProspAgg" = "Economic Grievance (Prospective-Agg)", 
  "DemonstrateNational" = "Demonstrated for National Values", 
  "NativeRights" = "Native Rights", 
  "NativeJobs" = "Native Jobs", 
  "VoteFarRight" = "Far Right Voter", 
  "Religiosity" = "Religiosity", 
  "GayNeighbor" = "Anti-Gay Neighbor", 
  "GayFamily" = "Anti-Gay Family", 
  "ForNeighbor" = "Anti-Foreigner Neighbor", 
  "ForPartner" = "Anti-Foreigner Partner", 
  "Ukraine" = "Anti-Ukrainian Refugee"
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
  ggtitle("Poland: Coefficient Estimates by Explanatory Variable") + 
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
ggsave("~/projects/bustikova/output/endorse/coef_plot/poland_coef_plot.pdf", plot, width = 12, height = 10)