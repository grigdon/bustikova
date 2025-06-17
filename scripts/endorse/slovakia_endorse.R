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
# 1. Data Loading and Initial Processing for Slovakia
#====================================================

SKdata <- read_sav("~/projects/bustikova/data/scrubbed_data/slovakia_scrubbed.sav")

# Define questions for the endorsement experiment
questions <- c("Control_A", "Control_B", "Control_C", 
               "Experimental_A", "Experimental_B", "Experimental_C")

# Select relevant columns for endorsement analysis
data_slvk_questions <- SKdata[questions]

# Define vars to keep 

vars <- c("age", "male", "educ", "capital", "ideology", "income", "DemPolGrievance", "PolicyPolGrievance",
          "EconGrievenceRetro", "EconGrievenceProspInd", "EconGrievenceProspAgg", "NativeRights", "NativeJobs",
          "DemonstrateNational", "GayNeighbor", "GayFamily", "ForNeighbor", "ForPartner", "Ukraine", "VoteFarRight",
          "FAMincome", "Religiosity"  
)

# Subset and recode variables
data_slvk_vars <- SKdata[vars]

# Merge the questions and standardized variables datasets
data_slvk <- bind_cols(data_slvk_questions, data_slvk_vars)

# Create named list for response questions
Y <- list(Q1 = c("Control_A", "Experimental_A"), 
          Q2 = c("Control_B", "Experimental_B"), 
          Q3 = c("Control_C", "Experimental_C"))

#====================================================
# 2. Creating the endorse object
#====================================================

# Creating an endorse object, excluding all covariates that are in the set { traditionalism }

endorse_object <- endorse(Y = Y, 
                          data = data_slvk,
                          identical.lambda = FALSE,
                          covariates = TRUE,
                          prop = 0.010,
                          formula.indiv = formula( ~ age + male + educ + 
                                                     capital + ideology + income + 
                                                     DemPolGrievance + PolicyPolGrievance + 
                                                     EconGrievenceRetro + EconGrievenceProspInd + 
                                                     EconGrievenceProspAgg + 
                                                     NativeRights + NativeJobs +
                                                     DemonstrateNational +
                                                     GayNeighbor+ GayFamily + ForNeighbor +
                                                     ForPartner + Ukraine + VoteFarRight + 
                                                     FAMincome + Religiosity
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
    title = "Slovakia: Metropolis-Hastings Acceptance Ratios by Question",
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
ggsave("~/projects/bustikova/output/endorse/metro_plot/slovakia_acceptance_ratios.pdf", 
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
ses_demographics <- c("age", "male", "educ", "capital", "ideology", "income", "FAMincome", "Religiosity")
political_economic_grievances <- c("DemPolGrievance", "PolicyPolGrievance", "EconGrievenceRetro", "EconGrievenceProspInd", "EconGrievenceProspAgg")
nationalism <- c("NativeRights", "NativeJobs", "DemonstrateNational", "VoteFarRight")
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
  "age" = "Age", 
  "male" = "Male", 
  "educ" = "Education", 
  "capital" = "Capital", 
  "ideology" = "Political Ideology", 
  "income" = "Personal Income", 
  "FAMincome" = "Family Income", 
  "DemPolGrievance" = "Political Grievance (Democracy)", 
  "PolicyPolGrievance" = "Political Grievance (Policy)", 
  "EconGrievenceRetro" = "Economic Grievance (Retro)", 
  "EconGrievenceProspInd" = "Economic Grievance (Prospective-Ind)", 
  "EconGrievenceProspAgg" = "Economic Grievance (Prospective-Agg)", 
  "NativeRights" = "Native Rights", 
  "NativeJobs" = "Native Jobs", 
  "DemonstrateNational" = "Demonstrated for National Values", 
  "VoteFarRight" = "Far Right Voter", 
  "GayNeighbor" = "Anti-Gay Neighbor", 
  "GayFamily" = "Anti-Gay Family", 
  "ForNeighbor" = "Anti-Foreigner Neighbor", 
  "ForPartner" = "Anti-Foreigner Partner", 
  "Ukraine" = "Anti-Ukrainian Refugee", 
  "Religiosity" = "Religiosity" 
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
  ggtitle("Support for Forum for Life in Slovakia") + 
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
ggsave("~/projects/bustikova/output/endorse/coef_plot/slovakia_coef_plot.pdf", plot, width = 12, height = 10)
