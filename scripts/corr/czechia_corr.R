# Load required libraries
library(corrplot)
library(haven)
library(dplyr)

# Read dataset 
data_cz <- read_sav("~/projects/bustikova/data/scrubbed_data/czechia_scrubbed.sav")

# Select only explanatory variables
data_cz <- data_cz[, 7:28]

# Define custom labels for variables

custom_labels <- c( "Age" = "Age",
                    "Male" = "Male",
                    "Education" = "Education",
                    "Capital" = "Capital",
                    "IdeologyLR" = "Political Ideology",
                    "Income" = "Personal Income",
                    "FamIncome" = "Family Income",
                    "DemPolGrievance" = "Political Grievance (Democracy)",
                    "PolicyPolGrievance" = "Policy Grievance",
                    "EconGrievanceRetro" = "Economic Grievance (Retro)",
                    "EconGrievanceProspInd" = "Economic Grievance (Prospective-Ind)",
                    "EconGrievanceProspAgg" = "Economic Grievance (Prospective-Agg)",
                    "EconGrievanceProspMostFams" = "Economic Grievance (ProspMostFams)",
                    "NativeRights" = "Native Rights",
                    "NativeJobs" = "Native Jobs",
                    "VoteFarRight" = "Far Right Voter",
                    "Religiosity" = "Religiosity",
                    "GayNeighbor" = "Anti-Gay Neighbor",
                    "GayFamily" = "Anti-Gay Family",
                    "ForNeighbor" = "Anti-Foreigner Neighbor",
                    "ForPartner" = "Anti-Foreigner Neighbor",
                    "Ukraine" = "Anti-Ukrainian Refugee"
)

# Calculate Spearman correlation coefficients
cor_matrix_cz <- cor(data_cz, method = "spearman", use = "pairwise.complete.obs")

# Get original column names to verify length
original_names <- colnames(cor_matrix_cz)

# Ensure custom labels match the number of variables
if(length(custom_labels) != length(original_names)) {
  stop("Number of custom labels doesn't match number of variables!")
}

# Set custom labels as the column/row names
colnames(cor_matrix_cz) <- custom_labels
rownames(cor_matrix_cz) <- custom_labels

# Create a correlation heatmap and define output directory

output_dir = "~/projects/bustikova/output/corr"

pdf(file = file.path(output_dir, "czechia_corr.pdf"), width = 8, height = 6)
corr_plot <- corrplot(cor_matrix_cz, 
                      method = "color",           # Color tiles
                      type = "upper",             # Show only upper triangle
                      order = "hclust",           # Hierarchical clustering order
                      hclust.method = "complete", # Clustering method
                      tl.cex = 0.7,               # Text label size
                      tl.col = "black",           # Text label color
                      tl.srt = 60,                # Text label rotation
                      number.cex = 0.5,           # Coefficient text size
                      col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200), # Blue-white-orange palette
                      diag = FALSE,               # Hide diagonal
                      mar = c(0,0,2,0),           # Margins
                      title = "Interdependencies Between Czech Explanatory Factors: Spearman Correlation",
                      cl.ratio = 0.2,             # Color key size
                      cl.align = "r",             # Color key alignment
)
dev.off()

# clears environment variables
rm(list = ls())