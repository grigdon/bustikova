library(forcats)
library(reshape2)
library(ggpubr)
library(ggplot2)
library(haven)
library(dplyr)

# Read dataset from SPSS file
data_cz <- read_sav("~/projects/bustikova/raw_data/czechia_raw_data.sav")

questions <- c("CD", "Q6AA", "Q6AB", "Q6AC", 
               "Q6BA", "Q6BB", "Q6BC")

data_cz_questions <- data_cz[questions]

# Convert response columns to numeric then recode: 1->4, 4->1, 2->3, 3->2
data_cz_questions <- data_cz_questions %>%
  mutate(across(-CD, ~ as.numeric(.x))) %>%
  mutate(across(-CD, ~ recode(.x,
                              `1` = 4,
                              `4` = 1,
                              `2` = 3,
                              `3` = 2)))

# Map '9' to '3' for all Q06 columns
data_cz_questions <- data_cz_questions %>%
  mutate(across(Q6AA:Q6BC, ~ ifelse(.x == 9, 3, .x)))


# Define a function to calculate the proportion of responses equal to y
prop <- function(x, y) {
  sum(x == y, na.rm = TRUE) / sum(!is.na(x))
}

# Calculate proportions for each response category (1 to 4) for questions 2 to 7
h <- data.frame(
  "Strongly Disagree" = apply(data_cz_questions[2:7], 2, prop, y = 1),
  "Disagree"          = apply(data_cz_questions[2:7], 2, prop, y = 2),
  "Agree"             = apply(data_cz_questions[2:7], 2, prop, y = 3),
  "Strongly Agree"    = apply(data_cz_questions[2:7], 2, prop, y = 4)
)

# Add row names and set a specific order
h$rowname <- rownames(h)

h$rowname <- factor(h$rowname, 
                    levels = c("Q6AA", "Q6BA",
                               "Q6AB", "Q6BB",
                               "Q6AC", "Q6BC"),
                    labels = c("Control A", "Experimental A",
                               "Control B", "Experimental B",
                               "Control C", "Experimental C"))

mh <- melt(h, id.vars = "rowname")

#===============================
# Create Stacked Bar Plot
#===============================
stacked_plot <- ggplot(mh, aes(x = rowname, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack", color = "black", size = 0.2) +
  coord_flip() +
  labs(title = "Czechia: Survey Response Distribution",
       subtitle = "Control vs. Experimental Conditions (4-Point Scale)",
       x = "Survey Question",
       y = "Proportion of Responses",
       fill = "Response Category") +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(face = "italic", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  )

ggsave(filename = "~/projects/bustikova/output/survey_response_distribution/czechia_stacked_bar_graph.png", 
       plot = stacked_plot,
       width = 10, height = 7, device = "png", bg = "white")

#===============================
# Calculate Means for Each Group
#===============================
control_means <- data_cz_questions %>%
  select(Q6AA, Q6AB, Q6AC) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))

treatment_means <- data_cz_questions %>%
  select(Q6BA, Q6BB, Q6BC) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))

# Reshape the means into a long format and add a grouping variable
control_long <- melt(control_means, variable.name = "question", value.name = "mean_response")
control_long$group <- "Control"
treatment_long <- melt(treatment_means, variable.name = "question", value.name = "mean_response")
treatment_long$group <- "Experimental"

# Combine the two groups
mean_df <- rbind(control_long, treatment_long)

# Map the question names to appropriate types based on the pattern
mean_df$question_type <- case_when(
  grepl("Q6AA|Q6BA", mean_df$question) ~ "a",
  grepl("Q6AB|Q6BB", mean_df$question) ~ "b",
  grepl("Q6AC|Q6BC", mean_df$question) ~ "c"
)

# Create factor for proper ordering and labeling
mean_df$question_type <- factor(mean_df$question_type,
                                levels = c("a", "b", "c"),
                                labels = c("Question A", "Question B", "Question C"))

# Create the grouped bar graph
compare_bar_graph <- ggplot(mean_df, aes(x = question_type, y = mean_response, fill = group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, color = "black", size = 0.2) +
  labs(title = "Czechia: Comparison of Mean Survey Responses",
       subtitle = "Control vs. Experimental Conditions (4-Point Scale)",
       x = "Question",
       y = "Mean Response (1 = Strongly Disagree, 4 = Strongly Agree)",
       fill = "Group") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(face = "italic", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  )

ggsave(filename = "~/projects/bustikova/output/average_treatment_standard/czechia_compare_bar_graph.png", 
       plot = compare_bar_graph,
       width = 10, height = 7, device = "png", bg = "white")

#===============================
# Calculate Difference (Experimental - Control)
#===============================
# Calculate means for each question
control_means_vec <- c(
  A = mean(data_cz_questions$Q6AA, na.rm = TRUE),
  B = mean(data_cz_questions$Q6AB, na.rm = TRUE),
  C = mean(data_cz_questions$Q6AC, na.rm = TRUE)
)

experimental_means_vec <- c(
  A = mean(data_cz_questions$Q6BA, na.rm = TRUE),
  B = mean(data_cz_questions$Q6BB, na.rm = TRUE),
  C = mean(data_cz_questions$Q6BC, na.rm = TRUE)
)

# Calculate differences (Experimental - Control)
differences <- experimental_means_vec - control_means_vec

# Create data frame for plotting
diff_df <- data.frame(
  question = factor(names(differences), levels = c("A", "B", "C")),
  difference = differences
)

# Create the difference bar graph
difference_plot <- ggplot(diff_df, aes(x = question, y = difference)) +
  geom_bar(stat = "identity", width = 0.6, 
           fill = ifelse(diff_df$difference >= 0, "steelblue", "coral"),
           color = "black", size = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.5) +
  coord_flip() +
  labs(title = "Czechia: Treatment Effect (Experimental - Control)",
       subtitle = "Difference in Mean Response by Question",
       x = "Question",
       y = "Difference in Mean Response\n(Positive = Higher Experimental Response)",
       caption = "Scale: 1 = Strongly Disagree, 4 = Strongly Agree") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(face = "italic", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 14),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 12, hjust = 0.5)
  ) +
  scale_y_continuous(limits = c(min(differences) - 0.1, max(differences) + 0.1))

ggsave(filename = "~/projects/bustikova/output/average_treatment_difference/czechia_difference_plot.png", 
       plot = difference_plot,
       width = 10, height = 7, device = "png", bg = "white")

#===============================
# Save Numerical Results to PDF
#===============================
library(gridExtra)
library(grid)

# Create formatted tables for PDF
pdf("~/projects/bustikova/output/numerical_results/czechia_numerical_results.pdf", width = 11, height = 8.5)

# Page 1: Response Proportions
grid.newpage()
grid.text("Czechia Analysis: Numerical Results", x = 0.5, y = 0.95, 
          gp = gpar(fontsize = 20, fontface = "bold"))
grid.text("Response Proportions by Question", x = 0.5, y = 0.85, 
          gp = gpar(fontsize = 16, fontface = "bold"))

# Format proportion table
prop_table <- round(h[1:4], 3)
prop_table$Question <- rownames(prop_table)
prop_table <- prop_table[, c(5, 1:4)]  # Reorder columns

# Convert to table for display
prop_grob <- tableGrob(prop_table, rows = NULL, 
                       theme = ttheme_default(base_size = 12))
grid.draw(prop_grob)

# Page 2: Treatment Effects
grid.newpage()
grid.text("Treatment Effects (Experimental - Control)", x = 0.5, y = 0.95, 
          gp = gpar(fontsize = 18, fontface = "bold"))

# Create comprehensive results table
results_table <- data.frame(
  Question = names(differences),
  Control_Mean = round(control_means_vec, 3),
  Experimental_Mean = round(experimental_means_vec, 3),
  Difference = round(differences, 3),
  Effect_Direction = ifelse(differences > 0, "Positive", "Negative"),
  stringsAsFactors = FALSE
)

results_grob <- tableGrob(results_table, rows = NULL,
                          theme = ttheme_default(base_size = 12))
grid.draw(results_grob)

# Add summary statistics as text
grid.text("Summary Statistics:", x = 0.1, y = 0.4, 
          gp = gpar(fontsize = 14, fontface = "bold"), just = "left")
grid.text(paste("Mean treatment effect:", round(mean(differences), 3)), 
          x = 0.1, y = 0.35, gp = gpar(fontsize = 12), just = "left")
grid.text(paste("Range of effects:", round(min(differences), 3), "to", round(max(differences), 3)), 
          x = 0.1, y = 0.32, gp = gpar(fontsize = 12), just = "left")
grid.text(paste("Number of positive effects:", sum(differences > 0)), 
          x = 0.1, y = 0.29, gp = gpar(fontsize = 12), just = "left")
grid.text(paste("Number of negative effects:", sum(differences < 0)), 
          x = 0.1, y = 0.26, gp = gpar(fontsize = 12), just = "left")

# Page 3: Sample Sizes
grid.newpage()
grid.text("Sample Sizes (Non-missing Responses)", x = 0.5, y = 0.95, 
          gp = gpar(fontsize = 18, fontface = "bold"))

sample_sizes <- data.frame(
  Question = c("Q6AA (Control A)", "Q6AB (Control B)", "Q6AC (Control C)", 
               "Q6BA (Experimental A)", "Q6BB (Experimental B)", "Q6BC (Experimental C)"),
  N = c(
    sum(!is.na(data_cz_questions$Q6AA)),
    sum(!is.na(data_cz_questions$Q6AB)),
    sum(!is.na(data_cz_questions$Q6AC)),
    sum(!is.na(data_cz_questions$Q6BA)),
    sum(!is.na(data_cz_questions$Q6BB)),
    sum(!is.na(data_cz_questions$Q6BC))
  )
)

sample_grob <- tableGrob(sample_sizes, rows = NULL,
                         theme = ttheme_default(base_size = 12))
grid.draw(sample_grob)

# Add transformation note
grid.text("Note: All values coded as '9' were recoded to '3' (Agree) before analysis", 
          x = 0.5, y = 0.2, gp = gpar(fontsize = 10, fontface = "italic"))

dev.off()

cat("Numerical results saved to: ~/projects/bustikova/output/czechia_numerical_results.pdf")

# Clean up
rm(list = ls())