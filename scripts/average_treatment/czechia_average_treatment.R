library(forcats)
library(reshape2)
library(ggpubr)
library(ggplot2)
library(haven)
library(dplyr)

# Read dataset from SPSS file
data_cz <- read_sav("~/projects/bustikova/raw_data/czechia_raw_data.sav")

questions <- c("Q6AA", "Q6AB", "Q6AC", 
               "Q6BA", "Q6BB", "Q6BC")

data_cz_questions <- data_cz[questions]

# Shift up by 1 only if the original is 3 or 4; leave 1, 2, and NA alone
data_cz_questions[] <- lapply(
  data_cz_questions,
  function(x) ifelse(x %in% c(1,2) | is.na(x), x, x + 1)
)

head(data_cz_questions)

# Map '10' to '3' for all Q06 columns
data_cz_questions <- data_cz_questions %>%
  mutate(across(Q6AA:Q6BC, ~ ifelse(.x == 10, 3, .x)))

head(data_cz_questions)

######################################################
# At this point, the scale has been properly reordered
######################################################
library(haven)
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(grid)

#=====================================
# Load in new data
#=====================================
scrubbed_cz_data <- read_sav("~/projects/bustikova/output/scrubbed_data/czechia_scrubbed_questions.sav")

# Define a function to calculate the proportion of responses equal to y
prop <- function(x, y) {
  sum(x == y, na.rm = TRUE) / sum(!is.na(x))
}

# Calculate proportions for each response category (1 to 4)
h <- data.frame(
  "Strongly Disagree" = apply(scrubbed_cz_data[ , c("Q6AA", "Q6AB", "Q6AC", "Q6BA", "Q6BB", "Q6BC")], 2, prop, y = 1),
  "Disagree"          = apply(scrubbed_cz_data[ , c("Q6AA", "Q6AB", "Q6AC", "Q6BA", "Q6BB", "Q6BC")], 2, prop, y = 2),
  "Agree"             = apply(scrubbed_cz_data[ , c("Q6AA", "Q6AB", "Q6AC", "Q6BA", "Q6BB", "Q6BC")], 2, prop, y = 3),
  "Strongly Agree"    = apply(scrubbed_cz_data[ , c("Q6AA", "Q6AB", "Q6AC", "Q6BA", "Q6BB", "Q6BC")], 2, prop, y = 4)
)

# Add and reorder row names
h$rowname <- rownames(h)
h$rowname <- factor(h$rowname,
                    levels = c("Q6AA", "Q6BA", "Q6AB", "Q6BB", "Q6AC", "Q6BC"),
                    labels = c("Control A", "Experimental A", "Control B", "Experimental B", "Control C", "Experimental C"))

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
       plot = stacked_plot, width = 10, height = 7, device = "png", bg = "white")

#===============================
# Calculate Means for Each Group
#===============================
control_means <- scrubbed_cz_data %>%
  select(Q6AA, Q6AB, Q6AC) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))

treatment_means <- scrubbed_cz_data %>%
  select(Q6BA, Q6BB, Q6BC) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))

# Reshape the means into a long format and add a grouping variable
control_long <- melt(control_means, variable.name = "question", value.name = "mean_response")
control_long$group <- "Control"
treatment_long <- melt(treatment_means, variable.name = "question", value.name = "mean_response")
treatment_long$group <- "Experimental"

# Combine both
mean_df <- rbind(control_long, treatment_long)

# Add question types
mean_df$question_type <- case_when(
  grepl("Q6AA|Q6BA", mean_df$question) ~ "a",
  grepl("Q6AB|Q6BB", mean_df$question) ~ "b",
  grepl("Q6AC|Q6BC", mean_df$question) ~ "c"
)

mean_df$question_type <- factor(mean_df$question_type,
                                levels = c("a", "b", "c"),
                                labels = c("Question A", "Question B", "Question C"))

#===============================
# Create Grouped Bar Graph
#===============================
compare_bar_graph <- ggplot(mean_df, aes(x = question_type, y = mean_response, fill = group)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7, color = "black", size = 0.2) +
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
    legend.position = "bottom"
  )

ggsave(filename = "~/projects/bustikova/output/average_treatment_standard/czechia_compare_bar_graph.png", 
       plot = compare_bar_graph, width = 10, height = 7, device = "png", bg = "white")

#===============================
# Treatment Effect (Difference)
#===============================
control_means_vec <- c(
  A = mean(scrubbed_cz_data$Q6AA, na.rm = TRUE),
  B = mean(scrubbed_cz_data$Q6AB, na.rm = TRUE),
  C = mean(scrubbed_cz_data$Q6AC, na.rm = TRUE)
)

experimental_means_vec <- c(
  A = mean(scrubbed_cz_data$Q6BA, na.rm = TRUE),
  B = mean(scrubbed_cz_data$Q6BB, na.rm = TRUE),
  C = mean(scrubbed_cz_data$Q6BC, na.rm = TRUE)
)

differences <- experimental_means_vec - control_means_vec

diff_df <- data.frame(
  question = factor(names(differences), levels = c("A", "B", "C")),
  difference = differences
)

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
    plot.caption = element_text(size = 12, hjust = 0.5)
  ) +
  scale_y_continuous(limits = c(min(differences) - 0.1, max(differences) + 0.1))

ggsave(filename = "~/projects/bustikova/output/average_treatment_difference/czechia_difference_plot.png", 
       plot = difference_plot, width = 10, height = 7, device = "png", bg = "white")

#===============================
# Save Numerical Results to PDF
#===============================
pdf("~/projects/bustikova/output/numerical_results/czechia_numerical_results.pdf", width = 11, height = 8.5)

# Page 1: Response proportions
grid.newpage()
grid.text("Czechia Analysis: Response Proportions", x = 0.5, y = 0.95, gp = gpar(fontsize = 16, fontface = "bold"))
prop_table <- round(h[, 1:4], 3)
rownames(prop_table) <- levels(h$rowname)
grid.table(prop_table)

# Page 2: Mean responses
grid.newpage()
grid.text("Czechia Analysis: Mean Responses", x = 0.5, y = 0.95, gp = gpar(fontsize = 16, fontface = "bold"))
mean_table <- data.frame(
  Question = mean_df$question,
  Group = mean_df$group,
  Mean_Response = round(mean_df$mean_response, 2)
)
grid.table(mean_table)

# Page 3: Differences
grid.newpage()
grid.text("Czechia Analysis: Treatment Effects", x = 0.5, y = 0.95, gp = gpar(fontsize = 16, fontface = "bold"))
grid.table(data.frame(
  Question = diff_df$question,
  Difference = round(diff_df$difference, 2)
))

dev.off()