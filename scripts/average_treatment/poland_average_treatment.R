library(forcats)
library(reshape2)
library(ggpubr)
library(ggplot2)
library(haven)
library(dplyr)

# Read dataset from SPSS file
data_poland <- read_sav("~/projects/bustikova/raw_data/poland_raw_data.sav")

# Select only the ID and Q06A–Q06F columns
questions <- c("id", "Q06A", "Q06B", "Q06C", "Q06D", "Q06E", "Q06F")
data_poland_questions <- data_poland[questions]

# Define a function to calculate the proportion of responses equal to y
prop <- function(x, y) {
  sum(x == y, na.rm = TRUE) / sum(!is.na(x))
}

# Quick check
summary(data_poland_questions)

#-----------------------------------
# Calculate proportions (1–4) per question
#-----------------------------------
h <- data.frame(
  "Strongly Disagree" = apply(data_poland_questions[2:7], 2, prop, y = 1),
  "Disagree"          = apply(data_poland_questions[2:7], 2, prop, y = 2),
  "Agree"             = apply(data_poland_questions[2:7], 2, prop, y = 3),
  "Strongly Agree"    = apply(data_poland_questions[2:7], 2, prop, y = 4)
)

# Add a nice rowname factor in the order we want
h$rowname <- rownames(h)
h$rowname <- factor(h$rowname,
                    levels = c("Q06A", "Q06D",
                               "Q06B", "Q06E",
                               "Q06C", "Q06F"),
                    labels = c("Control A", "Experimental A",
                               "Control B", "Experimental B",
                               "Control C", "Experimental C"))

mh <- melt(h, id.vars = "rowname")

#===============================
# Create Poland Stacked Bar Plot
#===============================
stacked_plot <- ggplot(mh, aes(x = rowname, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack", color = "black", size = 0.2) +
  coord_flip() +
  labs(title    = "Poland: Survey Response Distribution",
       subtitle = "Control vs. Experimental Conditions (4-Point Scale)",
       x        = "Survey Question",
       y        = "Proportion of Responses",
       fill     = "Response Category") +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title    = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(face = "italic", size = 14, hjust = 0.5),
    axis.title    = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  )

ggsave(filename = "~/projects/bustikova/output/poland_stacked_bar_graph.png",
       plot     = stacked_plot,
       width    = 10, height = 7,
       device   = "png", bg = "white")

#===============================
# Calculate Means for Each Group
#===============================
control_means <- data_poland_questions %>%
  select(Q06A, Q06B, Q06C) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))

treatment_means <- data_poland_questions %>%
  select(Q06D, Q06E, Q06F) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))

# Reshape into long format
control_long <- melt(control_means, variable.name = "question", value.name = "mean_response")
control_long$group <- "Control"
treatment_long <- melt(treatment_means, variable.name = "question", value.name = "mean_response")
treatment_long$group <- "Experimental"
mean_df <- bind_rows(control_long, treatment_long)

# Label question types
mean_df$question_type <- case_when(
  grepl("Q06A|Q06D", mean_df$question) ~ "a",
  grepl("Q06B|Q06E", mean_df$question) ~ "b",
  grepl("Q06C|Q06F", mean_df$question) ~ "c"
)
mean_df$question_type <- factor(mean_df$question_type,
                                levels = c("a","b","c"),
                                labels = c("Question A","Question B","Question C"))

#===============================
# Create Poland Comparison Bar Graph
#===============================
compare_bar_graph <- ggplot(mean_df, aes(x = question_type, y = mean_response, fill = group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8),
           width = 0.7, color = "black", size = 0.2) +
  labs(title    = "Poland: Comparison of Mean Survey Responses",
       subtitle = "Control vs. Experimental Conditions (4-Point Scale)",
       x        = "Question",
       y        = "Mean Response (1 = Strongly Disagree, 4 = Strongly Agree)",
       fill     = "Group") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title    = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(face = "italic", size = 14, hjust = 0.5),
    axis.title    = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  )

ggsave(filename = "~/projects/bustikova/output/poland_compare_bar_graph.png",
       plot     = compare_bar_graph,
       width    = 10, height = 7,
       device   = "png", bg = "white")

#===============================================
# Calculate Difference (Experimental – Control)
#===============================================
control_means_vec <- c(
  A = mean(data_poland_questions$Q06A, na.rm = TRUE),
  B = mean(data_poland_questions$Q06B, na.rm = TRUE),
  C = mean(data_poland_questions$Q06C, na.rm = TRUE)
)
experimental_means_vec <- c(
  A = mean(data_poland_questions$Q06D, na.rm = TRUE),
  B = mean(data_poland_questions$Q06E, na.rm = TRUE),
  C = mean(data_poland_questions$Q06F, na.rm = TRUE)
)
differences <- experimental_means_vec - control_means_vec

diff_df <- data.frame(
  question   = factor(names(differences), levels = c("A","B","C")),
  difference = differences
)

#===============================
# Create Poland Difference Plot
#===============================
difference_plot <- ggplot(diff_df, aes(x = question, y = difference)) +
  geom_bar(stat = "identity", width = 0.6,
           fill = ifelse(diff_df$difference >= 0, "steelblue", "coral"),
           color = "black", size = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.5) +
  coord_flip() +
  labs(title    = "Poland: Treatment Effect (Experimental – Control)",
       subtitle = "Difference in Mean Response by Question",
       x        = "Question",
       y        = "Difference in Mean Response\n(Positive = Higher Experimental Response)",
       caption  = "Scale: 1 = Strongly Disagree, 4 = Strongly Agree") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title    = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(face = "italic", size = 14, hjust = 0.5),
    axis.title    = element_text(face = "bold", size = 14),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    plot.caption  = element_text(size = 12, hjust = 0.5)
  ) +
  scale_y_continuous(limits = c(min(differences) - 0.1, max(differences) + 0.1))

ggsave(filename = "~/projects/bustikova/output/poland_difference_plot.png",
       plot     = difference_plot,
       width    = 10, height = 7,
       device   = "png", bg = "white")

# Clean up
# rm(list = ls())