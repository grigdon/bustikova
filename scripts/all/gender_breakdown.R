library(forcats)
library(reshape2)
library(ggpubr)
library(ggplot2)
library(haven)
library(dplyr)

# Read dataset from SPSS file
data_cz <- read_sav("~/projects/bustikova/data/scrubbed_data/czechia_scrubbed.sav")

# Define questions for analysis
questions_control <- c("Control_A", "Control_B", "Control_C")
questions_experimental <- c("Experimental_A", "Experimental_B", "Experimental_C")
all_questions <- c(questions_control, questions_experimental)


# Function to calculate the proportion of responses equal to y
prop_func <- function(x, y) {
  sum(x == y, na.rm = TRUE) / sum(!is.na(x))
}

# Prepare data for plotting
# Rename 'Gender' column for clarity and ensure it's a factor
# Adjusted Gender levels: 1 = Male, 2 = Female
data_cz <- data_cz %>%
  rename(Gender = Gender) %>% # Renaming 'Gender' to 'Gender' for better labeling
  mutate(Gender = factor(Gender, levels = c(1, 2), labels = c("Male", "Female")))

# Melt the data for easier processing
data_long <- data_cz %>%
  select(Gender, all_of(all_questions)) %>%
  melt(id.vars = "Gender", variable.name = "Question", value.name = "Response") %>%
  mutate(
    Condition = case_when(
      grepl("Control", Question) ~ "Control",
      grepl("Experimental", Question) ~ "Experimental"
    ),
    Question_Type = case_when(
      grepl("_A", Question) ~ "Traditional Marriage",
      grepl("_B", Question) ~ "Abortion Ban",
      grepl("_C", Question) ~ "Christianity in Schools"
    )
  ) %>%
  filter(!is.na(Response)) # Remove NA responses for proportion calculation

# Calculate proportions for Total, Male, and Female
calculate_proportions <- function(data_subset, group_label) {
  data_subset %>%
    group_by(Condition, Question_Type, Response) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    group_by(Condition, Question_Type) %>%
    mutate(Proportion = Count / sum(Count)) %>%
    ungroup() %>%
    filter(Response %in% c(1, 2, 3, 4)) %>% # Ensure only valid responses are included
    pivot_wider(names_from = Response, values_from = Proportion, values_fill = 0) %>%
    rename(
      `Strongly Disagree` = `1`,
      `Disagree` = `2`,
      `Agree` = `3`,
      `Strongly Agree` = `4`
    ) %>%
    melt(id.vars = c("Condition", "Question_Type"), variable.name = "Response_Category", value.name = "Proportion") %>%
    mutate(
      Group = group_label,
      Category_Label = paste(Condition, " (", group_label, ")", sep = "")
    )
}

# Calculate for Total
prop_total <- calculate_proportions(data_long, "Total")

# Calculate for Male
prop_male <- calculate_proportions(filter(data_long, Gender == "Male"), "Male")

# Calculate for Female
prop_female <- calculate_proportions(filter(data_long, Gender == "Female"), "Female")

# Combine all proportions
combined_proportions <- bind_rows(prop_total, prop_male, prop_female)

# Order the response categories for plotting
combined_proportions$Response_Category <- factor(combined_proportions$Response_Category,
                                                 levels = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"))

# Order the groups for plotting
combined_proportions$Group <- factor(combined_proportions$Group,
                                     levels = c("Total", "Male", "Female"))

# Order the Question_Type for plotting (this ensures consistent order across facets)
combined_proportions$Question_Type <- factor(combined_proportions$Question_Type,
                                             levels = c("Traditional Marriage", "Abortion Ban", "Christianity in Schools"))

# Create the stacked bar plot
stacked_plot_gender <- ggplot(combined_proportions, aes(x = Category_Label, y = Proportion, fill = Response_Category)) +
  geom_bar(stat = "identity", position = "stack", color = "black", size = 0.2) +
  facet_grid(Question_Type ~ Group, scales = "free_y", space = "free_y") + # Use facet_grid for rows and columns
  labs(
    title = "Czechia: Movement for Life - Survey Response Distribution by Gender",
    x = "Group and Condition",
    y = "Proportion of Responses",
    fill = "Response"
  ) +
  scale_fill_brewer(palette = "Blues", direction = -1) + # Invert colors for darker blues for agreement
  coord_flip() + # Flip coordinates to have bars horizontal
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 12),
    legend.position = "bottom",
    panel.grid.major.x = element_line(color = "grey80"), # Horizontal grid lines for flipped coord
    panel.grid.minor = element_blank(),
    strip.text.y = element_text(angle = 270, face = "bold"), # Rotate y-axis facet labels
    strip.text.x = element_text(face = "bold"), # X-axis facet labels
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotate x-axis text for readability
  )

# Save the plot
ggsave(filename = "~/projects/bustikova/output/avg_treat/survey_response_distribution/czechia_stacked_bar_gender_breakdown.png",
       plot = stacked_plot_gender,
       width = 12, height = 8, device = "png", bg = "white")

# Clean up
rm(list = ls())