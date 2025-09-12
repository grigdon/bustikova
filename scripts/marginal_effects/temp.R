library(endorse)
library(haven)
library(dplyr)
library(ggplot2)
library(scales)
library(Cairo)
library(cowplot)

# 1. Load Data
# Note: You may need to update this path to your local file location.
HUData <- read_sav("~/projects/bustikova/data/scrubbed_data/hungary_scrubbed.sav")
questions <- c("A6A_1", "A6A_2", "A6A_3", "A6B_1", "A6B_2", "A6B_3")
data_hu_questions <- HUData[questions]

vars_to_keep <- c("ChildHome", "ChurchPolitics", "LawOrder", "GayPartner", "Religiosity",
                  "Children", "MaleJobs", "Age", "Capital", "Married", "Income",
                  "Gender", "EconomicFuture", "Education", "GovDissatisfaction", "Fidesz_vote")

data_hu_vars <- HUData[vars_to_keep]
data_hu_vars <- mutate(data_hu_vars, across(everything(), ~as.numeric(.)))
data_hu <- bind_cols(data_hu_questions, data_hu_vars)

# Standardize continuous variables for the model
data_hu$Age_std <- scale(data_hu$Age)
data_hu$Income_std <- scale(data_hu$Income)


# 2. Define and Run Models
model_formula <- formula( ~ ChildHome + ChurchPolitics + LawOrder + GayPartner + Religiosity +
                            Children + MaleJobs + Age_std + Capital + Married + Income_std +
                            Gender + EconomicFuture + Education + GovDissatisfaction + Fidesz_vote)

# Model Q1
Y_Q1 <- list(Q1 = c("A6A_1", "A6B_1"))
endorse_Q1 <- endorse(Y = Y_Q1, data = data_hu, identical.lambda = FALSE,
                      covariates = TRUE, prop = 0.008, formula.indiv = model_formula,
                      omega2.out = TRUE, hierarchical = FALSE, verbose = FALSE)
# Model Q2
Y_Q2 <- list(Q1 = c("A6A_2", "A6B_2"))
endorse_Q2 <- endorse(Y = Y_Q2, data = data_hu, identical.lambda = FALSE,
                      covariates = TRUE, prop = 0.008, formula.indiv = model_formula,
                      omega2.out = TRUE, hierarchical = FALSE, verbose = FALSE)
# Model Q3
Y_Q3 <- list(Q1 = c("A6A_3", "A6B_3"))
endorse_Q3 <- endorse(Y = Y_Q3, data = data_hu, identical.lambda = FALSE,
                      covariates = TRUE, prop = 0.008, formula.indiv = model_formula,
                      omega2.out = TRUE, hierarchical = FALSE, verbose = FALSE)


# 3. Plotting Setup
var_config <- list(
  "Age" = list(range = c(15, 95), label = "Age"),
  "Income" = list(range = c(0, 1000000), label = "Income"),
  "Gender" = list(range = c(1, 2), label = "Female"),
  "Married" = list(range = c(1, 2), label = "Married"),
  "Capital" = list(range = c(1, 2), label = "Rural"),
  "Fidesz_vote" = list(range = c(0, 1), label = "Fidesz Vote"),
  "Religiosity" = list(range = c(1, 5), label = "Religiosity"),
  "GayPartner" = list(range = c(1, 4), label = "Gay Partner"),
  "ChurchPolitics" = list(range = c(1, 4), label = "Church in Politics"),
  "ChildHome" = list(range = c(1, 4), label = "Child Mom Dad"),
  "LawOrder" = list(range = c(1, 4), label = "Law and Order"),
  "Children" = list(range = c(0, 5), label = "Number of Children"),
  "MaleJobs" = list(range = c(1, 4), label = "Male Jobs"),
  "EconomicFuture" = list(range = c(1, 5), label = "Economic Future Fear"),
  "Education" = list(range = c(1, 10), label = "Education"),
  "GovDissatisfaction" = list(range = c(1, 4), label = "Gov. Dissatisfaction")
)


question_labels <- list(
  "Q1" = "Policy 1: Same Sex Marriage",
  "Q2" = "Policy 2: Abortion Legal",
  "Q3" = "Policy 3: Christianity in Schools"
)

theme_publication <- function(base_size = 11, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
      strip.background = element_rect(fill = "white", color = "black", linewidth = 0.6),
      strip.text = element_text(size = rel(0.9), face = "bold", color = "black"),
      axis.line = element_blank(), axis.ticks = element_line(color = "black", linewidth = 0.4),
      axis.ticks.length = unit(0.15, "cm"), axis.text = element_text(color = "black", size = rel(0.85)),
      axis.text.x = element_text(margin = margin(t = 5)), axis.text.y = element_text(margin = margin(r = 5)),
      axis.title = element_text(color = "black", size = rel(0.95), face = "bold"),
      axis.title.x = element_text(margin = margin(t = 15)), axis.title.y = element_text(margin = margin(r = 15)),
      plot.title = element_text(size = rel(1.1), face = "bold", color = "black", hjust = 0, margin = margin(b = 10)),
      plot.subtitle = element_text(size = rel(0.9), color = "gray30", hjust = 0, margin = margin(b = 15)),
      plot.caption = element_text(size = rel(0.7), color = "gray50", hjust = 0, margin = margin(t = 15)),
      legend.background = element_rect(fill = "white", color = NA), legend.key = element_rect(fill = "white", color = NA),
      legend.key.size = unit(0.8, "lines"), legend.text = element_text(size = rel(0.8)),
      legend.title = element_text(size = rel(0.85), face = "bold"), legend.margin = margin(t = 10),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    )
}


# 4. Generate and Save Plots
create_marginal_plot <- function(endorse_model, var_name, question_name, data, var_config) {
  var_range <- var_config[[var_name]]$range
  var_label <- var_config[[var_name]]$label
  question_label <- question_labels[[question_name]]

  is_discrete <- !(var_name %in% c("Age", "Income"))

  var_seq <- if(is_discrete) {
    # Sequence for all discrete integer-based scales
    seq(from = floor(var_range[1]), to = ceiling(var_range[2]), by = 1)
  } else {
    # Sequence for continuous variables
    seq(var_range[1], var_range[2], length.out = 100)
  }

  mean_age <- mean(data$Age, na.rm = TRUE)
  sd_age <- sd(data$Age, na.rm = TRUE)
  mean_income <- mean(data$Income, na.rm = TRUE)
  sd_income <- sd(data$Income, na.rm = TRUE)

  baseline_data <- data %>% summarise(across(where(is.numeric), mean, na.rm = TRUE))
  results_list <- list()

  for(val in var_seq) {
    temp_data <- baseline_data
    if (var_name == "Age") {
      temp_data$Age_std <- (val - mean_age) / sd_age
    } else if (var_name == "Income") {
      temp_data$Income_std <- (val - mean_income) / sd_income
    } else {
      temp_data[[var_name]] <- val
    }
    prediction_draws <- predict(endorse_model, newdata = temp_data, type = "prob.support", standardize = TRUE)
    results_list[[length(results_list) + 1]] <- data.frame(
      var_value = val, mean_prob = mean(prediction_draws),
      lower_ci = quantile(prediction_draws, 0.025), upper_ci = quantile(prediction_draws, 0.975)
    )
  }

  pred_summary <- do.call(rbind, results_list)

  # Common plot elements
  common_labs <- labs(
    title = paste("Marginal Effect of", gsub(" \\(.*\\)", "", var_label)),
    subtitle = paste("Effect on endorser support •", question_label),
    caption = "Note: Error bars represent 95% credible interval. Dashed line indicates 0.5 probability threshold."
  )
  common_scales <- list(
    scale_y_continuous(
      name = "Predicted Probability of Positive Support", breaks = pretty_breaks(n = 6),
      limits = c(0, 1), expand = expansion(mult = c(0.02, 0.02))
    ),
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray40", linewidth = 0.5)
  )

  if (is_discrete) {
    # Plot for discrete variables (points and error bars only)
    p <- ggplot(pred_summary, aes(x = factor(var_value), y = mean_prob)) +
      geom_pointrange(aes(ymin = lower_ci, ymax = upper_ci), size = 0.8) +
      common_scales +
      common_labs +
      scale_x_discrete(name = var_label) +
      theme_publication()
  } else {
    # Plot for continuous variables (line and ribbon)
    p <- ggplot(pred_summary, aes(x = var_value, y = mean_prob)) +
      geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "gray20", alpha = 0.2, color = NA) +
      geom_line(color = "black", linewidth = 0.8, alpha = 0.9) +
      common_scales +
      common_labs

    if (var_name == "Income") {
      p <- p + scale_x_continuous(name = var_label, labels = label_number(scale_cut = cut_short_scale()))
    } else {
      p <- p + scale_x_continuous(name = var_label, breaks = pretty_breaks())
    }
    p <- p + theme_publication()
  }

  return(p)
}


vars_of_interest <- names(var_config)
model_list <- list(Q1 = endorse_Q1, Q2 = endorse_Q2, Q3 = endorse_Q3)

# MODIFICATION: Define base output directory and subdirectories
output_dir <- "~/projects/bustikova/output/marginal_effects/hungary_standardized/" # Update if needed
png_dir <- file.path(output_dir, "png")
pdf_dir <- file.path(output_dir, "pdf")

# MODIFICATION: Create subdirectories if they don't exist
if (!dir.exists(png_dir)) {
  dir.create(png_dir, recursive = TRUE)
}
if (!dir.exists(pdf_dir)) {
  dir.create(pdf_dir, recursive = TRUE)
}

# Generate and Save Individual Plots
for (q_name in names(model_list)) {
  for (var in vars_of_interest) {
    current_model <- model_list[[q_name]]
    current_plot <- create_marginal_plot(current_model, var, q_name, data_hu, var_config)

    # MODIFICATION: Update file paths to use subdirectories
    file_name_png <- file.path(png_dir, paste0("marginal_effect_", q_name, "_", var, ".png"))
    ggsave(
      filename = file_name_png, plot = current_plot, width = 7, height = 5,
      dpi = 600, type = "cairo-png", bg = "white"
    )

    # MODIFICATION: Update file paths to use subdirectories
    file_name_pdf <- file.path(pdf_dir, paste0("marginal_effect_", q_name, "_", var, ".pdf"))
    ggsave(
      filename = file_name_pdf, plot = current_plot, width = 7, height = 5,
      device = cairo_pdf, bg = "white"
    )
  }
}

# Generate and Save Combined Plots
create_combined_plots <- function() {
  for (var in vars_of_interest) {
    plot_list <- list()
    for (q_name in names(model_list)) {
      current_model <- model_list[[q_name]]
      plot_list[[q_name]] <- create_marginal_plot(current_model, var, q_name, data_hu, var_config) +
        theme(plot.subtitle = element_blank(), plot.caption = element_blank()) +
        labs(title = question_labels[[q_name]])
    }

    # MODIFICATION: Base name for files, without directory
    base_file_name <- paste0("combined_", var, "_all_questions")

    tryCatch({
      combined_plot <- plot_grid(
        plotlist = plot_list,
        nrow = 1, labels = c("A", "B", "C"), label_size = 12
      )

      # MODIFICATION: Save to specific subdirectories
      ggsave(file.path(png_dir, paste0(base_file_name, ".png")), combined_plot, width = 15, height = 5,
             dpi = 600, type = "cairo-png", bg = "white")

      ggsave(file.path(pdf_dir, paste0(base_file_name, ".pdf")), combined_plot, width = 15, height = 5,
             device = cairo_pdf, bg = "white")

    }, error = function(e) {
      message("Could not create combined plot for: ", var)
      message("Error: ", e$message)
    })
  }
}

create_combined_plots()