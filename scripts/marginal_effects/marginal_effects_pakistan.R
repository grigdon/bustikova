library(endorse)
library(ggplot2)
library(scales)
library(Cairo)
library(cowplot)
library(plyr)

data(pakistan)
data_pk <- pakistan

predictor_vars <- c("female", "rural", "edu", "inc")
data_pk <- data_pk %>%
  mutate(across(all_of(predictor_vars), as.numeric()))

model_formula <- formula(~ female + rural + edu + inc)

# Define response variables for each policy question.
Y_Q1 <- list(Q1 = c("Polio.a", "Polio.b", "Polio.c", "Polio.d", "Polio.e"))
Y_Q2 <- list(Q1 = c("FCR.a", "FCR.b", "FCR.c", "FCR.d", "FCR.e"))
Y_Q3 <- list(Q1 = c("Durand.a", "Durand.b", "Durand.c", "Durand.d", "Durand.e"))
Y_Q4 <- list(Q1 = c("Curriculum.a", "Curriculum.b", "Curriculum.c", "Curriculum.d", "Curriculum.e"))

# Run the endorsement model for each question.
endorse_Q1 <- endorse(Y = Y_Q1, data = data_pk, identical.lambda = TRUE,
                      covariates = TRUE, formula.indiv = model_formula,
                      hierarchical = FALSE, verbose = TRUE)
endorse_Q2 <- endorse(Y = Y_Q2, data = data_pk, identical.lambda = TRUE,
                      covariates = TRUE, formula.indiv = model_formula,
                      hierarchical = FALSE, verbose = TRUE)
endorse_Q3 <- endorse(Y = Y_Q3, data = data_pk, identical.lambda = TRUE,
                      covariates = TRUE, formula.indiv = model_formula,
                      hierarchical = FALSE, verbose = TRUE)
endorse_Q4 <- endorse(Y = Y_Q4, data = data_pk, identical.lambda = TRUE,
                      covariates = TRUE, formula.indiv = model_formula,
                      hierarchical = FALSE, verbose = TRUE)

# 3. Plotting Setup
# Configuration for variables.
var_config <- list(
  "edu"    = list(range = c(1, 5), label = "Education Level"),
  "inc"    = list(range = c(1, 4), label = "Monthly Income Level"),
  "female" = list(range = c(0, 1), label = "Female"),
  "rural"  = list(range = c(0, 1), label = "Rural Household")
)

# Labels for each policy question.
question_labels <- list(
  "Q1" = "Policy 1: Polio Vaccinations",
  "Q2" = "Policy 2: FCR Reform",
  "Q3" = "Policy 3: Durand Line Peace Jirgas",
  "Q4" = "Policy 4: Madaris Curriculum Reform"
)

# Publication-quality theme for ggplot2.
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
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    )
}

# 4. Generate and Save Plots
# **FIXED** Function to create a single marginal effect plot.
create_marginal_plot <- function(endorse_model, var_name, question_name, data, var_config) {
  var_range <- var_config[[var_name]]$range
  var_label <- var_config[[var_name]]$label
  question_label <- question_labels[[question_name]]

  var_seq <- seq(from = floor(var_range[1]), to = ceiling(var_range[2]), by = 1)

  # Create baseline means for all predictors
  predictor_vars <- c("female", "rural", "edu", "inc")
  baseline_means_list <- lapply(data[predictor_vars], function(x) mean(as.numeric(x), na.rm = TRUE))

  results_list <- list()

  for(val in var_seq) {
    # Create the prediction data frame
    temp_list <- baseline_means_list
    temp_list[[var_name]] <- val
    temp_data <- as.data.frame(temp_list)

    # Debug: Print what we're predicting
    cat("Predicting for", var_name, "=", val, "\n")
    cat("Temp data structure:\n")
    print(str(temp_data))

    # Try different prediction approaches
    tryCatch({
      # First try with standardize = TRUE
      prediction_draws <- predict(endorse_model, newdata = temp_data, type = "prob.support", standardize = TRUE)

      # Check if prediction_draws is valid
      if (is.null(prediction_draws) || !is.numeric(prediction_draws) || all(is.na(prediction_draws))) {
        cat("Warning: standardize=TRUE failed, trying standardize=FALSE\n")
        # Try without standardize
        prediction_draws <- predict(endorse_model, newdata = temp_data, type = "prob.support", standardize = FALSE)
      }

      if (is.null(prediction_draws) || !is.numeric(prediction_draws) || all(is.na(prediction_draws))) {
        cat("Warning: prob.support failed, trying different type\n")
        # Try different type
        prediction_draws <- predict(endorse_model, newdata = temp_data, type = "support")
        if (!is.null(prediction_draws) && is.numeric(prediction_draws)) {
          # Convert to probabilities if needed
          prediction_draws <- pnorm(prediction_draws)
        }
      }

      # Final check
      if (is.null(prediction_draws) || !is.numeric(prediction_draws)) {
        stop("Could not get valid predictions")
      }

      # Remove NAs
      prediction_draws <- prediction_draws[!is.na(prediction_draws)]

      if (length(prediction_draws) == 0) {
        stop("All predictions are NA")
      }

      # Calculate summary statistics
      mean_prob <- mean(prediction_draws, na.rm = TRUE)
      lower_ci <- quantile(prediction_draws, 0.025, na.rm = TRUE)
      upper_ci <- quantile(prediction_draws, 0.975, na.rm = TRUE)

      results_list[[length(results_list) + 1]] <- data.frame(
        var_value = val,
        mean_prob = mean_prob,
        lower_ci = lower_ci,
        upper_ci = upper_ci
      )

    }, error = function(e) {
      cat("Error for", var_name, "=", val, ":", e$message, "\n")
      # Add a row with NAs to maintain structure
      results_list[[length(results_list) + 1]] <<- data.frame(
        var_value = val,
        mean_prob = NA,
        lower_ci = NA,
        upper_ci = NA
      )
    })
  }

  pred_summary <- do.call(rbind, results_list)

  # Remove rows with NA values
  pred_summary <- pred_summary[!is.na(pred_summary$mean_prob), ]

  if (nrow(pred_summary) == 0) {
    stop(paste("No valid predictions for", var_name, "in", question_name))
  }

  common_labs <- labs(
    title = paste("Marginal Effect of", var_label),
    subtitle = paste("Effect on endorser support •", question_label),
    caption = "Note: Error bars represent 95% credible interval."
  )
  common_elements <- list(
    scale_y_continuous("Predicted Probability of Positive Support", limits = c(0, 1)),
    scale_x_discrete(name = var_label),
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray40"),
    theme_publication()
  )

  # All covariates are treated as discrete factors for plotting.
  p <- ggplot(pred_summary, aes(x = factor(var_value), y = mean_prob)) +
    geom_pointrange(aes(ymin = lower_ci, ymax = upper_ci), size = 0.8, fatten = 2) +
    common_labs + common_elements

  return(p)
}

# --- Test that prediction works ---
model_list <- list(Q1 = endorse_Q1, Q2 = endorse_Q2, Q3 = endorse_Q3, Q4 = endorse_Q4)

# --- Main Plotting Loop ---
vars_of_interest <- names(var_config)

output_dir <- "~/projects/bustikova/output/marginal_effects/pakistan_predict_standardized/"
png_dir <- file.path(output_dir, "png")
pdf_dir <- file.path(output_dir, "pdf")

if (!dir.exists(png_dir)) { dir.create(png_dir, recursive = TRUE) }
if (!dir.exists(pdf_dir)) { dir.create(pdf_dir, recursive = TRUE) }

# Loop to generate and save a plot for each variable and each question.
for (q_name in names(model_list)) {
  for (var in vars_of_interest) {
    cat("\n=== Creating plot for", q_name, "-", var, "===\n")

    tryCatch({
      current_plot <- create_marginal_plot(model_list[[q_name]], var, q_name, data_pk, var_config)

      ggsave(
        file.path(png_dir, paste0("marginal_", q_name, "_", var, ".png")),
        current_plot, width = 7, height = 5, dpi = 600, type = "cairo-png"
      )
      ggsave(
        file.path(pdf_dir, paste0("marginal_", q_name, "_", var, ".pdf")),
        current_plot, width = 7, height = 5, device = cairo_pdf
      )

      cat("Successfully saved plots for", q_name, "-", var, "\n")

    }, error = function(e) {
      cat("Failed to create plot for", q_name, "-", var, ":", e$message, "\n")
    })
  }
}

# Loop to generate and save combined plots (one for each variable, showing all questions).
for (var in vars_of_interest) {
  cat("\n=== Creating combined plot for", var, "===\n")

  plot_list <- list()

  for (q_name in names(model_list)) {
    tryCatch({
      plot <- create_marginal_plot(model_list[[q_name]], var, q_name, data_pk, var_config) +
        labs(title = question_labels[[q_name]], subtitle = NULL, caption = NULL)
      plot_list[[q_name]] <- plot
    }, error = function(e) {
      cat("Failed to create subplot for", q_name, "-", var, ":", e$message, "\n")
    })
  }

  if (length(plot_list) > 0) {
    tryCatch({
      combined_plot <- plot_grid(
        plotlist = plot_list,
        nrow = 1, labels = "AUTO", label_size = 12
      )

      base_name <- paste0("combined_", var, "_all_questions")
      ggsave(
        file.path(png_dir, paste0(base_name, ".png")),
        combined_plot, width = 20, height = 5, dpi = 600, type = "cairo-png"
      )
      ggsave(
        file.path(pdf_dir, paste0(base_name, ".pdf")),
        combined_plot, width = 20, height = 5, device = cairo_pdf
      )

      cat("Successfully saved combined plot for", var, "\n")

    }, error = function(e) {
      cat("Failed to create combined plot for", var, ":", e$message, "\n")
    })
  } else {
    cat("No valid subplots for combined plot of", var, "\n")
  }
}