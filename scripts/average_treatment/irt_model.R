# load in all question data before running script
# remove ID entries if included 

# remove id column
data_hu_questions <- data_hu_questions[, -1]

# remove id column 
data_slvk_questions <- data_slvk_questions[, -1]

# now all ID entries are NOT included 

#################################################

# Now, combine the dataframes:
# 4-pt scale first
names(data_slvk_questions) <- names(data_hu_questions)
likert_four_question_data <- rbind(data_slvk_questions, data_hu_questions)

# 5-pt scale next 
names(data_poland_questions) <- names(data_cz_questions)
likert_five_question_data <- rbind(data_poland_questions, data_cz_questions)

#############################################################
# end of transformations 
# clear out all other dataframes 

names(likert_five_question_data) <- names(likert_four_question_data)

rm(list = setdiff(ls(), c("likert_four_question_data", "likert_five_question_data" , "data_cz_questions", "data_poland_questions")))

#############################################################
library(mirt)
library(dplyr)

# Step 1: Train the model on 4-point data (your ground truth)
# Clean 4-point data (remove all-NA rows)
four_pt_clean <- likert_four_question_data %>%
  filter(rowSums(!is.na(.)) > 0)

# Fit the 4-point model (this is your reference model)
mod_4pt_reference <- mirt(four_pt_clean, 1, itemtype="graded")

# Step 2: Create conversion function for 5-point to 4-point
convert_5pt_to_4pt_irt <- function(five_pt_data, reference_model) {
  
  # Function to convert individual responses
  convert_response <- function(response_5pt, theta_score, item_num) {
    if(is.na(response_5pt)) return(NA)
    
    # Get item parameters from reference 4-point model
    item_params <- extract.item(reference_model, item_num)
    
    # Calculate response probabilities at this person's theta level
    probs_4pt <- probtrace(item_params, theta_score)
    
    # Convert based on 5-point response
    if(response_5pt == 1) {
      return(1)  # Strongly Disagree stays 1
    } else if(response_5pt == 2) {
      return(2)  # Disagree stays 2  
    } else if(response_5pt == 3) {
      # Neutral (3) gets redistributed to 2 or 3 based on person's theta
      # Use the model's probabilities for categories 2 and 3
      neutral_probs <- probs_4pt[c(2,3)] / sum(probs_4pt[c(2,3)])
      return(sample(c(2,3), 1, prob=neutral_probs))
    } else if(response_5pt == 4) {
      return(3)  # Agree becomes 3
    } else if(response_5pt == 5) {
      return(4)  # Strongly Agree becomes 4
    }
  }
  
  # First, get rough theta estimates from 5-point data
  # We need to fit a temporary 5-point model for theta estimation
  five_pt_clean <- five_pt_data[rowSums(!is.na(five_pt_data)) > 0, ]
  
  if(nrow(five_pt_clean) == 0) return(five_pt_data)
  
  # Fit temporary 5-point model just to get theta scores
  mod_5pt_temp <- mirt(five_pt_clean, 1, itemtype="graded")
  theta_scores <- fscores(mod_5pt_temp, method="EAP")
  
  # Convert responses
  converted_data <- five_pt_data
  theta_index <- 1
  
  for(i in 1:nrow(five_pt_data)) {
    if(rowSums(!is.na(five_pt_data[i,])) > 0) {
      current_theta <- theta_scores[theta_index]
      
      for(j in 1:ncol(five_pt_data)) {
        converted_data[i,j] <- convert_response(
          five_pt_data[i,j], 
          current_theta, 
          j
        )
      }
      theta_index <- theta_index + 1
    }
  }
  
  return(converted_data)
}

######################################

##### czechia data

cz_original_question_data <- data_cz_questions

# Step 3: Apply conversion to your 5-point data
set.seed(12345)  # For reproducibility

# Convert control group (first 3 columns)
cz_control_5pt <- data_cz_questions[, 1:3]
cz_control_converted <- convert_5pt_to_4pt_irt(cz_control_5pt, mod_4pt_reference)

# Convert experimental group (last 3 columns)
cz_exp_5pt <- data_cz_questions[, 4:6]
cz_exp_converted <- convert_5pt_to_4pt_irt(cz_exp_5pt, mod_4pt_reference)

# Combine back into original structure
cz_final_converted <- cbind(cz_control_converted, cz_exp_converted)

# Verify conversion
head(cz_final_converted)
summary(cz_final_converted)

summary(cz_original_question_data)

write_sav(cz_final_converted, "~/projects/bustikova/output/scrubbed_data/czechia_scrubbed_questions.sav")

################################ poland data

poland_original_question_data <- data_poland_questions

# Step 3: Apply conversion to your 5-point data
set.seed(12345)  # For reproducibility

# Convert control group (first 3 columns)
pol_control_5pt <- data_poland_questions[, 1:3]
pol_control_converted <- convert_5pt_to_4pt_irt(pol_control_5pt, mod_4pt_reference)

# Convert experimental group (last 3 columns)
pol_exp_5pt <- data_poland_questions[, 4:6]
pol_exp_converted <- convert_5pt_to_4pt_irt(pol_exp_5pt, mod_4pt_reference)

# Combine back into original structure
pol_final_converted <- cbind(pol_control_converted, pol_exp_converted)

# Verify conversion
head(pol_final_converted)
summary(pol_final_converted)

summary(poland_original_question_data)

write_sav(pol_final_converted, "~/projects/bustikova/output/scrubbed_data/poland_scrubbed_questions.sav")

