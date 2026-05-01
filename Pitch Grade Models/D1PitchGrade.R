#==============================================================================
# FINDING MEAN DELTA_RUN_EXPECTANCY BY COUNT AND EVENT
#==============================================================================

library(dplyr)

D1TM_Combined$decision <- with(D1TM_Combined, 
                               ifelse(PitchCall %in% c("BallCalled", "BallInDirt", "BallIntentional"), "ball",
                                      ifelse(PitchCall == "InPlay", "hit_into_play",
                                             ifelse(PitchCall %in% c("StrikeSwinging", "StrkeSwinging"), "swinging_strike",
                                                    ifelse(PitchCall %in% c("StrikeCalled", "StreikC"), "called_strike",
                                                           ifelse(PitchCall == "HitByPitch", "hit_by_pitch",
                                                                  ifelse(PitchCall %in% c("FoulBallNotFieldable", "FouldBallNotFieldable"), "foul", NA)))))))

D1TM_Combined$event <- with(D1TM_Combined,
                            ifelse(KorBB %in% c("Strikeout", "StrikeOut"), "strikeout",
                                   ifelse(PlayResult %in% c("Out", "FieldersChoice", "Sacrifice"), "field_out",
                                          ifelse(PlayResult == "Double", "double",
                                                 ifelse(PlayResult %in% c("Single", "SIngle"), "single",
                                                        ifelse(PlayResult == "Triple", "triple",
                                                               ifelse(PlayResult %in% c("HomeRun", "Homerun"), "home_run",
                                                                      ifelse(KorBB %in% c("walk", "Walk"), "walk", NA))))))))

D1TM_Combined$swing <- ifelse(D1TM_Combined$decision %in% c("swinging_strike", "hit_into_play", "foul"), 1, 0)

# Define the run values
run_values <- data.frame(
  count = c("0-0", "1-0", "0-1", "2-0", "1-1", "0-2", "3-0", "2-1", "1-2", "3-1", "2-2", "3-2"),
  value = c(0.001695997, 0.039248042, -0.043581338, -0.043581338, -0.015277684, 
            -0.103242476, 0.200960731, 0.034545018, -0.080485991, 0.138254876, 
            -0.039716495, 0.048505049)
)

outcome_values <- data.frame(
  outcome = c("walk", "hit_by_pitch", "single", "double", "triple", "home_run", "field_out", "strikeout"),
  value = c(0.31, 0.33, 0.46, 0.79, 1.07, 1.41, -0.33, -0.284)
)

# Calculate delta run expectancy
D1TM_Combined <- D1TM_Combined %>%
  mutate(
    current_count = paste(Balls, Strikes, sep = "-"),
    current_re = run_values$value[match(current_count, run_values$count)],
    final_re = case_when(
      !is.na(event) & event == "walk" ~ 0.31,
      !is.na(event) & event == "hit_by_pitch" ~ 0.33,
      !is.na(event) & event == "single" ~ 0.46,
      !is.na(event) & event == "double" ~ 0.79,
      !is.na(event) & event == "triple" ~ 1.07,
      !is.na(event) & event == "home_run" ~ 1.41,
      !is.na(event) & event == "field_out" ~ -0.33,
      !is.na(event) & event == "strikeout" ~ -0.284,
      decision == "ball" & Balls < 3 ~ {
        new_count <- paste(Balls + 1, Strikes, sep = "-")
        run_values$value[match(new_count, run_values$count)]
      },
      decision %in% c("called_strike", "swinging_strike") & Strikes < 2 ~ {
        new_count <- paste(Balls, Strikes + 1, sep = "-")
        run_values$value[match(new_count, run_values$count)]
      },
      decision == "foul" & Strikes < 2 ~ {
        new_count <- paste(Balls, Strikes + 1, sep = "-")
        run_values$value[match(new_count, run_values$count)]
      },
      decision == "foul" & Strikes == 2 ~ current_re,
      decision == "hit_into_play" & is.na(event) ~ current_re,
      TRUE ~ current_re
    ),
    avg_delta_run_exp_mapped = final_re - current_re
  ) %>%
  select(-current_count, -current_re, -final_re)

# Reset PA value at the beginning of each inning
D1TM_Combined <- D1TM_Combined %>%
  group_by(PAofInning) %>%
  mutate(
    PA_reset = PAofInning == 1
  ) %>%
  ungroup()

# Check what pitch types are available
unique_pitch_types <- unique(D1TM_Combined$TaggedPitchType)
print(paste("Available pitch types:", paste(unique_pitch_types, collapse = ", ")))

# Create a lookup table of average values for each pitcher using a more flexible approach
pitcher_averages <- D1TM_Combined %>%
  filter(TaggedPitchType %in% c("Fastball", "FourSeamFastBall", "TwoSeamFastBall", "Sinker"), 
         !is.na(RelSpeed), !is.na(ax0), !is.na(az0)) %>%
  group_by(Pitcher) %>%
  summarize(
    avg_mph = mean(RelSpeed, na.rm = TRUE),
    avg_ax  = mean(ax0, na.rm = TRUE),
    avg_az  = mean(az0, na.rm = TRUE),
    .groups = "drop"
  )

# Check if we found any pitcher averages
print(paste("Number of pitchers with fastball data:", nrow(pitcher_averages)))

# Join the averages back to the full dataset by Pitcher
D1TM_Combined <- D1TM_Combined %>%
  left_join(pitcher_averages, by = "Pitcher")

# Check what columns we actually have after the join
print("Columns after join:")
print(names(D1TM_Combined)[grepl("avg_", names(D1TM_Combined))])

# Add the difference columns only if we have averages
if(nrow(pitcher_averages) > 0 && "avg_mph" %in% names(D1TM_Combined)) {
  D1TM_Combined <- D1TM_Combined %>%
    mutate(
      mph_diff = case_when(
        is.na(avg_mph) ~ NA_real_,
        is.na(RelSpeed) ~ NA_real_,
        TRUE ~ RelSpeed - avg_mph
      ),
      ax_diff = case_when(
        is.na(avg_ax) ~ NA_real_,
        is.na(ax0) ~ NA_real_,
        TRUE ~ ax0 - avg_ax
      ),
      az_diff = case_when(
        is.na(avg_az) ~ NA_real_,
        is.na(az0) ~ NA_real_,
        TRUE ~ az0 - avg_az
      )
    )
} else {
  # If no fastball data found, create columns with NA
  D1TM_Combined$avg_mph <- NA
  D1TM_Combined$avg_ax <- NA
  D1TM_Combined$avg_az <- NA
  D1TM_Combined$mph_diff <- NA
  D1TM_Combined$ax_diff <- NA
  D1TM_Combined$az_diff <- NA
  print("Warning: avg_mph column not found after join, created NA columns")
}

#===============================================================================
#Pitch Grade Model v2 04/26/2025
#===============================================================================

# Load necessary libraries
library(lightgbm)
library(rBayesianOptimization)

# Get unique pitch types from the dataset
pitch_types <- unique(D1TM_Combined$TaggedPitchType)
pitch_types <- pitch_types[!is.na(pitch_types)]

# Function to train and predict for each pitch type
train_predict_pitch_type <- function(pitch_type) {
  # Filter data for the current pitch type
  data_pitch <- D1TM_Combined %>% filter(TaggedPitchType == !!pitch_type)
  
  # Prepare features
  one_hot_pbp <- data_pitch %>%
    select(RelSpeed, SpinRate, SpinAxis, x0, z0, az0, ax0, Extension, 
           mph_diff, az_diff, ax_diff, avg_delta_run_exp_mapped)
  
  X_train <- one_hot_pbp %>% select(-avg_delta_run_exp_mapped)
  y_train <- one_hot_pbp$avg_delta_run_exp_mapped
  X_train_matrix <- as.matrix(X_train)
  lgb_train <- lgb.Dataset(data = X_train_matrix, label = y_train)
  
  # Bayesian Optimization
  opt_results <- BayesianOptimization(
    FUN = function(num_leaves, max_depth, learning_rate) {
      params <- list(
        objective = "regression",
        metric = "rmse",
        num_leaves = round(num_leaves),
        max_depth = round(max_depth),
        learning_rate = learning_rate,
        min_data_in_leaf = 30,
        feature_pre_filter = FALSE,
        feature_fraction = 0.8,
        bagging_fraction = 0.8,
        bagging_freq = 1
      )
      
      cv_results <- lgb.cv(
        params = params,
        data = lgb_train,
        nrounds = 1000,
        nfold = 5,
        early_stopping_rounds = 50,
        verbose = -1
      )
      
      best_score <- min(cv_results$best_score)
      return(list(Score = -best_score))
    },
    bounds = list(
      num_leaves = c(20L, 100L),
      max_depth = c(3L, 15L),
      learning_rate = c(0.01, 0.3)
    ),
    init_points = 5,
    n_iter = 10,
    acq = "ucb"
  )
  
  # Best parameters
  best_params <- list(
    objective = "regression",
    metric = "rmse",
    num_leaves = round(opt_results$Best_Par["num_leaves"]),
    max_depth = round(opt_results$Best_Par["max_depth"]),
    learning_rate = opt_results$Best_Par["learning_rate"],
    min_data_in_leaf = 30,
    feature_pre_filter = FALSE,
    feature_fraction = 0.8,
    bagging_fraction = 0.8,
    bagging_freq = 1
  )
  
  # Train final model
  final_model <- lgb.train(
    params = best_params,
    data = lgb_train,
    nrounds = 1000,
    verbose = 0
  )
  
  # Add this before your train_predict_pitch_type function definition
  save_dir <- "C:/Users/dcawt/Documents/R Projects/Scripts/Pitch Grade Models/PitchGradeModels/"
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
    cat("Created directory:", save_dir, "\n")
  }
  
  # Save model to your folder
  save_path <- paste0("C:/Users/dcawt/Documents/R Projects/Scripts/Pitch Grade Models/PitchGradeModels/",
                      gsub("[[:space:]]+", "_", pitch_type), "_PitchGradeModel.txt")
  lgb.save(final_model, save_path)
  cat("Model saved for", pitch_type, "at:", save_path, "\n")
  
  # Predictions
  data_pitch$xRV_3 <- predict(final_model, X_train_matrix)
  
  return(data_pitch)
}

# Apply the function to each pitch type and combine results
results <- lapply(pitch_types, train_predict_pitch_type)
combined_results <- bind_rows(results)

# Add predictions back to the original dataset
D1TM_Combined <- D1TM_Combined %>%
  left_join(combined_results %>% select(row_index, xRV_3), by = "row_index")

# Calculate RMSE for the combined model
rmse_v3 <- rmse(D1TM_Combined$xRV_3, D1TM_Combined$avg_delta_run_exp_mapped)

# Calculate leaderboard with pitch_grade compared to the mean and sd of individual pitch types
leaderboard <- D1TM_Combined %>%
  group_by(player_name, game_year, TaggedPitchType) %>%
  summarise(
    total_pitches = n(),
    avg_RelSpeed = mean(RelSpeed, na.rm = TRUE),
    avg_release_pos_z = mean(release_pos_z, na.rm = TRUE),
    avg_release_pos_x = mean(release_pos_x, na.rm = TRUE),
    avg_pfx_x = mean(pfx_x, na.rm = TRUE),
    avg_pfx_z = mean(pfx_z, na.rm = TRUE),
    xRV_per_100 = sum(xRV_2, na.rm = TRUE) / total_pitches * 100,
    .groups = "drop"
  ) %>%
  filter(total_pitches >= 50) %>%
  group_by(TaggedPitchType) %>%
  mutate(
    pitch_grade = 100 + 5 * (-xRV_per_100 - mean(-xRV_per_100, na.rm = TRUE)) / sd(-xRV_per_100, na.rm = TRUE)
  ) %>%
  ungroup()

# Print RMSE
print(paste("RMSE for the combined model:", rmse_v3))
