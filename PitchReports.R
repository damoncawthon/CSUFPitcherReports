# ====================== CSUF Pitch Visuals — FULL SCRIPT (Stuff+ hidden, tables matched) ======================
library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(ggimage)
library(ggrepel)
library(plotly)
library(car)
library(grid)
library(png)
library(showtext)
library(lightgbm)
library(gt)
# -------------------- Font --------------------
library(sysfonts)
library(showtext)

# ---- FUNCTION 1: Calculate Fastball Averages ----
calculate_pitcher_fastball_averages <- function(dataset) {
  fastball_types <- c("Fastball", "FourSeamFastBall", "TwoSeamFastBall", "Sinker")
  
  pitcher_averages <- dataset %>%
    filter(
      TaggedPitchType %in% fastball_types,
      !is.na(RelSpeed), !is.na(ax0), !is.na(az0)
    ) %>%
    group_by(Pitcher) %>%
    summarize(
      avg_mph = mean(RelSpeed, na.rm = TRUE),
      avg_ax  = mean(ax0, na.rm = TRUE),
      avg_az  = mean(az0, na.rm = TRUE),
      .groups = "drop"
    )
  
  dataset %>%
    left_join(pitcher_averages, by = "Pitcher") %>%
    mutate(
      mph_diff = ifelse(is.na(avg_mph) | is.na(RelSpeed), NA_real_, RelSpeed - avg_mph),
      ax_diff = ifelse(is.na(avg_ax) | is.na(ax0), NA_real_, ax0 - avg_ax),
      az_diff = ifelse(is.na(avg_az) | is.na(az0), NA_real_, az0 - avg_az)
    )
}

# ---- FUNCTION 2: Build Feature Matrix ----
build_stuff_plus_features <- function(df) {
  get_col <- function(df, primary, alts = character()) {
    cols <- c(primary, alts)
    found <- cols[cols %in% names(df)][1]
    if (is.na(found)) return(rep(NA_real_, nrow(df)))
    as.numeric(df[[found]])
  }
  
  feature_matrix <- data.frame(
    RelSpeed   = get_col(df, "RelSpeed", c("release_speed", "Velo")),
    SpinRate   = get_col(df, "SpinRate", c("release_spin_rate")),
    SpinAxis   = get_col(df, "SpinAxis", c("spin_axis", "Tilt")),
    x0         = get_col(df, "x0", c("RelSide")),
    z0         = get_col(df, "z0", c("RelHeight")),
    az0        = get_col(df, "az0", c("az")),
    ax0        = get_col(df, "ax0", c("ax")),
    Extension  = get_col(df, "Extension", c("extension", "Ext")),
    mph_diff   = get_col(df, "mph_diff"),
    az_diff    = get_col(df, "az_diff"),
    ax_diff    = get_col(df, "ax_diff")
  )
  
  as.matrix(feature_matrix)
}

# ---- FUNCTION 3: Load Models ----
load_pitch_grade_models <- function(model_dir) {
  model_files <- list.files(
    model_dir, 
    pattern = "_PitchGradeModel\\.txt$", 
    full.names = TRUE
  )
  
  if (length(model_files) == 0) {
    warning("No pitch grade models found in: ", model_dir)
    return(list())
  }
  
  models <- list()
  for (file in model_files) {
    pitch_type <- sub("_PitchGradeModel\\.txt$", "", basename(file))
    pitch_type <- gsub("_", " ", pitch_type)
    
    models[[pitch_type]] <- tryCatch(
      lgb.load(file),
      error = function(e) { warning("Failed to load ", pitch_type); NULL }
    )
  }
  
  models[!sapply(models, is.null)]
}

# ---- FUNCTION 4: Resolve Pitch Types ----
resolve_pitch_type <- function(tagged_type) {
  alias_map <- c(
    "FourSeamFastBall" = "Fastball",
    "TwoSeamFastBall"  = "Fastball",
    "Changeup"         = "ChangeUp",
    "Undefined"        = "Fastball"
  )
  
  if (is.null(tagged_type) || is.na(tagged_type) || tagged_type == "") {
    return("Fastball")
  }
  
  if (tagged_type %in% names(alias_map)) {
    return(alias_map[[tagged_type]])
  }
  
  return(tagged_type)
}

# ---- FUNCTION 5: Predict Stuff+ ----
predict_stuff_plus <- function(dataset, models, reference_dataset = NULL) {
  if (is.null(reference_dataset)) {
    reference_dataset <- dataset
  }
  
  dataset$raw_pred <- NA_real_
  
  pitch_types <- unique(dataset$TaggedPitchType)
  pitch_types <- pitch_types[!is.na(pitch_types) & pitch_types != ""]
  
  predictions_list <- list()
  
  for (pt in pitch_types) {
    model_key <- resolve_pitch_type(pt)
    
    if (!model_key %in% names(models)) {
      next
    }
    
    pitch_data <- dataset %>% filter(TaggedPitchType == pt)
    
    if (nrow(pitch_data) == 0) next
    
    X <- tryCatch(
      build_stuff_plus_features(pitch_data),
      error = function(e) NULL
    )
    
    if (is.null(X)) next
    
    pitch_data$raw_pred <- tryCatch(
      as.numeric(predict(models[[model_key]], X)),
      error = function(e) rep(NA_real_, nrow(pitch_data))
    )
    
    predictions_list[[pt]] <- pitch_data
  }
  
  if (length(predictions_list) == 0) {
    dataset$raw_pred <- NA_real_
    dataset$StuffPlus <- NA_real_
    return(dataset)
  }
  
  dataset_with_preds <- bind_rows(predictions_list)
  
  reference_stats <- reference_dataset %>%
    filter(!is.na(raw_pred)) %>%
    group_by(TaggedPitchType) %>%
    summarise(
      ref_mean = mean(raw_pred, na.rm = TRUE),
      ref_sd   = sd(raw_pred, na.rm = TRUE),
      .groups = "drop"
    )
  
  dataset_with_preds <- dataset_with_preds %>%
    left_join(reference_stats, by = "TaggedPitchType") %>%
    mutate(
      StuffPlus = case_when(
        is.na(raw_pred) ~ NA_real_,
        is.na(ref_mean) | is.na(ref_sd) ~ NA_real_,
        ref_sd <= 0 ~ NA_real_,
        TRUE ~ 100 - 10 * (raw_pred - ref_mean) / ref_sd
      )
    ) %>%
    select(-ref_mean, -ref_sd)
  
  return(dataset_with_preds)
}

# create a dedicated, writable temp folder in the project dir
tmp <- file.path(getwd(), "tmp_showtext")
if (!dir.exists(tmp)) dir.create(tmp, recursive = TRUE)

Sys.setenv(TMPDIR = tmp)  # must be set before font_add_google()

# try the download; fall back if it still fails
ok <- TRUE
tryCatch(
  sysfonts::font_add_google("Markazi Text", "markazi", regular.wt = 600),
  error = function(e) { message("Google font fetch failed: ", e$message); ok <<- FALSE }
)

if (!ok) {
  # Optional: fall back to a bundled font or a system font you know exists
  # sysfonts::font_add(family = "markazi", regular = "/System/Library/Fonts/Supplemental/Georgia.ttf")
  # Or skip custom font entirely and let device default:
  sysfonts::font_add(family = "markazi", regular = "")
}

showtext_auto()



# ==================== ONE-TIME: set your RStudio pane ratio ====================
PANE_AR <- getOption("csuf_pane_ar", 1.617978)

# small null-coalesce used once
`%||%` <- function(a, b) if (!is.null(a)) a else b




# -------------------- FIXED convertHeights function --------------------
convertHeights <- function(df) {
  convert <- function(height_str) {
    if (is.na(height_str)) return(NA)
    if (is.numeric(height_str)) return(height_str)
    height_str <- trimws(gsub('["\']', '', height_str))
    if (grepl("'", height_str)) {
      parts <- strsplit(height_str, "'")[[1]]
      feet <- as.numeric(parts[1])
      inches <- if (length(parts) > 1 && parts[2] != "") as.numeric(parts[2]) else 0
    } else {
      feet <- 0
      inches <- as.numeric(height_str)
    }
    feet + (inches/12)
  }
  height_cols <- c("RelHeight", "RelSide", "PlateLocHeight", "PlateLocSide")
  existing_cols <- height_cols[height_cols %in% names(df)]
  if (length(existing_cols) > 0) {
    for (col in existing_cols) {
      if (is.character(df[[col]])) df[[col]] <- sapply(df[[col]], convert)
    }
  }
  if ("SpinEfficiency" %in% names(df)) {
    df$SpinEfficiency <- as.numeric(gsub("%", "", df$SpinEfficiency))
  }
  df
}

addPitcherThrows <- function(df) {
  right_handed <- c("Meyer, Gavin", "Smith, Dylan", "Langley, Aidan", "Hernandez, Chris",
                    "Turner, Derek", "Gurnea, Chad", "Faris, Grady", "Goff, Dylan", "Ritter, Tyler")
  left_handed  <- c("Negrete, Mikiah", "Harper, Jayden", "Wright, Andrew", "Hawkinson, Payton", "Dockan, Brady")
  df$PitcherThrows <- ifelse(df$Pitcher %in% right_handed, "Right",
                             ifelse(df$Pitcher %in% left_handed, "Left", NA))
  df
}

processData <- function(df) df %>% convertHeights() %>% addPitcherThrows()

arm_angle_categories <- function(df) {
  df %>%
    mutate(
      arm_angle_type = case_when(
        arm_angle_savant >= 60                          ~ "Over The Top",
        arm_angle_savant >= 50 & arm_angle_savant < 60  ~ "High Three-Quarters",
        arm_angle_savant >= 40 & arm_angle_savant < 50  ~ "Three-Quarters",
        arm_angle_savant >= 25 & arm_angle_savant < 40  ~ "Low Three-Quarters",
        arm_angle_savant >= 20 & arm_angle_savant < 25  ~ "Slinger",
        arm_angle_savant >=  0 & arm_angle_savant < 20  ~ "Sidearm",
        arm_angle_savant <  0                           ~ "Submarine",
        TRUE ~ NA_character_
      )
    )
}

# -------------------- ARM ANGLE CALC (join by Pitcher + PitcherTeam) --------------------
arm_angle_calc <- function(data_frame) {
  normalize_name  <- function(x) trimws(gsub("\\s+", " ", as.character(x)))
  normalize_team  <- function(x) tolower(gsub("[^a-z]", "", as.character(x)))
  normalize_throws <- function(x) {
    x <- as.character(x); x <- trimws(tolower(x))
    dplyr::case_when(
      x %in% c("r","rh","rhp","right","righty") ~ "Right",
      x %in% c("l","lh","lhp","left","lefty")   ~ "Left",
      TRUE ~ NA_character_
    )
  }
  
  # --- Normalize heights table ---
  ht <- D1PitcherHeights %>%
    dplyr::mutate(
      Pitcher   = ifelse(Pitcher == "Allen, Cade Van", "Van Allen, Cade", Pitcher),
      Pitcher   = normalize_name(Pitcher),
      team_norm = normalize_team(PitcherTeam),
      PitcherHeight = as.numeric(Height),  # already inches
      PitcherThrows_ht = normalize_throws(PitcherThrows)
    ) %>%
    dplyr::select(Pitcher, team_norm, PitcherHeight, PitcherThrows_ht)
  
  # --- Normalize input dataset ---
  df0 <- data_frame %>%
    dplyr::mutate(
      Pitcher       = normalize_name(Pitcher),
      team_norm     = normalize_team(PitcherTeam),
      PitcherThrows = normalize_throws(PitcherThrows)
    )
  
  # --- Left join; unmatched heights will be NA ---
  df1 <- df0 %>%
    dplyr::left_join(ht, by = c("Pitcher","team_norm")) %>%
    dplyr::mutate(
      PitcherHeight = PitcherHeight,
      PitcherThrows = dplyr::coalesce(PitcherThrows, PitcherThrows_ht)
    ) %>%
    dplyr::select(-PitcherThrows_ht)
  
  # --- Geometry ---
  df1 %>%
    dplyr::mutate(
      arm_length   = PitcherHeight * 0.39,
      RelSide_in   = RelSide * 12,
      RelHeight_in = RelHeight * 12,
      shoulder_pos = PitcherHeight * 0.70,
      Adj = RelHeight_in - shoulder_pos,
      Opp = abs(RelSide_in),
      arm_angle_rad = atan2(Opp, Adj),
      arm_angle = arm_angle_rad * (180 / pi)
    ) %>%
    dplyr::select(-Opp, -arm_angle_rad) %>%
    dplyr::mutate(
      arm_angle_180 = dplyr::case_when(
        PitcherThrows == "Left"  ~ 180 - arm_angle,
        PitcherThrows == "Right" ~ 180 + arm_angle,
        TRUE ~ arm_angle
      ),
      arm_angle_savant = dplyr::case_when(
        is.na(arm_angle) ~ NA_real_,
        arm_angle >= 0 & arm_angle <= 90 ~ arm_angle,
        arm_angle > 90 ~ 180 - arm_angle,
        TRUE ~ abs(arm_angle)
      )
    )
}

# ============================================================
# ========== TOP BAR HELPERS (outing metrics + percentiles) ==
# ============================================================
# ============================================================
# ========== TOP BAR HELPERS (outing metrics + percentiles) ==
# ============================================================

in_zone_loc <- function(s, h) {
  !is.na(s) & !is.na(h) & s >= -0.86 & s <= 0.86 & h >= 1.50 & h <= 3.60
}

swing_events <- c("InPlay", "FoulBall", "StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable")
is_swing <- function(pc) pc %in% swing_events

# General strikes (include fouls)
is_strike_event <- function(pc) {
  pc %in% c("StrikeCalled","StrikeSwinging","FoulBall","FoulBallNotFieldable","FoulBallFieldable")
}

# Put-away strikes (exclude fouls)
is_strike_event_putaway <- function(pc) pc %in% c("StrikeCalled","StrikeSwinging")

# ---------- NEW: reusable labeling helper ----------
label_pitch_region <- function(df) {
  df %>%
    mutate(
      xStrike = suppressWarnings(as.numeric(xStrike)),
      xSwing  = suppressWarnings(as.numeric(xSwing)),
      xStrike0 = coalesce(xStrike, 0),
      xSwing0  = coalesce(xSwing,  0),
      
      # same “heart” geometry as your strike zone UI (≈ middle box)
      Meatball = !is.na(PlateLocSide) & !is.na(PlateLocHeight) &
        PlateLocSide  >= -0.417 & PlateLocSide  <=  0.417 &
        PlateLocHeight >=  1.83  & PlateLocHeight <=  3.17,
      
      Likely   = (xStrike0 > 0.05) | (xSwing0 > 0.05),
      Waste    = (xStrike0 < 0.05) & (xSwing0 < 0.05),
      
      PitchRegion = dplyr::case_when(
        Meatball                        ~ "Meatball",
        !Meatball & Likely              ~ "Edge-Expand",
        Waste                           ~ "Waste",
        TRUE                            ~ "Other"
      )
    )
}
# one outing (all pitch types) => raw top-bar metrics (0–1), no formatting
compute_top_bar_raw <- function(df, pitcher, date_value) {
  date_only <- as.Date(date_value)
  
  d <- df %>%
    mutate(.date_only = as.Date(Date)) %>%
    filter(Pitcher == pitcher, .date_only == date_only)
  
  if (!nrow(d)) {
    return(tibble::tibble(
      Pitches = 0, `Strike%` = NA_real_, `Swing%` = NA_real_, `Whiff%` = NA_real_,
      `Chase%` = NA_real_, `FPS%` = NA_real_, `Put Away%` = NA_real_,
      `Meatball%` = NA_real_, `Edge-Expand%` = NA_real_, `Waste%` = NA_real_
    ))
  }
  
  # apply our region labeling
  d <- label_pitch_region(d) %>%
    mutate(
      in_zone     = in_zone_loc(PlateLocSide, PlateLocHeight),
      swing       = is_swing(PitchCall),
      whiff       = PitchCall == "StrikeSwinging",
      out_of_zone = !is.na(PlateLocSide) & !is.na(PlateLocHeight) & !in_zone,
      first_pitch = dplyr::coalesce(PitchofPA == 1, FALSE)
    ) %>%
    group_by(pa_id = cumsum(first_pitch))
  
  total_pitches <- nrow(d)
  
  strike_rate <- if (total_pitches > 0) mean(d$in_zone, na.rm = TRUE) else NA_real_
  swing_rate  <- if (total_pitches > 0) mean(d$swing,   na.rm = TRUE) else NA_real_
  whiff_rate  <- if (sum(d$swing, na.rm = TRUE) > 0) mean(d$whiff[d$swing], na.rm = TRUE) else NA_real_
  chase_rate  <- if (sum(d$out_of_zone, na.rm = TRUE) > 0) mean(d$swing[d$out_of_zone], na.rm = TRUE) else NA_real_
  fps_rate    <- if (sum(d$first_pitch, na.rm = TRUE) > 0)
    mean(is_strike_event(d$PitchCall[d$first_pitch]), na.rm = TRUE)
  else NA_real_
  
  # Put Away%: reached 2 strikes and ended on a called/swinging strike (no fouls)
  pa_last      <- d %>% slice_tail(n = 1) %>% ungroup()
  pa_reached2  <- d %>% summarise(reached2 = any(Strikes >= 2, na.rm = TRUE), .groups = "drop")
  pa_k         <- pa_last %>% mutate(k_out = (Strikes == 2) & is_strike_event_putaway(PitchCall))
  putaway_n    <- sum(pa_reached2$reached2, na.rm = TRUE)
  putaway_rate <- if (putaway_n > 0) mean(pa_k$k_out[pa_reached2$reached2], na.rm = TRUE) else NA_real_
  
  # ungroup before final summary
  d <- d %>% ungroup()
  
  tibble::tibble(
    Pitches        = total_pitches,
    `Strike%`      = strike_rate,
    `Swing%`       = swing_rate,
    `Whiff%`       = whiff_rate,
    `Chase%`       = chase_rate,
    `FPS%`         = fps_rate,
    `Put Away%`    = putaway_rate,
    `Meatball%`    = mean(d$PitchRegion == "Meatball",     na.rm = TRUE),
    `Edge-Expand%` = mean(d$PitchRegion == "Edge-Expand",  na.rm = TRUE),
    `Waste%`       = mean(d$PitchRegion == "Waste",        na.rm = TRUE)
  )
}
# ==================== LOAD XSWING & XCALLEDSTRIKE MODELS ====================

library(lightgbm)
library(xgboost)

cat("Loading feature specifications and models...\n")

# Load feature names from RDS files
xswing_features <- tryCatch(readRDS('/Users/a13105/Documents/R Projects/Scripts/Pitcher Reports 2026/www/final_model_xSwing_features.rds'), 
                            error = function(e) NULL)
xstrike_features <- tryCatch(readRDS('/Users/a13105/Documents/R Projects/Scripts/Pitcher Reports 2026/www/xgb_model_pitchcall_features.rds'),
                             error = function(e) NULL)

xstrike_classmap <- tryCatch(readRDS('/Users/a13105/Documents/R Projects/Scripts/Pitcher Reports 2026/www/xgb_model_pitchcall_classmap.rds'),
                             error = function(e) NULL)

# Load xSwing LightGBM model from disk
xswing_model <- tryCatch(lgb.load('/Users/a13105/Documents/R Projects/Scripts/Pitcher Reports 2026/www/final_model_xSwing_model.txt'),
                         error = function(e) { 
                           warning("Could not load xSwing model:", e$message); 
                           NULL 
                         })

# Load xCalledStrike XGBoost model from disk
xstrike_model <- tryCatch(xgb.load('/Users/a13105/Documents/R Projects/Scripts/Pitcher Reports 2026/www/final_model_xCalledStrike_model.xgb'),
                          error = function(e) { 
                            warning("Could not load xCalledStrike model:", e$message); 
                            NULL 
                          })



cat("xSwing model loaded:", !is.null(xswing_model), "\n")
cat("xCalledStrike model loaded:", !is.null(xstrike_model), "\n")
cat("xSwing features (n=", length(xswing_features), "):", paste(head(xswing_features, 5), collapse = ", "), "...\n")
cat("xCalledStrike features:", paste(xstrike_features, collapse = ", "), "\n")



# Process base dataset
CSUF25_proc <- processData(CSUF25)

# Calculate fastball averages (RECALCULATED EACH RUN)
cat("Calculating pitcher fastball averages...\n")
CSUF25_proc <- calculate_pitcher_fastball_averages(CSUF25_proc)

# ==================== LOAD D1 REFERENCE STATISTICS ====================
cat("Loading Stuff+ reference statistics from D1 population...\n")

stuff_plus_reference <- readRDS(
  "/Users/a13105/Documents/R Projects/Scripts/Pitcher Reports 2026/www/stuff_plus_reference.rds"
)

cat("D1 Reference Statistics loaded:\n")
print(stuff_plus_reference)

# Load pitch grade models
model_dir <- "/Users/a13105/Documents/R Projects/Scripts/Pitcher Reports 2026/www/CSUF Report Pitch Grade copy"
pitch_grade_models <- load_pitch_grade_models(model_dir)
cat("Loaded", length(pitch_grade_models), "pitch grade models\n")

save_ggplot_png <- function(p, file, width = 6, height = 4, dpi = 200) {
  ggplot2::ggsave(filename = file, plot = p, width = width, height = height, dpi = dpi, bg = "white")
  file.exists(file)
}

save_baseplot_png <- function(plot_fun, file, width_px = 1200, height_px = 900, res = 200) {
  grDevices::png(filename = file, width = width_px, height = height_px, res = res, bg = "white")
  on.exit(grDevices::dev.off(), add = TRUE)
  plot_fun()
  TRUE
}

safe_read_png <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(png::readPNG(path), error = function(e) NULL)
}



# ==================== FUNCTION: Calculate Stuff+ with D1 Reference ====================
# ==================== FUNCTION: Calculate Stuff+ with D1 Reference ====================
calculate_stuff_plus_with_d1_reference <- function(dataset, models, reference_stats) {
  cat("\nCalculating Stuff+ for", nrow(dataset), "pitches using D1 reference...\n")
  
  # Check if fastball averages already exist, if not calculate them
  if (!all(c("mph_diff", "az_diff", "ax_diff") %in% names(dataset))) {
    cat("Calculating fastball averages (not found in dataset)...\n")
    dataset <- calculate_pitcher_fastball_averages(dataset)
  }
  
  # Initialize
  dataset$raw_pred <- NA_real_
  dataset$StuffPlus <- NA_real_
  
  # Get unique pitch types in this dataset
  pitch_types <- unique(dataset$TaggedPitchType)
  pitch_types <- pitch_types[!is.na(pitch_types) & pitch_types != ""]
  
  # Predict for each pitch type
  for (pitch_type in pitch_types) {
    # Resolve model key (handle aliases)
    model_key <- resolve_pitch_type(pitch_type)
    
    if (!model_key %in% names(models)) {
      cat("  No model for", pitch_type, "(tried", model_key, ")\n")
      next
    }
    
    # Filter data for this pitch type
    pitch_data <- dataset %>% filter(TaggedPitchType == pitch_type)
    if (nrow(pitch_data) == 0) next
    
    # Build feature matrix
    X <- tryCatch(
      build_stuff_plus_features(pitch_data),
      error = function(e) {
        warning(paste("Error building features for", pitch_type, ":", e$message))
        NULL
      }
    )
    
    if (is.null(X)) next
    
    # Predict raw values
    raw_preds <- tryCatch(
      as.numeric(predict(models[[model_key]], X)),
      error = function(e) {
        warning(paste("Error predicting for", pitch_type, ":", e$message))
        rep(NA_real_, nrow(pitch_data))
      }
    )
    
    # Assign raw predictions
    dataset$raw_pred[dataset$TaggedPitchType == pitch_type] <- raw_preds
    
    # Get D1 reference stats for this pitch type
    ref_row <- reference_stats %>% filter(TaggedPitchType == pitch_type)
    
    if (nrow(ref_row) == 1) {
      ref_mean <- ref_row$ref_mean
      ref_sd <- ref_row$ref_sd
      
      # Calculate Stuff+ using D1 population stats
      stuff_plus_vals <- 100 - 10 * (raw_preds - ref_mean) / ref_sd
      dataset$StuffPlus[dataset$TaggedPitchType == pitch_type] <- stuff_plus_vals
      
      cat("  ", pitch_type, ": predicted", length(raw_preds), "pitches",
          "(mean Stuff+:", round(mean(stuff_plus_vals, na.rm = TRUE), 1), ")\n")
    } else {
      cat("  ", pitch_type, ": no D1 reference stats available\n")
    }
  }
  
  cat("Stuff+ calculation complete!\n")
  return(dataset)
}
# Calculate Stuff+ using D1 reference
cat("Calculating Stuff+ predictions using D1 reference...\n")
CSUF25_proc <- calculate_stuff_plus_with_d1_reference(
  dataset = CSUF25_proc,
  models = pitch_grade_models,
  reference_stats = stuff_plus_reference
)

# Verify Stuff+ was calculated
cat("\nStuff+ Summary for this game:\n")
print(CSUF25_proc %>% 
        group_by(TaggedPitchType) %>% 
        summarise(
          n = n(),
          avg_stuff_plus = mean(StuffPlus, na.rm = TRUE),
          min_stuff_plus = min(StuffPlus, na.rm = TRUE),
          max_stuff_plus = max(StuffPlus, na.rm = TRUE),
          .groups = "drop"
        ))

# ==================== DEFINE PREDICTION FUNCTIONS ====================
# THESE MUST BE DEFINED BEFORE THEY ARE CALLED

apply_xswing <- function(df, model, feature_names) {
  if (is.null(model)) {
    warning("xSwing model not available; setting xSwing to NA")
    df$xSwing <- NA_real_
    return(df)
  }
  
  tryCatch({
    cat("Applying xSwing predictions...\n")
    
    available_features <- intersect(feature_names, names(df))
    cat("Using", length(available_features), "of", length(feature_names), "xSwing features\n")
    
    # Create full feature matrix with ALL features (pad missing ones with 0)
    X_full <- data.frame(matrix(0, nrow = nrow(df), ncol = length(feature_names)))
    names(X_full) <- feature_names
    
    # Fill in available features
    for (feat in available_features) {
      X_full[[feat]] <- df[[feat]]
    }
    
    X_matrix <- as.matrix(X_full)
    pred <- predict(model, X_matrix)
    
    df$xSwing <- as.numeric(pred)
    cat("xSwing range:", paste(round(range(df$xSwing, na.rm = TRUE), 3), collapse = " to "), "\n")
    df
  }, error = function(e) {
    warning(paste("Error applying xSwing:", e$message))
    df$xSwing <- NA_real_
    df
  })
}

apply_xstrike <- function(df, model, feature_names, classmap = NULL) {
  # df: your working data.frame
  # model: xgboost binary:logistic model you just trained/saved
  # feature_names: c("PlateLocSide","PlateLocHeight") from RDS
  # classmap: optional vector from RDS; for binary it can be c(0,1)
  
  if (is.null(model)) {
    warning("xCalledStrike model not available; setting xStrike to NA")
    df$xStrike <- NA_real_
    return(df)
  }
  
  # Ensure all required features exist; create missing with NA
  miss <- setdiff(feature_names, names(df))
  if (length(miss)) {
    warning("apply_xstrike: missing features in df: ", paste(miss, collapse = ", "),
            " — creating as NA")
    for (m in miss) df[[m]] <- NA_real_
  }
  
  # Build feature matrix in the exact order of 'feature_names'
  X <- as.matrix(df[, feature_names, drop = FALSE])
  
  # If there are NAs in features, xgboost will still predict, but warn user
  if (any(!is.finite(X))) {
    warning("apply_xstrike: non-finite values in xStrike features; predictions may be degraded")
  }
  
  # Predict
  dtest <- xgb.DMatrix(X)
  pred  <- tryCatch(predict(model, dtest),
                    error = function(e) { warning("xgb predict failed: ", e$message); return(rep(NA_real_, nrow(df))) })
  
  # --- Shape handling ---
  # New binary model: predict() returns length == nrow(df)
  # Old 3-class model (multi:softprob): length == nrow(df) * 3 -> we must pick the StrikeCalled column.
  if (length(pred) == nrow(df)) {
    # Binary logistic: direct probability of class "1" (StrikeCalled)
    pred_vec <- as.numeric(pred)
    
  } else if (length(pred) == nrow(df) * 3) {
    # Fallback for an old 3-class model that might still be on disk
    pred_matrix <- matrix(pred, nrow = nrow(df), ncol = 3, byrow = TRUE)
    
    # Use classmap if provided; otherwise assume labels c(0,1,2) with "1" = StrikeCalled
    labels <- if (is.null(classmap)) c(0, 1, 2) else as.numeric(classmap)
    strike_col <- which(labels == 1)
    if (length(strike_col) != 1) {
      warning("apply_xstrike: could not resolve StrikeCalled column from classmap; using column 2")
      strike_col <- 2
    }
    pred_vec <- as.numeric(pred_matrix[, strike_col])
    
  } else {
    warning("apply_xstrike: unexpected prediction length (", length(pred), 
            ") for nrow=", nrow(df), "; setting xStrike=NA")
    pred_vec <- rep(NA_real_, nrow(df))
  }
  
  df$xStrike <- pred_vec
  cat("xStrike range:", paste(round(range(df$xStrike, na.rm = TRUE), 3), collapse = " to "), "\n")
  df
}


# Continue with existing processing
CSUF25_proc <- apply_xswing(CSUF25_proc, xswing_model, xswing_features)
CSUF25_proc <- apply_xstrike(CSUF25_proc, xstrike_model, xstrike_features)
CSUF25_with_angles <- arm_angle_calc(CSUF25_proc) %>% arm_angle_categories()
AAFall24 <- CSUF25_with_angles

# ==================== CREATE LIVAA AND LIHAA ====================
# Add this after CSUF25_proc is created but before applying xSwing/xStrike models

cat("Computing xVAA and xHAA expected values...\n")

# Create one-hot encoding for pitch types
CSUF25_proc <- CSUF25_proc %>%
  mutate(TaggedPitchType = as.factor(TaggedPitchType))

# One-hot encode pitch type
TaggedPitchType_onehot_vaa <- model.matrix(~ TaggedPitchType - 1, data = CSUF25_proc)

# Prepare data for VAA model
model_data_vaa <- cbind(
  CSUF25_proc %>% select(PlateLocSide, VertApprAngle),
  TaggedPitchType_onehot_vaa
)

# Fit VAA model
VAA_model <- tryCatch({
  lm(VertApprAngle ~ PlateLocSide + ., data = model_data_vaa)
}, error = function(e) {
  warning("Could not fit VAA model:", e$message)
  NULL
})

# Predict xVAA if model was successful
if (!is.null(VAA_model)) {
  CSUF25_proc$xVAA <- predict(VAA_model, newdata = model_data_vaa)
  CSUF25_proc$LIVAA <- CSUF25_proc$xVAA - CSUF25_proc$VertApprAngle
  cat("xVAA computed. LIVAA range:", 
      paste(round(range(CSUF25_proc$LIVAA, na.rm = TRUE), 2), collapse = " to "), "\n")
} else {
  CSUF25_proc$xVAA <- NA_real_
  CSUF25_proc$LIVAA <- 0
}

# Prepare data for HAA model
TaggedPitchType_onehot_haa <- model.matrix(~ TaggedPitchType - 1, data = CSUF25_proc)

model_data_haa <- cbind(
  CSUF25_proc %>% select(PlateLocHeight, HorzApprAngle),
  TaggedPitchType_onehot_haa
)

# Fit HAA model
HAA_model <- tryCatch({
  lm(HorzApprAngle ~ PlateLocHeight + ., data = model_data_haa)
}, error = function(e) {
  warning("Could not fit HAA model:", e$message)
  NULL
})

# Predict xHAA if model was successful
if (!is.null(HAA_model)) {
  CSUF25_proc$xHAA <- predict(HAA_model, newdata = model_data_haa)
  CSUF25_proc$LIHAA <- CSUF25_proc$xHAA - CSUF25_proc$HorzApprAngle
  cat("xHAA computed. LIHAA range:", 
      paste(round(range(CSUF25_proc$LIHAA, na.rm = TRUE), 2), collapse = " to "), "\n")
} else {
  CSUF25_proc$xHAA <- NA_real_
  CSUF25_proc$LIHAA <- 0
}

cat("LIVAA and LIHAA columns created.\n")

# ==================== LOAD XSWING & XCALLEDSTRIKE MODELS ====================
library(lightgbm)
library(xgboost)

cat("Loading feature specifications and models...\n")

# Load feature names from RDS files
xswing_features <- tryCatch(readRDS('/Users/a13105/Documents/R Projects/Scripts/Pitcher Reports 2026/www/final_model_xSwing_features.rds'), 
                            error = function(e) NULL)
xstrike_features <- tryCatch(readRDS('/Users/a13105/Documents/R Projects/Scripts/Pitcher Reports 2026/www/xgb_model_pitchcall_features.rds'),
                             error = function(e) NULL)

# Load xSwing LightGBM model from disk
xswing_model <- tryCatch(lgb.load('/Users/a13105/Documents/R Projects/Scripts/Pitcher Reports 2026/www/final_model_xSwing_model.txt'),
                         error = function(e) { 
                           warning("Could not load xSwing model:", e$message); 
                           NULL 
                         })

# Load xCalledStrike XGBoost model from disk
xstrike_model <- tryCatch(xgb.load('/Users/a13105/Documents/R Projects/Scripts/Pitcher Reports 2026/www/final_model_xCalledStrike_model.xgb'),
                          error = function(e) { 
                            warning("Could not load xCalledStrike model:", e$message); 
                            NULL 
                          })

cat("xSwing model loaded:", !is.null(xswing_model), "\n")
cat("xCalledStrike model loaded:", !is.null(xstrike_model), "\n")


# ==================== CREATE ONE-HOT ENCODED PITCH TYPE FEATURES ====================

cat("Creating one-hot encoded pitch type features...\n")

pitch_types <- c("Changeup", "ChangeUp", "Curveball", "Cutter", "Fastball", 
                 "FourSeamFastBall", "Knuckleball", "OneSeamFastBall", "Other",
                 "Sinker", "Slider", "Splitter", "Sweeper", "TwoSeamFastBall", "Undefined")

for (pt in pitch_types) {
  col_name <- paste0("TaggedPitchType", pt)
  CSUF25_proc[[col_name]] <- as.numeric(CSUF25_proc$TaggedPitchType == pt)
}

if (!"row_index" %in% names(CSUF25_proc)) {
  CSUF25_proc$row_index <- seq_len(nrow(CSUF25_proc))
}

cat("One-hot encoded pitch type features created.\n")

# Verify all features now exist
available_features_check <- intersect(xswing_features, names(CSUF25_proc))
cat("xSwing features now available:", length(available_features_check), "of", length(xswing_features), "\n")

# ==================== NOW APPLY MODELS ====================

cat("\n=== Applying Models ===\n")
CSUF25_proc <- apply_xswing(CSUF25_proc, xswing_model, xswing_features)
CSUF25_proc <- apply_xstrike(CSUF25_proc, xstrike_model, xstrike_features)

# ==================== CREATE FINAL DATASET ====================
CSUF25_with_angles <- arm_angle_calc(CSUF25_proc) %>% arm_angle_categories()
AAFall24 <- CSUF25_with_angles
cat("Dataset ready with xSwing and xCalledStrike predictions.\n")

saveRDS(AAFall24,
        "/Users/a13105/Documents/R Projects/Scripts/Pitcher Reports 2026/www/last_run_AAFall24.rds")


# ---- optional: export to your R session + persist to disk
if (interactive()) {
  assign("CSUF25_with_angles", CSUF25_with_angles, envir = .GlobalEnv)
  assign("AAFall24", AAFall24, envir = .GlobalEnv)
}
saveRDS(CSUF25_with_angles,
        "/Users/a13105/Documents/R Projects/Scripts/Pitcher Reports 2026/www/last_run_CSUF25_with_angles.rds")
saveRDS(AAFall24,
        "/Users/a13105/Documents/R Projects/Scripts/Pitcher Reports 2026/www/last_run_AAFall24.rds")



# ---------- Percentiles loader (pitcher-season reference), optional ----------
percentile_ref_path <- "/Users/a13105/Documents/R Projects/Scripts/Pitcher Reports 2026/www/percentiles_pitcher_season_ref.rds"
percentile_ref <- tryCatch(readRDS(percentile_ref_path), error = function(e) NULL)

percentile_of <- function(x, sorted_vec) {
  if (is.null(sorted_vec) || !length(sorted_vec) || !is.finite(x)) return(NA_real_)
  idx <- findInterval(x, sorted_vec, all.inside = TRUE)
  idx / length(sorted_vec)  # fraction <= x
}

apply_percentiles_to_topbar <- function(topbar_raw_one_row, ref) {
  if (is.null(ref)) return(topbar_raw_one_row)
  mv <- ref$metric_values_sorted
  
  # normalize any percent-like reference vectors to 0–1
  norm01 <- function(v) {
    if (!length(v) || all(!is.finite(v))) return(v)
    mx <- suppressWarnings(max(v, na.rm = TRUE))
    if (is.finite(mx) && mx > 1.000001) v/100 else v
  }
  mv$Strike     <- norm01(mv$Strike)
  mv$Swing      <- norm01(mv$Swing)
  mv$Whiff      <- norm01(mv$Whiff)
  mv$Chase      <- norm01(mv$Chase)
  mv$FPS        <- norm01(mv$FPS)
  mv$Meatball   <- norm01(mv$Meatball)
  mv$EdgeExpand <- norm01(mv$EdgeExpand)  # <-- the one biting you
  mv$Waste      <- norm01(mv$Waste)
  mv$PutAway    <- norm01(mv$PutAway)
  
  get_col <- function(df, name_a, name_b = NULL) {
    if (name_a %in% names(df)) return(df[[name_a]])
    if (!is.null(name_b) && name_b %in% names(df)) return(df[[name_b]])
    NA_real_
  }
  
  edge_val <- get_col(topbar_raw_one_row, "Edge-Expand%", "EdgeExpand%")
  
  dplyr::bind_cols(
    topbar_raw_one_row,
    tibble::tibble(
      `Strike%_pct`      = percentile_of(get_col(topbar_raw_one_row, "Strike%"),      mv$Strike),
      `Swing%_pct`       = percentile_of(get_col(topbar_raw_one_row, "Swing%"),       mv$Swing),
      `Whiff%_pct`       = percentile_of(get_col(topbar_raw_one_row, "Whiff%"),       mv$Whiff),
      `Chase%_pct`       = percentile_of(get_col(topbar_raw_one_row, "Chase%"),       mv$Chase),
      `FPS%_pct`         = percentile_of(get_col(topbar_raw_one_row, "FPS%"),         mv$FPS),
      `Meatball%_pct`    = percentile_of(get_col(topbar_raw_one_row, "Meatball%"),    mv$Meatball),
      `Edge-Expand%_pct` = percentile_of(edge_val,                                     mv$EdgeExpand),
      `Waste%_pct`       = percentile_of(get_col(topbar_raw_one_row, "Waste%"),       mv$Waste),
      `Put Away%_pct`    = percentile_of(get_col(topbar_raw_one_row, "Put Away%"),    mv$PutAway)
    )
  )
}

theme_report <- function(
    base_size = 12,           # overall text
    title_size = 16,          # plot title
    subtitle_size = 12,       # plot subtitle
    axis_size = 9,            # axis tick labels
    legend_title = 10,
    legend_text = 9
) {
  theme_minimal(base_size = base_size, base_family = "markazi") +
    theme(
      plot.title    = element_text(hjust = 0.5, size = title_size, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = subtitle_size, face = "bold"),
      axis.title    = element_text(size = axis_size + 1),
      axis.text     = element_text(size = axis_size),
      legend.title  = element_text(size = legend_title),
      legend.text   = element_text(size = legend_text)
    )
}


# FIXED generate_pitcher_reports_pdf function
# Replace your existing function with this version

generate_pitcher_reports_pdf <- function(dataset_with_angles,
                                         output_filename = "Pitcher_Reports.pdf",
                                         pitcher_team_filter = NULL) {
  
  # Most recent date in the dataset
  most_recent_date <- max(as.Date(dataset_with_angles$Date), na.rm = TRUE)
  
  df_recent <- dataset_with_angles %>%
    dplyr::mutate(.date_only = as.Date(Date)) %>%
    dplyr::filter(.date_only == most_recent_date)
  
  if (!is.null(pitcher_team_filter) && "PitcherTeam" %in% names(df_recent)) {
    df_recent <- df_recent %>% dplyr::filter(PitcherTeam == pitcher_team_filter)
  }
  
  pitchers <- sort(unique(as.character(df_recent$Pitcher)))
  if (!length(pitchers)) {
    message("No pitchers found for most recent date: ", most_recent_date)
    return(NULL)
  }
  
  cat("Generating Pitcher PDF for", length(pitchers), "pitchers on", as.character(most_recent_date), "\n")
  cat("Saving to:", output_filename, "\n")
  
  # Showtext for PDF clarity
  showtext::showtext_opts(dpi = 300)
  showtext::showtext_auto(TRUE)
  
  tmp_dir <- tempdir()
  
  # Open PDF device
  grDevices::pdf(output_filename, width = 11, height = 8.5)
  
  for (pname in pitchers) {
    cat("Processing:", pname, "\n")
    
    tryCatch({
      # --- Build plots ---
      p_sz_r  <- sz_RHH(pname, dataset_with_angles, most_recent_date)
      p_mov   <- movement_plot(pname, dataset_with_angles, most_recent_date)
      p_sz_l  <- sz_LHH(pname, dataset_with_angles, most_recent_date)
      p_arm   <- pitcher_plot_arm_angle(dataset_with_angles, pname, most_recent_date)
      
      # base plots
      base_traj_r_fun <- function() trajectories_avg_by_type_RHH(pname, dataset_with_angles, most_recent_date)
      base_traj_l_fun <- function() trajectories_avg_by_type_LHH(pname, dataset_with_angles, most_recent_date)
      
      # --- Save six visuals to PNG ---
      f_sz_r  <- file.path(tmp_dir, paste0("sz_r_",  gsub("[^A-Za-z0-9]", "_", pname), ".png"))
      f_mov   <- file.path(tmp_dir, paste0("mov_",   gsub("[^A-Za-z0-9]", "_", pname), ".png"))
      f_sz_l  <- file.path(tmp_dir, paste0("sz_l_",  gsub("[^A-Za-z0-9]", "_", pname), ".png"))
      f_tr_r  <- file.path(tmp_dir, paste0("tr_r_",  gsub("[^A-Za-z0-9]", "_", pname), ".png"))
      f_arm   <- file.path(tmp_dir, paste0("arm_",   gsub("[^A-Za-z0-9]", "_", pname), ".png"))
      f_tr_l  <- file.path(tmp_dir, paste0("tr_l_",  gsub("[^A-Za-z0-9]", "_", pname), ".png"))
      
      # Save with explicit checks
      if (!save_ggplot_png(p_sz_r, f_sz_r, width = 4.2, height = 3.0, dpi = 200)) {
        warning("Failed to save strike zone RHH plot for ", pname)
      }
      if (!save_ggplot_png(p_mov,  f_mov,  width = 4.2, height = 3.0, dpi = 200)) {
        warning("Failed to save movement plot for ", pname)
      }
      if (!save_ggplot_png(p_sz_l, f_sz_l, width = 4.2, height = 3.0, dpi = 200)) {
        warning("Failed to save strike zone LHH plot for ", pname)
      }
      if (!save_ggplot_png(p_arm,  f_arm,  width = 4.2, height = 3.0, dpi = 200)) {
        warning("Failed to save arm angle plot for ", pname)
      }
      
      save_baseplot_png(base_traj_r_fun, f_tr_r, width_px = 1400, height_px = 900, res = 200)
      save_baseplot_png(base_traj_l_fun, f_tr_l, width_px = 1400, height_px = 900, res = 200)
      
      # Verify files were created
      files_to_check <- c(f_sz_r, f_mov, f_sz_l, f_tr_r, f_arm, f_tr_l)
      for (f in files_to_check) {
        if (!file.exists(f) || file.size(f) == 0) {
          warning("Missing or empty file: ", basename(f))
        }
      }
      
      # --- Build gt tables, save to PNG ---
      tb_raw <- compute_top_bar_raw(dataset_with_angles, pitcher = pname, date_value = most_recent_date)
      tb_pct <- apply_percentiles_to_topbar(tb_raw, percentile_ref)
      gt_top <- topbar_gt_table(tb_raw, tb_pct)
      
      gt_pitch <- {
        tab <- make_pitchtype_report_table(dataset_with_angles, pname, most_recent_date)
        if (!nrow(tab)) {
          gt::gt(data.frame()) %>% gt::tab_header(title = "No data")
        } else {
          cols <- c("Pitch Type","Pitches","Usage%","Velo","Max Velocity","IVB","HB",
                    "Spin Direction","Release Height","Arm Angle","TopVAA","BotVAA",
                    "Extension","Strike%","Whiff%","CSW%","Chase%","Stuff+")
          tab <- tab[, intersect(cols, names(tab)), drop = FALSE]
          
          gt::gt(tab, rowname_col = NULL) |>
            gt::tab_header(title = gt::md("**Per-Pitch-Type Summary**")) |>
            gt::cols_label(
              `Pitch Type` = "Pitch",
              `Max Velocity` = "Max Velo",
              `Spin Direction` = "Spin Dir",
              `Release Height` = "Rel H",
              `Arm Angle` = "Arm Ang",
              `TopVAA` = "Top VAA",
              `BotVAA` = "Bot VAA",
              `Stuff+` = "Stuff+"
            ) |>
            gt::sub_missing(gt::everything(), missing_text = "—") |>
            gt::tab_options(
              table.font.names = "Markazi Text",
              table.font.size = gt::px(12),
              table.width = gt::pct(100),
              data_row.padding = gt::px(3)
            ) |>
            gt::opt_table_lines() |>
            gt::opt_row_striping()
        }
      }
      
      f_topbar <- file.path(tmp_dir, paste0("topbar_", gsub("[^A-Za-z0-9]", "_", pname), ".png"))
      f_ptab   <- file.path(tmp_dir, paste0("ptab_",   gsub("[^A-Za-z0-9]", "_", pname), ".png"))
      
      gt::gtsave(gt_top,   f_topbar, vwidth = 1400, vheight = 220)
      gt::gtsave(gt_pitch, f_ptab,   vwidth = 1400, vheight = 520)
      
      # --- NEW: Start composing the page with better error handling ---
      grid::grid.newpage()
      
      title_text <- paste0(pname, " — Pitcher Report — ", format(most_recent_date, "%B %d, %Y"))
      grid::grid.text(
        title_text, x = 0.5, y = 0.94,
        gp = grid::gpar(fontsize = 20, fontface = "bold", fontfamily = "markazi")
      )
      
      # Read all PNGs - verify they exist AND have content first
      img_sz_r <- if (file.exists(f_sz_r) && file.size(f_sz_r) > 0) safe_read_png(f_sz_r) else NULL
      img_mov  <- if (file.exists(f_mov) && file.size(f_mov) > 0) safe_read_png(f_mov) else NULL
      img_sz_l <- if (file.exists(f_sz_l) && file.size(f_sz_l) > 0) safe_read_png(f_sz_l) else NULL
      img_tr_r <- if (file.exists(f_tr_r) && file.size(f_tr_r) > 0) safe_read_png(f_tr_r) else NULL
      img_arm  <- if (file.exists(f_arm) && file.size(f_arm) > 0) safe_read_png(f_arm) else NULL
      img_tr_l <- if (file.exists(f_tr_l) && file.size(f_tr_l) > 0) safe_read_png(f_tr_l) else NULL
      
      # Count how many images we have
      n_images <- sum(!is.null(img_sz_r), !is.null(img_mov), !is.null(img_sz_l),
                      !is.null(img_tr_r), !is.null(img_arm), !is.null(img_tr_l))
      cat("  Successfully loaded", n_images, "of 6 plot images\n")
      
      # 2x3 plot grid (upper section)
      grid::pushViewport(grid::viewport(x = 0.5, y = 0.62, width = 0.98, height = 0.55))
      lay <- grid::grid.layout(nrow = 2, ncol = 3, 
                               widths = grid::unit(rep(1,3), "null"), 
                               heights = grid::unit(rep(1,2), "null"))
      grid::pushViewport(grid::viewport(layout = lay))
      
      place_img <- function(img, r, c, label = "") {
        grid::pushViewport(grid::viewport(layout.pos.row = r, layout.pos.col = c))
        if (is.null(img)) {
          # Draw placeholder instead of nothing
          grid::grid.rect(gp = grid::gpar(fill = "white", col = "gray"))
          grid::grid.text(paste("No data:", label), 
                          gp = grid::gpar(fontsize = 10, col = "gray50"))
        } else {
          tryCatch({
            grid::grid.raster(img, interpolate = TRUE)
          }, error = function(e) {
            cat("  Error rendering image at row", r, "col", c, ":", e$message, "\n")
            grid::grid.rect(gp = grid::gpar(fill = "white", col = "red"))
            grid::grid.text("Error", gp = grid::gpar(fontsize = 10, col = "red"))
          })
        }
        grid::popViewport()
      }
      
      place_img(img_sz_r, 1, 1, "SZ RHH")
      place_img(img_mov,  1, 2, "Movement")
      place_img(img_sz_l, 1, 3, "SZ LHH")
      place_img(img_tr_r, 2, 1, "Traj RHH")
      place_img(img_arm,  2, 2, "Arm Angle")
      place_img(img_tr_l, 2, 3, "Traj LHH")
      
      grid::popViewport(2)
      
      # Topbar (middle)
      topbar_img <- if (file.exists(f_topbar) && file.size(f_topbar) > 0) safe_read_png(f_topbar) else NULL
      if (!is.null(topbar_img)) {
        cat("  Placing topbar table\n")
        tryCatch({
          grid::pushViewport(grid::viewport(x = 0.5, y = 0.26, width = 0.96, height = 0.10))
          grid::grid.raster(topbar_img, interpolate = TRUE)
          grid::popViewport()
        }, error = function(e) {
          cat("  Error placing topbar:", e$message, "\n")
        })
      } else {
        cat("  Topbar image not available\n")
      }
      
      # Pitch-type table (bottom)
      ptab_img <- if (file.exists(f_ptab) && file.size(f_ptab) > 0) safe_read_png(f_ptab) else NULL
      if (!is.null(ptab_img)) {
        cat("  Placing pitch-type table\n")
        tryCatch({
          grid::pushViewport(grid::viewport(x = 0.5, y = 0.13, width = 0.96, height = 0.22))
          grid::grid.raster(ptab_img, interpolate = TRUE)
          grid::popViewport()
        }, error = function(e) {
          cat("  Error placing pitch table:", e$message, "\n")
        })
      } else {
        cat("  Pitch table image not available\n")
      }
      
      # Cleanup temp files
      unlink(c(f_sz_r, f_mov, f_sz_l, f_tr_r, f_arm, f_tr_l, f_topbar, f_ptab))
      
    }, error = function(e) {
      cat("Error processing", pname, ":", e$message, "\n")
      grid::grid.newpage()
      grid::grid.text(
        paste("Error generating report for", pname),
        x = 0.5, y = 0.5,
        gp = grid::gpar(fontsize = 14, fontfamily = "markazi")
      )
    })
  }
  
  # Close PDF device
  grDevices::dev.off()
  showtext::showtext_opts(dpi = 96)
  
  cat("\n=== PITCHER PDF GENERATION COMPLETE ===\n")
  cat("File saved to:", output_filename, "\n")
  cat("File exists:", file.exists(output_filename), "\n")
  if (file.exists(output_filename)) {
    cat("File size:", file.size(output_filename), "bytes\n")
  }
  
  return(output_filename)
}



# ===== Formatting helpers for the band =====
ordinal <- function(n) {
  if (is.na(n)) return(NA_character_)
  n <- as.integer(n)
  s <- if (n %% 100L %in% c(11L, 12L, 13L)) "th"
  else switch(as.character(n %% 10L),
              "1" = "st", "2" = "nd", "3" = "rd", "th")
  paste0(n, s)
}


fmt_val_ptile <- function(val, ptile = NA_real_) {
  if (is.na(val)) return("<span class='muted'>&mdash;</span>")
  if (is.na(ptile)) return(sprintf("%s%%", sprintf("%.1f", 100*val)))
  ord <- ordinal(as.integer(round(100*ptile)))
  sprintf("%s%% <span class='muted'>(%s)</span>", sprintf("%.1f", 100*val), ord)
}
fmt_pct_plain <- function(val) if (is.na(val)) "<span class='muted'>&mdash;</span>" else sprintf("%.1f%%", 100*val)
fmt_int <- function(x) if (is.na(x) || x == 0) "<span class='muted'>&mdash;</span>" else format(x, big.mark = ",")

# ---- NEW: simple global helpers used by format_topbar_display ----
fmt_pct   <- function(x) ifelse(is.na(x), "—", sprintf("%.1f%%", 100 * x))
fmt_ptile <- function(p) {
  if (is.na(p)) "" else {
    ord <- ordinal(as.integer(round(100 * p)))
    sprintf(" <span class='percentile'>(%s)</span>", ord)
  }
}
# === NEW/CHANGED === Top bar as gt (helper to print percent + percentile) ----
fmt_pct_html <- function(val, ptile = NA_real_) {
  if (is.na(val)) return("—")
  v <- sprintf("%.1f%%", 100 * as.numeric(val))
  if (is.na(ptile)) return(v)
  ord <- ordinal(as.integer(round(100 * as.numeric(ptile))))
  paste0(v, "<br/><span style='font-size:10px;color:#666;'>(", ord, ")</span>")
}

# === NEW/CHANGED === Build a one-row gt table with spanners matching your bottom table
topbar_gt_table <- function(df_raw, df_pct) {
  row <- tibble::tibble(
    `Pitches`        = format(df_raw$Pitches[1] %||% 0, big.mark = ","),
    `Strike%`        = fmt_pct_html(df_raw$`Strike%`[1],      df_pct$`Strike%_pct`[1]),
    `Swing%`         = fmt_pct_html(df_raw$`Swing%`[1],       df_pct$`Swing%_pct`[1]),
    `Whiff%`         = fmt_pct_html(df_raw$`Whiff%`[1],       df_pct$`Whiff%_pct`[1]),
    `Chase%`         = fmt_pct_html(df_raw$`Chase%`[1],       df_pct$`Chase%_pct`[1]),
    `FPS%`           = fmt_pct_html(df_raw$`FPS%`[1],         df_pct$`FPS%_pct`[1]),
    `Put Away%`      = fmt_pct_html(df_raw$`Put Away%`[1],    df_pct$`Put Away%_pct`[1]),
    `Meatball%`      = fmt_pct_html(df_raw$`Meatball%`[1],    df_pct$`Meatball%_pct`[1]),
    `Edge-Expand%`   = fmt_pct_html(df_raw$`Edge-Expand%`[1], df_pct$`Edge-Expand%_pct`[1]),
    `Waste%`         = fmt_pct_html(df_raw$`Waste%`[1],       df_pct$`Waste%_pct`[1])
  )
  
  gt(row) |>
    tab_spanner(label = md("**Throw Strikes**"),
                columns = c(`Strike%`,`Swing%`,`Whiff%`,`Chase%`)) |>
    tab_spanner(label = md("**Early & Often**"),
                columns = c(`FPS%`,`Put Away%`)) |>
    tab_spanner(label = md("**Compete & Execute**"),
                columns = c(`Meatball%`,`Edge-Expand%`,`Waste%`)) |>
    cols_align("center") |>
    fmt_markdown(everything()) |>
    tab_options(
      table.font.names = "Markazi Text",
      table.font.size  = px(16),
      table.width      = pct(100),
      data_row.padding = px(6),
      # heading.hidden = TRUE,  # <-- removed (not supported in your gt)
      table.border.top.width    = px(2),
      table.border.top.color    = "black",
      table.border.bottom.width = px(1),
      table.border.bottom.color = "black",
      column_labels.font.weight = "bold",
      column_labels.padding     = px(6)
    ) |>
    opt_table_lines()
}

# Updated CSS in your UI
tags$style(HTML(
  "
  .plot-row { margin-bottom: 2px; }
  
  /* TOP BAR styling */
  .topbar { margin-top: 6px; margin-bottom: 2px; }
  .topbar .band { 
    width: 100%; 
    border-collapse: collapse; 
    table-layout: fixed; 
    font-size: 16px;
  }

  /* Headers - Bold */
  .topbar .band th {
    padding: 6px 8px; 
    text-align: center; 
    font-weight: 600;
    line-height: 1.05;
    border: 2px solid #000000;
    background: transparent;
  }

  /* Data cells - Normal weight */
  .topbar .band td {
    padding: 6px 8px; 
    text-align: center; 
    font-weight: 400;     
    line-height: 1.05;
    border: 1px solid #000000;
    font-size: 16px;      
  }

  /* Main headers extra bold */
  .topbar .band th[colspan] {
    font-weight: 800;
    background: transparent;
  }

  /* Percentile styling */
  .topbar .percentile {
    font-size: 10px;      
    color: #666;          
    font-weight: 400;     
  }
  
  .topbar .muted { 
    color: #888; 
    font-weight: 400;
  }
  
  .topbar .defs { 
    margin-top: 6px; 
    font-size: 12px; 
    color: #555; 
  }

  /* PITCH-TYPE TABLE SPECIFIC STYLING */
  .pitch-table .band {
    font-size: 8px !important;  /* Smaller base font for pitch table */
  }
  
/* Bottom table: make header text smaller and wrap */
.pitch-table .band th {
  padding: 2px 4px !important;    /* tighter padding */
  font-size: 10px !important;      /* smaller header font */
  line-height: 1.0 !important;     /* compact lines */
  white-space: normal !important;  /* allow wrapping */
  word-break: break-word;          /* break long words if needed */
}

/* Title cell (the first row with colspan) slightly bigger but still small */
.pitch-table .band th[colspan] {
  font-size: 11px !important;
  font-weight: 700 !important;
}

/* Data cells can stay tiny */
.pitch-table .band td {
  font-size: 9px !important;
  padding: 3px 4px !important;
}

  "
))

# -------------------- Build data (CSUF25 only for plotting) ----------------
# -------------------- Build data (CSUF25 only for plotting) ----------------
if (!exists("CSUF25")) stop("CSUF25 not found. Load it before running the app.")
if (!exists("D1PitcherHeights")) stop("D1PitcherHeights not found. Load it before running the app.")
D1PitcherHeights$Pitcher <- ifelse(D1PitcherHeights$Pitcher == "Allen, Cade Van","Van Allen, Cade", D1PitcherHeights$Pitcher)

if (!exists("CSUF25_with_angles") || !all(c("xSwing","xStrike") %in% names(CSUF25_with_angles))) {
  CSUF25_proc <- processData(CSUF25)
  # ensure models are applied so xSwing/xStrike exist
  CSUF25_proc <- apply_xswing(CSUF25_proc, xswing_model, xswing_features)
  CSUF25_proc <- apply_xstrike(CSUF25_proc, xstrike_model, xstrike_features)
  CSUF25_with_angles <- arm_angle_calc(CSUF25_proc) %>% arm_angle_categories()
}

# ===== FILTER TO CAL_FUL ONLY =====
CSUF25_with_angles <- CSUF25_with_angles %>%
  filter(PitcherTeam == "CAL_FUL")

AAFall24 <- CSUF25_with_angles

# -------------------- Plotting functions ----------
pitch_colors <- c(
  "Fastball"="#D22D49","Sinker"="#FE9D00","Cutter"="#933F2C","Slider"="#EEE716",
  "Curveball"="#00D1ED","Splitter"="#3BACAC","ChangeUp"="#1DBE3A","Sweeper"="#DDB33A"
)

# 1. Pitch type abbreviation function
# Replace your abbreviate_pitch_type() with this:
abbreviate_pitch_type <- function(pitch_type) {
  m <- c(
    "Fastball"         = "FB",
    "FourSeamFastBall" = "FB",
    "TwoSeamFastBall"  = "SI",   # if you’d rather make this "SI", change it here
    "Sinker"           = "SI",
    "ChangeUp"         = "CH",
    "Changeup"         = "CH",
    "Slider"           = "SL",
    "Curveball"        = "CB",
    "Cutter"           = "CT",
    # keep sensible defaults for others you may see
    "Sweeper"          = "SW",
    "Splitter"         = "SP",
    "Knuckleball"      = "KN",
    "Undefined"        = "UND",
    "Other"            = "OTH"
  )
  out <- unname(m[as.character(pitch_type)])
  ifelse(is.na(out), toupper(substr(as.character(pitch_type), 1, 2)), out)
}


movement_plot <- function(PitcherName, Dataset, date_value, ellipse_level = 0.80) {
  pitcher_col <- "Pitcher"; date_col <- "Date"; x_col <- "HorzBreak"; y_col <- "InducedVertBreak"; type_col <- "TaggedPitchType"
  target_date <- as.Date(date_value)
  df <- Dataset %>% mutate(.date_only = as.Date(.data[[date_col]])) %>%
    filter(.data[[pitcher_col]] == PitcherName, .date_only == target_date)
  present_types <- unique(df[[type_col]]); color_vals <- pitch_colors[names(pitch_colors) %in% present_types]
  df_ell <- df %>% group_by(.data[[type_col]]) %>% filter(n() >= 3) %>% ungroup()
  avg_points <- df %>% group_by(.data[[type_col]]) %>%
    summarise(avg_x = mean(.data[[x_col]], na.rm = TRUE),
              avg_y = mean(.data[[y_col]], na.rm = TRUE), .groups = "drop")
  ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    geom_point(aes(fill = .data[[type_col]]), shape = 21, size = 3, alpha = 0.7, color = "black", stroke = 0.3) +
    stat_ellipse(data = df_ell, aes(fill = .data[[type_col]]), level = ellipse_level, type = "norm",
                 alpha = 0.20, geom = "polygon", color = NA) +
    geom_point(data = avg_points, aes(x = avg_x, y = avg_y, fill = .data[[type_col]]),
               inherit.aes = FALSE, shape = 21, size = 5, color = "black", stroke = 0.8) +
    scale_fill_manual(values = color_vals, drop = TRUE) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
    annotate("segment", x = -0.83, xend = 0.83, y = 0, yend = 0, colour = "black", size = 1) +
    coord_equal(xlim = c(-25, 25), ylim = c(-25, 25)) +
    labs(title = "Pitch Movement", x = "Horizontal Break", y = "Induced Vertical Break", fill  = "Pitch Type") +
    theme_report(base_size = 12, title_size = 12, axis_size = 9,
                   legend_title = 9, legend_text = 8) +
    theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))
  
}

# ===== Base-R trajectory helper (aspect ratio locked here) =====
.avg_traj_plot_side <- function(player_name, dataset, date_value, side_label,
                                x_limits = c(-9, 9), z_limits = c(-6, 13),
                                tighten_release = FALSE, max_release_dist = 0.25,
                                compress_x = 0.8,
                                batter_image_path = "/Users/a13105/Downloads/BatterPerspective.png") {
  pitch_equation <- function(a, v, p0, t) a * t^2 + v * t + p0
  
  df <- dataset |>
    dplyr::filter(Pitcher == player_name,
                  is.finite(ZoneTime),
                  !is.na(TaggedPitchType),
                  !is.na(BatterSide)) |>
    dplyr::mutate(.date_only = as.Date(Date)) |>
    dplyr::filter(.date_only == as.Date(date_value),
                  BatterSide == side_label)
  if (nrow(df) == 0) {
    plot.new(); title(main = paste("No data for", player_name, date_value, paste0(" (", side_label, " hitters)")))
    return(invisible(NULL))
  }
  
  if (isTRUE(tighten_release)) {
    mean_x0 <- mean(df$x0, na.rm = TRUE); mean_z0 <- mean(df$z0, na.rm = TRUE)
    df <- df |>
      dplyr::mutate(release_dist = sqrt((x0 - mean_x0)^2 + (z0 - mean_z0)^2)) |>
      dplyr::filter(release_dist <= max_release_dist)
    if (nrow(df) == 0) {
      plot.new(); title(main = paste("All rows filtered by release cluster (", side_label, ")", sep = ""))
      return(invisible(NULL))
    }
  }
  
  avg_traj <- df |>
    dplyr::group_by(TaggedPitchType) |>
    dplyr::summarise(
      ax0 = mean(ax0, na.rm = TRUE),
      vx0 = mean(vx0, na.rm = TRUE),
      x0  = mean(x0,  na.rm = TRUE),
      az0 = mean(az0, na.rm = TRUE),
      vz0 = mean(vz0, na.rm = TRUE),
      z0  = mean(z0,  na.rm = TRUE),
      ZoneTime = mean(ZoneTime, na.rm = TRUE),
      .groups = "drop"
    )
  if (nrow(avg_traj) == 0) {
    plot.new(); title(main = paste("No pitch types after averaging (", side_label, ")", sep = ""))
    return(invisible(NULL))
  }
  
  op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
  par(mar = c(0, 0, 4.5, 0), family = "markazi")
  plot(NA, xlim = x_limits, ylim = z_limits, xlab = "", ylab = "",
       main = paste0("Avg Trajectory by Pitch Type vs. ", side_label, " Handed Hitters"),
       cex.main = 1.8, axes = FALSE)
  mtext("One mean trajectory per pitch type (colored). End markers at plate.",
        side = 3, line = 0.25, cex = 1)
  
  rect(-2.5, -3.5, 2.5, 7, border = "black", lwd = 1.5, lty = 2)
  
  xleft <- -0.75; xright <- 0.15; ybottom <- 2.75; ytop <- 2.70
  center_x <- (xleft + xright)/2; center_y <- (ybottom + ytop)/2
  angle_deg <- if (side_label == "Right") 359 else 1; angle_rad <- angle_deg * pi / 180
  corners <- data.frame(x = c(xleft,xright,xright,xleft), y = c(ybottom,ybottom,ytop,ytop))
  rotated_corners <- corners |>
    dplyr::mutate(
      x_rot = center_x + (x - center_x) * cos(angle_rad) - (y - center_y) * sin(angle_rad),
      y_rot = center_y + (x - center_y) * sin(angle_rad) + (y - center_y) * cos(angle_rad)
    )
  for (j in 1:4) {
    x1 <- rotated_corners$x_rot[j]; y1 <- rotated_corners$y_rot[j]
    x2 <- rotated_corners$x_rot[ifelse(j == 4, 1, j + 1)]
    y2 <- rotated_corners$y_rot[ifelse(j == 4, 1, j + 1)]
    lines(c(x1, x2), c(y1, y2), col = "black", lwd = 1)
  }
  x1_top <- rotated_corners$x_rot[3]; y1_top <- rotated_corners$y_rot[3]
  x2_top <- rotated_corners$x_rot[4]; y2_top <- rotated_corners$y_rot[4]
  dir_x <- x2_top - x1_top; dir_y <- y2_top - y1_top
  dir_length <- sqrt(dir_x^2 + dir_y^2)
  norm_dir_x <- dir_x / dir_length; norm_dir_y <- dir_y / dir_length
  extension <- 0.2; y_offset <- 0.19
  extended_x1 <- x1_top - (extension * norm_dir_x)
  extended_y1 <- y1_top - (extension * norm_dir_y) + y_offset
  extended_x2 <- x2_top + (extension * norm_dir_x)
  extended_y2 <- y2_top + (extension * norm_dir_y) + y_offset
  lines(c(extended_x1, extended_x2), c(extended_y1, extended_y2), col = "black", lwd = 1)
  
  angle_15_rad <- 194 * (pi / 180)
  edge_angle <- atan2(dir_y, dir_x)
  new_angle_right <- edge_angle - angle_15_rad
  angled_x2 <- extended_x1 + cos(new_angle_right)
  angled_y2 <- extended_y1 + sin(new_angle_right)
  lines(c(extended_x1, angled_x2), c(extended_y1, angled_y2), col = "black", lwd = 1)
  
  second_angle_deg <- 345
  angle_left_rad <- second_angle_deg * (pi / 180)
  new_angle_left <- edge_angle - angle_left_rad
  angled_x2_left <- extended_x2 + cos(new_angle_left)
  angled_y2_left <- extended_y2 + sin(new_angle_left)
  lines(c(extended_x2, angled_x2_left), c(extended_y2, angled_y2_left), col = "black", lwd = 1)
  
  pt1 <- c(angled_x2, angled_y2); pt2 <- c(angled_x2_left, angled_y2_left)
  oval_center_x <- (pt1[1] + pt2[1]) / 2; oval_center_y <- (pt1[2] + pt2[2]) / 2
  oval_width <- sqrt((pt2[1] - pt1[1])^2 + (pt2[2] - pt1[2])^2); oval_height <- 1
  oval_angle_rad <- if (side_label == "Right") 359 * pi / 180 else 1 * pi / 180
  theta <- seq(pi, 2 * pi, length.out = 200)
  a <- oval_width / 2; b <- oval_height / 2
  x_vals <- oval_center_x + a * cos(theta) * cos(oval_angle_rad) - b * sin(theta) * sin(oval_angle_rad)
  y_vals <- oval_center_y + a * cos(theta) * sin(oval_angle_rad) + b * sin(theta) * cos(oval_angle_rad)
  lines(x_vals, y_vals, col = "black", lwd = 1)
  
  # Plot trajectories
  for (i in seq_len(nrow(avg_traj))) {
    row <- avg_traj[i, ]
    col <- c(
      "Fastball"="#D22D49","Sinker"="#FE9D00","Cutter"="#933F2C","Slider"="#EEE716",
      "Curveball"="#00D1ED","Splitter"="#3BACAC","ChangeUp"="#1DBE3A","Sweeper"="#DDB33A"
    )[[as.character(row$TaggedPitchType)]]
    if (is.na(col)) col <- "#444444"
    t_seq <- seq(0, row$ZoneTime, length.out = 200)
    x_traj <- pitch_equation(row$ax0, row$vx0, row$x0, t_seq) * compress_x
    z_traj <- pitch_equation(row$az0, row$vz0, row$z0, t_seq)
    col_rgb <- col2rgb(col) / 255
    lines(x_traj, z_traj, col = rgb(col_rgb[1], col_rgb[2], col_rgb[3], alpha = 0.9), lwd = 2)
    points(tail(x_traj, 1), tail(z_traj, 1), pch = 21, bg = col, col = "black", cex = 1.1, lwd = 0.7)
  }
  
  # Batter image (optional)
  if (file.exists(batter_image_path)) {
    batter_img <- tryCatch(png::readPNG(batter_image_path), error = function(e) NULL)
    if (!is.null(batter_img)) {
      batter_stands <- ifelse(side_label == "Right", "Right", "Left")
      if (batter_stands == "Right") {
        xleft_adj  <- -8.5; xright_adj <- -3.5; ybottom_adj <- -13; ytop_adj <- 20
        img_use <- batter_img[, ncol(batter_img):1, ]
      } else {
        xleft_adj  <- 3.5; xright_adj <- 8.5; ybottom_adj <- -13; ytop_adj <- 20
        img_use <- batter_img
      }
      rasterImage(img_use, xleft_adj, ybottom_adj, xright_adj, ytop_adj, interpolate = FALSE)
    }
  }
  
  used_types <- as.character(avg_traj$TaggedPitchType)
  used_cols  <- c(
    "Fastball"="#D22D49","Sinker"="#FE9D00","Cutter"="#933F2C","Slider"="#EEE716",
    "Curveball"="#00D1ED","Splitter"="#3BACAC","ChangeUp"="#1DBE3A","Sweeper"="#DDB33A"
  )[used_types]
  used_cols[is.na(used_cols)] <- "#444444"
  legend("topright", legend = used_types, pch = 15, pt.cex = 1, col = used_cols,
         bty = "n", cex = 0.8, title = "Pitch Type")
}

# Wrappers
trajectories_avg_by_type_RHH <- function(player_name, dataset, date_value) {
  .avg_traj_plot_side(player_name, dataset, date_value, "Right")
}
trajectories_avg_by_type_LHH <- function(player_name, dataset, date_value) {
  .avg_traj_plot_side(player_name, dataset, date_value, "Left")
}

# -------------------- Strike Zone (original ggplot) --------------------
.sz_plot <- function(PitcherName, Dataset, date_value, batter_side) {
  pitch_colors <- c("Fastball"="#D22D49","Sinker"="#FE9D00","Cutter"="#933F2C","Slider"="#EEE716",
                    "Curveball"="#00D1ED","Splitter"="#3BACAC","ChangeUp"="#1DBE3A")
  pitcher_col <- "Pitcher"; date_col <- "Date"; x_col <- "PlateLocSide"; y_col <- "PlateLocHeight"
  type_col <- "TaggedPitchType"; batter_col <- "BatterSide"
  zx_min <- -0.83; zx_max <- 0.83; zy_min <- 1.60; zy_max <- 3.50
  x1 <- zx_min + (zx_max - zx_min)/3; x2 <- zx_min + 2*(zx_max - zx_min)/3
  y1 <- zy_min + (zy_max - zy_min)/3; y2 <- zy_min + 2*(zy_max - zy_min)/3
  heart_xmin <- -0.56; heart_xmax <- 0.56; heart_ymin <- 1.83; heart_ymax <- 3.17
  shadow_xmin <- -1.11; shadow_xmax <- 1.11; shadow_ymin <- 1.17; shadow_ymax <- 3.83
  xlim_low <- -3; xlim_high <- 3; ylim_low <- -0.5; ylim_high <- 5.5
  target_date <- as.Date(date_value)
  df <- Dataset %>% mutate(.date_only = as.Date(.data[[date_col]])) %>%
    filter(.data[[pitcher_col]] == PitcherName, .date_only == target_date, .data[[batter_col]] == batter_side)
  present_types <- unique(df[[type_col]]); color_vals <- pitch_colors[names(pitch_colors) %in% present_types]
  ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    geom_rect(xmin = zx_min, xmax = zx_max, ymin = zy_min, ymax = zy_max, fill = NA, color = "black", linewidth = 1) +
    geom_segment(x = x1, xend = x1, y = zy_min, yend = zy_max, color = "grey60", linewidth = 0.4) +
    geom_segment(x = x2, xend = x2, y = zy_min, yend = zy_max, color = "grey60", linewidth = 0.4) +
    geom_segment(y = y1, yend = y1, x = zx_min, xend = zx_max, color = "grey60", linewidth = 0.4) +
    geom_segment(y = y2, yend = y2, x = zx_min, xend = zx_max, color = "grey60", linewidth = 0.4) +
    geom_rect(xmin = shadow_xmin, xmax = shadow_xmax, ymin = shadow_ymin, ymax = shadow_ymax,
              fill = NA, color = "grey60", linetype = "dotted", linewidth = 0.4) +
    geom_rect(xmin = heart_xmin, xmax = heart_xmax, ymin = heart_ymin, ymax = heart_ymax,
              fill = NA, color = "grey60", linetype = "dotted", linewidth = 0.4) +
    annotate("segment", x = -0.83, xend = 0.83, y = 0,   yend = 0,   colour = "black", size = 0.6) +
    annotate("segment", x = -0.83, xend = -0.83, y = 0,  yend = 0.3, colour = "black", size = 0.6) +
    annotate("segment", x =  0.83, xend =  0.83, y = 0,  yend = 0.3, colour = "black", size = 0.6) +
    annotate("segment", x =  0.83, xend =  0,    y = 0.3,yend = 0.5, colour = "black", size = 0.6) +
    annotate("segment", x = -0.83, xend =  0,    y = 0.3,yend = 0.5, colour = "black", size = 0.6) +
    geom_point(aes(fill = .data[[type_col]]), shape = 21, size = 2, alpha = 1, color = "black", stroke = 0.8) +
    scale_fill_manual(values = color_vals, drop = TRUE, name = "Pitch Type") +
    coord_equal(xlim = c(xlim_low, xlim_high), ylim = c(ylim_low, ylim_high)) +
    labs(title = paste0("Total Pitches vs. ", batter_side, " Handed Hitters"), x = NULL, y = NULL, fill = "Pitch Type") +
    theme_report(base_size = 12, title_size = 12, axis_size = 9,
                 legend_title = 9, legend_text = 8) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)
    )
  
}

sz_RHH <- function(PitcherName, Dataset, date_value) .sz_plot(PitcherName, Dataset, date_value, batter_side = "Right")
sz_LHH <- function(PitcherName, Dataset, date_value) .sz_plot(PitcherName, Dataset, date_value, batter_side = "Left")

# Arm Angle plot helpers
circleFun <- function(center = c(0, 0), radius = 24, npoints = 100) {
  tt <- seq(0, 2 * pi, length.out = npoints)
  data.frame(x = center[1] + radius * cos(tt), y = center[2] * 0 + radius * sin(tt))
}
circle <- circleFun(center = c(0, 0), radius = 24)
mound <- { theta <- seq(0, pi, length.out = 100); r <- 40; data.frame(x = r * cos(theta), y = 4 * sin(theta)) }

# Fixed pitcher_plot_arm_angle function with corrected geometry
pitcher_plot_arm_angle <- function(df, name, date) {
  date_only <- as.Date(date)
  
  filtered_data <- df %>%
    mutate(.date_only = as.Date(Date)) %>%
    filter(Pitcher == name, .date_only == date_only)
  if (nrow(filtered_data) == 0) stop("No data found for the specified pitcher and date.")
  
  filtered_data <- filtered_data %>%
    mutate(
      arm_length = PitcherHeight * 0.39,
      RelSide_in = RelSide * 12,
      RelHeight_in = RelHeight * 12,
      shoulder_pos = PitcherHeight * 0.70,
      horizontal_offset = abs(RelSide_in),
      vertical_offset = pmax(RelHeight_in - shoulder_pos, 0.1),
      arm_angle_rad = atan2(horizontal_offset, vertical_offset),
      arm_angle_degrees = arm_angle_rad * (180 / pi),
      arm_angle_savant = pmin(pmax(90 - arm_angle_degrees, 0), 90),
      arm_angle_type = dplyr::case_when(
        arm_angle_savant >= 75 ~ "Over The Top",
        arm_angle_savant >= 60 ~ "High Three-Quarters",
        arm_angle_savant >= 45 ~ "Three-Quarters",
        arm_angle_savant >= 30 ~ "Low Three-Quarters",
        arm_angle_savant >= 15 ~ "Slinger",
        arm_angle_savant >=  0 ~ "Sidearm",
        TRUE ~ "Submarine"
      )
    )
  
  median_arm_angle_savant <- median(filtered_data$arm_angle_savant, na.rm = TRUE)
  arm_angle_type_display <- dplyr::case_when(
    median_arm_angle_savant >= 75 ~ "Over The Top",
    median_arm_angle_savant >= 60 ~ "High Three-Quarters",
    median_arm_angle_savant >= 45 ~ "Three-Quarters",
    median_arm_angle_savant >= 30 ~ "Low Three-Quarters",
    median_arm_angle_savant >= 15 ~ "Slinger",
    median_arm_angle_savant >=  0 ~ "Sidearm",
    TRUE ~ "Submarine"
  )
  
  summary_data <- filtered_data %>%
    group_by(TaggedPitchType) %>%
    summarise(
      PitcherHeight = median(PitcherHeight, na.rm = TRUE),
      shoulder_pos  = median(shoulder_pos,  na.rm = TRUE),
      release_pos_x = median(RelSide * 12,   na.rm = TRUE),
      release_pos_z = median(RelHeight * 12, na.rm = TRUE),
      arm_angle_savant = median(arm_angle_savant, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      relx = dplyr::case_when(
        release_pos_x > 20 ~ 20,
        release_pos_z > 20 ~ 20 * (release_pos_x / (release_pos_z - shoulder_pos)),
        TRUE ~ release_pos_x
      ),
      relz = dplyr::case_when(
        release_pos_x > 20 ~ 20 * ((release_pos_z - shoulder_pos) / release_pos_x),
        release_pos_z > 20 ~ 20,
        TRUE ~ release_pos_z
      ),
      arm_length = PitcherHeight * .39,
      arm_dist   = sqrt((release_pos_x - 0)^2 + (release_pos_z - shoulder_pos)^2),
      arm_scale  = arm_length / arm_dist,
      should_x   = 0,
      should_y   = dplyr::case_when(
        median_arm_angle_savant >= 60 ~ 62.5,
        median_arm_angle_savant >= 15 ~ 56,
        TRUE ~ 45
      ),
      rel_x = should_x + (arm_scale * (release_pos_x - should_x)),
      rel_z = shoulder_pos + (arm_scale * (release_pos_z - shoulder_pos)) + should_y - (shoulder_pos)
    )
  
  # --- pick image based on slot + handedness ---
  PitcherThrows <- dplyr::first(na.omit(filtered_data$PitcherThrows))
  slot_bucket <- dplyr::case_when(
    median_arm_angle_savant >= 60 ~ "top",
    median_arm_angle_savant >= 15 ~ "mid",
    TRUE ~ "low"
  )
  image_path <- switch(
    paste0(slot_bucket, "_", PitcherThrows),
    "top_Right" = "/Users/a13105/Documents/R Projects/Scripts/Pitcher Reports 2026/www/SavantPitchers_top_right_front-svg.png",
    "mid_Right" = "/Users/a13105/Documents/R Projects/Scripts/Pitcher Reports 2026/www/SavantPitchers_mid_right_front.png",
    "low_Right" = "/Users/a13105/Documents/R Projects/Scripts/Pitcher Reports 2026/www/SavantPitchers_low_right_front-svg.png",
    "top_Left"  = "/Users/a13105/Documents/R Projects/Scripts/Pitcher Reports 2026/www/SavantPitchers_top_left_front-svg.png",
    "mid_Left"  = "/Users/a13105/Documents/R Projects/Scripts/Pitcher Reports 2026/www/ArmAngleLeft.png",
    "low_Left"  = "/Users/a13105/Documents/R Projects/Scripts/Pitcher Reports 2026/www/SavantPitchers_low_left_front-svg.png",
    "SavantPitchers_mid_right_front.png"
  )
  
  load_pitcher_png <- function(fname) {
    cand <- c(file.path("www", fname), fname)
    for (p in cand) if (file.exists(p)) return(png::readPNG(p))
    NULL
  }
  img_data <- load_pitcher_png(image_path)
  
  base_plot <- ggplot() +
    geom_polygon(data = mound, aes(x = x, y = y), fill = "#8B4513")
  
  if (!is.null(img_data)) {
    base_plot <- base_plot +
      annotation_custom(
        rasterGrob(img_data, interpolate = TRUE),
        xmin = -50, xmax = 50, ymin = 0, ymax = 100
      )
  }
  
  base_plot +
    coord_equal(xlim = c(-50, 50), ylim = c(0, 100)) +
    geom_rect(aes(xmin = -9, xmax = 9, ymin = 4, ymax = 4.5),
              fill = "white", color = "black") +
    geom_segment(
      data = summary_data,
      aes(x = 0,
          y = shoulder_pos + (6 + should_y - shoulder_pos),
          xend = -rel_x,
          yend = rel_z,
          color = TaggedPitchType),
      size = 5, alpha = 0.7
    ) +
    geom_point(
      data = summary_data,
      aes(x = -rel_x, y = rel_z, color = "black", fill = TaggedPitchType),
      pch = 21, size = 3.2, stroke = 1.5
    ) +
    scale_color_manual(values = pitch_colors, guide = "none") +
    scale_fill_manual(values = pitch_colors, name = "Pitch Type") +
    guides(fill = guide_legend(override.aes = list(size = 3.2))) +
    labs(
      title = "Hitter's Perspective", x = "", y = "",
      subtitle = paste0("Arm Angle: ", round(median_arm_angle_savant), "° - ", arm_angle_type_display)
    ) +
    theme_void(base_family = "markazi") +
    theme(
      plot.title    = element_text(hjust = 0.5, size = 12, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 9, face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(size = 9),
      legend.text  = element_text(size = 8)
    )
}

# -------------------- (legacy) small summary function kept if needed --------
compute_pitch_summary <- function(dataset, pitcher, date_value) {
  date_only <- as.Date(date_value)
  df <- dataset %>% mutate(.date_only = as.Date(Date)) %>%
    filter(Pitcher == pitcher, .date_only == date_only)
  total_pitches <- nrow(df)
  in_zone <- !is.na(df$PlateLocSide) & !is.na(df$PlateLocHeight) &
    df$PlateLocSide >= -0.83 & df$PlateLocSide <= 0.83 &
    df$PlateLocHeight >= 1.5  & df$PlateLocHeight <= 3.6
  outside_zone <- !is.na(df$PlateLocSide) & !is.na(df$PlateLocHeight) & !in_zone
  n_in_zone <- sum(in_zone, na.rm = TRUE)
  n_whiff   <- sum(df$PitchCall == "StrikeSwinging", na.rm = TRUE)
  n_csw     <- sum(df$PitchCall %in% c("StrikeCalled","StrikeSwinging"), na.rm = TRUE)
  n_outside <- sum(outside_zone, na.rm = TRUE)
  n_chase   <- sum((df$PitchCall == "StrikeSwinging") & outside_zone, na.rm = TRUE)
  first_pitch <- !is.na(df$PitchofPA) & suppressWarnings(as.numeric(df$PitchofPA) == 1)
  fps_denom   <- sum(first_pitch, na.rm = TRUE)
  fps_in_zone <- sum(first_pitch & in_zone, na.rm = TRUE)
  pct <- function(n, d) ifelse(d > 0, n / d, NA_real_)
  fmt_pct_legacy <- function(x) ifelse(is.na(x), "—", sprintf("%.1f%%", 100 * x))
  data.frame(
    Pitches = format(total_pitches, big.mark = ","),
    `Strike%` = fmt_pct_legacy(pct(n_in_zone, total_pitches)),
    `Whiff%`  = fmt_pct_legacy(pct(n_whiff, total_pitches)),
    `CSW%`    = fmt_pct_legacy(pct(n_csw, total_pitches)),
    `Chase%`  = fmt_pct_legacy(pct(n_chase, n_outside)),
    `FPS%`    = fmt_pct_legacy(pct(fps_in_zone, fps_denom)),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# ===================== Stuff+ additions (calculated but NOT displayed) =====================

# ---- Harmonizer for flexible column names ----
col_or <- function(df, primary, alts = character(), default = NA_real_) {
  nm <- c(primary, alts)[c(primary, alts) %in% names(df)][1]
  if (is.na(nm)) return(rep(default, nrow(df)))
  as.numeric(df[[nm]])
}

# ---- Spin Direction formatter (round to nearest 15 min) ----
clock_from_degrees <- function(deg_vec) {
  mins <- (deg_vec %% 360) / 360 * 720
  mins_rounded <- round(mins / 15) * 15
  mins_rounded <- mins_rounded %% 720
  hh <- floor(mins_rounded / 60); hh[hh == 0] <- 12
  mm <- mins_rounded %% 60
  sprintf("%d:%02d", hh, mm)
}

# ---- Build the requested per-pitch-type table for a given outing (Stuff+ computed but DROPPED from output) ----
is_swing_event <- function(pc) pc %in% c("InPlay","FoulBall","StrikeSwinging","FoulBallNotFieldable","FoulBallFieldable")

# 6. Simplified make_pitchtype_report_table
make_pitchtype_report_table <- function(dataset, pitcher, date_value) {
  date_only <- as.Date(date_value)
  d0 <- dataset %>%
    dplyr::mutate(.date_only = as.Date(Date)) %>%
    dplyr::filter(Pitcher == pitcher, .date_only == date_only)
  
  if (!nrow(d0)) return(dplyr::tibble())
  
  d0$raw_pred <- NA_real_
  total_pitches <- nrow(d0)
  
  d_pred <- d0 %>%
    dplyr::mutate( 
      Velo      = col_or(d0, "release_speed", c("RelSpeed","Velo")),
      IVB       = col_or(d0, "InducedVertBreak", c("ivb","InducedVB")),
      HB        = col_or(d0, "HorzBreak", c("HB","pfx_x")),
      TiltDeg   = col_or(d0, "spin_axis", c("Tilt","SpinAxis")),
      RelH      = col_or(d0, "RelHeight", c("release_pos_z","z0")),
      ArmAng_raw = col_or(d0, "arm_angle_savant", c("ArmAngle","arm_angle")),
      ArmAng     = ifelse(
        is.finite(ArmAng_raw),
        pmax(0, pmin(90, 90 - ArmAng_raw)),  # clamp to [0, 90]
        NA_real_
      ),
      VAA       = col_or(d0, "VertApprAngle", c("VAA","vert_appr_angle")),
      Ext       = col_or(d0, "Extension", c("extension")),
      PlateX    = col_or(d0, "PlateLocSide", c("plate_x","px")),
      PlateZ    = col_or(d0, "PlateLocHeight", c("plate_z","pz")),
      in_zone      = in_zone_loc(PlateX, PlateZ),
      swung        = is_swing_event(PitchCall),
      whiff        = PitchCall == "StrikeSwinging",
      called_strk  = PitchCall == "StrikeCalled",
      outside_zone = !is.na(PlateX) & !is.na(PlateZ) & !in_zone
    )
  
  tab <- d_pred %>%
    dplyr::group_by(TaggedPitchType) %>%
    dplyr::summarise(
      Pitches          = dplyr::n(),
      `Usage%`         = if (total_pitches > 0) Pitches / total_pitches else NA_real_,
      
      Velo_avg         = mean(Velo, na.rm = TRUE),
      Max_Velocity_raw = suppressWarnings(max(Velo, na.rm = TRUE)),
      
      IVB              = mean(IVB, na.rm = TRUE),
      HB               = mean(HB,  na.rm = TRUE),
      `Spin Direction` = { avg_deg <- mean(TiltDeg, na.rm = TRUE); if (is.finite(avg_deg)) clock_from_degrees(avg_deg) else NA_character_ },
      `Release Height` = mean(RelH, na.rm = TRUE),
      `Arm Angle`      = mean(ArmAng, na.rm = TRUE),
      TopVAA           = suppressWarnings(max(VAA, na.rm = TRUE)),
      BotVAA           = suppressWarnings(min(VAA, na.rm = TRUE)),
      Extension        = mean(Ext,  na.rm = TRUE),
      `Strike%`        = mean(in_zone, na.rm = TRUE),
      `Whiff%`         = { swings <- sum(swung, na.rm = TRUE); if (swings > 0) mean(whiff[swung], na.rm = TRUE) else NA_real_ },
      `CSW%`           = mean(whiff | called_strk, na.rm = TRUE),
      `Chase%`         = { oz <- sum(outside_zone, na.rm = TRUE); if (oz > 0) mean(swung[outside_zone], na.rm = TRUE) else NA_real_ },
      `Stuff+` = mean(StuffPlus, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      `Max Velocity` = ifelse(is.finite(Max_Velocity_raw), Max_Velocity_raw, NA_real_)
    ) %>%
    dplyr::rename(
      Velo = Velo_avg
    ) %>%
    dplyr::arrange(dplyr::desc(Pitches))
  
  fmt <- function(x, d=1) ifelse(is.na(x), NA, round(x, d))
  tab %>%
    dplyr::mutate(
      `Usage%`        = fmt(`Usage%`*100, 1),
      Velo            = fmt(Velo, 1),
      `Max Velocity`  = fmt(`Max Velocity`, 1),
      IVB             = fmt(IVB, 1),
      HB              = fmt(HB, 1),
      `Release Height`= fmt(`Release Height`, 2),
      `Arm Angle`     = fmt(`Arm Angle`, 0),
      TopVAA          = fmt(TopVAA, 1),
      BotVAA          = fmt(BotVAA, 1),
      Extension       = fmt(Extension, 2),
      `Strike%`       = fmt(`Strike%`*100, 1),
      `Whiff%`        = fmt(`Whiff%`*100, 1),
      `CSW%`          = fmt(`CSW%`*100, 1),
      `Chase%`        = fmt(`Chase%`*100, 1),
      `Stuff+` = fmt(`Stuff+`, 0)
    ) %>%
    dplyr::mutate(`Pitch Type` = abbreviate_pitch_type(TaggedPitchType)) %>%
    dplyr::select(-TaggedPitchType)
}
# ==================== SHINY UI (search pitchers, CSUF25 only) ===============
# ==================== SHINY UI (search pitchers, CSUF25 only) ===============
# ===== FILTER PITCHER LIST TO CAL_FUL ONLY =====
pitchers_all <- sort(unique(as.character(CSUF25_with_angles$Pitcher[CSUF25_with_angles$PitcherTeam == "CAL_FUL"])))
dates_for <- function(p) {
  df_filtered <- CSUF25_with_angles %>% 
    filter(PitcherTeam == "CAL_FUL", Pitcher == p)
  sort(unique(as.Date(df_filtered$Date)))
}
ui <- fluidPage(
  titlePanel("CSUF Pitcher Reports"),
  tags$head(
    tags$style(HTML(
      "
  .plot-row { margin-bottom: 2px; }
  /* TOP BAR styling */
.topbar { margin-top: 6px; margin-bottom: 2px; }
.topbar .band { 
  width: 100%; 
  border-collapse: collapse; 
  table-layout: fixed; 
  font-size: 16px;  /* Main table font size */
}

/* Headers - Bold */
.topbar .band th {
  padding: 6px 8px; 
  text-align: center; 
  font-weight: 600;
  line-height: 1.05;
  border: 2px solid #000000;
  background: transparent;
}

/* Data cells - Normal weight */
.topbar .band td {
  padding: 6px 8px; 
  text-align: center; 
  font-weight: 400;     
  line-height: 1.05;
  border: 1px solid #000000;
  font-size: 16px;      
}

/* Main headers extra bold */
.topbar .band th[colspan] {
  font-weight: 800;
  background: transparent;
}

/* Percentile styling - completely independent sizing */
.topbar .percentile {
  font-size: 10px;      
  color: #666;          
  font-weight: 400;     
}
.topbar .muted { 
  color: #888; 
  font-weight: 400;
}
.topbar .defs { 
  margin-top: 6px; 
  font-size: 12px; 
  color: #555; 
}
  "
    ))
  ),
  sidebarLayout(
    sidebarPanel(width = 3,
                 selectizeInput("pitcher", "Pitcher", choices = pitchers_all,
                                options = list(placeholder = 'Search pitcher...')),
                 uiOutput("date_ui"),
                 actionButton("generate_pitcher_pdf",
                              "Generate PDF for All Pitchers (Most Recent Date)",
                              class = "btn-primary", width = "100%")
                 
                 
    ),
    mainPanel(width = 9,
              # Top row
              fluidRow(class = "plot-row",
                       column(4, plotOutput("p_sz_rhh", height = "250px")),
                       column(4, plotOutput("p_movement", height = "250px")),
                       column(4, plotOutput("p_sz_lhh", height = "250px"))
              ),
              # Bottom row  
              fluidRow(class = "plot-row",
                       column(4, plotOutput("p_traj_rhh", height = "250px")),
                       column(4, plotOutput("p_arm_angle", height = "250px")),
                       column(4, plotOutput("p_traj_lhh", height = "250px"))
              ),
              # ===== TOP BAR directly under plots =====
              # === NEW/CHANGED === use gt table instead of HTML UI
              fluidRow(
                class = "plot-row",
                column(12, gt_output("topbar_table_ui"))
              ),
              # ===== Pitch-type table (styled like top bar) =====
              fluidRow(class = "plot-row",
                       column(12, gt_output("pitchtype_table_ui")))
    )
  )
)

server <- function(input, output, session) {
  showtext_auto(TRUE)
  
  observeEvent(input$generate_pitcher_pdf, {
    showModal(modalDialog(
      title = "Generating Pitcher PDF",
      "Please wait while the PDF is being generated...",
      footer = NULL
    ))
    
    pdf_filename <- paste0("CSUF_Pitcher_Reports_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    full_path <- file.path(getwd(), pdf_filename)
    
    tryCatch({
      result <- generate_pitcher_reports_pdf(
        dataset_with_angles = CSUF25_with_angles,
        output_filename = full_path,
        pitcher_team_filter = "CAL_FUL"  # <-- EXPLICITLY SET TO CAL_FUL
      )
      
      # ... rest of the handler remains the same
      
      removeModal()
      
      if (file.exists(full_path)) {
        showModal(modalDialog(
          title = "Success",
          HTML(paste0("PDF generated successfully!<br><br>",
                      "<strong>Filename:</strong> ", pdf_filename, "<br>",
                      "<strong>Location:</strong> ", getwd(), "<br><br>",
                      "<strong>Full path:</strong><br>", full_path, "<br><br>",
                      "<strong>File size:</strong> ",
                      format(file.size(full_path), big.mark = ","), " bytes")),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      } else {
        showModal(modalDialog(
          title = "Warning",
          HTML(paste0("PDF generation completed but file not found at expected location.<br><br>",
                      "<strong>Expected:</strong> ", full_path, "<br><br>",
                      "Check console output for details.")),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }
    }, error = function(e) {
      removeModal()
      showModal(modalDialog(
        title = "Error",
        paste("Failed to generate PDF:", e$message),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
  })
  
  
  output$date_ui <- renderUI({
    req(input$pitcher)
    ds <- dates_for(input$pitcher)
    selectInput("date", "Date", choices = as.character(ds),
                selected = if (length(ds)) as.character(max(ds)) else NULL)
  })
  
  output$p_sz_rhh <- renderPlot({
    req(input$pitcher, input$date)
    sz_RHH(input$pitcher, CSUF25_with_angles, input$date)
  })
  
  output$p_movement <- renderPlot({
    req(input$pitcher, input$date)
    movement_plot(input$pitcher, CSUF25_with_angles, input$date)
  })
  
  output$p_sz_lhh <- renderPlot({
    req(input$pitcher, input$date)
    sz_LHH(input$pitcher, CSUF25_with_angles, input$date)
  })
  
  output$p_traj_rhh <- renderPlot({
    req(input$pitcher, input$date)
    trajectories_avg_by_type_RHH(input$pitcher, CSUF25_with_angles, input$date)
  })
  
  output$p_arm_angle <- renderPlot({
    req(input$pitcher, input$date)
    pitcher_plot_arm_angle(CSUF25_with_angles, input$pitcher, input$date)
  })
  
  output$p_traj_lhh <- renderPlot({
    req(input$pitcher, input$date)
    trajectories_avg_by_type_LHH(input$pitcher, CSUF25_with_angles, input$date)
  })
  
  # === NEW/CHANGED === Top bar as gt (replaces renderUI("top_bar"))
  output$topbar_table_ui <- gt::render_gt({
    req(input$pitcher, input$date)
    tb_raw <- compute_top_bar_raw(CSUF25_with_angles, pitcher = input$pitcher, date_value = input$date)
    tb_pct <- apply_percentiles_to_topbar(tb_raw, percentile_ref)
    topbar_gt_table(tb_raw, tb_pct)
  })
  
  # ---------- Pitch-type table (gt) ----------
  output$pitchtype_table_ui <- render_gt({
    req(input$pitcher, input$date)
    tab <- make_pitchtype_report_table(CSUF25_with_angles, input$pitcher, input$date)
    
    if (nrow(tab) == 0) {
      return(gt(data.frame()) %>% tab_header(title = "No data available"))
    }
    
    cols <- c("Pitch Type","Pitches","Usage%","Velo","Max Velocity","IVB","HB",
              "Spin Direction","Release Height","Arm Angle","TopVAA","BotVAA",
              "Extension","Strike%","Whiff%","CSW%","Chase%","Stuff+")
    tab <- tab[, intersect(cols, names(tab)), drop = FALSE]
    
    gt(tab, rowname_col = NULL) |>
      tab_header(title = md("**Per-Pitch-Type Summary**")) |>
      cols_label(
        `Pitch Type` = "Pitch",
        `Max Velocity` = "Max Velo",
        `Spin Direction` = "Spin Dir",
        `Release Height` = "Rel H",
        `Arm Angle` = "Arm Ang",
        `TopVAA` = "Top VAA",
        `BotVAA` = "Bot VAA",
        `Stuff+` = "Stuff+"
      ) |>
      fmt_number(columns = c("Usage%","Velo","Max Velocity","IVB","HB",
                             "TopVAA","BotVAA","Strike%","Whiff%","CSW%","Chase%"),
                 decimals = 1) |>
      fmt_number(columns = c("Release Height","Extension"), decimals = 2) |>
      fmt_number(columns = c("Stuff+","Arm Angle"), decimals = 0) |>
      sub_missing(everything(), missing_text = "—") |>
      tab_options(
        table.font.names = "Markazi Text",
        table.font.size = px(14),
        table.width = pct(100),
        data_row.padding = px(4),
        heading.padding = px(6),
        column_labels.font.weight = "bold",
        column_labels.padding = px(6),
        table.border.top.width = px(2),
        table.border.top.color = "black",
        table.border.bottom.width = px(1),
        table.border.bottom.color = "black"
      ) |>
      opt_table_lines() |>
      opt_row_striping()
  })
} 

shinyApp(ui, server)
# ====================== END FULL SCRIPT ======================
