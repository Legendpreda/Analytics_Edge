# Geospatial Exploratory Data Analysis
# Plot TOTAL_POWER_MW by state

# Load required libraries
library(tidyverse)
library(sf)
library(maps)
library(ggplot2)
library(dplyr)
library(tigris)
library(spdep)


# Load both CSV files from data directory
cat("Loading data files...\n")
data_centers <- read_csv("data/data_center_inventory_sample_sep_2025_with_Balancing_Authorities.csv")
state_data <- read_csv("data/State_data.csv", skip=1)

# Display basic information about the datasets
cat("\n=== Data Center Inventory Columns ===\n")
print(colnames(data_centers))
cat("\n=== State Data Columns ===\n")
print(colnames(state_data))

# Find state and power columns (case-insensitive search)
state_col <- grep("^state$|^STATE$", colnames(data_centers), ignore.case = TRUE, value = TRUE)[1]
power_col <- grep("TOTAL_POWER_MW|total_power_mw|Total_Power_MW", colnames(data_centers), ignore.case = FALSE, value = TRUE)[1]

# If not found, try broader search
if (is.na(state_col)) {
  state_col <- grep("state", colnames(data_centers), ignore.case = TRUE, value = TRUE)[1]
}
if (is.na(power_col)) {
  power_col <- grep("power|POWER", colnames(data_centers), ignore.case = TRUE, value = TRUE)[1]
}

cat("\nUsing state column:", state_col, "\n")
cat("Using power column:", power_col, "\n")

if (is.na(state_col) || is.na(power_col)) {
  stop("Could not find required columns. Please check column names in the data.")
}

# Aggregate total power by state
state_power <- data_centers %>%
  group_by(!!sym(state_col)) %>%
  summarise(
    TOTAL_POWER_MW = sum(!!sym(power_col), na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  rename(STATE = !!sym(state_col)) %>%
  filter(!is.na(STATE), STATE != "")

cat("\n=== State Power Summary ===\n")
print(state_power)

# Get US state boundaries using maps package
usa_states <- map_data("state")

# Prepare state names for joining (convert to lowercase)
state_power_map <- state_power %>%
  mutate(
    STATE = trimws(STATE),
    region = tolower(
      ifelse(
        nchar(STATE) == 2,
        state.name[match(toupper(STATE), state.abb)],
        STATE
      )
    )
  )



# Join power data with state boundaries
state_map_data <- usa_states %>%
  left_join(state_power_map, by = "region")

# Create flag for no-data states
state_map_data <- state_map_data %>%
  mutate(
    data_flag = ifelse(is.na(TOTAL_POWER_MW), "No data", "Data available")
  )

# Create the geospatial plot using ggplot2
cat("\nCreating geospatial plot...\n")
p1 <- ggplot(state_map_data, aes(x = long, y = lat, group = group, fill = TOTAL_POWER_MW, alpha = data_flag)) +
  geom_polygon(color = "white", linewidth = 0.1) +
  coord_fixed(1.3) +
  scale_fill_viridis_c(
    name = "Total Power\n(MW)", 
    option = "plasma", 
    na.value = "grey90",
    labels = scales::comma
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right"
  ) +
  labs(
    title = "Total Power Capacity by State",
    subtitle = "Data Center Inventory Analysis",
    caption = "Source: Data Center Inventory Sample"
  ) +
  # discrete legend entry for "No data"
  scale_alpha_manual(
    name = NULL,
    values = c("Data available" = 1, "No data" = 1),
    breaks = "No data",              # only show one key
    labels = "No data",
    guide = guide_legend(
      override.aes = list(
        fill = "grey80",             # grey box in legend
        color = "grey80"
      )
    )
  )

print(p1)

# Summary statistics
cat("\n=== Summary Statistics ===\n")
cat("\nTotal Power by State:\n")
print(summary(state_power$TOTAL_POWER_MW))
cat("\nTop 10 States by Power:\n")
print(state_power %>% arrange(desc(TOTAL_POWER_MW)) %>% head(10))
cat("\nStates with data:", nrow(state_power), "\n")
cat("Total Power (MW):", sum(state_power$TOTAL_POWER_MW, na.rm = TRUE), "\n")





##### Log-transform left skewed response variable

state_data_panel <- state_data %>%
  mutate(
    STATE_CODE = trimws(STATE_CODE),
    TOT_POWER_MW = suppressWarnings(as.numeric(TOT_POWER_MW)),
    log_TOT_POWER_MW = ifelse(
      !is.na(TOT_POWER_MW) & TOT_POWER_MW > 0,
      log(TOT_POWER_MW),
      NA_real_
    )
  )

# get US state polygons
options(tigris_use_cache = TRUE)

us_states <- states(cb = TRUE) %>%
  st_transform(4326) %>%
  filter(!STUSPS %in% c("PR", "VI", "GU", "AS", "MP")) %>%   # continental states only
  select(STATE_CODE = STUSPS, geometry)

# compute centroids in geographic coordinates
state_centroids <- st_centroid(us_states)

coords <- st_coordinates(state_centroids)

# KNN with k = 4
knn_obj <- knearneigh(coords, k = 4)

knn_nb <- knn2nb(knn_obj)

# convert to listw
listw_knn <- nb2listw(knn_nb, style = "W")

state_vals <- state_data_panel %>%
  group_by(STATE_CODE) %>%
  summarize(log_TOT_POWER_MW = mean(log_TOT_POWER_MW, na.rm = TRUE),
            .groups = "drop")

state_vals_ordered <- state_vals %>%
  slice(match(us_states$STATE_CODE, STATE_CODE))

aligned <- us_states %>%
  left_join(state_vals, by = "STATE_CODE") %>%
  arrange(STATE_CODE)

# Extract numeric vector
log_power_vec <- aligned$log_TOT_POWER_MW

moran_result <- moran.test(log_power_vec, listw_knn, na.action = na.exclude)
print(moran_result)

# ==============================================================================
# TEMPORAL TREND ANALYSIS - LINEAR REGRESSION
# ==============================================================================
# Based on Linear Regression_Akash.R workflow
# Goal: Predict future MW capacity expansion based on historical data

cat("\n\n=== TEMPORAL TREND ANALYSIS ===\n")

# Load additional libraries for temporal analysis
if (!require("lubridate")) install.packages("lubridate")
if (!require("scales")) install.packages("scales")
if (!require("glmnet")) install.packages("glmnet")
if (!require("plm")) install.packages("plm")
if (!require("splm")) install.packages("splm")

library(lubridate)
library(scales)
library(glmnet)
library(plm)
library(splm)

# ------------------------------------------------------------------------------
# DATA CLEANING AND PREPROCESSING (Temporal)
# ------------------------------------------------------------------------------

clean_dc <- data_centers %>%
  mutate(
    # Parse dates using mdy() as the format appears to be MM/DD/YYYY
    Announced_Date = mdy(DATA_CENTER_ANNOUNCED_DATE),
    Activation_Date = mdy(DATA_CENTER_ACTIVATION_DATE),
    
    # Extract Year for aggregation
    Announced_Year = year(Announced_Date),
    Activation_Year = year(Activation_Date),
    
    # Consolidate Power Capacity
    Capacity_MW = coalesce(SELECTED_POWER_CAPACITY_MW, ATERIO_EST_TOT_POWER_CAPACITY_MW)
  ) %>%
  # Filter out records where capacity or dates are missing
  filter(!is.na(Capacity_MW)) %>%
  # Remove unrealistic dates
  filter(Activation_Year >= 2010 & Activation_Year <= 2030)

# ------------------------------------------------------------------------------
# EXPLORATORY DATA ANALYSIS - AGGREGATION
# ------------------------------------------------------------------------------

annual_trends <- clean_dc %>%
  group_by(Activation_Year) %>%
  summarise(
    Total_MW_Added = sum(Capacity_MW, na.rm = TRUE),
    Project_Count = n()
  ) %>%
  ungroup()

cat("\nHistorical Annual Trends:\n")
print(annual_trends)

# ------------------------------------------------------------------------------
# BASIC LINEAR REGRESSION MODEL
# ------------------------------------------------------------------------------

# Filter for completed years (up to 2025)
model_data <- annual_trends %>%
  filter(Activation_Year <= 2025)

lm_model <- lm(Total_MW_Added ~ Activation_Year, data = model_data)

cat("\nLinear Regression Model Summary:\n")
print(summary(lm_model))

# ------------------------------------------------------------------------------
# FORECASTING
# ------------------------------------------------------------------------------

# Predict for the next 3 years
future_years <- data.frame(Activation_Year = c(2026, 2027, 2028))
predictions <- predict(lm_model, newdata = future_years, interval = "confidence")

forecast <- cbind(future_years, predictions) %>%
  rename(Predicted_MW = fit, Lower_CI = lwr, Upper_CI = upr)

cat("\nForecasted Expansion (MW) for 2026-2028:\n")
print(forecast)

# ------------------------------------------------------------------------------
# VISUALIZATION - TEMPORAL TREND
# ------------------------------------------------------------------------------

plot_data <- bind_rows(
  annual_trends %>% mutate(Type = "Historical"),
  forecast %>% rename(Total_MW_Added = Predicted_MW) %>% mutate(Type = "Forecast")
)

p_temporal <- ggplot(plot_data, aes(x = Activation_Year, y = Total_MW_Added, color = Type)) +
  geom_point(size = 3) +
  geom_line() +
  geom_ribbon(data = subset(plot_data, Type == "Forecast"), 
              aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, linetype = 0) +
  labs(
    title = "US Data Center Capacity Expansion Trend (2010-2028)",
    subtitle = "Historical Activation Data & Linear Regression Forecast",
    x = "Year",
    y = "Total Power Capacity Added (MW)",
    caption = "Source: Data Center Inventory"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "bottom")

print(p_temporal)

# ==============================================================================
# SPATIAL KNN VS BASIC LINEAR REGRESSION (USING clean_dc LAT/LON)
# Goal: Compare R² and OOS R² of a spatial KNN model to a simple linear trend.
# ==============================================================================

cat("\n\n=== SPATIAL KNN VS BASIC LINEAR REGRESSION (FACILITY-LEVEL) ===\n")

# Extra library for KNN regression
if (!require("FNN")) install.packages("FNN")
library(FNN)

# ------------------------------------------------------------------------------
# 1. Build a modeling dataset from clean_dc
#    Using facility-level observations with:
#      - response: Capacity_MW
#      - predictors: Activation_Year, LOCATION_LONGITUDE, LOCATION_LATITUDE
# ------------------------------------------------------------------------------

model_dc <- clean_dc %>%
  mutate(
    STATE      = trimws(STATE_NAME),
    STATE_CODE = ifelse(
      nchar(STATE) == 2,
      toupper(STATE),
      state.abb[match(STATE, state.name)]
    )
  ) %>%
  filter(
    !is.na(Capacity_MW),
    !is.na(Activation_Year),
    !is.na(LOCATION_LATITUDE),
    !is.na(LOCATION_LONGITUDE)
  ) %>%
  # keep only records with valid state codes (optional but usually helpful)
  filter(!is.na(STATE_CODE))

cat("Modeling rows (facility-level):", nrow(model_dc), "\n")

y <- model_dc$Capacity_MW
n <- length(y)

if (n < 30) {
  warning("Very few observations; cross-validation diagnostics may be unstable.")
}

# ------------------------------------------------------------------------------
# 2. Helper: R² function
# ------------------------------------------------------------------------------

rsq <- function(y, yhat) {
  1 - sum((y - yhat)^2, na.rm = TRUE) /
    sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
}

# ------------------------------------------------------------------------------
# 3. Baseline model: Linear regression (Capacity_MW ~ Activation_Year)
# ------------------------------------------------------------------------------

lm_facility <- lm(Capacity_MW ~ Activation_Year, data = model_dc)
r2_lm_in <- summary(lm_facility)$r.squared

# 5-fold cross-validated OOS R² for LM
set.seed(123)
k_folds <- min(5, n)          # handle small samples gracefully
fold_id <- sample(rep(1:k_folds, length.out = n))
model_dc$fold_id <- fold_id

cv_pred_lm <- rep(NA_real_, n)

for (k in 1:k_folds) {
  train <- model_dc[model_dc$fold_id != k, ]
  test  <- model_dc[model_dc$fold_id == k, ]
  
  m_k <- lm(Capacity_MW ~ Activation_Year, data = train)
  cv_pred_lm[model_dc$fold_id == k] <- predict(m_k, newdata = test)
}

r2_lm_oos <- rsq(y, cv_pred_lm)

# ------------------------------------------------------------------------------
# 4. Spatial KNN regression: predictors = (lon, lat, Activation_Year)
# ------------------------------------------------------------------------------

X <- model_dc %>%
  transmute(
    lon  = LOCATION_LONGITUDE,
    lat  = LOCATION_LATITUDE,
    year = Activation_Year
  ) %>%
  as.matrix()

# Standardize features
X_scaled <- scale(X)

k_neighbors <- 5   # you can tune this

# In-sample KNN R² (apparent fit)
knn_insample <- FNN::knn.reg(
  train = X_scaled,
  test  = X_scaled,
  y     = y,
  k     = k_neighbors
)
r2_knn_in <- rsq(y, knn_insample$pred)

# 5-fold cross-validated OOS R² for KNN
cv_pred_knn <- rep(NA_real_, n)

for (k in 1:k_folds) {
  train_idx <- which(model_dc$fold_id != k)
  test_idx  <- which(model_dc$fold_id == k)
  
  knn_k <- FNN::knn.reg(
    train = X_scaled[train_idx, , drop = FALSE],
    test  = X_scaled[test_idx,  , drop = FALSE],
    y     = y[train_idx],
    k     = k_neighbors
  )
  
  cv_pred_knn[test_idx] <- knn_k$pred
}

r2_knn_oos <- rsq(y, cv_pred_knn)

# ------------------------------------------------------------------------------
# 5. Side-by-side comparison
# ------------------------------------------------------------------------------

cat("\n=== MODEL COMPARISON (FACILITY-LEVEL) ===\n")
comparison_tbl <- tibble::tibble(
  Model        = c("Linear regression (Year only)",
                   "Spatial KNN (lon, lat, year)"),
  In_sample_R2 = c(r2_lm_in, r2_knn_in),
  OOS_R2_5fold = c(r2_lm_oos, r2_knn_oos)
)

print(comparison_tbl)

cat("\nInterpretation:\n")
cat(" - In_sample_R2: standard R² on the full dataset.\n")
cat(" - OOS_R2_5fold: cross-validated R² (approximate out-of-sample performance).\n")
cat("If Spatial KNN has higher OOS_R2_5fold than the linear model,\n")
cat("then exploiting (LOCATION_LONGITUDE, LOCATION_LATITUDE, year) is improving predictive power.\n")

# ==============================================================================
# SIDE-BY-SIDE RESIDUALS VS FITTED PLOTS (LM vs KNN)
# ==============================================================================

# OOS fitted values
fitted_lm_oos  <- cv_pred_lm
fitted_knn_oos <- cv_pred_knn

resid_lm_oos   <- y - fitted_lm_oos
resid_knn_oos  <- y - fitted_knn_oos

par(mfrow = c(1, 2))

# ----------------------------
# 1. Linear Regression Residual Plot
plot(
  fitted_lm_oos, resid_lm_oos,
  pch = 19, col = rgb(0, 0, 1, 0.5),
  xlab = "Fitted (OOS) - Linear Model",
  ylab = "Residuals",
  main = "Linear Regression\nResiduals vs Fitted"
)
abline(h = 0, col = "red", lwd = 2)

# ----------------------------
# 2. KNN Residual Plot
plot(
  fitted_knn_oos, resid_knn_oos,
  pch = 19, col = rgb(0, 0, 1, 0.5),
  xlab = "Fitted (OOS) - KNN",
  ylab = "Residuals",
  main = "Spatial KNN\nResiduals vs Fitted"
)
abline(h = 0, col = "red", lwd = 2)


fitted_lm_ins  <- fitted(lm_facility)
resid_lm_ins   <- residuals(lm_facility)

fitted_knn_ins <- knn_insample$pred
resid_knn_ins  <- y - fitted_knn_ins

par(mfrow = c(1, 2))

plot(
  fitted_lm_ins, resid_lm_ins,
  pch = 19, col = rgb(0, 0, 1, 0.5),
  xlab = "Fitted (in-sample) - LM",
  ylab = "Residuals",
  main = "LM Residuals vs Fitted"
)
abline(h = 0, col = "red", lwd = 2)

plot(
  fitted_knn_ins, resid_knn_ins,
  pch = 19, col = rgb(0, 0, 1, 0.5),
  xlab = "Fitted (in-sample) - KNN",
  ylab = "Residuals",
  main = "KNN Residuals vs Fitted"
)
abline(h = 0, col = "red", lwd = 2)

