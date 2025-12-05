# ==============================================================================
# Project: US Data Center Temporal Trend Prediction & Spatial Analysis
# Author: Akash / Research Analyst
# Goal: Predict future MW capacity expansion based on historical announcement/activation data.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Setup and Library Loading
# ------------------------------------------------------------------------------
# We utilize the 'tidyverse' for data manipulation and 'lubridate' for robust date handling.
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("lubridate")) install.packages("lubridate")
if (!require("scales")) install.packages("scales")

library(tidyverse)
library(lubridate)
library(scales)

# ------------------------------------------------------------------------------
# 2. Data Ingestion
# ------------------------------------------------------------------------------
# Load the dataset. Ensure the file is in the working directory or provide full path.
# Source ID: 
file_name <- "data_center_inventory_sample_sep_2025_with_Balancing_Authorities.csv"

# Read CSV with explicit column types to prevent parsing errors on mixed fields
dc_data <- read_csv(file_name, show_col_types = FALSE)

# ------------------------------------------------------------------------------
# 3. Data Cleaning and Preprocessing
# ------------------------------------------------------------------------------
# We focus on two key dates: 'DATA_CENTER_ANNOUNCED_DATE' and 'DATA_CENTER_ACTIVATION_DATE'.
# We also need to consolidate Power Capacity (MW) which may be in different columns.

clean_dc <- dc_data %>%
  mutate(
    # Parse dates using mdy() as the format in snippet appears to be MM/DD/YYYY
    Announced_Date = mdy(DATA_CENTER_ANNOUNCED_DATE),
    Activation_Date = mdy(DATA_CENTER_ACTIVATION_DATE),
    
    # Extract Year for aggregation
    Announced_Year = year(Announced_Date),
    Activation_Year = year(Activation_Date),
    
    # Consolidate Power Capacity: Use 'SELECTED_POWER_CAPACITY_MW' if available,
    # otherwise fallback to 'ATERIO_EST_TOT_POWER_CAPACITY_MW'
    Capacity_MW = coalesce(SELECTED_POWER_CAPACITY_MW, ATERIO_EST_TOT_POWER_CAPACITY_MW)
  ) %>%
  # Filter out records where capacity or dates are missing for the trend analysis
  filter(!is.na(Capacity_MW)) %>%
  # Remove unrealistic historical dates (e.g., pre-2000) or far-future errors if any
  filter(Activation_Year >= 2010 & Activation_Year <= 2030)

# ------------------------------------------------------------------------------
# 4. Exploratory Data Analysis (EDA) - Aggregation
# ------------------------------------------------------------------------------
# Aggregate total MW coming online per year
annual_trends <- clean_dc %>%
  group_by(Activation_Year) %>%
  summarise(
    Total_MW_Added = sum(Capacity_MW, na.rm = TRUE),
    Project_Count = n()
  ) %>%
  ungroup()

print("Historical Annual Trends:")
print(annual_trends)

# ------------------------------------------------------------------------------
# 5. Statistical Modeling: Linear Regression
# ------------------------------------------------------------------------------
# We hypothesize a linear or exponential growth in power demand.
# Model: Total_MW_Added = Intercept + Beta * Year

# Filter for completed years to avoid partial year bias in the model (e.g., up to 2024/2025)
# Using data up to 2025 as the dataset includes forward-looking estimates/activations.
model_data <- annual_trends %>%
  filter(Activation_Year <= 2025)

lm_model <- lm(Total_MW_Added ~ Activation_Year, data = model_data)

# Output Model Summary
print("Linear Regression Model Summary:")
summary(lm_model)

# ------------------------------------------------------------------------------
# 6. Forecasting
# ------------------------------------------------------------------------------
# Predict for the next 3 years (2026, 2027, 2028)
future_years <- data.frame(Activation_Year = c(2026, 2027, 2028))

# Generate predictions with confidence intervals
predictions <- predict(lm_model, newdata = future_years, interval = "confidence")

# Combine predictions with the years
forecast <- cbind(future_years, predictions) %>%
  rename(Predicted_MW = fit, Lower_CI = lwr, Upper_CI = upr)

print("Forecasted Expansion (MW) for 2026-2028:")
print(forecast)

# ------------------------------------------------------------------------------
# 7. Visualization
# ------------------------------------------------------------------------------
# Combine historical and forecast data for plotting
plot_data <- bind_rows(
  annual_trends %>% mutate(Type = "Historical"),
  forecast %>% rename(Total_MW_Added = Predicted_MW) %>% mutate(Type = "Forecast")
)

ggplot(plot_data, aes(x = Activation_Year, y = Total_MW_Added, color = Type)) +
  geom_point(size = 3) +
  geom_line() +
  geom_ribbon(data = subset(plot_data, Type == "Forecast"), 
              aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, linetype = 0) +
  labs(
    title = "US Data Center Capacity Expansion Trend (2010-2028)",
    subtitle = "Historical Activation Data & Linear Regression Forecast",
    x = "Year",
    y = "Total Power Capacity Added (MW)",
    caption = "Source: Aterio Data Center Inventory"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "bottom")

# End of Script
