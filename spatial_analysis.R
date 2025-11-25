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
