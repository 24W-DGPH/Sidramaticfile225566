# Load additional libraries using pacman
pacman::p_load(
  tidyverse,   # Core data science packages (ggplot2, dplyr, tidyr, etc.)
  data.table,  # Efficient data manipulation
  lubridate,   # Date/time handling
  stringr,     # String manipulation
  readxl,      # Excel file handling
  openxlsx,    # Alternative Excel file handling
  plotly,      # Interactive visualizations
  shiny,       # Web apps in R
  caret,       # Machine learning tools
  janitor,     # Cleaning tool
  rio,         # For importing/exporting files
  here,         # File path handling
  dplyr,        # Functions for data manipulation
  sf,          # working with
  writexl,      # exports as an excel sheet 
  here          # imports files     
)

# Import dataset
data <- read_excel(here::here("dataset.xlsx"))

# View column names
colnames(data)

# View summary of the DISTRICT column
summary(data$DISTRICT)

# Clean column names
data <- data %>%
  janitor::clean_names() 

# Select specific columns
data %>%
  select(jan, feb, mar, apr, may, jun) # Ensure these columns exist

# Add a calculated column for rainfall and capitalize district/state names
data <- data %>%
  mutate(
    rainfall_cm = annual / 10, # Ensure `annual` column exists
  )