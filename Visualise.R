# Load necessary packages
pacman::p_load(ggplot2, dplyr, sf, tidyr)

# Convert data to long format for monthly heatmap (First 10 Districts)
data_long <- data %>%
  slice(1:10) %>%  # Subset first 10 rows
  pivot_longer(
    cols = matches("jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec"),
    names_to = "month",
    values_to = "value"
  ) %>%
  mutate(
    month = str_to_title(tolower(month)),  # Ensure proper month formatting
    district = as.factor(district)  # Ensure district is categorical
  ) %>%
  drop_na()  # Remove any rows with missing values

# Correct month order
data_long$month <- factor(data_long$month, 
                          levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Heatmap function
heatmap_plot <- function(dataset, x_col, y_col, fill_col) {
  ggplot(dataset, aes(x = .data[[x_col]], y = .data[[y_col]], fill = .data[[fill_col]])) +
    geom_tile() +
    scale_fill_gradient(low = "blue", high = "red") +  # Color gradient for heatmap
    labs(title = "Monthly Rainfall Heatmap (First 10 Districts)", 
         x = "Month", 
         y = "District", 
         fill = "Rainfall (mm)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
}

# Generate and display heatmap
heatmap <- heatmap_plot(data_long, "month", "district", "value")  
print(heatmap)


# Annual rainfall bar chart
ggplot(data, aes(x = reorder(state_ut_name, annual, sum), y = annual, fill = state_ut_name)) +
  geom_bar(stat = "identity") +
  coord_flip() +57
  labs(title = "Annual Rainfall by State/UT", x = "State/UT", y = "Rainfall (mm)")

# Monthly rainfall trends line chart
ggplot(data_long, aes(x = factor(month, levels = month.abb), y = value, color = state_ut_name)) +
  geom_line() +
  labs(title = "Monthly Rainfall Trends", x = "Month", y = "Rainfall (mm)")

# Save plots to file
ggsave("heatmap_plot.png", width = 8, height = 5)
ggsave("rainfall_plot.png", width = 8, height = 5)
ggsave("rainfall_plot_2.png", width = 8, height = 5)
