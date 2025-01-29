# Load necessary packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  shiny, dplyr, ggplot2, tidyverse, readxl, sf, janitor, 
  plotly, lubridate, stringr, writexl, rio, forcats
)

# Load dataset
data_path <- "updated_rainfall_data.xlsx"
rainfall_data <- rio::import(data_path) %>% 
  janitor::clean_names() 

# Convert data to long format for heatmap
data_long <- rainfall_data %>%
  pivot_longer(
    cols = matches("jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec"),
    names_to = "month",
    values_to = "value"
  ) %>%
  mutate(
    month = str_to_title(tolower(month)),  # Ensure proper month formatting
    district = as.factor(district)  # Ensure district is categorical
  ) %>%
  drop_na()

# Correct month order
data_long$month <- factor(data_long$month, 
                          levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Heatmap function
heatmap_plot <- function(dataset) {
  ggplot(dataset, aes(x = month, y = district, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low = "blue", high = "red") +
    labs(title = "Monthly Rainfall Heatmap", x = "Month", y = "District", fill = "Rainfall (mm)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Define UI for Shiny app
ui <- fluidPage(
  titlePanel("Rainfall Data Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("select_district", "Select District", choices = unique(rainfall_data$district), selected = unique(rainfall_data$district)[1]),
      sliderInput("select_month", "Select Month", min = 1, max = 12, value = c(1, 12), step = 1, ticks = TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Heatmap", plotOutput("heatmap")),
        tabPanel("Bar Chart", plotOutput("bar_chart"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Filter data based on user inputs
  filtered_data <- reactive({
    data_long %>%
      filter(district == input$select_district) %>%
      filter(as.numeric(match(month, month.abb)) >= input$select_month[1] & 
               as.numeric(match(month, month.abb)) <= input$select_month[2])
  })
  
  # Render heatmap
  output$heatmap <- renderPlot({
    heatmap_plot(filtered_data())
  })
  
  # Render bar chart of annual rainfall by state
  output$bar_chart <- renderPlot({
    ggplot(rainfall_data, aes(x = reorder(state_ut_name, annual, sum), y = annual, fill = state_ut_name)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Annual Rainfall by State/UT", x = "State/UT", y = "Rainfall (mm)") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui, server)
