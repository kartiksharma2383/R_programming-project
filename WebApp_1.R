library(shiny)
library(ggplot2)
library(dplyr)
library(jsonlite)

# Load your dataset (replace 'your_data.csv' with your dataset file)
my_data <- read.csv("D://R_Dataset//fraudTest.csv")

# Define UI for the app
ui <- fluidPage( 
  titlePanel("Interactive Data Visualizations with Shiny"),
  sidebarLayout( 
    sidebarPanel( 
      # Input for selecting variable to visualize
      selectInput("var", "Choose a variable:", 
                  choices = names(my_data)),
      # Input for selecting type of plot
      selectInput("plotType", "Select Plot Type:",
                  choices = c("Histogram" = "hist",
                              "Scatter Plot" = "scatter",
                              "Box Plot" = "box")),
      # Optional: Add a second variable input if needed for scatter plots
      conditionalPanel(
        condition = "input.plotType == 'scatter'", 
        selectInput("var2", "Choose a second variable for Scatter Plot:", 
                    choices = names(my_data)) 
      )
    ),
    mainPanel(
      # Display the plot
      plotOutput("myPlot")
    )
  )
)

# Define Server logic for the app
server <- function(input, output) {
  output$myPlot <- renderPlot({
    # Subset data to handle missing values and avoid issues in plotting
    data <- my_data %>%
      filter(!is.na(!!sym(input$var)))  # Remove missing values for the selected variable
    
    if (input$plotType == "hist") {
      # Histogram
      ggplot(data, aes_string(x = input$var)) +
        geom_histogram(bins = 30, fill = "skyblue", color = "black") +
        theme_minimal() +
        labs(x = input$var, y = "Count", title = paste("Histogram of", input$var))
      
    } else if (input$plotType == "scatter") {
      # Ensure the second variable is selected
      req(input$var2)
      data <- data %>% filter(!is.na(!!sym(input$var2)))  # Remove missing values for second variable
      
      # Scatter Plot
      ggplot(data, aes_string(x = input$var, y = input$var2)) +
        geom_point(color = "darkblue") +
        theme_minimal() +
        labs(x = input$var, y = input$var2, title = paste("Scatter Plot of", input$var, "vs", input$var2))
      
    } else if (input$plotType == "box") {
      # Box Plot
      ggplot(data, aes_string(x = input$var)) +
        geom_boxplot(fill = "coral", color = "black") +
        theme_minimal() +
        labs(x = input$var, y = "Value", title = paste("Box Plot of", input$var))
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server) 