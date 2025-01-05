library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# Load your dataset (replace 'your_data.csv' with your dataset file)
my_data <- read.csv("D:/R_Dataset/fraudTest.csv")
# Define UI for the app
ui <- fluidPage(
  titlePanel("Interactive Data Visualizations and Dataset Viewer"),
  
  # Apply custom styling using CSS
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f5f5f5; /* Light background */
        color: #333333; /* Dark text for readability */
      }
      .tabbable > .nav > li > a {
        color: #555555;
      }
      .tabbable > .nav > li[class=active] > a {
        background-color: #007BFF; /* Blue active tab */
        color: #ffffff;
      }
      .sidebar {
        background-color: #ffffff; /* White background for side panel */
        border: 1px solid #dddddd; /* Light gray border */
      }
      .sidebar .selectize-input {
        background-color: #e0f7fa; /* Soft blue input */
        color: #333333;
        border-color: #007BFF;
      }
      .btn-primary {
        background-color: #007BFF;
        border-color: #007BFF;
        color: #ffffff;
      }
      .btn-primary:hover {
        background-color: #0056b3;
      }
      h3, h4 {
        color: #333333;
      }
      table.dataTable thead th {
        background-color: #007BFF; /* Table header background */
        color: #ffffff; /* Header text color */
      }
      table.dataTable tbody tr:nth-child(even) {
        background-color: #f2f2f2; /* Light alternating row */
      }
      table.dataTable tbody tr:nth-child(odd) {
        background-color: #ffffff; /* White alternating row */
      }
    "))
  ),
  
  # Create tab layout
  tabsetPanel(
    # Tab for displaying plots
    tabPanel("Visualizations",
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
                 # Display the plot directly below the selected fields
                 plotOutput("myPlot"),
                 # Display the summary section below the plot
                 h3("Summary Statistics"),
                 verbatimTextOutput("summary")
               )
             )
    ),
    
    # Tab for displaying the dataset
    tabPanel("Dataset",
             sidebarLayout(
               sidebarPanel(
                 # Select columns to display
                 checkboxGroupInput("columns", "Select Columns to Display:",
                                    choices = names(my_data),
                                    selected = names(my_data))
               ),
               mainPanel(
                 # Display the data table below the selected checkboxes
                 DT::dataTableOutput("dataTable")
               )
             )
    )
  )
)

# Define Server logic for the app
server <- function(input, output) {
  
  # Generate the selected plot
  output$myPlot <- renderPlot({
    # Generate the plot based on user selections
    if (input$plotType == "hist") {
      # Histogram
      ggplot(my_data, aes_string(x = input$var)) +
        geom_histogram(bins = 30, fill = "#007BFF", color = "white") +
        theme_minimal() +
        labs(x = input$var, y = "Count", title = paste("Histogram of", input$var))
      
    } else if (input$plotType == "scatter") {
      # Scatter Plot
      ggplot(my_data, aes_string(x = input$var, y = input$var2)) +
        geom_point(color = "#007BFF") +
        theme_minimal() +
        labs(x = input$var, y = input$var2, title = paste("Scatter Plot of", input$var, "vs", input$var2))
      
    } else if (input$plotType == "box") {
      # Box Plot
      ggplot(my_data, aes_string(x = input$var)) +
        geom_boxplot(fill = "#FF5722", color = "black") +
        theme_minimal() +
        labs(x = input$var, y = "Value", title = paste("Box Plot of", input$var))
    }
  })
  
  # Generate the summary statistics based on selected variables
  output$summary <- renderPrint({
    req(input$var)  # Ensure a variable is selected
    
    if (input$plotType == "scatter" && !is.null(input$var2)) {
      # If scatter plot, show summary for both selected variables
      summary(my_data[, c(input$var, input$var2)])
    } else {
      # Otherwise, show summary for the single selected variable
      summary(my_data[[input$var]])
    }
  })
  
  # Generate the data table with selected columns
  output$dataTable <- DT::renderDataTable({
    my_data %>%
      select(all_of(input$columns))  # Select only the chosen columns
  })
}

# Run the application
shinyApp(ui = ui, server = server) 