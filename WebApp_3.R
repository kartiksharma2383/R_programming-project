# Load necessary libraries
library(shiny)
library(ggplot2)
library(randomForest)
library(caret)
library(DT)  # Add DT for the data table output

# Load your dataset (make sure to replace the path with your file path)
my_data <- read.csv("D:/R_Dataset/fraudTest.csv")

# Preprocess the data
my_data$trans_date_trans_time <- as.POSIXct(my_data$trans_date_trans_time, format="%d-%m-%Y %H:%M")

# Define UI for the app 
ui <- fluidPage(
  titlePanel("Interactive Data Visualizations, Dataset Viewer, and Simple Machine Learning Model"),
  
  tags$head(
    tags$style(HTML("
      body { background-color: #f5f5f5; color: #333333; }
      .tabbable > .nav > li > a { color: #333333; }
      .tab-content { background-color: #ffffff; padding: 15px; }
    "))
  ),
  
  navbarPage("Main Menu",
             tabPanel("Data Viewer",
                      sidebarLayout(
                        sidebarPanel(
                          h3("Data Overview"),
                          p("Explore the dataset interactively."),
                          selectInput("column_select", "Choose Column to View:", 
                                      choices = colnames(my_data), selected = "amt")
                        ),
                        mainPanel(
                          DTOutput("data_table")  # Fixed: DTOutput now works with DT package
                        )
                      )),
             
             tabPanel("Visualizations",
                      sidebarLayout(
                        sidebarPanel(
                          h3("Data Visualization"),
                          selectInput("x_var", "X-Axis:", choices = colnames(my_data), selected = "amt"),
                          selectInput("y_var", "Y-Axis:", choices = colnames(my_data), selected = "lat"),
                          selectInput("plot_type", "Plot Type:", choices = c("Scatter", "Histogram"), selected = "Scatter")
                        ),
                        mainPanel(
                          plotOutput("data_plot")
                        )
                      )),
             
             tabPanel("Machine Learning Model",
                      sidebarLayout(
                        sidebarPanel(
                          h3("Predict Fraud"),
                          selectInput("model_feature", "Choose Feature to Train Model:", 
                                      choices = colnames(my_data), selected = "amt"),
                          actionButton("train_model", "Train Model")
                        ),
                        mainPanel(
                          textOutput("model_results")
                        )
                      ))
  )
)

# Define server logic for the app
server <- function(input, output, session) {
  
  # Render Data Table
  output$data_table <- renderDT({
    datatable(my_data)
  })
  
  # Render Plot based on user input
  output$data_plot <- renderPlot({
    if (input$plot_type == "Scatter") {
      ggplot(my_data, aes_string(x = input$x_var, y = input$y_var)) +
        geom_point() +
        labs(title = "Scatter Plot", x = input$x_var, y = input$y_var)
    } else {
      ggplot(my_data, aes_string(x = input$x_var)) +
        geom_histogram(binwidth = 1) +
        labs(title = "Histogram", x = input$x_var, y = "Frequency")
    }
  })
  
  # Train a simple ML model when button is clicked
  observeEvent(input$train_model, {
    # Prepare data for training
    train_data <- my_data %>%
      select(c(input$model_feature, "is_fraud")) %>%
      drop_na()  # Remove rows with NA values
    
    # Train a model (logistic regression in this case)
    model <- train(is_fraud ~ ., data = train_data, method = "glm", family = "binomial")
    
    # Display model results
    output$model_results <- renderText({
      paste("Model Accuracy: ", round(model$results$Accuracy, 4))
    })
  })
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
