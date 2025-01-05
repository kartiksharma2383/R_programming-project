library(shiny)
library(randomForest)
library(caret)
library(ggplot2)

# Define UI for the app 
ui <- fluidPage(
  titlePanel("Fraud Detection Model with Random Forest"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("trainModel", "Train and Predict"),
      h4("Model Summary"),
      verbatimTextOutput("modelSummary")
    ),
    mainPanel(
      plotOutput("plot"),
      h4("Model Accuracy (RMSE, Rsquared, MAE)"),
      verbatimTextOutput("modelAccuracy")
    )
  )
)

# Define Server logic for the app
server <- function(input, output, session) {
  
  # Path to the dataset
  dataset_path <- "D:/R_Dataset/fraudtest.csv"
  
  # Reactive values to store model, predictions, and accuracy
  values <- reactiveValues(model = NULL, predictions = NULL, accuracy = NULL, data_loaded = FALSE)
  
  # Load and prepare data when the app starts
  dataset <- reactive({
    # Load the CSV file
    data <- tryCatch(
      read.csv(dataset_path),
      error = function(e) {
        showNotification("Error loading dataset: Check the file path or file format.", type = "error")
        return(NULL)
      }
    )
    
    # Required columns
    required_columns <- c("amt", "city_pop", "lat", "long", "is_fraud")
    
    # Check if all required columns are present
    if (!all(required_columns %in% colnames(data))) {
      showNotification("Required columns are missing from the dataset!", type = "error")
      return(NULL)
    }
    
    # Remove rows with missing values and store data
    data <- na.omit(data[required_columns])
    values$data_loaded <- TRUE  # Indicate data is loaded successfully
    return(data)
  })
  
  # Train the model and make predictions when the button is clicked
  observeEvent(input$trainModel, {
    if (!values$data_loaded) {
      showNotification("Data not loaded properly. Check your dataset.", type = "error")
      return(NULL)
    }
    
    data <- dataset()
    
    # Split data into training and test sets
    set.seed(123)
    sample_index <- sample(1:nrow(data), 0.8 * nrow(data))
    train_data <- data[sample_index, ]
    test_data <- data[-sample_index, ]
    
    # Train the Random Forest model
    rf_model <- tryCatch({
      randomForest(is_fraud ~ amt + city_pop + lat + long, data = train_data, ntree = 100)
    }, error = function(e) {
      showNotification("Error in training the model.", type = "error")
      return(NULL)
    })
    
    # If model training fails, do not proceed
    if (is.null(rf_model)) return()
    
    values$model <- rf_model  # Store the model
    
    # Predict on the test data
    predictions <- predict(rf_model, newdata = test_data)
    values$predictions <- data.frame(Index = 1:length(predictions),
                                     Actual = test_data$is_fraud,
                                     Predicted = predictions)
    
    # Calculate model accuracy
    values$accuracy <- caret::postResample(pred = predictions, obs = test_data$is_fraud)
    
    # Notify user of successful training
    showNotification("Model trained successfully!", type = "message")
  })
  
  # Display model summary
  output$modelSummary <- renderPrint({
    if (is.null(values$model)) {
      "Model not yet trained."
    } else {
      values$model
    }
  })
  
  # Display plot of actual vs. predicted values
  output$plot <- renderPlot({
    if (is.null(values$predictions)) {
      plot.new()
      title("Train the model to view predictions")
      return()
    }
    
    ggplot(values$predictions, aes(x = Index)) +
      geom_line(aes(y = Actual, color = "Actual"), size = 1, alpha = 0.8) +
      geom_line(aes(y = Predicted, color = "Predicted"), size = 1, alpha = 0.8, linetype = "dashed") +
      labs(title = "Actual vs Predicted Values",
           x = "Index",
           y = "Values") +
      theme_minimal() +
      scale_color_manual(values = c("Actual" = "red", "Predicted" = "blue")) +
      theme(legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      geom_point(aes(y = Actual), color = "red", size = 1.5, alpha = 0.5) + 
      geom_point(aes(y = Predicted), color = "blue", size = 1.5, alpha = 0.5)
  })
  
  # Display model accuracy
  output$modelAccuracy <- renderPrint({
    if (is.null(values$accuracy)) {
      "Model accuracy not available yet."
    } else {
      values$accuracy
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server) 
