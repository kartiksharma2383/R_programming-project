library(shiny)
library(randomForest)
library(caret)
library(ggplot2)

# Define UI for the app
ui <- fluidPage(
  titlePanel("Fraud Detection with Random Forest"),
  sidebarLayout(
    sidebarPanel(
      h4("Model Training"),
      actionButton("trainModel", "Train Model"),
      br(),
      h4("Predictions"),
      actionButton("predict", "Predict on Test Data")
    ),
    mainPanel(
      h4("Model Summary"),
      verbatimTextOutput("modelSummary"),
      h4("Feature Importance"),
      plotOutput("featureImportance"),
      h4("Prediction Results"),
      plotOutput("predictionPlot"),
      h4("Confusion Matrix"),
      verbatimTextOutput("confusionMatrix")
    )
  )
)

# Define Server logic for the app
server <- function(input, output, session) {
  
  # Reactive values to store data, model, predictions, and confusion matrix
  values <- reactiveValues(data = NULL, model = NULL, predictions = NULL, cm = NULL)
  
  # Load and prepare data
  observe({
    file_path <- "D:/R_Dataset/fraudTest.csv"  # Ensure the file path uses forward slashes
    if (file.exists(file_path)) {
      data <- read.csv(file_path)
      required_columns <- c("is_fraud", "amt", "state", "city_pop")
      if (all(required_columns %in% colnames(data))) {
        # Select and preprocess the required columns
        data <- data[required_columns]
        data$state <- as.factor(data$state)  # Ensure categorical variables are factors
        data$is_fraud <- as.factor(data$is_fraud)  # Target variable as a factor
        data <- na.omit(data)  # Remove rows with missing values
        values$data <- data
        message("Data loaded successfully.")
      } else {
        stop("The dataset does not contain the required columns!")
      }
    } else {
      stop("Dataset not found. Please check the file path!")
    }
  })
  
  # Train the RandomForest model
  observeEvent(input$trainModel, {
    if (!is.null(values$data)) {
      data <- values$data
      set.seed(123)
      trainIndex <- sample(1:nrow(data), 0.8 * nrow(data))
      train_data <- data[trainIndex, ]
      test_data <- data[-trainIndex, ]
      
      # Train the Random Forest model
      rf_model <- randomForest(is_fraud ~ amt + state + city_pop, data = train_data, ntree = 100, importance = TRUE)
      values$model <- rf_model
      values$test_data <- test_data
      message("Model trained successfully.")
    } else {
      stop("Data is not loaded properly!")
    }
  })
  
  # Predict on test data
  observeEvent(input$predict, {
    if (!is.null(values$model) && !is.null(values$test_data)) {
      test_data <- values$test_data
      predictions <- predict(values$model, newdata = test_data, type = "response")
      values$predictions <- data.frame(Actual = test_data$is_fraud, Predicted = predictions)
      values$cm <- confusionMatrix(factor(predictions), factor(test_data$is_fraud))
    } else {
      stop("Model is not trained or test data is unavailable!")
    }
  })
  
  # Display model summary
  output$modelSummary <- renderPrint({
    if (is.null(values$model)) {
      "Model not yet trained."
    } else {
      values$model
    }
  })
  
  # Display feature importance
  output$featureImportance <- renderPlot({
    if (is.null(values$model)) return(NULL)
    importance <- as.data.frame(importance(values$model))
    importance$Feature <- rownames(importance)
    ggplot(importance, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      coord_flip() +
      labs(title = "Feature Importance", x = "Features", y = "Importance (Mean Decrease Gini)") +
      theme_minimal()
  })
  
  # Display predictions
  output$predictionPlot <- renderPlot({
    if (is.null(values$predictions)) return(NULL)
    ggplot(values$predictions, aes(x = Actual, fill = as.factor(Predicted))) +
      geom_bar(position = "dodge") +
      labs(title = "Actual vs Predicted Fraud Cases",
           x = "Actual Fraud Cases",
           fill = "Predicted Fraud") +
      theme_minimal()
  })
  
  # Display confusion matrix
  output$confusionMatrix <- renderPrint({
    if (is.null(values$cm)) {
      "No predictions made yet." 
    } else {
      values$cm
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
