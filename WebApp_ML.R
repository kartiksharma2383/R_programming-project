library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(caret)
library(data.table)

# Load only necessary columns for visualization and ML
my_data <- fread("D:/R_Dataset/fraudTest.csv", select = c("amt", "city_pop", "lat", "long", "is_fraud"))

# Define UI for the app
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f8f9fa;
        color: #333;
        font-family: 'Arial', sans-serif;
      }
      .navbar {
        background-color: #007BFF;
        border-radius: 0;
        padding: 10px 20px;
      }
      .navbar .navbar-right {
        float: right;
      }
      .navbar .navbar-right a {
        color: #ffffff;
        font-weight: bold;
        text-decoration: none;
        margin-left: 15px;
      }
      .navbar .navbar-right a:hover {
        text-decoration: underline;
      }
      .auth-text {
        color: white;
        font-weight: bold;
        margin-right: 15px;
      }
    "))
  ),
  
  div(class = "navbar",
      div(class = "container",
          div(class = "navbar-right",
              span(class = "auth-text", "Register or Sign in"),
              a(href = "#", "Forgot Password?", style = "color: #FFD700; font-size: 14px; text-decoration: underline;")
          )
      )
  ),
  
  tabsetPanel(
    tabPanel("Visualizations",
             sidebarLayout(
               sidebarPanel(
                 selectInput("var", "Choose a variable:", choices = names(my_data)),
                 selectInput("plotType", "Select Plot Type:", choices = c("Histogram" = "hist", "Scatter Plot" = "scatter", "Box Plot" = "box")),
                 conditionalPanel(
                   condition = "input.plotType == 'scatter'",
                   selectInput("var2", "Choose a second variable for Scatter Plot:", choices = names(my_data))
                 )
               ),
               mainPanel(
                 plotOutput("myPlot"),
                 h3("Summary Statistics"),
                 verbatimTextOutput("summary")
               )
             )
    ),
    
    tabPanel("Dataset",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("columns", "Select Columns to Display:", choices = names(my_data), selected = names(my_data))
               ),
               mainPanel(
                 DT::dataTableOutput("dataTable")
               )
             )
    ),
    
    tabPanel("ML Model",
             sidebarLayout(
               sidebarPanel(
                 selectInput("targetVar", "Select Target Variable:", choices = names(my_data)),
                 checkboxGroupInput("predictors", "Select Predictor Variables:", choices = names(my_data)),
                 actionButton("trainModel", "Train Model")
               ),
               mainPanel(
                 h3("Model Summary"),
                 verbatimTextOutput("modelSummary")
               )
             )
    ),
    
    tabPanel("Sign In",
             sidebarLayout(
               sidebarPanel(
                 textInput("username", "Username"),
                 passwordInput("password", "Password"),
                 actionButton("loginBtn", "Sign In", class = "btn btn-primary"),
                 tags$hr(),
                 a(href = "#", "Forgot Password?", style = "color: #007BFF; text-decoration: underline;")
               ),
               mainPanel(
                 h3("Welcome Back!"),
                 verbatimTextOutput("loginStatus")
               )
             )
    ),
    
    tabPanel("Sign Up",
             sidebarLayout(
               sidebarPanel(
                 textInput("newUsername", "Username"),
                 passwordInput("newPassword", "Password"),
                 passwordInput("confirmPassword", "Confirm Password"),
                 actionButton("signupBtn", "Sign Up", class = "btn btn-primary")
               ),
               mainPanel(
                 h3("Create Your Account"),
                 verbatimTextOutput("signupStatus")
               )
             )
    ),
    
    tabPanel("Forgot Password",
             sidebarLayout(
               sidebarPanel(
                 textInput("email", "Enter your email to reset password:"),
                 actionButton("resetBtn", "Reset Password", class = "btn btn-primary")
               ),
               mainPanel(
                 h3("Password Reset"),
                 verbatimTextOutput("resetStatus")
               )
             )
    )
  )
)

# Define server logic for the app
server <- function(input, output, session) {
  # Placeholder user database
  userDatabase <- reactiveVal(data.frame(username = "admin", password = "1234", email = "admin@example.com"))
  
  # Login logic
  observeEvent(input$loginBtn, {
    req(input$username, input$password)
    users <- userDatabase()
    if (nrow(users[users$username == input$username & users$password == input$password, ]) > 0) {
      output$loginStatus <- renderText("Login Successful!")
    } else {
      output$loginStatus <- renderText("Invalid username or password.")
    }
  })
  
  # Sign-up logic
  observeEvent(input$signupBtn, {
    req(input$newUsername, input$newPassword, input$confirmPassword)
    if (input$newPassword != input$confirmPassword) {
      output$signupStatus <- renderText("Passwords do not match.")
    } else {
      users <- userDatabase()
      userDatabase(rbind(users, data.frame(username = input$newUsername, password = input$newPassword, email = NA)))
      output$signupStatus <- renderText("Account created successfully!")
    }
  })
  
  # Password reset logic
  observeEvent(input$resetBtn, {
    req(input$email)
    users <- userDatabase()
    if (input$email %in% users$email) {
      output$resetStatus <- renderText("A password reset link has been sent to your email.")
    } else {
      output$resetStatus <- renderText("Email not found in our records.")
    }
  })
  
  # Generate the selected plot
  output$myPlot <- renderPlot({
    if (input$plotType == "hist") {
      ggplot(my_data, aes_string(x = input$var)) +
        geom_histogram(bins = 30, fill = "#007BFF", color = "white") +
        theme_minimal() +
        labs(x = input$var, y = "Count", title = paste("Histogram of", input$var))
      
    } else if (input$plotType == "scatter") {
      ggplot(my_data, aes_string(x = input$var, y = input$var2)) +
        geom_point(color = "#007BFF") +
        theme_minimal() +
        labs(x = input$var, y = input$var2, title = paste("Scatter Plot of", input$var, "vs", input$var2))
      
    } else if (input$plotType == "box") {
      ggplot(my_data, aes_string(x = input$var)) +
        geom_boxplot(fill = "#FF5722", color = "black") +
        theme_minimal() +
        labs(x = input$var, y = "Value", title = paste("Box Plot of", input$var))
    }
  })
  
  # Generate summary statistics
  output$summary <- renderPrint({
    req(input$var)
    summary(my_data[[input$var]])
  })
  
  # Generate the data table with selected columns
  output$dataTable <- DT::renderDataTable({
    my_data %>%
      select(all_of(input$columns))
  })
  
  # Reactive event for training the model
  model <- eventReactive(input$trainModel, {
    req(input$targetVar, input$predictors)
    formula <- as.formula(paste(input$targetVar, "~", paste(input$predictors, collapse = "+")))
    train_data <- my_data[, c(input$targetVar, input$predictors), with = FALSE]
    train_data <- na.omit(train_data)
    train(formula, data = train_data, method = "glm", family = "binomial")
  })
  
  # Output model summary
  output$modelSummary <- renderPrint({
    req(model())
    summary(model())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
