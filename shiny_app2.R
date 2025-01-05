# Install required packages if not already installed
if (!requireNamespace("shinyjs", quietly = TRUE)) install.packages("shinyjs")
if (!requireNamespace("plotly", quietly = TRUE)) install.packages("plotly")
if (!requireNamespace("shinythemes", quietly = TRUE)) install.packages("shinythemes")
if (!requireNamespace("gargle", quietly = TRUE)) install.packages("gargle")
if (!requireNamespace("googleAuthR", quietly = TRUE)) install.packages("googleAuthR")
if (!requireNamespace("DBI", quietly = TRUE)) install.packages("DBI")
if (!requireNamespace("RSQLite", quietly = TRUE)) install.packages("RSQLite")
if (!requireNamespace("sodium", quietly = TRUE)) install.packages("sodium")

library(DBI)
library(RSQLite)
library(sodium)
library(shiny)
library(shinythemes)
library(plotly)
library(shinyjs)
library(gargle)
library(googleAuthR)
library(DT)

# Create or connect to SQLite database
db <- dbConnect(SQLite(), "users.db")

# Create a 'users' table if it doesn't exist
dbExecute(db, "
  CREATE TABLE IF NOT EXISTS users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    username TEXT UNIQUE NOT NULL,
    password_hash TEXT NOT NULL
  )
")

# Insert test users (run only once; comment after first run)
test_password_hash <- sodium::password_store("password123")
dbExecute(db, "INSERT OR IGNORE INTO users (username, password_hash) VALUES ('admin', ?)", list(test_password_hash))

test_password_hash <- sodium::password_store("@123")
dbExecute(db, "INSERT OR IGNORE INTO users (username, password_hash) VALUES ('kartik', ?)", list(test_password_hash))

# Close the database connection
dbDisconnect(db)

# Replace with your Google API credentials
options(
  gargle_oauth_client = gargle::gargle_oauth_client(
    id = "YOUR_GOOGLE_CLIENT_ID",
    secret = "YOUR_GOOGLE_CLIENT_SECRET"
  )
)
options(gargle_oauth_email = TRUE)
options(gargle_oauth_cache = ".secrets")
unlink(".secrets", recursive = TRUE, force = TRUE)

# Load the dataset
data <- read.csv("D:\\R_Dataset\\fraudtest.csv")

# Define the UI
ui <- navbarPage(
  id = "navbarPage",
  title = div("Fraud Data Analysis"),
  
  
  # Login Page
  tabPanel(
    "Login",
    fluidPage(
      useShinyjs(),
      titlePanel(tags$h2("Login to Fraud Data Analytics", style = "text-align: center;")),
      fluidRow(
        column(
          width = 4, offset = 4,
          wellPanel(
            style = "background-color: #444; border-radius: 10px;",
            textInput("username", "Username", placeholder = "Enter your username", width = "100%"),
            passwordInput("password", "Password", placeholder = "Enter your password", width = "100%"),
            div(
              style = "text-align: center;",
              actionButton("login_btn", "Login", class = "btn btn-success"),
              br(), br(),
              h4("Or", style = "color: #f4f4f4; text-align: center;"),
              actionButton("google_login_btn", "Login with Google", class = "btn btn-info"),
              br(), br(),
              actionButton("logout_btn", "Logout from Google", class = "btn btn-danger")
            )
          )
        )
      )
    )
  ),
  
  
  # Welcome Page
  tabPanel(
    "Welcome",
    fluidPage(
      titlePanel("Welcome to Fraud Detection Dashboard"),
      p("Fraud detection is a critical aspect of modern data analytics, particularly in financial and transactional domains. This tool empowers users to analyze and visualize fraudulent activities effectively."),
      tags$ul(
        tags$li("Detect fraudulent patterns in data."),
        tags$li("Use interactive visualizations for deep insights."),
        tags$li("Generate detailed summaries for further analysis.")
      ),
      p("Navigate through the tabs to explore various functionalities!")
    )
  ),

  
  # Filtered Data Page
  tabPanel(
    "Filtered Data",
    fluidPage(
      h3("Filter Data and View Summary"),
      selectInput("category_filter", "Select Category:", choices = unique(data$fraud_category), multiple = TRUE),
      DTOutput("filtered_data"),
      verbatimTextOutput("data_summary")
    )
  ),
  
  # Advanced Analysis Page
  tabPanel(
    "Advanced Analysis",
    fluidPage(
      h3("3D Visualization and Analysis"),
      plotlyOutput("advanced_plot"),
      p("Explore fraudulent data in an interactive 3D view for better understanding of patterns.")
    )
  ),
  
  # About Page
  tabPanel(
    "About",
    fluidPage(
      h3("About This Project"),
      p("This fraud detection dashboard was developed by Kartik Sharma. It leverages modern tools and technologies to deliver actionable insights."),
      tags$ul(
        tags$li("User-friendly interface."),
        tags$li("Robust authentication system."),
        tags$li("Interactive data visualizations.")
      )
    )
  )
)

server <- function(input, output, session) {
  # Reconnect to the database
  db <- dbConnect(SQLite(), "users.db")
  
  # User state management
  user <- reactiveValues(authenticated = FALSE, info = NULL)
  
  # Handle Username/Password Login
  observeEvent(input$login_btn, {
    user_data <- dbGetQuery(db, "SELECT * FROM users WHERE username = ?", params = list(input$username))
    if (nrow(user_data) == 1 && sodium::password_verify(user_data$password_hash, input$password)) {
      user$authenticated <- TRUE
      user$info <- list(name = input$username, email = paste0(input$username, "@example.com"))
      showNotification("Login successful!", type = "message")
    } else {
      showNotification("Invalid username or password", type = "error")
    }
  })
  
  # Handle Google Login
  observeEvent(input$google_login_btn, {
    token <- tryCatch(
      gargle::token_fetch(scopes = c("https://www.googleapis.com/auth/drive")),
      error = function(e) NULL
    )
    if (!is.null(token)) {
      user$authenticated <- TRUE
      user$info <- list(name = "Google User", email = token$email)
      showNotification("Google Authentication Successful!", type = "message")
      updateTabsetPanel(session, "navbarPage", selected = "Dashboard")
    } else {
      showNotification("Google Authentication Failed", type = "error")
    }
  })
  
  # Handle Logout
  observeEvent(input$logout_btn, {
    user$authenticated <- FALSE
    user$info <- NULL
    showNotification("Logged out successfully!", type = "message")
    updateTabsetPanel(session, "navbarPage", selected = "Login")
  })
  
  # Filtered Data Output
  output$filtered_data <- renderDT({
    req(user$authenticated)
    if (is.null(input$category_filter)) {
      data
    } else {
      data[data$fraud_category %in% input$category_filter, ]
    }
  })
  
  # Data Summary Output
  output$data_summary <- renderPrint({
    req(user$authenticated)
    summary(data)
  })
  
  # Advanced Analysis 3D Plot
  output$advanced_plot <- renderPlotly({
    req(user$authenticated)
    plot_ly(data, x = ~fraud_category, y = ~total_amount, z = ~frequency,
            type = "scatter3d", mode = "markers", color = ~fraud_category) %>%
      layout(title = "3D Fraud Data Visualization")
  })
}




# Run the Application
shinyApp(ui = ui, server = server)
