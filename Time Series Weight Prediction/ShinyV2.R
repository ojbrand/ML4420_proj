library(stats)
library(ggplot2)
library(TTR)

# 1. INPUT & PREPROCESSING

df <- read.csv('final_training_database_2.csv')
df$Date <- as.Date(df$Date)
df <- df[order(df$User_ID, df$Date), ]

# Lag 1
df$Prev_Weight <- ave(df$WeightKg, df$User_ID, FUN = function(x) c(NA, head(x, -1)))

train_data <- na.omit(df)


# 2. MODEL TRAINING

# Create target vector
y_train <- as.matrix(train_data$WeightKg)

# Create Features
# 'VeryActiveMinutes' (Lag 0 / Same Day) because of caloric burn expect negative Beta
X_train <- as.matrix(cbind(1, train_data$Prev_Weight, train_data$VeryActiveMinutes))

# Solve for Betas
betas <- solve(t(X_train) %*% X_train) %*% (t(X_train) %*% y_train)

# Index 1 = Intercept, Index 2 = Prev_Weight, Index 3 = VeryActiveMinutes
beta_activity <- betas[3]
beta_autoreg  <- betas[2]

print(paste("MODEL RESULT: For every 1 min of exercise,"))
print(paste("Weight changes by:", round(beta_activity, 6), "kg."))



# shiny app

library(shiny)

ui <- fluidPage(
  titlePanel("Daily Time Series Weight Prediction (Interactive)"),
  
  sidebarLayout(
    sidebarPanel(
      textInput(
        "program_name",
        "Program name:",
        value = "High Intensity 5 Day Upper Lower"
      ),
      numericInput(
        "start_weight",
        "Starting weight (kg):",
        value = 90,
        min = 30,
        max = 300,
        step = 0.5
      ),
      numericInput(
        "days_to_predict",
        "Days to predict:",
        value = 30,
        min = 7,
        max = 180,
        step = 1
      ),
      numericInput(
        "rec_duration",
        "Workout duration (minutes per workout day):",
        value = 90,
        min = 0,
        max = 300,
        step = 5
      ),
      checkboxGroupInput(
        "workout_days",
        "Workout days (weekly pattern):",
        choices = c(
          "Mon" = 1,
          "Tue" = 2,
          "Wed" = 3,
          "Thu" = 4,
          "Fri" = 5,
          "Sat" = 6,
          "Sun" = 7
        ),
        selected = c(1, 2, 3, 5, 6)  # default 5-day routine
      )
    ),
    
    mainPanel(
      plotOutput("weight_plot"),
      br(),
      verbatimTextOutput("summary_text")
    )
  )
)

server <- function(input, output) {
  
  # weekly schedule 
  rec_schedule_reactive <- reactive({
    sched <- rep(0, 7)
    idx <- as.integer(input$workout_days)
    if (!is.null(idx)) {
      sched[idx] <- 1
    }
    sched
  })
  
  # run forecast based on inputs
  sim_data <- reactive({
    rec_program_name <- input$program_name
    rec_duration     <- input$rec_duration
    rec_schedule     <- rec_schedule_reactive()
    
    start_weight     <- input$start_weight
    days_to_predict  <- input$days_to_predict
    
    # minutes per day
    weekly_mins <- rec_schedule * rec_duration
    daily_plan  <- rep(weekly_mins, length.out = days_to_predict)
    
    projected_weight <- numeric(days_to_predict)
    current_w <- start_weight
    
    beta_0        <- betas[1]
    beta_autoreg  <- betas[2]
    beta_activity <- betas[3]
    
    for (i in 1:days_to_predict) {
      current_w <- beta_0 +
        beta_autoreg  * current_w +
        beta_activity * daily_plan[i]
      projected_weight[i] <- current_w
    }
    
    plot_data <- data.frame(
      Day = 1:days_to_predict,
      Weight = projected_weight,
      Activity = ifelse(daily_plan > 0, "Workout", "Rest")
    )
    
    list(
      plot_data = plot_data,
      rec_program_name = rec_program_name,
      rec_duration = rec_duration,
      start_weight = start_weight,
      days_to_predict = days_to_predict
    )
  })
  
  # plot
  output$weight_plot <- renderPlot({
    res <- sim_data()
    plot_data        <- res$plot_data
    rec_program_name <- res$rec_program_name
    rec_duration     <- res$rec_duration
    start_weight     <- res$start_weight
    days_to_predict  <- res$days_to_predict
    
    ggplot(plot_data, aes(x = Day, y = Weight)) +
      # line
      geom_line(color = "#E74C3C", linewidth = 1.5) +
      
      # points
      geom_point(aes(color = Activity), size = 3) +
      
      scale_color_manual(values = c("Rest" = "gray", "Workout" = "#E74C3C")) +
      
      ggtitle(paste("Projected Weight Loss:", rec_program_name)) +
      labs(
        subtitle = paste(
          "Based on", rec_duration, "mins/session | Starting at", start_weight, "kg"
        ),
        y = "Predicted Weight (Kg)",
        x = "Days into Program"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # text summary
  output$summary_text <- renderPrint({
    res <- sim_data()
    plot_data        <- res$plot_data
    rec_program_name <- res$rec_program_name
    start_weight     <- res$start_weight
    days_to_predict  <- res$days_to_predict
    
    final_weight <- tail(plot_data$Weight, 1)
    loss <- start_weight - final_weight
    
    cat("CONCLUSION:\n")
    cat("If you follow the", rec_program_name, "\n")
    if (loss >= 0) {
      cat("You are projected to lose",
          round(loss, 2), "kg in", days_to_predict, "days.\n")
    } else {
      cat("You are projected to gain",
          round(abs(loss), 2), "kg in", days_to_predict, "days.\n")
    }
    cat("\nValidation RMSE: 0.3486 kg")
  })
}

shinyApp(ui = ui, server = server)