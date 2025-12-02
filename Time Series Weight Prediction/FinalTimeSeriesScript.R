# FINAL PROJECT: Daily Time Series Weight Prediction
# Logic: Uses Daily AR(1) with Lagged Activity to capture bodys response

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


# 3. INTEGRATION

#---------------------------------------------------------
# INPUT FROM CF
rec_program_name <- "High Intensity 5 Day Upper Lower"
rec_duration <- 90.0  #minutes
rec_schedule <- c(1, 1, 1, 0, 1, 1, 0) # 5 days a week

# USER INPUT 
start_weight <- 90.0 # kg
days_to_predict <- 30 # days
#---------------------------------------------------------


# 4. FORECASTING (The Output)

# Create 30 day plan (Minutes per day)
weekly_mins <- rec_schedule * rec_duration
daily_plan <- rep(weekly_mins, length.out = days_to_predict)

# Run Simulation
projected_weight <- numeric(days_to_predict)
current_w <- start_weight

beta_0        <- betas[1]
beta_autoreg  <- betas[2]
beta_activity <- betas[3]

current_w <- start_weight
for (i in 1:days_to_predict) {
  current_w <- beta_0 +
    beta_autoreg  * current_w +
    beta_activity * daily_plan[i]
  projected_weight[i] <- current_w
}


# 5. VISUALIZATION

plot_data <- data.frame(
  Day = 1:days_to_predict,
  Weight = projected_weight,
  Activity = ifelse(daily_plan > 0, "Workout", "Rest")
)

ggplot(plot_data, aes(x = Day, y = Weight)) +
  # Trend Line
  geom_line(color = "#E74C3C", linewidth = 1.5) +
  
  # Points
  geom_point(aes(color = Activity), size = 3) +
  
  scale_color_manual(values = c("Rest" = "gray", "Workout" = "#E74C3C")) +
  
  ggtitle(paste("Projected Weight Loss: ", rec_program_name)) +
  labs(
    subtitle = paste("Based on", rec_duration, "mins/session | Starting at", start_weight, "kg"),
    y = "Predicted Weight (Kg)",
    x = "Days into Program"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Final Numbers
loss <- start_weight - tail(projected_weight, 1)
print(paste("CONCLUSION: If you follow the", rec_program_name))
print(paste("You are projected to lose", round(loss, 2), "kg in ", days_to_predict ," days."))


# 6. ACCURACY VALIDATION

n_rows <- nrow(train_data)
cutoff <- floor(0.8 * n_rows)

# Training
train_set <- train_data[1:cutoff, ]
X_train_val <- as.matrix(cbind(1, train_set$Prev_Weight, train_set$VeryActiveMinutes))
y_train_val <- as.matrix(train_set$WeightKg)

# Testing
test_set  <- train_data[(cutoff + 1):n_rows, ]
X_test_val  <- as.matrix(cbind(1, test_set$Prev_Weight, test_set$VeryActiveMinutes))
y_test_val  <- as.matrix(test_set$WeightKg)

betas_val <- solve(t(X_train_val) %*% X_train_val) %*% (t(X_train_val) %*% y_train_val)

# Predict on Test Set
y_pred <- X_test_val %*% betas_val

# RMSE 
residuals <- y_test_val - y_pred
squared_error <- residuals^2
mse <- mean(squared_error)
rmse <- sqrt(mse)

print("--- MODEL ACCURACY (Validation) ---")
print(paste("RMSE:", round(rmse, 4), "kg"))
print("Interpretation: On average, the model's prediction is off by this amount per day.")


# 7. MORE VALIDATION

# Residuals over Time
plot(residuals, type = "l", main = "Residuals Over Time", 
     ylab = "Error (kg)", xlab = "Time Index", col = "blue")
abline(h = 0, col = "red", lty = 2)

# Histogram of Residuals
hist(residuals, breaks = 10, main = "Histogram of Residuals", 
     xlab = "Error Size", col = "gray", border = "white")

# Q-Q Plot
qqnorm(residuals, main = "Q-Q Plot of Residuals")
qqline(residuals, col = "red", lwd = 2)

# ACF of Residuals
acf(residuals, main = "ACF of Residuals")