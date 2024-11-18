# Install and load required package
install.packages("rgl")
library(rgl)

# Sample data
set.seed(123)
your_data <- data.frame(
  score = rnorm(100, 50, 10),
  hours = rnorm(100, 10, 2),
  a_points = rnorm(100, 5, 1)
)

# Fit the model
model <- lm(score ~ hours + a_points, data = your_data)

# Create grid for regression plane
grid_hours <- seq(min(your_data$hours), max(your_data$hours), length.out = 30)
grid_a_points <- seq(min(your_data$a_points), max(your_data$a_points), length.out = 30)
grid <- expand.grid(hours = grid_hours, a_points = grid_a_points)
grid$score <- predict(model, newdata = grid)

# Calculate residuals
your_data$predicted <- predict(model)
your_data$residuals <- your_data$score - your_data$predicted

# 3D scatter plot with rgl
plot3d(
  your_data$hours, your_data$a_points, your_data$score,
  col = "blue", size = 1.5, type = "s", xlab = "Hours", ylab = "A Points", zlab = "Score"
)
# Add regression plane
planes3d(
  model$coefficients["hours"],
  model$coefficients["a_points"],
  -1,
  model$coefficients["(Intercept)"], col = "green", alpha = 0.5
)
# Add residual lines
for (i in 1:nrow(your_data)) {
  segments3d(
    c(your_data$hours[i], your_data$hours[i]),
    c(your_data$a_points[i], your_data$a_points[i]),
    c(your_data$score[i], your_data$predicted[i]),
    col = "red"
  )
}
