# Load required package
library(rgl)

# Fit the model
model <- lm(score ~ hours + a_points, data = exam)

# Create grid for regression plane
grid_hours <- seq(min(exam$hours), max(exam$hours), length.out = 30)
grid_a_points <- seq(min(exam$a_points), max(exam$a_points), length.out = 30)
grid <- expand.grid(hours = grid_hours, a_points = grid_a_points)
grid$score <- predict(model, newdata = grid)

# Calculate residuals
exam$predicted <- predict(model)
exam$residuals <- exam$score - exam$predicted

# 3D scatter plot with rgl
plot3d(
  exam$hours, exam$a_points, exam$score,
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
for (i in 1:nrow(exam)) {
  segments3d(
    c(exam$hours[i], exam$hours[i]),
    c(exam$a_points[i], exam$a_points[i]),
    c(exam$score[i], exam$predicted[i]),
    col = "red"
  )
}
rgl.snapshot(
  filename = "3d_model_2.png"
)
