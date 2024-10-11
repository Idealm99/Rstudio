library(MASS)
data("Boston")
mydata <- Boston
head(mydata)

model <- lm(medv ~ crim+tax+chas+ptratio+rm+dis+rad+lstat, data = mydata)
summary(model)


predicted_values <- predict(model, newdata = Boston)

# MSE

actual_values <- Boston$medv

# MSE (Mean Squared Error)
mse <- mean((actual_values - predicted_values)^2)
mse

# RMSE (Root Mean Squared Error)
rmse <- sqrt(mse)
rmse

# MAE (Mean Absolute Error)
mae <- mean(abs(actual_values - predicted_values))
mae


par(mfrow = c(2, 2))

plot(model)


