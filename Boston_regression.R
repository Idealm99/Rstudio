library(MASS)
data("Boston")
mydata <- Boston
head(mydata)

# 1. 모형 적합
model <- lm(medv ~ crim+zn+indus, data = mydata)
summary(model)

# 2. 예측값 생성
predicted_values <- predict(model, newdata = Boston)

# MSE
# 실제값과 예측값
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

# 3. 모형 진단
# 레이아웃 설정: 2x2 그리드 (한 화면에 4개의 그래프 표시)
par(mfrow = c(2, 2))

# 회귀모형 진단 플롯 생성
plot(model)


