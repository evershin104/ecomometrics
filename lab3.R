setwd('C:/Users/sunch/Desktop/эконометрика')
T <- read.table("reg.txt", header=TRUE)
T <- T[order(T[,1]),]

A <- 1350

library(forecast)


# i - Индекс экзогенного столбца 
# j - индекс эндогенного столбца
calculate_lr <- function(i, j){
  reg <- lm(formula = T[,j] ~ T[,i])
  
  # Вывод необходимых данных
  cat(sprintf('Коэффициент детерминации = %f\n',
              summary(reg)$r.squared))
  cat(sprintf('p-value для свободного члена = %f\n',
              coefficients(summary(reg))[1,4]))
  cat(sprintf('p-value для коэффициента переменной = %f\n',
              coefficients(summary(reg))[2,4]))
  cat(sprintf('Pred = (%f) + (%f) * A1 + eps\n',
              summary(reg)$coefficients[1,1],
              summary(reg)$coefficients[2,1]))
  cat(sprintf('Матожидание остатков = %f\n', mean(reg$residuals)))
  
  
  # Построение графика остатков
  res_plot <- plot(reg$residuals, main = 'График остатков',
                   xlab = 'Номер наблюдения', ylab = 'Остатки')
  res_plot <- abline(h = 0)
  
  # Вывод данных по MAPE
  cat(sprintf('Относительная ошибка аппроксимации (MAPE) = %f\n',
              accuracy(reg)[5]))
  
  # График линейной регрессии и прогноз для точки 1350
  reg_plot <- plot(T[,i], T[,j], main = 'График регрессии',
                   ylab = sprintf('A%i', j), xlab = 'A1')
  abline(reg, col = 'black', lwd = 2)
  T <- T[1,]
  T$A1 <- A
  prediction = as.numeric(format(
    predict(reg, newdata=T, interval="confidence", 
                 level=0.9), digits=10))
  cat(sprintf('Прогноз значения = %f, [%f; %f]\n', 
              prediction[1],
              prediction[2],
              prediction[3]))
  reg_plot <- points(A, prediction[1], col = 'red')
  reg_plot <- lines(rep(A, 2), c(prediction[2], prediction[3]), col = 'red')
}


