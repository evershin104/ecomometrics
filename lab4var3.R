setwd('C:/Users/sunch/Desktop/эконометрика')
library("forecast") 
T <- read.csv("var3data.csv", header=TRUE, sep = '\t', dec = ',')
T <- T[order(T[,1]),]
A <- 30000


analyze_regressions <- function(i, j){
  # Создание моделей
  linear_reg <- lm(formula = T[,j] ~ T[,i])
  log_reg <- lm(formula = T[,j] ~ log(T[,i]))
  #string_formula <- sprintf("T$A%i ~ p1 + p2/(p3 + T$A%i)", j ,i)
  #par_reg <- nls(as.formula(string_formula), 
  #               data=T, start = list(p1 = 500, p2 = -10000, p3 = -1000)) 
  
  
  # Вывод информации о линейной регрессии
  cat(sprintf('Коэффициент детерминации = %f\n',
              summary(linear_reg)$r.squared))
  cat(sprintf('p-value для свободного члена = %f\n',
              coefficients(summary(linear_reg))[1,4]))
  cat(sprintf('p-value для коэффициента переменной = %f\n',
              coefficients(summary(linear_reg))[2,4]))
  print(coefficients(summary(linear_reg))[2,4])
  cat(sprintf('Pred = (%f) + (%f) * x + eps\n',
              summary(linear_reg)$coefficients[1,1],
              summary(linear_reg)$coefficients[2,1]))
  cat(sprintf('Матожидание остатков = %f\n', mean(linear_reg$residuals)))
  cat(sprintf('Относительная ошибка аппроксимации (MAPE) = %f\n',
              accuracy(linear_reg)[5]))
  cat(sprintf('Точечный прогноз = %f\n\n', 
              summary(linear_reg)$coefficients[1,1] + 
                summary(linear_reg)$coefficients[2,1] * A))
  
  
  # Вывод информации о логарифмической регрессии
  cat(sprintf('Коэффициент детерминации = %f\n',
              summary(log_reg)$r.squared))
  cat(sprintf('p-value для свободного члена = %f\n',
              coefficients(summary(log_reg))[1,4]))
  cat(sprintf('p-value для коэффициента переменной = %f\n',
              coefficients(summary(log_reg))[2,4]))
  print(coefficients(summary(log_reg))[2,4])
  cat(sprintf('Pred = (%f) + (%f) * ln(x) + eps\n',
              summary(log_reg)$coefficients[1,1],
              summary(log_reg)$coefficients[2,1]))
  cat(sprintf('Матожидание остатков = %f\n', mean(log_reg$residuals)))
  cat(sprintf('Относительная ошибка аппроксимации (MAPE) = %f\n',
              accuracy(log_reg)[5]))
  cat(sprintf('Точечный прогноз = %f\n\n', 
              summary(log_reg)$coefficients[1,1] + 
                summary(log_reg)$coefficients[2,1] * log(A)))
  
  
  # Вывод информации о параметрической регрессии
  #P<-coef(par_reg) 
  #cat(sprintf('Коэффициент детерминации = %f\n',
  #            summary(par_reg)$r.squared))
  #print('Значимость параметров:', quote = FALSE)
  #print(summary(par_reg)$parameters[,4])
  #cat(sprintf('Pred = (%f) + (%f)/(x + (%f)) + eps\n', P[1], P[2], P[3]))
  #cat(sprintf('Относительная ошибка аппроксимации (MAPE) = %f\n',
  #    sum(abs((T[,j] - (P[1]+ P[2]/(P[3]+T[,i])))/T[,j]))/length(T[,j])*100))
  #cat(sprintf('Точечный прогноз = %f', P[1] + P[2]/(P[3] + 1350)))
  
  
  # График остатков линейной регрессии
  plot(linear_reg$residuals, main="График остатков линейной регресии",
       xlab="Номер наблюдения", ylab="Остатки")
  abline(h=0, col="red")
  
  
  # График остатков логарифмической регрессии
  plot(log_reg$residuals, main="График остатков логарифмической регресии",
       xlab="Номер наблюдения", ylab="Остатки")
  abline(h=0, col="red")
  
  
  # Построение диаграммы рассеяния
  plot(T[,i], T[,j], main = 'Диаграмма рассеяния', xlab = 'A1',
       ylab = sprintf('A%i', j))
  abline(linear_reg, col = 'red', lwd = 2)
  curve(coef(log_reg)[1] + coef(log_reg)[2]*log(x), add=TRUE,
       col="green", lwd = 2)
  #curve(P[1] + P[2]/(x+P[3]), add=TRUE, col="blue", lwd = 2)
  #legend('bottomright', c("Линейная","Логарифмическая", "Параметрическая"), col=c("red","green", "blue"),
  #      pch = c('-', '-', '-'), lwd = rep(3, 3))
}


