setwd('C:/Users/sunch/Desktop/������������')
T <- read.table("reg.txt", header=TRUE)
T <- T[order(T[,1]),]

A <- 1350

library(forecast)


# i - ������ ����������� ������� 
# j - ������ ����������� �������
calculate_lr <- function(i, j){
  reg <- lm(formula = T[,j] ~ T[,i])
  
  # ����� ����������� ������
  cat(sprintf('����������� ������������ = %f\n',
              summary(reg)$r.squared))
  cat(sprintf('p-value ��� ���������� ����� = %f\n',
              coefficients(summary(reg))[1,4]))
  cat(sprintf('p-value ��� ������������ ���������� = %f\n',
              coefficients(summary(reg))[2,4]))
  cat(sprintf('Pred = (%f) + (%f) * A1 + eps\n',
              summary(reg)$coefficients[1,1],
              summary(reg)$coefficients[2,1]))
  cat(sprintf('����������� �������� = %f\n', mean(reg$residuals)))
  
  
  # ���������� ������� ��������
  res_plot <- plot(reg$residuals, main = '������ ��������',
                   xlab = '����� ����������', ylab = '�������')
  res_plot <- abline(h = 0)
  
  # ����� ������ �� MAPE
  cat(sprintf('������������� ������ ������������� (MAPE) = %f\n',
              accuracy(reg)[5]))
  
  # ������ �������� ��������� � ������� ��� ����� 1350
  reg_plot <- plot(T[,i], T[,j], main = '������ ���������',
                   ylab = sprintf('A%i', j), xlab = 'A1')
  abline(reg, col = 'black', lwd = 2)
  T <- T[1,]
  T$A1 <- A
  prediction = as.numeric(format(
    predict(reg, newdata=T, interval="confidence", 
                 level=0.9), digits=10))
  cat(sprintf('������� �������� = %f, [%f; %f]\n', 
              prediction[1],
              prediction[2],
              prediction[3]))
  reg_plot <- points(A, prediction[1], col = 'red')
  reg_plot <- lines(rep(A, 2), c(prediction[2], prediction[3]), col = 'red')
}


