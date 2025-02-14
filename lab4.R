setwd('C:/Users/sunch/Desktop/������������')
library("forecast") 
T <- read.table("reg.txt", header=TRUE)
T <- T[order(T[,1]),]
A <- 1350


analyze_regressions <- function(i, j){
  # �������� �������
  linear_reg <- lm(formula = T[,j] ~ T[,i])
  log_reg <- lm(formula = T[,j] ~ log(T[,i]))
  string_formula <- sprintf("T$A%i ~ p1 + p2/(p3 + T$A%i)", j ,i)
  par_reg <- nls(as.formula(string_formula), 
                 data=T, start = list(p1 = 500, p2 = -10000, p3 = -1000)) 
  
  
  # ����� ���������� � �������� ���������
  cat(sprintf('����������� ������������ = %f\n',
              summary(linear_reg)$r.squared))
  cat(sprintf('p-value ��� ���������� ����� = %f\n',
              coefficients(summary(linear_reg))[1,4]))
  cat(sprintf('p-value ��� ������������ ���������� = %f\n',
              coefficients(summary(linear_reg))[2,4]))
  cat(sprintf('Pred = (%f) + (%f) * x + eps\n',
              summary(linear_reg)$coefficients[1,1],
              summary(linear_reg)$coefficients[2,1]))
  cat(sprintf('����������� �������� = %f\n', mean(linear_reg$residuals)))
  cat(sprintf('������������� ������ ������������� (MAPE) = %f\n',
              accuracy(linear_reg)[5]))
  cat(sprintf('�������� ������� = %f\n\n', 
              summary(linear_reg)$coefficients[1,1] + 
                summary(linear_reg)$coefficients[2,1] * A))
  
  
  # ����� ���������� � ��������������� ���������
  cat(sprintf('����������� ������������ = %f\n',
              summary(log_reg)$r.squared))
  cat(sprintf('p-value ��� ���������� ����� = %f\n',
              coefficients(summary(log_reg))[1,4]))
  cat(sprintf('p-value ��� ������������ ���������� = %f\n',
              coefficients(summary(log_reg))[2,4]))
  cat(sprintf('Pred = (%f) + (%f) * ln(x) + eps\n',
              summary(log_reg)$coefficients[1,1],
              summary(log_reg)$coefficients[2,1]))
  cat(sprintf('����������� �������� = %f\n', mean(log_reg$residuals)))
  cat(sprintf('������������� ������ ������������� (MAPE) = %f\n',
              accuracy(log_reg)[5]))
  cat(sprintf('�������� ������� = %f\n\n', 
              summary(log_reg)$coefficients[1,1] + 
                summary(log_reg)$coefficients[2,1] * log(A)))
  
  
  # ����� ���������� � ��������������� ���������
  P<-coef(par_reg) 
  cat(sprintf('����������� ������������ = %f\n',
              summary(par_reg)$r.squared))
  print('���������� ����������:', quote = FALSE)
  print(summary(par_reg)$parameters[,4])
  cat(sprintf('Pred = (%f) + (%f)/(x + (%f)) + eps\n', P[1], P[2], P[3]))
  cat(sprintf('������������� ������ ������������� (MAPE) = %f\n',
      sum(abs((T[,j] - (P[1]+ P[2]/(P[3]+T[,i])))/T[,j]))/length(T[,j])*100))
  cat(sprintf('�������� ������� = %f', P[1] + P[2]/(P[3] + 1350)))
  
  
  # ������ �������� �������� ���������
  plot(linear_reg$residuals, main="������ �������� �������� ��������",
       xlab="����� ����������", ylab="�������")
  abline(h=0, col="red")
  
  
  # ������ �������� ��������������� ���������
  plot(log_reg$residuals, main="������ �������� ��������������� ��������",
       xlab="����� ����������", ylab="�������")
  abline(h=0, col="red")
  
  
  # ���������� ��������� ���������
  plot(T[,i], T[,j], main = '��������� ���������', xlab = 'A1',
       ylab = sprintf('A%i', j))
  abline(linear_reg, col = 'red', lwd = 2)
  curve(coef(log_reg)[1] + coef(log_reg)[2]*log(x), add=TRUE,
        col="green", lwd = 2)
  curve(P[1] + P[2]/(x+P[3]), add=TRUE, col="blue", lwd = 2)
  legend('bottomright', c("��������","���������������", "���������������"), col=c("red","green", "blue"),
         pch = c('-', '-', '-'), lwd = rep(3, 3))
}


