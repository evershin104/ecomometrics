setwd('C:/Users/sunch/Desktop/������������')
data <- read.table('data.txt', sep=',', header = TRUE)
plot(data$dist, data$salary, xlab="���������� �� ������", ylab="������� ���������� �����", 
     main="��������� ���������", col="red", pch=2)
text(data$dist, data$salary + 400, labels=data$cityname)
legend(1250,25700,c("�����","�������"), col=c("red","green"), text.col = "blue", pch = c(2,22), 
       bg="yellow") 
abline(a = 90824061/1867, b = -7450/1867, col = 'blue')