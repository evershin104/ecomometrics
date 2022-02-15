setwd('C:/Users/sunch/Desktop/эконометрика')
data <- read.table('data.txt', sep=',', header = TRUE)
plot(data$dist, data$salary, xlab="Расстояние до Москвы", ylab="Средняя заработная плата", 
     main="Диаграмма рассеяния", col="red", pch=2)
text(data$dist, data$salary + 400, labels=data$cityname)
legend(1250,25700,c("Город","Область"), col=c("red","green"), text.col = "blue", pch = c(2,22), 
       bg="yellow") 
abline(a = 90824061/1867, b = -7450/1867, col = 'blue')