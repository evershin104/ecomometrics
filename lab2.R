
T <- read.table("reg.txt", header=TRUE)

# Âåêòîðà a1, a2
compare_data <- function(a1, a2){
  plot(a1, a2)
  abline(lm(a2 ~ a1))
  cor_ = round(cor(a1, a2), 2)
  cor_test = cor.test(a1, a2)
  return(c(cor_, round(cor_test$p.value, 2)))
}

