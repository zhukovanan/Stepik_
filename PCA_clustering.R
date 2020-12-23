#Напишите функцию smart_hclust, которая получает на вход dataframe  с произвольным 
#числом количественных переменных и число кластеров, которое необходимо выделить при помощи иерархической кластеризации.
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data_hclust.csv")
smart_hclust<-  function(test_data, cluster_number){
  fit <- hclust(dist(test_data))
  cluster <- cutree(fit, cluster_number)
  test_data <- cbind(test_data, cluster)
}



#Напишите функцию get_difference, которая получает на вход два аргумента: 
#test_data — набор данных с произвольным числом количественных переменных.
#n_cluster — число кластеров, которое нужно выделить в данных при помощи иерархической кластеризации.
#Функция должна вернуть названия переменных, по которым были обнаружен значимые различия между выделенными кластерами (p < 0.05)

get_difference <- function(test_data, n_cluster) {
  d <- dist(test_data)
  fit <- hclust(d)
  test_data$cluster <- factor(cutree(fit, k =  n_cluste))
  mod <- sapply(test_data[,colnames(test_data) != "cluster"], function(x) anova(aov(x ~ cluster , data = test_data))$P[1])
  return(names(mod)[mod < 0.05])
}


#Напишите функцию get_pc, которая получает на вход dataframe с произвольным числом количественных переменных. 
#Функция должна выполнять анализ главных компонент и добавлять в 
#исходные данные две новые колонки со значениями первой и второй 
#главной компоненты. Новые переменные должны называться "PC1"  и "PC2" соответственно

test_data <- read.csv("https://stepic.org/media/attachments/course/524/pca_test.csv")
get_pc <- function(d){
  test_data <- cbind(test_data,prcomp(test_data)$x[,c(1,2)] )
  
}


#Усложним предыдущую задачу! 
#Напишите функцию get_pca2, которая принимает на вход dataframe с произвольным числом 
#количественных переменных. Функция должна рассчитать, какое минимальное число главных компонент объясняет 
#больше 90% изменчивости в исходных данных и добавлять значения этих компонент в исходный dataframe в виде новых переменных
get_pca2 <- function(data){
  fit <- prcomp(swiss)$x
  imp <- summary(prcomp(swiss))$importance[3,]
  cnt = 0
  n = 0
  for (l in 1:length(imp) ) {
    if( cnt >= 0.90) {
      break
    } else {
      n = n + 1
      cnt = imp[l]
    }
  }
  data <- cbind(data , fit[,c(seq(n))] )
}

#Задача для Чака Норриса.
d <- dist(swiss)
fit <- hclust(d)
swiss$cluster <- factor(cutree(fit, k =  2))

d <- dist(swiss)
fit <- hclust(d)
swiss$cluster <- factor(cutree(fit, k =  2))
# дополните код, чтобы получить график
library(ggplot2)
my_plot <- ggplot(swiss, aes(Education, Catholic, col = cluster))+
  geom_point()+
  geom_smooth(method = "lm")
