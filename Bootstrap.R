#В этой задаче вам необходимо оценить 95% интервал для медианы при помощи basic bootstrap. 
#Напишите функцию median_cl_boot, которая получает на вход числовой вектор произвольной длины и возвращает 
#вектор из двух значений - верхней и нижней границы доверительного интервала для медианы.

median_cl_boot <- function(x){
  med <- median(x)
  vec <- sapply(1:1000, function(x) median(sample(vect, replace = TRUE)) - med) %>% quantile(c(.05,.95))+ med
  
  
}


#В этой задаче вам необходимо оценить 95% интервал для коэффициента наклона в линейной регрессии при помощи basic bootstrap. Напишите функцию 
#slope_cl_boot, которая получает на вход dataframe с двумя переменными x и y 
#произвольной длины и возвращает вектор из двух значений - верхней и нижней границы доверительного интервала для коэффициента наклона в модели y ~ x

slope_cl_boot <- function(t){
  slope <- lm(y ~ x, data = t )$coefficients[1]
  vec <- slope - sapply(1:1000, function(x) lm(y ~ x , data = t[sample(1:nrow(t), replace = TRUE),])$coefficients[2] %>% quantile(c(.05,.95))+slope
}

