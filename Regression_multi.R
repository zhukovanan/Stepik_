#Давайте реализуем простейший вариант теста для проверки наличия гетероскедастичности.  Напишите функцию hetero_test, которая получает 
#на вход набор данных. Первая колонка в данных - зависимая переменная, остальные колонки - независимые. 
#Функция строит регрессионную модель, используя эти переменные, а затем проверяет, есть ли в данных  гетероскедастичность.

hetero_test <- {
fm <- as.formula(paste(colnames(test_data)[1], "~","."))
fit <- lm( fm  , data = test_data )
model <- cbind(fit$residuals , test_data)
summary(lm( (model$`fit$residuals`)^2 ~ . , data = model[-2]))$r.squared
}

#Самостоятельно реализуйте расчет показателя vif.  Напишите функцию VIF, которая получает на вход набор данных. 
#Первая колонка в данных - зависимая переменная, остальные колонки - независимые. 

VIF <-  function(test_data){
  sapply( colnames(test_data)[-1] , function(x) 1/(1-summary(lm(as.formula(paste(x,"~",".")) , data = test_data[-1]))$r.squared))
}
del <- function(x) {sort(VIF(x)[sapply(VIF(x), function(x) x >= 10)], decreasing = TRUE)}


library(dplyr)
rm.pred <- names(del(mtcars))[1]
lt <- NULL
for (i in length(rm.pred)) {
  
  mtcars %>%
    select(-i) %>%
    del(.) %>%
    length(.)
}




#Давайте реализуем поиск наиболее подходящей степени для трансформации независимой переменной в обычной регрессии, с одним предиктором.
#Ваша функция transform_x получает на вход набор данных из двух колонок, первая колонка y и вторая x. Функция должна 
#найти такой показатель степени для трансформации x, при котором между x и y будет максимальное абсолютное значение корреляции. 
#Правило трансформации, как мы разбирали на лекциях:

set.seed(42)
test_data <- data.frame(y = rnorm(10, 10, 1), x = rnorm(10, 10, 1))
transform_x <-  function(test_data){
  p <- seq(-2,2,0.1)
  convert <- function(x, h) { ifelse(h == 0 , return(log(x)), ifelse(h >0 , return(x^h), return(-(x)^h)))}
  convert(test_data$x, p[which.max(abs(sapply(p, function(h) cor(test_data$y, convert(test_data$x,h)))))])
}