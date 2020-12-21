#Начнем с простого и вспомним, как применять логистическую регрессию в R. Напишите функцию get_coefficients, которая получает на вход dataframe с двумя переменными x ( фактор с произвольным числом градаций) и y ( фактор с двумя градациями ﻿). Функция строит логистическую модель, где y — зависимая переменная, а x — независимая, и возвращает вектор со значением экспоненты коэффициентов модели.
test_data <- read.csv("https://stepik.org/media/attachments/course/524/test_data_01.csv")
test_data <- transform(test_data, x = factor(x), y = factor(y))
get_coefficients <- function(dataset){
  sapply((glm(y ~ x , data = test_data, family = "binomial")$coef), exp)
}

#Центрирование переменных
library(data.table)
test_data <- read.csv("https://stepic.org/media/attachments/course/524/cen_data.csv")
var_names = c("X4", "X2", "X1")
centered <- function(test_data, var_names){
  test_data <- as.data.table(test_data)
  test_data[, var_names[] := lapply(.SD, function(x) x - mean(x)), .SDcols = var_names ]
}

#Показать значимые предикторы
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_2.csv")
get_features <- function(dataset){
  fit <- glm(factor(is_prohibited) ~ ., dataset, family = "binomial")
  result <- anova(fit, test = "Chisq")$`Pr(>Chi)`[-1]
  names(result) <- colnames(dataset[,-1])
  if (length(result[result < 0.05]) == 0) {
    print("Prediction makes no sense")
  } else {
    names(result[result <0.05])
  }
  
  
}
#Предсказание подозрительного багажа
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data_passangers.csv")
data_for_predict <-read.csv("https://stepic.org/media/attachments/course/524/predict_passangers.csv")
most_suspucious <- function(x) {
  fit <- glm(factor(is_prohibited) ~ ., test_data, family = "binomial")
  result <- predict.glm(fit, data_for_predict, type = "response")
  data_for_predict <-cbind(data_for_predict, result)
  data_for_predict$passangers[result == max(result)]
}

#Normality test
test <- read.csv("https://stepic.org/media/attachments/course/524/test.csv")
normality_test <- function(dataset){
  unlist(lapply(dataset[sapply(dataset, is.numeric)] , function(x) shapiro.test(x)$p.value))
  
}

#Smart anova
test_data <- read.csv("https://stepic.org/media/attachments/course/524/s_anova_test.csv")
library(data.table)
library(magrittr)
smart_anova <- function(test_data) {
vector <- setDT(test_data)[, lapply(.SD, function(x) shapiro.test(x)$p.value ) , .SDcols = 1 , by = y][, list(x)] %>% dplyr::pull()
if (bartlett.test(x ~ y, test_data)$p.value >= 0.05 & length(vector[vector >= 0.05]) == 3 ) {
  c("ANOVA" = summary(aov(x ~ y, test_data))[[1]]$'Pr(>F)'[1])
} else {
  c("KW" = kruskal.test(x ~ y, test_data)$p.value)
} 
}

#Normality_by
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_for_norm.csv")
setDT(test_data)[, lapply(.SD, function(x) shapiro.test(x)$p.value ) , .SDcols = 1 , by = list(y,z)]
library(dplyr)
normality_by <- function(test){    
  grouped <- test %>%    
    group_by_(.dots = colnames(.)[2:3]) %>%         
    summarise_each(funs(get_p_value))         
  names(grouped)[3] <- 'p_value'         
  return(grouped)         
}

#Распределение переменной
library(ggplot2)
ggplot(iris, aes(Sepal.Length, fill = Species)) +
  geom_density(alpha = 0.2)