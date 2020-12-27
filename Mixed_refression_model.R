#Постройте график, отображающий различие в высоте голоса (frequency) для каждого предложения (scenario) в зависимости от типа 
#социальной ситуации (attitude)
#Не забудьте перевести переменную scenario в фактор.
library(ggplot2)
exp_data <- read.csv("https://raw.githubusercontent.com/opetchey/BIO144/master/3_datasets/politeness_data.csv")
ggplot(data = exp_data, aes(x = factor(scenario), y = frequency, fill = attitude))+
  geom_boxplot()

#Визуализируйте распределение высоты голоса у испытуемых в зависимости от пола и номера испытуемог
ggplot(data = exp_data, aes(x = frequency ,  fill = subject))+
  geom_density()+
  facet_grid(gender ~ .)

#Итак, ваша задача — построить следующую регрессионную модель: 
#зависимая переменная — frequency, 
#фиксированный эффект — attitude,
#а также добавлен случайный intercept для переменных subject и scenario.

fit_1<- lmer(frequency ~ attitude + (1|subject) + (1|scenario) , data = exp_data)

#Теперь добавьте пол испытуемых в модель в качестве фиксированного эффекта*. 
#Сохраните обновленную версию модели в переменную fit_2.
fit_2 <- lmer(frequency ~ attitude + gender + (1|subject) + (1|scenario) , data = exp_data)

#Давайте теперь добавим случайный коэффициент наклона к нашим двум случайным эффектам. Сохраните финальную модель в переменную fit_3

fit_3 <- lmer(frequency ~ attitude + gender + (1 + attitude|subject) + (1 + attitude|scenario) , data = exp_data)