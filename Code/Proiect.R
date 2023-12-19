install.packages('tidyverse')
install.packages('openintro')
install.packages("tibble")
install.packages('glmnet')
install.packages("sjPlot")

library(sjPlot)
library(tibble)
library(tidyverse)
library(openintro)
library(gapminder)
library(ggplot2)
library(caret)
library(rsample)
library(vip)
library(dplyr)
library(AICcmodavg)
library(glmnet)


setwd("/Users/damiandubciuc/Desktop/AD")

cars <- read.csv('vehicles.csv', sep = ';')

cars2000 <- cars %>%
  filter(year >= 2000)


cars %>%
  summarize(mean(year),
            median(year))

summaries <- cars2000 %>%
  group_by(fuel) %>%
  summarize(
    sd_year = sd(year),
    iqr_year = IQR(year),
    num_cars = n()
  )



# Crearea diagramei cu bare empilate
ggplot(cars, aes(x = year, y = price, fill = "Preturi")) +
  geom_bar(stat = "identity") +
  labs(title = "Prețurile automobilelor în dependență de vârsta lor", x = "Year", y = "Price") +
  theme_minimal()


# Crearea diagramei cu linii multiple
ggplot(cars, aes(x = year, y = price, color = year)) +
  geom_line() +
  labs(title = "Prețurile automobilelor în dependență de vârsta lor", x = "Anul", y = "Tip de combustibil") +
  theme_minimal()



# Crearea unui heatmap -----
ggplot(cars, aes(x = year, y = price)) +
  geom_tile(aes(fill = price), width = 1, height = 1000) +
  labs(title = "Heatmap - Preț vs. Vârsta", x = "Anii", y = "Preț") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal()


# Crearea diagramei cu bare apilabile -------
ggplot(cars, aes(x = year, y = price, fill = condition)) +
  geom_bar(stat = "identity") +
  labs(title = "Vârsta și Preț", x = "Vârsta", y = "Preț") +
  theme_minimal()



# Crearea diagramă boxplot
ggplot(cars, aes(x = factor(year), y = price)) +
  geom_boxplot() +
  labs(title = "Distribuția prețului în funcție de an", x = "An", y = "Stare") +
  theme_minimal()

# Crearea diagrama densitatii preturilor
ggplot(data = cars, aes(x = price)) + geom_density()

# Regresie liniara simpla
ggplot(data = cars, aes(y = price, x = year)) +
  geom_point() + 
  geom_smooth(method = "lm")

glimpse(cars)

# Factorizare pentru variabilele categoriale
cars$region <- as.factor(cars$region)
cars$manufacturer <- as.factor(cars$manufacturer)
cars$model <- as.factor(cars$model)
cars$condition <- as.factor(cars$condition)
cars$cylinders <- as.factor(cars$cylinders)
cars$fuel <- as.factor(cars$fuel)
cars$transmission <- as.factor(cars$transmission)
cars$type <- as.factor(cars$type)
cars$paint_color <- as.factor(cars$paint_color)

# Construirea modelului liniar
data_split <- initial_split(cars, prop = 0.7, strata = 'price')
data_train <- training(data_split)
data_test <- testing(data_split)

attach(data_train)


#model 1 Model Liniar Simpla cu O Singură Variabilă Predictorie:

model1 <- lm(price ~ year, data = cars)

summary(model1)

ggplot(cars, aes(x = year, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Regresie Liniară - Pret în funcție de Ani")

# Folosirea modelului pentru a face predicții pe setul de test
predictions <- predict(model1, newdata = cars)






#model 2 Model Liniar cu Mai Multe Variabile Predictorii:

model2 <- lm(price ~ id +
               region + 
               year + 
               manufacturer +
               model +
               condition +
               cylinders +
               fuel +
               odometer +
               transmission +
               type +
               paint_color, 
            data = cars)

summary(model2)

ggplot(cars, aes(x = year, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "green") +
  labs(title = "Regresie Liniară Multiplă - Pret în funcție de An")

# Folosirea modelului pentru a face predicții pe setul de test
predictions2 <- predict(model2, newdata = cars)

# Afișarea rezultatului
print(paste("R-squared:", round(r_squared2, 4)))




#model 3 Model Liniar cu Interacțiune:

model3 <- lm(price ~ year * condition, data = cars)

summary(model3)

ggplot(cars, aes(x = year, y = price, color = condition, linetype = condition)) +
  geom_line() +
  labs(title = "Interacțiunea dintre An și Stare în Modelul de Regresie",
       x = "An",
       y = "Preț") +
  theme_minimal()

# Folosirea modelului pentru a face predicții pe setul de test
predictions3 <- predict(model3, newdata = cars)

# Afișarea rezultatului
print(paste("R-squared:", round(r_squared3, 4)))




#model 4 Model Liniar cu Transformare Logaritmică:
model4 <- lm(log(price) ~ id +
               region + 
               year + 
               manufacturer +
               model +
               condition +
               cylinders +
               fuel +
               odometer +
               transmission +
               type +
               paint_color, 
             data = cars)

summary(model4)

ggplot(cars, aes(x = year, y = price)) +
  geom_point() +
  facet_wrap(~condition + transmission, scales = "free_y", ncol = 3) +
  labs(title = "Efectele variabilelor asupra logaritmului prețului",
       x = "An",
       y = "Logaritmul Prețului") +
  theme_minimal()

# Folosirea modelului pentru a face predicții pe setul de test
predictions4 <- predict(model4, newdata = cars)

# Afișarea rezultatului
print(paste("R-squared:", round(r_squared4, 4)))



#model 5 Model Liniar cu Variabile Categorice:

model5 <- lm(price ~ factor(year), data = cars)

summary(model5)

ggplot(cars, aes(x = as.factor(fuel), y = price)) +
  geom_boxplot() +
  labs(title = "Boxplot - Pret în funcție de Tipul carburantului")

# Folosirea modelului pentru a face predicții pe setul de test
predictions5 <- predict(model5, newdata = cars)

# Afișarea rezultatului
print(paste("R-squared:", round(r_squared5, 4)))



#model 6 Model Liniar cu Termen Pătratic:

model6 <- lm(price ~ year + I(year^2), data = cars)

summary(model6)

# Crearea unui set de date pentru plot
plot_data <- data.frame(year = seq(min(cars$year), max(cars$year), length.out = 100))

# Calcularea valorilor prezise folosind modelul cu termen pătratic
plot_data$predicted_price <- predict(model6, newdata = plot_data)

# Crearea plotului
ggplot(cars, aes(x = year, y = price)) +
  geom_point(color = "blue", size = 3) +
  geom_line(data = plot_data, aes(x = year, y = predicted_price), color = "red") +
  labs(title = "Regresie Liniară cu Termen Pătratic - Pret în funcție de An",
       x = "An",
       y = "Preț Prevăzut") +
  theme_minimal()

# Folosirea modelului pentru a face predicții pe setul de test
predictions6 <- predict(model6, newdata = cars)

# Afișarea rezultatului
print(paste("R-squared:", round(r_squared6, 4)))



#model 7 Regresie liniară polinomială
model7 <- lm(price ~ poly(year, degree = 2), data = cars)

summary(model7)

# Calcularea valorilor prezise folosind modelul cu regresie polinomială
plot_data$predicted_price <- predict(model7, newdata = plot_data)

# Crearea plotului cu multiple panouri
ggplot(plot_data, aes(x = year, y = predicted_price)) +
  geom_line() +
  geom_point(data = cars, aes(x = year, y = price, color = condition), size = 2) +
  facet_wrap(~condition, scales = "free_y", ncol = 2) +
  labs(title = "Regresie Polinomială - Pret în funcție de An și Condiție",
       x = "An",
       y = "Preț Prevăzut") +
  theme_minimal()

# Folosirea modelului pentru a face predicții pe setul de test
predictions7 <- predict(model7, newdata = cars)

# Afișarea rezultatului
print(paste("R-squared:", round(r_squared7, 4)))
