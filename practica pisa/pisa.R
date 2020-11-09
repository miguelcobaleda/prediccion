library(readr) # para leer documentos
library(dplyr) # para utilizar %>%
library(janitor) # para hacer clean data
library(imputeTS) # para sustituir na por un valor
library(gam) # para hacer gam
library(magrittr) # para utilizar %<>%



pisa <- read.csv('./pisasci2006.csv')
head(pisa)
attach(pisa)

pisa %<>% select(Overall, Interest, Support, Income, Health, Edu, HDI)
pisa %<>% clean_names()   


sum(is.na(pisa))
pisa <- na_mean(pisa) # sustituimos na por la media de la columna

# variable interest
spline_interest <- smooth.spline(x = interest, y = overall, cv = TRUE)
spline_interest$df

# variable support
spline_support <- smooth.spline(x = support, y = overall, cv = TRUE)
spline_support$df

# variable income
spline_income <- smooth.spline(x = income, y = overall, cv = TRUE)
spline_income$df

# variable health
spline_health <- smooth.spline(x = health, y = overall, cv = TRUE)
spline_health$df

# variable edu
spline_edu <- smooth.spline(x = edu, y = overall, cv = TRUE)
spline_edu$df

# variable hdi
spline_hdi <- smooth.spline(x = hdi, y = overall, cv = TRUE)
spline_hdi$df


modelgam1 <- gam(overall ~  s(interest) + s(support) + s(income) + s(health) + s(edu) + s(hdi), data = pisa)
par(mfrow = c(2, 3))
plot(modelgam1, se = TRUE, col = 'blue') # mostramos el grafico del primer modelo gam

modelgam2 <- gam(overall ~  interest + s(support) + income + health + edu + hdi, data = pisa)
par(mfrow = c(2, 3))
plot(modelgam2, se = TRUE, col = 'blue') # mostramos el grafico del segundo modelo gam

anova(modelgam1, modelgam2, test='F') # utilizamos la tabla anova para elegir entre los dos modelos

