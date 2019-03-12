library(tidyverse)
library(stargazer)
library(estimatr)
library(skimr)
library(broom)
library(mice)

wage<-read.csv("~/Desktop/5253/wages.csv")
wage<-wage%>%drop_na(hgc,tenure)
stargazer(wage)

#Without NA
wage1 <- wage[complete.cases(wage), ]
stargazer(wage1)
summary(wage)
est1 <- lm(logwage ~ hgc + college + tenure + age + married , data=wage1)

#Mean
wage2 <- wage
wage.mean <- wage2$logwage[which(is.na(wage2$logwage))] <- mean(wage2$logwage, na.rm = TRUE)
est2 <- lm(logwage ~ hgc + college + tenure + tenure^2 + age + married, data=wage2)

#Prediction
test <- function(t)
{x <- dim(length(t))
x[which(!is.na(t))] = 1
x[which(is.na(t))] = 0
return(x)}
wage3<- wage
wage3$test.logwage <- test(wage3$logwage)
wage_function <- wage1
wage_function$logwage[is.na(wage_function$logwage)] <- predict(est1, newdata=wage[is.na(wage_function$logwage),])

est3 <-lm(logwage ~ hgc + college + tenure + tenure^2 + age + married, data=wage_function)

stargazer(est1, est2, est3)


#Mice 
wage_mice <- wage
wage_mice <- mice(wage, logwage = 12345)
fit <- with(wage_mice, lm(logwage ~ hgc + college + tenure + age + married))

round(summary(pool(fit)),2)

