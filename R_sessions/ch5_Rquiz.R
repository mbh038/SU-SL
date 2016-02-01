library(ISLR)
load ("./data/5.R.RData")
attach(Xy)
lfit<-glm(y~X1+X2,data=Xy)
lfit
summary(lfit)
summary(lm(y~.,data=Xy)) # model answer

matplot(Xy,type="l")
