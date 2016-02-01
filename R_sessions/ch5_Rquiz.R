library(ISLR)
library(car)
load ("./data/5.R.RData")
attach(Xy)
lfit<-lm(y~X1+X2,data=Xy)
lfit
summary(lfit)
summary(lm(y~.,data=Xy)) # model answer
summary(lm(y~X1*X2,data=Xy))
summary(lm(y~X1,data=Xy))
summary(lm(y~X2,data=Xy))
summary(lm(X1~X2,data=Xy))

matplot(Xy,type="l")
library(scatterplot3d)
scatterplot3d(X1,X2,y[1:1000],type="l",color ="blue")


lfitBeta1=function(df){
lfit<-lm(df[,3]~df[,1]+df[,2],data=df)  
lfit$coefficients[2]
}



lfitBeta1.fn=function(data, index){
  lfitBeta1(data[index,])
}

boot.out=boot(Xy,lfitBeta1.fn,R=1000)
boot.out
plot(boot.out)

ns<-c(1,101,201,301,401,501,601,701,801,901)

  
str(newRows)
# summary(newRows)

lfitBeta1.fn=function(data,ns){
  newBlock<-sample(ns,10,replace=TRUE)
  newBlock
  newRows<-vector(mode="numeric", length=0)
  for (i in 1:10){
    nex100=seq(from=newBlock[i],to=newBlock[i]+99,by=1)
    newRows<-c(newRows,nex100)
  }
  lfitBeta1(data[newRows,])
}

boot.out=boot(Xy,lfitBeta1.fn,R=1000)
boot.out
plot(boot.out)