

numTrials<-1000
testN=1000
library(e1071)
dfnames<-c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","y")

# svm with default settings
error.rates<-replicate(numTrials,{
    
    x0=matrix(rnorm(500),50,10)
    x1=matrix(rnorm(500),50,10)
    x1[,1:5]<-x1[,1:5]+1
    x<-rbind(x0,x1)
    y=rep(c(0,1),c(50,50))
    train=data.frame(x,y=as.factor(y))
    names(train)<-dfnames

    x0=matrix(rnorm(5*testN),testN/2,10)
    x1=matrix(rnorm(5*testN),testN/2,10)
    x1[,1:5]<-x1[,1:5]+1
    x<-rbind(x0,x1)
    y=rep(c(0,1),c(testN/2,testN/2))
    test=data.frame(x,y=as.factor(y))
    names(test)<-dfnames
    
    #svm default settings
    svmfit=svm(y~.,data=train)
    svm.pred = predict(svmfit, newdata = test)
    error.rate<-mean(svm.pred!=test$y)
    
#     # svm linear kernel
#     svmfit=svm(y~.,data=train,kernel="linear",scale=FALSE,cost=10)
#     svm.pred = predict(svmfit, newdata = test)
#     error.rate<-mean(svm.pred!=test$y)
#     
#     # logistic regression
#     glm.fit=glm(y~.,data=train,family=binomial)
#     glm.probs = predict(glm.fit, newdata = test,type="response")
#     glm.pred=rep(0,testN)
#     glm.pred[glm.probs>.5]=1

})
mean(error.rates)
sd(error.rates)
hist(error.rates)

