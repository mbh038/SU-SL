library(ISLR)
df<-read.csv("./data/Advertising.csv")
model<-lm(Sales~TV+Radio,data=df)
confint(model)
predict(model,data.frame(TV=100,Radio=20),interval="confidence")
predict(model,data.frame(TV=100,Radio=20),interval="prediction")


library(ISLR)
library(MASS)
str(Default)
lda.fit=lda(default~student+balance,data=Default)
lda.fit
#plot(lda.fit)

lda.pred=predict(lda.fit, data=Default,na.action="na.omit", CV=TRUE)
lda.class=lda.pred$class
table(lda.class,Default$default)
sum(table(lda.pred$posterior[,1]<0.01,Default$default))

nthresh=500
maxthresh=0.5
TPR<- vector(mode="numeric", length=nthresh)
FPR<- vector(mode="numeric", length=nthresh)
FNR<- vector(mode="numeric", length=nthresh)
TER<- vector(mode="numeric", length=nthresh)
threshold<-vector(mode="numeric", length=nthresh)
for ( i in 1:nthresh){
    threshold[i]=maxthresh*i/nthresh
    tab=table(lda.pred$posterior[,2]>=threshold[i],Default$default)
    #print(threshold[i])
    #print(tab)
    if(nrow(tab)==1){
        tab<-rbind(tab,c(0,0))
    }
    if(ncol(tab)==1){
        tab<-cbind(tab,c(0,0))
    }
    TPR[i]=tab[2,2]/sum(tab[,2])
    FNR[i]=1-TPR[i]
    FPR[i]=tab[2,1]/sum(tab[,1])
    TER[i]=(tab[1,2]+tab[2,1])/sum(tab)
    }

plot(FPR,TPR,type="l",col="blue",main="ROC curve")
plot(threshold,FNR,type="l",col="blue",xlab="threshold",ylab="Error rate")
lines(threshold,FPR,type="l",col="red")
lines(threshold,TER,type="l",col="black")

library(pROC)
roc_curve<-roc(Default$default,lda.pred$posterior[,1],plot=TRUE,direction=">",auc=TRUE)
#plot(roc_curve)
auc(Default$default, lda.pred$posterior[,1])


mean(lda.class==Default$default)

sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)
lda.pred$posterior[1:20,1]
lda.class[1:20]
sum(lda.pred$posterior[,1]>.9)