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
plot(lda.fit)

lda.pred=predict(lda.fit, Default)
lda.class=lda.pred$class
table(lda.class,Default$default)
sum(table(lda.pred$posterior[,1]<0.01,Default$default))

TPR<- vector(mode="numeric", length=500)
FPR<- vector(mode="numeric", length=500)
FNR<- vector(mode="numeric", length=500)
threshold<-vector(mode="numeric", length=500)
for ( i in 1:1000){
    threshold[i]=i/1000
    tab=table(lda.pred$posterior[,1]>=threshold[i],Default$default)
    if(nrow(tab)==1){
        tab<-rbind(tab,c(0,0))
    }
    if(ncol(tab)==1){
        tab<-cbind(tab,c(0,0))
    }
    TPR[i]=tab[1,2]/sum(tab[,2])
    FNR[i]=1-TPR[i]
    FPR[i]=tab[1,1]/sum(tab[,1])
}

plot(FPR,TPR)
plot(threshold,FNR)
plot(threshold,FPR)
plot(threshold,TPR)


mean(lda.class==Default$default)

sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)
lda.pred$posterior[1:20,1]
lda.class[1:20]
sum(lda.pred$posterior[,1]>.9)