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
lda.class=lda.pred$class # prediction of the class (no (does not default) or yes (does) )
table(lda.class,Default$default) # confusion table
table(lda.pred$posterior[,2]>0.5,Default$default) # same table

# might be easier to understand if we turn lda.pred object (a list) into a data frame

lda.pred<-data.frame(lda.pred)
head(lda.pred,5) # class 

table(lda.pred$posterior.Yes>0.5,Default$default) # same table
# means that if probability of a Yes is > 0.5, then we classify it as a Yes, etc
# if choose higher thereshold, are less likely to classify as Yes, so more likely
# to make False Negative error rate (FNR, Type 2 error) - 
# Simliarly, the False Positive error rate (FPR, Type 1 error) will decline as 
# the threshold increases, since we are less likely to classify something as positive 
# if the threshold is higher.

# FNR - to to classify as negative something that is actually positive.
# FPR - to to classify as positive something that is actually negative.

#Find ROC and AUC manually
nthresh=500
maxthresh=0.5
TPR<- vector(mode="numeric", length=nthresh)
FPR<- vector(mode="numeric", length=nthresh)
FNR<- vector(mode="numeric", length=nthresh)
TER<- vector(mode="numeric", length=nthresh)
threshold<-vector(mode="numeric", length=nthresh)
for ( i in 1:nthresh){
    threshold[i]=maxthresh*i/nthresh
    tab=table(lda.pred$posterior.Yes>=threshold[i],Default$default)
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

# now see how much easier this is to do using pROC package
library(pROC)
roc_curve<-roc(Default$default,lda.pred$posterior.Yes)
#also calculate AUC
auc(Default$default, lda.pred$posterior.Yes)

mean(lda.class==Default$default)

sum(lda.pred$posterior.No>=.5)
sum(lda.pred$posterior.No<.5)
lda.pred$posterior[1:20,1]
lda.class[1:20]
sum(lda.pred$posterior[,1]>.9)