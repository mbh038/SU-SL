library(ISLR)
library(car)
library(boot)
load ("./data/5.R.RData")
attach(Xy)
lfit<-lm(y~X1+X2,data=Xy)
lfit
summary(lfit)
summary(lm(y~.,data=Xy)) # model answer

matplot(Xy,type="l")
library(scatterplot3d)
scatterplot3d(X1,X2,y[1:1000],type="l",color ="blue")
# plots show great deal of autocorrelation, so error estimates of betas in lfit
# likely to be too small.

# There is very strong autocorrelation between consecutive rows of the data matrix.
# Roughly speaking, we have about 10-20 repeats of every data point, so the sample
# size is in effect much smaller than the number of rows (1000 in this case).

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

system.time(
    {
        ns<-c(1,101,201,301,401,501,601,701,801,901)
        
        lfitBeta1.fn=function(data,ns){
            newBlock<-sample(ns,10,replace=TRUE)
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
    }
)

## Do block bootstrapping manually (just to see how it could be done)

system.time({
    # resample the data in blocks of length 100
    resamples<-lapply(1:1000,function(i){
        ns<-c(1,101,201,301,401,501,601,701,801,901)
        newBlock<-sample(ns,10,replace=TRUE)
        newBlock
        newRows<-vector(mode="numeric", length=0)
        for (i in 1:10){
            nex100=seq(from=newBlock[i],to=newBlock[i]+99,by=1)
            newRows<-c(newRows,nex100)
        }
        #str(newRows)
        XyBlockResample<-Xy[newRows,];
    })
    #str(resamples)
    
    # find beta1 for each block bootstrap sample
    r.lfitBeta1 <- sapply(resamples, lfitBeta1)
    summary(r.lfitBeta1)
    
    # find standard deviation of distribution of beta1
    sqrt(var(r.lfitBeta1))
    
    # plot histogram and qq plot
    par(mfrow=c(1,2))
    hist(r.lfitBeta1)
    qqnorm(r.lfitBeta1)
    qqline(r.lfitBeta1, col = 2,lwd=2,lty=2)
    })
