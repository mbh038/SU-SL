## Ch 10 Unsupservised Learning

# Quiz

load ("./data/10.R.RData")
#load ("https://lagunita.stanford.edu/c4x/HumanitiesSciences/StatLearning/asset/10.R.RData")

# concatenate x and x text

xall<-rbind(x,x.test)

#Principal Components Analysis
pca.out=prcomp(xall, scale=TRUE)
summary(pca.out)
# plot variance associated with each PC
plot(pca.out,type="l")

# plot the proportion of variance explained by each PC
pca.var<-pca.out$sdev^2
pve=pca.var/sum(pca.var)
pve[1:10]
plot(pve[1:10],xlab="Principal Component",ylab="Proportion of Variance Explained",ylim=c(0,0.2),type='b')

# plot the cumulative proportion of variance explained by first n PCs
cpve<-cumsum(pve)
cpve[1:10]
plot(cpve,xlab="Principal Component",ylab="Cumulative Proportion of Variance Explained",ylim=c(0,1),type='b')

names(pca.out)
biplot(pca.out, scale=0,cex=0.6)

#create new z features as linear combinations of x features, using loadings of the PCAs
xmat<-as.matrix(x)
z<-xmat %*% pca.out$rotation
z<-data.frame(z)

# regress y on first 5 PCAs
yfit<-lm(y~PC1+PC2+PC3+PC4+PC5,data=z)


# create new z.test features from x.test
x.testmat<-as.matrix(x.test)
z.test<-x.testmat %*% pca.out$rotation
z.test<-as.data.frame(z.test)

# find mse of predictions of this linear model on y.test
ypred<-predict(yfit,newdata=z.test)
mse<-sum((ypred-y.test)^2)/length(y.test)
mse

library(hydroGOF)
mse(ypred,y.test) # could do mse in one using this

# now fit ordinarly least quares (OLS) on y and predict y.test
yfitOLS<-lm(y~.,data=x)
yfitOLSpred<-predict(yfitOLS,newdata=x.test)
mse<-sum((yfitOLSpred-y.test)^2)/length(y.test)
mse
mse(yfitOLSpred,y.test)
