## R exercise for ISLR Ch7 quiz

## Non linear models


library(ISLR)

# load data
data<-load("./data/7.R_data_for_exercise.Rdata")

# plot with linear fit and 95% confidence interval
par(mfrow=c(1,1))
fit<-lm(y~x)
fit
xlims=range(x)
x.grid=seq(from=xlims[1],to=xlims[2],.1)
preds=predict(fit,newdata=list(x=x.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se,preds$fit-2*preds$se)
plot(x,y,col="darkgrey")
lines(x.grid,preds$fit,lwd=2,col="blue")
matlines(x.grid,se.bands,col="blue",lty=2)


fit<-lm(y~x + I(x^2))
fit
xlims=range(x)
x.grid=seq(from=xlims[1],to=xlims[2],.1)
preds=predict(fit,newdata=list(x=x.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se,preds$fit-2*preds$se)
plot(x,y,col="darkgrey")
lines(x.grid,preds$fit,lwd=2,col="blue")
matlines(x.grid,se.bands,col="blue",lty=2)