par(mfrow=c(1,2))
for (i in c(10:29,32:38,40:48)){plot(donate.data[,i],donate.data[,20],xlab=paste("x", i),ylab="y",main=paste("y vs x", i))}

# residual vs fitted
par(mfrow=c(1,1))
plot(predict(donate.logistic,newdata=donate.resample[,c(22,21,29,32,35,13,12,11,25,26,46,38,40)],
             type='response'),residuals(donate.logistic))

#standardized residual vs ID
a<-rstandard(donate.logistic)
b<-data.frame(cbind(donate.resample$ID,a))

plot(donate.resample$ID,a)
ind <- which(abs(b$a)>10) 
b[ind,]

library(car)
vif(donate.logistic)

library(pROC)
plot.roc(donate.resample$IF_TARGET_10,donate.logistic$fitted.values,xlab="1-Specificity")


library(dplyr)
x<-sample_n(donate.data,1000)
top1000test <- sum(x$TARGDOL)
top1000test
