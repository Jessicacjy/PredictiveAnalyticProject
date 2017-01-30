source('revenueprediction.R', echo=TRUE)
dona <- read.csv('1312.csv')
dona<-subset(dona,select=-c(average.amount,SLCOD1,SLCOD2,SLCOD3,CNCOD3,CNCOD2,CNCOD1,STATCODE,CNMON2,CNMON3))
dona_test <- subset(dona, IFTEST == TRUE & IFDONATE == TRUE)

dona3 <- na.omit(dona)

dona_test <- subset(dona, IFTEST == TRUE & IFDONATE == TRUE)
dona1<-subset(dona,select=-c(CNDAT2,CNDAT3))

dona1<-dona1[ , colSums(is.na(dona1)) == 0]


seperate_dona <- function(min, max, data){
data <- data[data$TARGDOL<max,]
data <- data[data$TARGDOL>min,]
return (data)
}


dona_fit <- function (train, test,n){
  dona.lm1 <- lm(TARGDOL~.-X-ID-Sub.Region-Region-IFTEST-IFDONATE-SEX,data = train)
  #dona.lm1 <- lm(TARGDOL~.-CNDAT1-CNDAT2-CNDAT3-X-CNDOL2-CNDOL3-Sub.Region-Region-IFTEST-IFDONATE-CNTYPE1-CNTYPE2-CNTYPE3,data = train)
  dona.lm2 <- lm(log(TARGDOL)~.-X-ID-CONLARG-CNDAT1-Sub.Region-Region-IFTEST-IFDONATE-CNTYPE1-SEX,data = train)
  print(summary(dona.lm1))
  print(summary(dona.lm2))
  return(dona.lm2)
}

train_test <- function (data,n){
  dona_train <- subset(data, IFTEST == FALSE & IFDONATE == TRUE)
  dona_fit(dona_train,dona_test,n)
}

#without edit any data, we have Predicted revenue: 16338.29 
#Actual revenue from the top 1000: 23617.14Predicted revenue: 16500.72 
#Actual revenue from the top 1000: 23617.14
logfit <- train_test(dona1, 1000)

par(mfrow = c(2,2))
plot(logfit)

logfit <- train_test(dona2,1000)
pred <- predict(logfit, newdata = dona_test)
dona2 <- seperate_dona(30,1500,dona1)
logfit2 <- train_test(dona2, 1000)

pred <- predict(logfit,dona_test)
lm_pred <- cbind(dona_test$ID,exp(pred))
write.csv(lm_pred,'lm_pred_1123.csv')
sqrt(mean((pred-dona_test$TARGDOL)^2))

log_pred <- read.csv('logistic_prediction_1123_2.csv')
log_pred_log = merge(log_pred,lm_pred,by.x = 'ID', by.y ='V1' )
log_pred_log$product<- log_pred_log$donate.logpredict*log_pred_log$V2
ordered_product <- log_pred_log[order(-log_pred_log$product),]
write.csv(log_pred_log,'log_pred_log_1123.csv')
sum(ordered_product[1:1000,'TARDOL'])
sum(ordered_product[1:1000,'product'])
ordered_product <- log_pred_log[order(-log_pred_log$TARDOL),]
sum(ordered_product[1:1000,'TARDOL'])
sum(ordered_product[1:1000,'product'])
