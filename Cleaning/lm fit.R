source('~/Cleaning/revenueprediction.R', echo=TRUE)
dona1 <- read.csv('1312.csv')
dona1<-subset(dona1,select=-c(average.amount,SLCOD1,SLCOD2,SLCOD3,CNCOD3,CNCOD2,CNCOD1,STATCODE,CNMON1,CNMON2,CNMON3,CNDAT2,CNDAT3,ID))

dona1<-dona1[ , colSums(is.na(dona1)) == 0]
dona_test <- subset(dona1, IFTEST == TRUE & IFDONATE == TRUE)

seperate_dona <- function(min, max, data){
data <- data[data$TARGDOL<max,]
data <- data[data$TARGDOL>min,]
return (data)
}


dona_fit <- function (train, test,n){
  dona.lm1 <- lm(TARGDOL~.-X-Sub.Region-Region-IFTEST-IFDONATE-SEX,data = train)
  #dona.lm1 <- lm(TARGDOL~.-CNDAT1-CNDAT2-CNDAT3-X-CNDOL2-CNDOL3-Sub.Region-Region-IFTEST-IFDONATE-CNTYPE1-CNTYPE2-CNTYPE3,data = train)
  dona.lm2 <- lm(log(TARGDOL)~.-X-CONLARG-CNDAT1-Sub.Region-Region-IFTEST-IFDONATE-CNTYPE1-SEX,data = test)
  print(summary(dona.lm1))
  print(summary(dona.lm2))
  revenueprediction(test,dona.lm1,n)
  revenueprediction(test,dona.lm2,n)
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

dona2 <- seperate_dona(30,1500,dona1)
logfit2 <- train_test(dona2, 1000)

pred <- predict(logfit2,dona_test)
sqrt(mean((pred-dona_test$TARGDOL)^2))

stepmodel <- step(logfit2, direction = "both")
