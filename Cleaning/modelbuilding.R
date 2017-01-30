### EDA and model building for 401 project ###
# Data
donations <- read.csv("C:/Users/Eric/Documents/GitHub/401-Project1/Data/1312.csv", sep = ",", header = TRUE)
str(donations)

# Libraries  
library(car)
library(MASS)
library(glmnet) 
library(pls)
set.seed(1) 


# Remove the unneeded variables, and subset the TARGDOL > 0 
dona1 <- donations[,9:37]
#dona1 <- subset(dona1, IFDONATE == TRUE)

# Transform the variables CNDOL2 and CNDOL3
# Set them to 0 if the number of lifetime contributions is less than 2 or 3. 
# Otherwise, set the value to the average amount of their lifetime donations
n <- nrow(dona1)
for(i in 1:n){
  # check for NA in 2
  if(is.na(dona1$CNDOL2[i])){
    # If lifetime is 1, set second donation to 0, otherwise set second to the average amount
    ifelse(dona1$CNTMLIF[i] == 1,dona1$CNDOL2[i] <- 0, dona1$CNDOL2[i] <- dona1$average.amount[i])
  }
  # Check for NA in 3
  if(is.na(dona1$CNDOL3[i])){
    # If lifetime is 2, set third donation to 0, otherwise set third to the average amount
    ifelse(dona1$CNTMLIF[i] == 1,dona1$CNDOL3[i] <- 0, dona1$CNDOL3[i] <- dona1$average.amount[i])
  }
}
colSums(is.na(dona1))
# Remaining NAs in CNDAT2,3 CNMON2,3 CNTYPE2,3 and SLTYPE2,3
# Can't impute, so exclude them
dona2 <- subset(dona1, select = -c(CNDAT2, CNDAT3, CNMON2, CNMON3, Region, CNTYPE2, CNTYPE3, SLTYPE2, SLTYPE3))
dona_train <- subset(dona2, IFTEST == FALSE)
c.train <- subset(dona2, IFTEST == FALSE)
c.test <- subset(dona2, IFTEST == TRUE)
dona_train <- subset(dona_train, IFDONATE == TRUE)
dona_train <- subset(dona_train, select = - c(ID, IFTEST, IFDONATE, average.amount, CNMON1))
dona_test <- subset(dona2, IFTEST == TRUE)
dona_test <- subset(dona_test, select = -c(ID, IFTEST, IFDONATE, average.amount, CNMON1))
# Now we have our valid training and test set data (excludes unneeded variables)


y.train <- dona_train$TARGDOL # TARGDOL for donators
n.train.y <- length(y.train) # 18123

### We must be careful that multicollinearity is not introduced into the models ###

### Model Creation ###

## Model 1, 2: Multiple linear regression

model.lr <- lm(TARGDOL ~. , data = dona_train)
summary(model.lr) 

# Remove the insignificant and dependent variable
model.lr <- lm(TARGDOL ~. - CONLARG - SEX - Sub.Region, data = dona_train)
summary(model.lr)

lr.pred <- predict(model.lr, dona_test) # validation predictions
mean((dona_test$TARGDOL - lr.pred)^2) # mean prediction error 25.27
sd((dona_test$TARGDOL - lr.pred)^2)/sqrt(length(dona_test$TARGDOL)) # std error 1.933

# Check assumptions
par(mfrow=c(2,2))
plot(model.lr)               
vif(model.lr) 
# Assumptions of homoskedasticity are violated. Normality doesn't look great either. 



## Heteroskedasticity-robust standard error calculation.
## All credit for this funciton is given to Dr. Ott Toomet (posted online and allowed to use)
summaryw <- function(model) {
  s <- summary(model)
  X <- model.matrix(model)
  u2 <- residuals(model)^2
  XDX <- 0
  
  ## Here one needs to calculate X'DX. But due to the fact that
  ## D is huge (NxN), it is better to do it with a cycle.
  for(i in 1:nrow(X)) {
    XDX <- XDX + u2[i]*X[i,]%*%t(X[i,])
  }
  
  # inverse(X'X)
  XX1 <- solve(t(X)%*%X)
  
  # Variance calculation (Bread x meat x Bread)
  varcovar <- XX1 %*% XDX %*% XX1
  
  # degrees of freedom adjustment
  dfc <- sqrt(nrow(X))/sqrt(nrow(X)-ncol(X))
  
  # Standard errors of the coefficient estimates are the
  # square roots of the diagonal elements
  stdh <- dfc*sqrt(diag(varcovar))
  
  t <- model$coefficients/stdh
  p <- 2*pnorm(-abs(t))
  results <- cbind(model$coefficients, stdh, t, p)
  dimnames(results) <- dimnames(s$coefficients)
  results
}

# see heteroskedasticity-robust standard errors!
test <- summaryw(model.lr)[,4]
for(i in 1:length(test)){
  if(test[i] > .05){print(test[i])}
}
# Recall, heteroskedasticity will inflate the significance of predictors
# Remove the insignificant variables according to above
# Model 2, also multiple regression, take into account heteroskedasticity
model.lr <- lm(TARGDOL ~. - CONLARG - SEX - Sub.Region - CONTRFST - CNDAT1 - CNTMLIF, data = dona_train)
summary(model.lr)

lr.pred <- predict(model.lr, dona_test) # validation predictions
mean((dona_test$TARGDOL - lr.pred)^2) # mean prediction error 25.6
sd((dona_test$TARGDOL - lr.pred)^2)/sqrt(length(dona_test$TARGDOL)) # std error 1.88 (slightly decreased)

# Check assumptions
par(mfrow=c(2,2))
plot(model.lr)               
vif(model.lr) 
# VIFs look somewhat better

## Model 3: Stepwise regression (using AIC as the decision)

model.step <- lm(TARGDOL ~., data = dona_train)
step <- stepAIC(model.step, direction="both")
step$anova # display results; note that the model is the same as above. Removes CNMON1, SEX, Sub.Region, CONLARG
summary(step) 

model.step <- lm(TARGDOL ~. - CNDAT1 - SEX - Sub.Region, data = dona_train)
summary(model.step)
step.pred <- predict(model.step, dona_test) # predictions
mean((dona_test$TARGDOL - step.pred)^2) # mean prediction error 25.11
sd((dona_test$TARGDOL - step.pred)^2)/sqrt(length(dona_test$TARGDOL)) # std error 1.90

# Check assumptions
par(mfrow=c(2,2))
plot(model.step)               
vif(model.step)

#Large VIF for CONLARG (12), but the rest look ok. 

## Model 4,5: Ridge/lasso Regression using 10 fold CV
# Must ingore the factor variables to coerce to a matrix 
str(dona_train)
x <- as.matrix(subset(dona_train, select = -c(TARGDOL, SEX, Sub.Region, CNTYPE1, SLTYPE1)))
y <- as.matrix(subset(dona_train, select = TARGDOL))
testx <- as.matrix(subset(dona_test, select = -c(TARGDOL, SEX, Sub.Region, CNTYPE1, SLTYPE1)))
testy <- as.matrix(subset(dona_test, select = TARGDOL))

# For (0 = ridge, 1 = lasso)
cv.out <- cv.glmnet(x, y, alpha=0, nfolds=10)
#cv.out <- cv.glmnet(x, y, alpha=1, nfolds=10)
bestlambda <- cv.out$lambda.1se
ridge <- glmnet(x, y, alpha=0) 
#lasso <- glmnet(x,y,alpha = 1)
pred.model <- predict(ridge, s=bestlambda, testx)
#pred.model <- predict(lasso, s = bestlambda, testx)
mean((testy - pred.model)^2) # mean prediction error 26.04, 25.79
sd((testy-pred.model)^2)/sqrt(length(testy)) # std error 3.12, 1.98



# What if we try a new regression model? 
# Partial least squares
# This model performs best if all data is included 
pls.fit <- plsr(TARGDOL ~. , data = dona_train, validation="CV")
summary(pls.fit)
pls.pred <- predict(pls.fit, dona_test,ncomp=5)
mean((dona_test$TARGDOL - pls.pred)^2) # mean prediction error 26.68
sd((dona_test$TARGDOL - pls.pred)^2)/sqrt(nrow(dona_test)) # std error 1.91

### Results ###
###############
#Model## Mean pred error## std error ##
# 1 # 25.27 # 1.933 #
# 2 # 25.61 # 1.881 #
# 3 # 25.11 # 1.902 #
# 4 # 26.04 # 3.122 #
# 5 # 25.79 # 1.981 #

# Selecting the model with the lowest standard error --> Model 2 
# This is the model created from fitting multiple linear regression and taking into account heteroskedasticity. 



# How does this model perform? 

# Read in the values from the logistic regression 
logpred <- read.csv("C:/Users/Eric/Documents/GitHub/401-Project1/Data/logistic_prediction_1123_2.csv", sep = ",", header = TRUE)
# Run our prediction model on the full test set 
pls.pred <- predict(pls.fit, dona_test,ncomp=5)
dona_test$pred <- pls.pred
dona_test$pred <- predict(model.lr, dona_test)

# Values are by the ID (take logvalues * regvalues to get EV)
logprob <- logpred$donate.logpredict
regvalue <- dona_test$pred
ev <- logprob*regvalue
# Add EV to our test set table
dona_test$EV <- ev
head(dona_test)

# How does this perform overall?
# Check the mean squared errors and the standard errors of the predictions
# Note; The predictions will be the value of EV
mean((dona_test$TARGDOL - dona_test$EV)^2) # mean prediction error 31.16
sd((dona_test$TARGDOL - dona_test$EV)^2)/sqrt(length(dona_test$TARGDOL)) # std error 1.613 

# How does the model perform is selecting the top 1000 donors? 
ordervalues <- dona_test[with(dona_test, order(-EV)), ]
head(ordervalues)
top1000pred <- sum(ordervalues$TARGDOL[1:1000])
top1000test <- sum(tail(sort(dona_test$TARGDOL), 1000))
cat("Pred rev:", top1000pred)
cat("Actual rev:", top1000test)







# LDA model 
#str(c.train)
ldafit <- lda(IFDONATE ~.- TARGDOL - CNDOL1 - CNTRLIF - CONLARG - CNDAT1 - CNMON1 - ID - IFTEST, data = c.train)
#ldafit

# Apply the discriminant functions to the donations data(training set first)
Ypred <- predict(ldafit, c.train)
# Create a table for the true values and predicted values
ctable <- table(c.train$IFDONATE, Ypred$class)
ctable
# Calculate the correct classification rate
correctrate<- sum(diag(ctable)[1:2])/nrow(c.train)
cat("The correct classification rate: ", round(correctrate*100, 2), "%", sep = "")

# How does it do on the test data? 
Ypred <- predict(ldafit, c.test)

# Create a table for true and predicted values
ctable <- table(c.test$IFDONATE, Ypred$class)
ctable
correctrate<- sum(diag(ctable)[1:2])/nrow(c.test)
cat("The correct classification rate: ", round(correctrate*100, 2), "%", sep = "")
# Problem with the model. Prediciting too many donators as non-donators

# How does this perform overall?
#regpred <- predict(pls.fit, dona_test,ncomp=5)
regpred <- predict(model.lr, dona_test)
dona_test$pred <- regpred

# Values are by the ID (take logvalues * regvalues to get EV)
logprob <- predict(ldafit, c.test)$posterior[,2] # Post. probs for the test set
ev <- logprob*regpred
# Add EV to our test set table
dona_test$EV <- ev
head(dona_test)


# Check the mean squared errors and the standard errors of the predictions
# Note; The predictions will be the value of EV
mean((dona_test$TARGDOL - dona_test$EV)^2) # mean prediction error 31.16
sd((dona_test$TARGDOL - dona_test$EV)^2)/sqrt(length(dona_test$TARGDOL)) # std error 1.613 

# How does the model perform is selecting the top 1000 donors? 
head(dona_test)
ordervalues <- dona_test[with(dona_test, order(-EV)), ]
head(ordervalues)
top1000pred <- sum(ordervalues$TARGDOL[1:1000])
top1000test <- sum(tail(sort(dona_test$TARGDOL), 1000))
cat("Pred rev:", top1000pred)
cat("Actual rev:", top1000test)



