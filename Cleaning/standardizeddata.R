# what if we standardize the data?
dona2 <- subset(dona1, select = -c(CNDAT2, CNDAT3, CNMON2, CNMON3, Region, CNTYPE2, CNTYPE3, SLTYPE2, SLTYPE3))

data.train <- dona2[dona2$IFTEST == FALSE, ]
data.train <- subset(data.train, select = -c(SEX, Sub.Region, CNTYPE1, SLTYPE1))
x.train <- subset(data.train, select = -c(TARGDOL, ID, IFTEST, IFDONATE, average.amount))
c.train <- data.train$IFDONATE #if donated
#n.train.c <- length(c.train) # 66134
y.train <- data.train$TARGDOL[data.train$IFDONATE == TRUE] # TARGDOL for those who donated
#n.train.y <- length(y.train) # 18123
#n.train.x <- length(x.train) # 18123
data.test <- dona2[dona2$IFTEST == TRUE, ]
data.test <- subset(data.test, select = -c(SEX, Sub.Region, CNTYPE1, SLTYPE1))
x.test <- data.test[data.test$IFDONATE == TRUE, ] # x for those who donated
x.test <- subset(data.test, select = -c(TARGDOL, ID, IFTEST, IFDONATE, average.amount))
c.test <- data.test$IFDONATE #if donated
n.test.c <- length(c.test) # 33066
y.test <- data.test$TARGDOL[data.test$IFDONATE == TRUE] # TARGDOL for those who donated in test
y.testfull <- data.test$TARGDOL
n.test.y <- length(y.test) # 9085

head(x.train)
# Mean for the training set of x variables
x.train.mean <- apply(x.train, 2, mean)
x.train.sd <- apply(x.train, 2, sd)
x.train.std <- t((t(x.train)-x.train.mean)/x.train.sd) # standardize to have zero mean and unit sd
data.train.std.c <- data.frame(x.train.std, donator=c.train) # to classify donor
data.train.std.y <- data.frame(x.train.std[c.train==1,], TARGDOL=y.train) # to predict TARGDOL when donated

x.test.std <- t((t(x.test)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.test.std.c <- data.frame(x.test.std, donator=c.test) # to classify donr
data.test.std.y <- data.frame(x.test.std[c.test==1,], TARGDOL=y.test) # to predict TARGDOL when donated
data.test.std.y.full <- data.frame(x.test.std, TARGDOL = y.testfull) # Test set, donated 0 or 1

# Try fitting a model 
dona_train <- data.train.std.y
dona_test <- data.test.std.y
model.step <- lm(TARGDOL ~., data = dona_train)
step <- stepAIC(model.step, direction="both")
step$anova # display results; note that the model is the same as above. Removes CNMON1, SEX, Sub.Region, CONLARG
summary(step) 

model.step <- lm(TARGDOL ~. -CNMON1 - CNTMLIF - CNDAT1, data = dona_train)
summary(model.step)
step.pred <- predict(model.step, dona_test) # predictions
mean((dona_test$TARGDOL - step.pred)^2) # mean prediction error 25.11
sd((dona_test$TARGDOL - step.pred)^2)/sqrt(length(dona_test$TARGDOL)) # std error 1.90





# How does this model perform? 

# Read in the values from the logistic regression 
logpred <- read.csv("C:/Users/Eric/Documents/GitHub/401-Project1/Data/logistic_prediction_1127_1.csv", sep = ",", header = TRUE)
# Run our prediction model on the full test set 
pred <- predict(model.step, data.test.std.y.full)
data.test.std.y.full$pred <- pred

# Values are by the ID (take logvalues * regvalues to get EV)
logprob <- logpred$donate.logpredict
ev <- logprob*pred
# Add EV to our test set table
dona_test <- data.test.std.y.full
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



dona_test <- data.test.std.y.full
# LDA model with standardized data. Performing not well (10000 vs 23617)
c.train <- data.train.std.c


c.test <- data.test.std.c
ldafit <- lda(donator ~. - CNDAT1, data = data.train.std.c)
#ldafit

# Apply the discriminant functions to the donations data(training set first)
Ypred <- predict(ldafit, c.train)
# Create a table for the true values and predicted values
ctable <- table(c.train$donator, Ypred$class)
ctable
# Calculate the correct classification rate
correctrate<- sum(diag(ctable)[1:2])/nrow(c.train)
cat("The correct classification rate: ", round(correctrate*100, 2), "%", sep = "")

# How does it do on the test data? 
Ypred <- predict(ldafit, c.test)
head(c.test)
# Create a table for true and predicted values
ctable <- table(c.test$donator, Ypred$class)
ctable
correctrate<- sum(diag(ctable)[1:2])/nrow(c.test)
cat("The correct classification rate: ", round(correctrate*100, 2), "%", sep = "")
# Problem with the model. Prediciting too many donators as non-donators

# How does this perform overall?
#regpred <- predict(pls.fit, dona_test,ncomp=5)
dona_test$pred <- regpred

# Values are by the ID (take logvalues * regvalues to get EV)
logprob <- predict(ldafit, c.test)$posterior[,2] # Post. probs for the test set
ev <- logprob*pred
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

