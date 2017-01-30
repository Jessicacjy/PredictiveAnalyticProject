donate.data=read.csv('parseddata1201.csv')
donate.data$fac_cntmlif=as.factor(donate.data$fac_cntmlif)
donate.data=cbind(donate.data,donate.data$TARGDOL>=5)
names(donate.data)[50]='IF_TARGET_5'
donate.data=cbind(donate.data,donate.data$TARGDOL>5)
names(donate.data)[51]='IF_TARGETL5'

donate.data=cbind(donate.data,donate.data$TARGDOL>=10)
names(donate.data)[52]='IF_TARGET_10'

donate.data=cbind(donate.data,is.na(donate.data$CNDOL2),is.na(donate.data$CNDOL3))
names(donate.data)[53]='IF_MISSING_CNDOL2'
names(donate.data)[54]='IF_MISSING_CNDOL3'

donate.data$CNDOL2[is.na(donate.data$CNDOL2)]=0
donate.data$CNDOL3[is.na(donate.data$CNDOL3)]=0

donate.data$CNMON2[is.na(donate.data$CNMON2)]=170
donate.data$CNMON3[is.na(donate.data$CNMON3)]=170

# split and standardize data
factorcol = c(1,2,3,4,5,6,7,8,9,21,27,28,29,30,31,33,34,35,36,37,38,47,50,51,52,53,54)
donate.factor = donate.data[,factorcol]
donate.numerical = donate.data[,-factorcol]
donate.numerical = scale(donate.numerical)
donate.data=cbind(donate.numerical,donate.factor)


donate.train=donate.data[donate.data$IFTEST==F,]
ntrain_t = sum(donate.train$IF_TARGET_10)
donate.resample=donate.train[donate.train$IF_TARGET_10==F, ]
set.seed(401)
donate.resample=rbind(donate.resample[sample(1:length(donate.resample$IF_TARGET_10),ntrain_t),],donate.train[donate.train$IF_TARGET_10==TRUE,])


#generate a main_effect formula
#varlist = c('CNDOL1','CNDOL2','CNDOL3','CNMON1','CNMON2','CNMON3',"CNMONF","CNMONL","CNTMLIF",
#          "CNTRLIF","CONLARG","CONTRFST","SEX","Sub.Region","CNTYPE1","SLTYPE1","average.amount","frequency")
varlist = c('CNDOL1','CNDOL2','CNDOL3','CNMON1','CNMON2','CNMON3',"CNMONF","CNMONL","CNTMLIF",
            "CNTRLIF","SEX","Sub.Region","CNTYPE1","SLTYPE1","average.amount","frequency",
            "log_conlarg","log_contrfst","log_average")
log_formula = 'IF_TARGET_10~1'
for (i in 1:length(varlist)){
  log_formula = paste(log_formula,varlist[i],sep="+")
}

#add quadratic
contvarlist = c('CNDOL1','CNDOL2','CNDOL3','CNMON1','CNMON2','CNMON3',"CNMONF","CNMONL","CNTMLIF",
                "CNTRLIF",
                "log_conlarg","log_contrfst","log_average")
#"CONLARG","CONTRFST","average.amount","frequency")

for (i in 1:length(contvarlist)){
  log_formula = paste(log_formula,'+I(',contvarlist[i],'^2)')
}

#add interaction terms
for (i in 1:(length(varlist)-1)){
  
  for (j in (i+1):length(varlist)){
    log_formula=paste(log_formula,paste(varlist[i],varlist[j],sep='*'),sep='+')
  }
}

#formula
log_formula=as.formula(log_formula)


#standardize the data matrix
library(glmnet)
donate.test = donate.data[donate.data$IFTEST,]
donate.resamplematrix = model.matrix(log_formula,data=donate.resample)
donate.testmatrix = model.matrix(log_formula,data=donate.test)

donate.trainmatrix = model.matrix(log_formula,data=donate.train)

cv.glmmod = cv.glmnet(x=donate.resamplematrix,y=as.numeric(donate.resample$IF_TARGET_10),alpha=1,nfold=5,family = 'binomial')
plot(cv.glmmod)
donate.lassomodel = glmnet(x=donate.resamplematrix,y=as.numeric(donate.resample$IF_TARGET_10),alpha=1,lambda = cv.glmmod$lambda.min,family = 'binomial')

#trial on regression model
donate.trainforreg=donate.train[donate.train$IFDONATE,]
donate.regtrainmatrix = model.matrix(log_formula,data=donate.trainforreg)
cv.regression = cv.glmnet(x=donate.trainmatrix,y=donate.train$log_target,alpha=1,family = 'gaussian',nfolds = 5)
plot(cv.regression)
donate.cvregmodel = glmnet(x=donate.trainmatrix,y=donate.train$log_target,alpha=1,family = 'gaussian',lambda=cv.regression$lambda.min)
donate.cvregpredict = predict(donate.cvregmodel,newx=donate.testmatrix)
plot(donate.test$TARGDOL,donate.cvregpredict)

write.csv(donate.cvregpredict,'regcvprediction_1129.csv')
#stepwise
#donate.nulllogistic = glm(IF_TARGET_10~1,family = binomial,data=donate.resample)
#donate.fulllogistic = glm(log_formula,family = binomial,data=donate.resample)
#donate.steplogistic = step(donate.nulllogistic,scope = list(lower=donate.nulllogistic,upper=donate.fulllogistic),direction = 'both')
#(step(house.nullmodel,scope=list(lower=house.nullmodel,upper=house.model),direction = 'forward')



#normal logistic
#log_formula = IF_TARGET_10~log_cnmonl+log_cntrlif+SEX+Sub.Region+CNTYPE1+SLTYPE1+CONTRFST+CONLARG+CNMONF+CNMONL+average.amount+frequency+I(log_cnmonl^2)+I(log_cntrlif^2)+CNTYPE1*frequency+SLTYPE1*frequency
#donate.logistic = glm(log_formula,family = binomial,data=donate.resample)
#donate.logistic = glm(IF_TARGET_10~CNMON1+SEX+Sub.Region+CNTYPE1+SLTYPE1+CONTRFST+CONLARG+CNTRLIF+CNMONF+CNMONL+fac_cntmlif+average.amount+frequency+I(CNMON1^2)+CNTYPE1*frequency+SLTYPE*frequency,family = binomial,data=donate.resample)
#                                   22    21    29        32      35     13         12    11      25      26      46           38               40                                                                   


#resample2
donate.resample2 = donate.train[donate.train$IFDONATE,]
donate.logistic = glm(IF_TARGETL5~CNMON1+SEX+Sub.Region+CNTYPE1+SLTYPE1+CONTRFST+CONLARG+CNTRLIF+CNMONF+CNMONL+fac_cntmlif+average.amount+frequency,family = binomial,data=donate.resample2)


donate.logistic = multinom(IFDONATE~SEX+Sub.Region+CNTYPE1+SLTYPE1+log_conlarg+log_cntrlif+log_cnmonf+log_cnmonl+fac_cntmlif+log_average+log_frequency,family = binomial,data=donate.data,subset=donate.data$IFTEST==FALSE)

#accuracy:train
donate.resamplepredict = predict(donate.steplogistic,newdata=donate.resample[,-51],type='response')
t = table(donate.resamplepredict>0.5,donate.resample$IF_TARGET_10)
t
sum(diag(t))/sum(t)


#accuracy:test
#donate.logpredict = predict(donate.logistic,newdata=donate.data[donate.data$IFTEST,c(21,29,32,35,13,12,25,26,42,45,38,40)],type='response')
donate.logpredict = predict(donate.lassomodel,newx=donate.testmatrix,type='response')
t = table(donate.logpredict>0.5,donate.data$IF_TARGET_10[donate.data$IFTEST])
t
sum(diag(t))/sum(t)

# top 1000 predicts
ntop=1000
donate.test = donate.data[donate.data$IFTEST,]
testorder=order(-donate.test$TARGDOL)
donate.toptest=donate.test[testorder,]
topid=donate.toptest$ID[1:ntop]

df=as.data.frame(cbind(donate.logpredict,donate.test$TARGDOL,donate.test$ID))
df2=df[order(-df$V2),]
dftop=df2[1:ntop,]




names(df)[c(2,3)]=c('TARDOL','ID')


mean(dftop$s0>.5)

write.csv(df,'logistic_prediction_1127_1.csv')

# glmnet
library(glmnet)
donate.matrix = model.matrix(log_formula,data=donate.resample)
#donate.glmnet = glmnet(x=as.matrix(donate.data[donate.data$IFTEST==F,c(21,29,32,35,41,42,43,44,45,46,47,48)]),y=as.numeric(donate.data$IFDONATE[donate.data$IFTEST==F]),alpha=1,family = 'binomial')

donate.glmnet = glmnet(x=donate.matrix,y=as.numeric(donate.resample$IF_TARGET_10),alpha=1,family = 'binomial')
cv.glmmod = cv.glmnet(x=donate.matrix,y=as.numeric(donate.resample$IF_TARGET_10),alpha=1,family = 'binomial')
