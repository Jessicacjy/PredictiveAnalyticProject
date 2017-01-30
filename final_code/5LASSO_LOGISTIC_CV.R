# load in data
trainset = read.csv('~/401-Project1/Data/trainset_resampled_noinfluential_standardized.csv')
testset = read.csv('~/401-Project1/Data/testset_standardized.csv')

trainset=trainset[,-1]

# LIST OF VARIABLES
varlist = names(trainset)

# drop responses and IDs
varlist = varlist[-c(1,2,3,4,5,6,23,24,40)]

log_formula = 'TARGDOL~1'
for (i in 1:length(varlist)){
  log_formula = paste(log_formula,varlist[i],sep="+")
}


#add quadratic
for (i in 1:length(varlist)){
  log_formula = paste(log_formula,'+I(',varlist[i],'^2)')
}

#add interaction terms
for (i in 1:(length(varlist)-1)){
  
  for (j in (i+1):length(varlist)){
    log_formula=paste(log_formula,paste(varlist[i],varlist[j],sep='*'),sep='+')
  }
}

#formula
log_formula=as.formula(log_formula)



#generate the model as input for glmnet function
trainmatrix= model.matrix(log_formula,data=trainset)

testmatrix = model.matrix(log_formula,data=testset)



# donate.regtrainmatrix = model.matrix(log_formula,data=donate.trainforreg)
library(glmnet)
cv.regression = cv.glmnet(x=trainmatrix,y=trainset$IF_TARGET_5TRUE,alpha=1,family = 'binomial' ,nfolds = 5)
plot(cv.regression)
donate.cvregmodel = glmnet(x=trainmatrix,y=trainset$IF_TARGET_5TRUE,alpha=1,family = 'binomial',lambda=cv.regression$lambda.min)
donate.cvregpredict = predict(donate.cvregmodel,newx=testmatrix,type='response')


#store the prediction into csv file
logitpredict = as.data.frame(cbind(donate.cvregpredict,testset$ID,testset$TARGDOL))
names(logitpredict)=c('p_logistic','ID','TARGDOL')
write.csv(logitpredict,'~/401-Project1/Data/logitpredict_targetL5_diagonsed_full.csv')

#pickle the model
save(donate.cvregmodel,file='~/401-Project1/Data/logitpredict_targetL10_diagonsed_full.rda')


#accuracy:train
donate.resamplepredict = predict(donate.cvregmodel,newdata=trainmatrix,type='response')
t = table(donate.resamplepredict>0.5,resample$IF_TARGET_10TRUE)
t
sum(diag(t))/sum(t)


#accuracy:test
#donate.logpredict = predict(donate.logistic,newdata=donate.data[donate.data$IFTEST,c(21,29,32,35,13,12,25,26,42,45,38,40)],type='response')
donate.logpredict = predict(donate.lassomodel,newx=donate.testmatrix,type='response')
t = table(donate.cvregpredict>0.5,testset$IF_TARGET_10TRUE)
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

write.csv(df,'~/401-Project1/Data/P_NewLogisticModel.csv')