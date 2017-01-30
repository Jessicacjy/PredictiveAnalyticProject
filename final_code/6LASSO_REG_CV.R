# load in data
trainset = read.csv('~/401-Project1/Data/trainset_multilinear_noinfluentialnoimputed_standardized.csv')
testset = read.csv('~/401-Project1/Data/testset_standardized.csv')

trainset = trainset[,-1]
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

trainmatrix = model.matrix(log_formula,data=trainset)

testmatrix = model.matrix(log_formula,data=testset)



# donate.regtrainmatrix = model.matrix(log_formula,data=donate.trainforreg)
cv.regression = cv.glmnet(x=trainmatrix,y=trainset$log_target,alpha=1,family = 'gaussian',nfolds = 5)
plot(cv.regression)
donate.cvregmodel = glmnet(x=trainmatrix,y=trainset$log_target,alpha=1,family = 'gaussian',lambda=cv.regression$lambda.min)
donate.cvregpredict = predict(donate.cvregmodel,newx=testmatrix)

#store the prediction into csv file
regpredict = as.data.frame(cbind(donate.cvregpredict,testset$ID,testset$TARGDOL))
write.csv(regpredict,'~/401-Project1/Data/regpredict_diagnosed_fullmodel2.csv')

#pickle the model
save(donate.cvregmodel,file='~/401-Project1/Data/reg_diagnosed_full2.rda')
