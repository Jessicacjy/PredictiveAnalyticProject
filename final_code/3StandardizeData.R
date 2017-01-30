donate.data=read.csv('~/401-Project1/Data/parseddata1201.csv')
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

#get the center and sd of each column
varlist = c('ID', "TARGDOL","IF_TARGET_5","IF_TARGETL5","IF_TARGET_10",'SLCOD1','CNDOL1','CNDOL2','CNDOL3',"CNDOL1","CNDOL2","CNDOL3","CNTRLIF","CONLARG","CONTRFST","SEX","CNMON1", "CNMON2","CNMON3","CNMONF","CNMONL" ,  "IFTEST", "IFDONATE" , "Sub.Region","Region",   
            "averageamount", "frequency",  "log_target",  "log_conlarg" , "log_cntrlif", "log_contrfst" ,"log_cnmonf", "log_cnmonl" ,"fac_cntmlif" ,"log_average" ,"log_frequency" ,"IF_MISSING_CNDOL2","IF_MISSING_CNDOL3"  )
log_formula = 'X~1'
for (i in 1:length(varlist)){
  log_formula = paste(log_formula,varlist[i],sep="+")
}
log_formula=as.formula(log_formula)
matrixfirst=model.matrix(log_formula,data=donate.data)[,-1]
matrixfirst_train = matrixfirst[which(donate.data$IFTEST==0),]
matrixfirst_test = matrixfirst[donate.data$IFTEST==1,]

meanlist_for_standardize = colMeans(matrixfirst_train)
meanlist_for_standardize[1:5]=0
sdlist_for_standardize = sqrt(apply(matrixfirst_train,2,var))
sdlist_for_standardize[1:5]=1

#standardize both the train set and the test set
xxx=t(t(matrixfirst_train) - meanlist_for_standardize)
matrixfirst_train=t(t(xxx)/sdlist_for_standardize)
matrixfirst_train=as.data.frame(matrixfirst_train)
xxx=t(t(matrixfirst_test) - meanlist_for_standardize)
matrixfirst_test=t(t(xxx)/sdlist_for_standardize)
matrixfirst_test=as.data.frame(matrixfirst_test)

#write into csv
write.csv(matrixfirst_train,"~/401-Project1/Data/trainset_standardized.csv")
write.csv(matrixfirst_test,"~/401-Project1/Data/testset_standardized.csv")