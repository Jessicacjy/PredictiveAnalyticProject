# load in data
trainset = read.csv('~/401-Project1/Data/trainset_standardized.csv')

# resample the data to make the dataset balanced
ntrain_t = sum(trainset$TARGDOL>0)
resample=trainset[(trainset$TARGDOL>0)==F, ]
set.seed(401)
resample=rbind(resample[sample(1:length(resample$TARGDOL),ntrain_t),],trainset[trainset$TARGDOL>0,])



# LIST OF VARIABLES
varlist = names(trainset)

# drop responses and IDs
varlist = varlist[-c(1,2,3,4,5,6,23,24,40)]

log_formula = 'TARGDOL~1'
for (i in 1:length(varlist)){
  log_formula = paste(log_formula,varlist[i],sep="+")
}

#formula
log_formula=as.formula(log_formula)

# lm and hatvalues
fit = lm(log_formula,data=resample)
p=length(fit$coefficients)
n=length(resample$TARGDOL)
h=hatvalues(fit)
h=as.data.frame(h)
hratio = 4

threshold = hratio*(p+1)/n
excludelistID = resample$ID[h>threshold]

resample = resample[-excludelistID, ]

write.csv(resample,'~/401-Project1/Data/trainset_resampled_noinfluential_standardized.csv')
write.csv(excludelistID,'~/401-Project1/Data/influentialID_in_resample.csv')


# FOR MULTILINEAR MODEL , lm and hatvalues
lmtrainset = subset(trainset,trainset$TARGDOL>0)
# in case if you want to include some target = 0 sample 
set.seed(401)
lmtrainset = rbind(trainset[sample(1:sum(trainset$TARGDOL==0),round(0.1*sum(trainset$TARGDOL>0))),  ],lmtrainset)

lmfit = lm(log_formula,data=lmtrainset)

p=length(lmfit$coefficients)
n=length(lmtrainset$TARGDOL)
h=hatvalues(lmfit)
h=as.data.frame(h)
hratio = 4

threshold = hratio*(p+1)/n
excludelistID_lm = lmtrainset$ID[h>threshold]

lmtrainset = lmtrainset[-excludelistID_lm, ]

write.csv(lmtrainset,'~/401-Project1/Data/trainset_multilinear_noinfluentialnoimputed_standardized.csv')
write.csv(excludelistID_lm,'~/401-Project1/Data/influentialID_noimputed_multilinear.csv')

