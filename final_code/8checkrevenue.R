reg = read.csv('~/401-Project1/Data/regpredict_diagnosed_fullmodel.csv')
log = read.csv('~/401-Project1/Data/logitpredict_lasso_diagonsed_full.csv')
predfull = cbind(reg,log)
predfull = cbind(predfull,predfull$s0*predfull$p_logistic)
names(predfull)[9] = 'product_pred'
predfull=predfull[order(-predfull$product_pred),]
sum(predfull$TARGDOL[1:1000])
