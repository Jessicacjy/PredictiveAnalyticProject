reg = read.csv('regpredict_diagnosed_fullmodel2.csv')
log = read.csv('logitpredict_targetL5_diagonsed_full.csv')
predfull = cbind(reg,log)
predfull = cbind(predfull,predfull$s0*predfull$p_logistic)
names(predfull)[9] = 'product_pred'
predfull=predfull[order(-predfull$product_pred),]
sum(predfull$TARGDOL[1:1000])
