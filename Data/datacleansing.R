donate.data = read.csv('12012120.csv')
donate.data = cbind(donate.data,rep(0,length(donate.data$X)))
names(donate.data)[40]='log_target'
donate.data$log_target[donate.data$TARGDOL>0]=log10(donate.data$TARGDOL[donate.data$TARGDOL>0])

# frequency recalculate:
donate.data$frequency = (donate.data$CNMON1+1)/donate.data$CNTMLIF

# LOG: CONLARGE CNTRLIF CONTRFST CNMONF CNMONL
donate.data = cbind(donate.data,log(donate.data$CONLARG),log(donate.data$CNTRLIF),log(donate.data$CONTRFST+1),log(donate.data$CNMONF+1),log(donate.data$CNMONL+1))

names(donate.data)[c(41,42,43,44,45)]=c('log_conlarg','log_cntrlif','log_contrfst','log_cnmonf','log_cnmonl')
donate.data$log_contrfst=log(donate.data$CONTRFST+1)
# CNTMLIF  1  2  3 4-10 >10
donate.data = cbind(donate.data,rep(0,length(donate.data$X)))
names(donate.data)[46]='fac_cntmlif'



for (i in 1:length(donate.data$X)){
  
  
  if (donate.data$CNTMLIF[i]==1){
    donate.data$fac_cntmlif[i]=1
  } 
  else if (donate.data$CNTMLIF[i]==2){
    donate.data$fac_cntmlif[i]=2
  } 
  else if (donate.data$CNTMLIF[i]==3){
    donate.data$fac_cntmlif[i]=3
  } 
  else if (donate.data$CNTMLIF[i]>=4 && donate.data$CNTMLIF[i]<=10 ){
    donate.data$fac_cntmlif[i]=4
  } 
  else {
    donate.data$fac_cntmlif[i]=5
  }
}

donate.data$fac_cntmlif = as.factor(donate.data$fac_cntmlif)


# log average/frequency
donate.data = cbind(donate.data,log(donate.data$averageamount),log(donate.data$frequency))
names(donate.data)[c(47,48)]=c('log_average','log_frequency')
