reg = read.csv('~/401-Project1/Data/regpredict_diagnosed_fullmodel2.csv')

#check MSE
reg$V4<-ifelse(reg$V3==0,1,reg$V3)
reg$V5<-reg$s0*0.39732633+2.232861e-01

sqrt(mean(( reg$V5 - log10(reg$V4) )^2))

sd(log10(reg$V5))
