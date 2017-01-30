setwd("~/401-Project1/Data")
library(data.table)
library(plyr)

dona <- read.csv('donation data.csv',sep = ',', header = T)
code <- read.csv('dmef1code.csv',sep = ',', header = T)
regions <- read.csv('usregions.csv',sep = ',', header = T)

IFTEST <- (1:length(dona$TARGDOL))%%3==0
IFDONATE <- dona$TARGDOL>0
dona <- cbind(dona,IFTEST,IFDONATE)

dona <- merge(x = dona, y = regions, by.x = 'STATCODE', by.y = 'State.Code', all.x = TRUE)

dona$Region[is.na(dona$Region)] <- "Sparse"
dona$Sub.Region[is.na(dona$Sub.Region)] <- "Sparse"
summary(dona)



dona1 <- merge(x = dona, y = code, by.x = 'CNCOD1', by.y = 'CODE', all.x = TRUE)
dona1 <- merge(x = dona1, y = code, by.x = 'CNCOD2', by.y = 'CODE', all.x = TRUE)
dona1 <- merge(x = dona1, y = code, by.x = 'CNCOD3', by.y = 'CODE', all.x = TRUE)
dona1 <- rename(dona1, c('CODETYPE.x' = 'CNTYPE1','CODETYPE.y' = 'CNTYPE2','CODETYPE'= 'CNTYPE3'))

dona1 <- merge(x = dona1, y = code, by.x = 'SLCOD1', by.y = 'CODE', all.x = TRUE)
dona1 <- merge(x = dona1, y = code, by.x = 'SLCOD2', by.y = 'CODE', all.x = TRUE)
dona1 <- merge(x = dona1, y = code, by.x = 'SLCOD3', by.y = 'CODE', all.x = TRUE)
dona1 <- rename(dona1, c('CODETYPE.x' = 'SLTYPE1','CODETYPE.y' = 'SLTYPE2','CODETYPE'= 'SLTYPE3'))

dona1[,37]<-dona1$CNTRLIF/dona1$CNTMLIF
names(dona1)[37]<-"averageamount"

dona1[,38]<-dona1$CNMONF/dona1$CNTMLIF
names(dona1)[38]<-"frequency"

dona2<-subset(dona1,select=-c(SLCOD3,CNCOD3,CNCOD2,CNDAT2,CNDAT3,CNCOD1,SLCOD1,SLCOD2,CNDOL2,CNDOL3,CNMON2,CNMON3,CNTYPE2,CNTYPE3,SLTYPE2,SLTYPE3,STATCODE))
write.csv(dona1,"12012120.csv")
