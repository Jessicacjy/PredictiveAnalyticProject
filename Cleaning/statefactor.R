regions <- read.csv('c:/Users/Eric/Desktop/Predictive Analytics/Data/usregions.csv',sep = ',', header = T)
test <- merge(x = dona1, y = regions, by.x = 'STATCODE', by.y = 'State.Code', all.x = TRUE)

test$Region[is.na(test$Region)] <- "Sparse"
test$Sub.Region[is.na(test$Sub.Region)] <- "Sparse"
