# Inputs: testdata, model, n = number of donators to predict

revenueprediction <- function(testdata, model, n){
  predicts <- predict(model, new = testdata)
  
  # Revenue from top 1000 predictions vs Revenue from the top 1000 test set
  recommend <- testdata[names(tail(sort(predicts), n)), 'TARGDOL']
  cat("Predicted revenue:", sum(recommend), "\n")
  
  cat("Actual revenue from the top 1000:", sum(tail(sort(testdata$TARGDOL), n)),'\n')
}
