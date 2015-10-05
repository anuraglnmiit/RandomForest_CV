evaluate_cv_error<-function(pred,testVal,metric,k){
  
    err.vect = rep(NA,nfolds)
  
    for(i in 1:nfolds){
      if(metric=auc){
        err.vect[i] = roc.area(cv.test[,1], prediction)$A 
        print(paste("AUC for fold", i, ":", err.vect[i]))
        
      }
      else if (metric=logloss){
        err.vect[i]=(-(sum(log(1-pred[testVal==0])) + sum(log(pred[testVal==1])))/length(pred))
      }
      else{
        return 0
      }
    }
  }



c<-function(Data_Train,metric,nfolds){
  
  n = floor(nrow(train)/nfolds)
  
  for(i in 1:nfolds){
    s1 = ((i - 1) * n+1) 
    s2 = (i * n)      
    subset = s1:s2   
    
    cv.train = train[-subset,]  
    cv.test = train[subset,] 
    
    
    fit = randomForest(x = cv.train[,-1], y = as.factor(cv.train[,1]))
    prediction = predict(fit, newdata = cv.test[,-1], type = "prob")[,2]
    
    testVal<-cv.test[,1]
    
    evaluate_cv_error(prediction,testVal,metric,nfolds)
  }
  
  
}


# TestRun
RandomForestCV(train,auc,5)

