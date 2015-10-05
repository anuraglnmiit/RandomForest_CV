
#Custom k-fold cross validation for Random Forest Algorithm

#Importing Required Packages
#install.packages('randomForest')
library(randomForest)
#install.packages('verification')
library(verification)


#Function to evaluate Cross Validation Error Score based on the input metric, we choose the two most commonly used metrics
#classification problem
evaluate_cv_score<-function(pred,testVal,metric,k){
    err.vect = rep(NA,nfolds)       #Initializing a vector (with NA values) that accumulates cv scores. 
    for(i in 1:nfolds){
      if(metric=auc){
        err.vect[i] = roc.area(cv.test[,1], prediction)$A  #Calculating Area under Receiver Order Characteristics Curve
        print(paste("AUC for fold", i, ":", err.vect[i]))
        return mean(err.vect[i])   #Returning the average AUC value.
      }
      else if (metric=logloss){
        err.vect[i]=(-(sum(log(1-pred[testVal==0])) + sum(log(pred[testVal==1])))/length(pred)) #Calculating accumulated logarithmic loss
        return mean(err.vect[i])   #Returning the average logarithmic loss score
     }
      else{
        return 0
      }
    }
  }


#Function to slice and bifurcate k-folds and subsequently train the Random Forest and carry out predictions
RandomForestCV <-function(Data_Train,metric,nfolds){
  
  n = floor(nrow(train)/nfolds)   # n would be the size of each fold
  
  for(i in 1:nfolds){    # We run for 'n'folds
    s1 = ((i - 1) * n+1) #s1 and s2 are the starting and ending indices for each folds 
    s2 = (i * n)      
    subset = s1:s2   
    
    cv.test = train[subset,]    #Our testing space for each fold
    cv.train = train[-subset,]  #The rest is for training
    
    
    fit = randomForest(x = cv.train[,-1], y = as.factor(cv.train[,1])) #Training our Random Forest in the traditional way!
    
    prediction = predict(fit, newdata = cv.test[,-1], type = "prob")[,2] #Test Set prediction
    
    testVal<-cv.test[,1] 
    
    #We pass the arguments to evaluate Cross Validation score
    err.cv<-evaluate_cv_score(prediction,testVal,metric,nfolds)
    return err.cv
  }
}

# TestRun
CVScore<-RandomForestCV(train,auc,5)
#print(CVScore)
