                # Implementation of Boosted decision stump

#Call the library and split dataset into test and train
library(MASS)
data(Boston)
set.seed(2310)
index <- sample(1:nrow(Boston),nrow(Boston)/2)
train<- Boston[index,]
X_train_lstat <- Boston[index,"lstat"]
X_train_rm <- Boston[index,"rm"]
y_train <- Boston[index,"medv"]
X_train <- cbind(X_train_lstat,X_train_rm)
test <- Boston[-index,]
B=1000
# Implement BDS with constant value of eta and number of B
BDS <- function(B_val,trainData,label,test){
  fhat<- matrix(0,nrow=B,ncol=nrow(trainData),byrow = TRUE)
  different_mse<- matrix(nrow=B,ncol=1,byrow = TRUE)
  DS <- matrix(nrow=B,ncol=4,byrow = TRUE )
  for(i in 1:B_val){
    # Update the labels for each B i.e., residual
    DS[i,]<- DecisionStump1(label)
    for(j in 1:nrow(trainData)){
      
      if(DS[i,1]=="lstat"){
        if(trainData$lstat[j] <  as.numeric(DS[i,2])){
          fhat[i,j] <- as.numeric(DS[i,3])
          
        }else{
          fhat[i,j] <- as.numeric(DS[i,4])
        }
      }
      else if(DS[i,1]=="rm"){
        if(trainData$rm[j] < as.numeric(DS[i,2])){
          fhat[i,j] <- as.numeric(DS[i,3])
          
        }else{
          fhat[i,j] <- as.numeric(DS[i,4])
        }
      }
      label[j] <- label[j]-0.01*fhat[i,j]
      
    }
    
    
    sqdiff<-0
    for(each_val in 1:nrow(test)){
      prule = sum(0.01*fhat[,each_val])
      sqdiff =sqdiff+(test$medv[each_val]- (prule))^2  
    }
    test_mse=sqdiff/nrow(test)
    different_mse[i,1]=test_mse
    # plot(different_mse,B)
    print(c("TEST MSE: ",test_mse,"FOR B=",i))
  }
  plot(1:B,different_mse,'o',main="Different value of MSE vs B ")
}
BDS(B,train,y_train,test)
