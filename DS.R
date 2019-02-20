		#Implementation of Decision Stump

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
DecisionStump1 <- function(y_train) {
  s_lstat_train <- seq(1.8, 37.9, 0.1)
  best_s_lstat = Inf
  best_threshold_lstat = 0
  
  s_rm_train <- seq(3.6, 8.7, 0.1)
  best_s_rm = Inf
  best_threshold_rm = 0
  
  for (j in s_lstat_train) {
    index_lstat_train_less = c()
    index_lstat_train_more = c()
    for (i in 1:length(y_train)){
      if (X_train_lstat[i] < j){
        index_lstat_train_less <- c(index_lstat_train_less,i)
      }
      else {
        index_lstat_train_more <- c(index_lstat_train_more,i)
      }
    }
    y_train_lstat_less <- y_train[index_lstat_train_less]
    y_train_lstat_more <- y_train[index_lstat_train_more]
    mean_lstat_less <- mean(y_train_lstat_less)
    mean_lstat_more <- mean(y_train_lstat_more)
    RSS_lstat <- sum((y_train_lstat_less - mean_lstat_less)^2) + sum((y_train_lstat_more - mean_lstat_more)^2) 
    
    if (RSS_lstat < best_s_lstat ){
      best_s_lstat = RSS_lstat
      best_threshold_lstat = j
      best_lstat_mean_less = mean_lstat_less
      best_lstat_mean_more = mean_lstat_more
    }
  }
  
  for (j in s_rm_train) {
    index_rm_train_less = c()
    index_rm_train_more = c()
    for (i in 1:length(y_train)){
      if (X_train_rm[i] < j){
        index_rm_train_less <- c(index_rm_train_less,i)
      }
      else {
        index_rm_train_more <- c(index_rm_train_more,i)
      }
    }
    # Compute the less than and greater than mean values for rm
    y_train_rm_less <- y_train[index_rm_train_less]
    y_train_rm_more <- y_train[index_rm_train_more]
    mean_rm_less <- mean(y_train_rm_less)
    mean_rm_more <- mean(y_train_rm_more)
    
    RSS_rm <- sum((y_train_rm_less - mean_rm_less)^2) + sum((y_train_rm_more - mean_rm_more)^2)
    
    # best RSS 
    if (RSS_rm < best_s_rm ){
      best_s_rm = RSS_rm
      best_threshold_rm = j
      best_rm_mean_less = mean_rm_less
      best_rm_mean_more = mean_rm_more
    }
  }
  
  best_s <- ifelse (best_s_rm < best_s_lstat, best_threshold_rm, best_threshold_lstat)
  best_mean_less <- ifelse(best_s_rm < best_s_lstat, best_rm_mean_less,best_lstat_mean_less)
  best_mean_more <- ifelse(best_s_rm < best_s_lstat, best_rm_mean_more,best_lstat_mean_more)
  best_attr <- ifelse(best_s_rm < best_s_lstat, "rm", "lstat")
  returnArray= c(best_attr,best_s,best_mean_less,best_mean_more)
  
}


DS<-DecisionStump1(y_train)
print(DS)

lstat_or_rm<-0
S_VAL=as.numeric(DS[2])
if(DS[1]=="lstat"){
  lstat_or_rm<-train$lstat
}else if(DS[1]=="rm"){
  lstat_or_rm<-train$lstat
}
mean_greater_tha=as.numeric(DS[4])
mean_less_tha=as.numeric(DS[3])

print(mean_greater_tha)
print(mean_less_tha)
# COMPUTATION OF TRAINING MSE
sumlessthanSquare <- sumgreaterthanSquare <- 0
for(i in 1:nrow(train)){
  if (lstat_or_rm[i] < S_VAL )
  {
    sumlessthanSquare <- sumlessthanSquare + (train$medv[i]-mean_less_tha)^2
  }
  if(lstat_or_rm[i] > S_VAL){
    sumgreaterthanSquare <-  sumgreaterthanSquare+(train$medv[i]-mean_greater_tha)^2
  }
}

final_train_rss=sumlessthanSquare + sumgreaterthanSquare
# COMPUTATION OF TRAINING MSE


lstat_or_rm<-0
S_VAL=as.numeric(DS[2])
if(DS[1]=="lstat"){
  lstat_or_rm<-test$lstat
}else if(DS[1]=="rm"){
  lstat_or_rm<-test$lstat
}
mean_greater_tha=as.numeric(DS[4])
mean_less_tha=as.numeric(DS[3])


# COMPUTATION OF TEST MSE
sumlessthanSquare <- sumgreaterthanSquare <- 0
for(i in 1:nrow(train)){
  if (lstat_or_rm[i] < S_VAL )
  {
    sumlessthanSquare <- sumlessthanSquare + (test$medv[i]-mean_less_tha)^2
  }
  if(lstat_or_rm[i] > S_VAL){
    sumgreaterthanSquare <-  sumgreaterthanSquare+(test$medv[i]-mean_greater_tha)^2
  }
}

final_test_rss=sumlessthanSquare + sumgreaterthanSquare
# COMPUTATION OF TEST MSE

print(c("TRAIN MSE : ",final_train_rss/nrow(train)))
print(c("TEST MSE : " ,final_test_rss/nrow(test)))

