###GINI Code###

#GINI Function

get.GINI <- function(input            # The name of the input data set
                     ,py              # The name of the column containing the predicted values
                     ,y               # The name of the column containing the actual values
                     ,filter          # The indicee desired to score the data i.e. when train = '1' (training data)
                     ,split_ind       # The name of the column containing the filter
)
{
  set.seed(1)   
  
  # Filter the data
  data <- input[which(input[,split_ind] == filter),]
  data$rand.unif <- runif(dim(data)[1])
  
  # Assign weight 1 to all observations
  data$w <- 1
  
  # Rank the data based on predictions
  data <- data[order(data[,py],data[,'rand.unif']),]
  
  test <- data
  
  #Accumulate w to calculate Gini
  for (i in 1:dim(test)[1]){
    if(i==1){test$cumm_w0[i] = 0 + test$w[i]}
    else{
      test$cumm_w0[i] <- test$cumm_w0[i-1] + test$w[i]
      
    }
    
  }
  
  # Calculate Gini
  a <- test[,y]*test$cumm_w0*test$w
  b <- test[,y]*test$w
  
  gini <- 1 - 2 / ( sum(test$w) - 1 )*( sum(test$w) - sum( a ) / sum( b ))
  
  print(paste("Estimated GINI on",filter,'is',round(gini,8),sep=' '))
}

setwd("F:/Study Materials/Travelers Case Competition") #setting working directory

train=read.csv("./Kangaroo_train.csv", stringsAsFactors = FALSE) #train data, to be split as training and validation
test=read.csv("./Kangaroo_valid.csv", stringsAsFactors = FALSE) #validation data set, to be used as test dataset
h=read.csv("./Kangaroo_hold.csv", stringsAsFactors = FALSE)

install.packages("caTools")
library("caTools") #for sample.split function

split = sample.split(train, SplitRatio = 0.6) #ratio can be changed as reqd.
tr = subset(train, split == TRUE) #creating new training data
v = subset(train, split == FALSE) #creating new validation data

#standardizing the data
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
tr$veh_value_std <- range01(tr$veh_value)
tr$exposure_std <- range01(tr$exposure)
tr$agecat_std <- range01(tr$agecat)
tr$veh_age_std <- range01(tr$veh_age)

v$veh_value_std <- range01(v$veh_value)
v$exposure_std <- range01(v$exposure)
v$agecat_std <- range01(v$agecat)
v$veh_age_std <- range01(v$veh_age)

test$veh_value_std <- range01(test$veh_value)
test$exposure_std <- range01(test$exposure)
test$agecat_std <- range01(test$agecat)
test$veh_age_std <- range01(test$veh_age)

h$veh_value_std <- range01(h$veh_value)
h$exposure_std <- range01(h$exposure)
h$agecat_std <- range01(h$agecat)
h$veh_age_std <- range01(h$veh_age)

#removing original un-standardized columns

tr <- subset(tr, select = -c(id,veh_value,exposure,veh_age,clm,numclaims,agecat))
tr$veh_body <- as.factor(tr$veh_body)
tr$gender <- as.factor(tr$gender)
tr$area <- as.factor(tr$area)

v <- subset(v, select = -c(id,veh_value,exposure,veh_age,clm,numclaims,agecat))
v$veh_body <- as.factor(v$veh_body)
v$gender <- as.factor(v$gender)
v$area <- as.factor(v$area)

test <- subset(test, select = -c(id,veh_value,exposure,veh_age,clm,numclaims,agecat))
test$veh_body <- as.factor(test$veh_body)
test$gender <- as.factor(test$gender)
test$area <- as.factor(test$area)

h <- subset(h, select = -c(veh_value,exposure,veh_age,clm,numclaims,agecat))
h$veh_body <- as.factor(h$veh_body)
h$gender <- as.factor(h$gender)
h$area <- as.factor(h$area)

#writing new datasets to file

write.csv(tr, "train_std.csv", row.names = FALSE)
write.csv(v, "valid_std.csv", row.names = FALSE)
write.csv(test, "test_std.csv", row.names = FALSE)
write.csv(h, "hold_std.csv", row.names = FALSE)

#reading standardized datasets to avoid doing above steps

#tr <- read.csv("train_std.csv", stringsAsFactors = FALSE)
#v <- read.csv("valid_std.csv", stringsAsFactors = FALSE)
#test <- read.csv("test_std.csv", stringsAsFactors = FALSE)

#Linear Regression

set.seed(1234)
a <- lm(claimcst0 ~ exposure_std + gender + agecat_std + area - 1, data = tr) #removing intercept
summary(a)
lm_v <- predict(a, newdata = v)
lm_te <- predict(a, newdata = test)
lm_h <- predict(a, newdata = h)

#random forest
install.packages("randomForest")
library(randomForest)
set.seed(1234)

rf <- randomForest(claimcst0 ~ exposure_std + gender + agecat_std + area - 1,data=tr, ntree=300, mtry=2,importance=TRUE,replace=FALSE)
rf_v <- predict(rf, newdata = v)
rf_te <- predict(rf, newdata = test)
rf_h <- predict(rf, newdata = h)

#TDBoost

install.packages("TDboost")
library("TDBoost")
set.seed(1234)

td_train<-TDboost(claimcst0 ~ exposure_std + veh_value_std + agecat_std - 1,
                  data = tr,
                  distribution = list(name="EDM",alpha=1.5),
                  var.monotone = c (0,0,0),
                  n.trees = 300,
                  interaction.depth = 2,
                  n.minobsinnode = 10,
                  shrinkage = 0.001,
                  bag.fraction = 0.5,
                  train.fraction = 1.0,
                  cv.folds=5,
                  keep.data = TRUE,
                  verbose = TRUE)

best.iter <- TDboost.perf(td_train,method="OOB")
print(best.iter)

summary(td_train,n.trees=1) # based on the first tree
summary(td_train,n.trees=best.iter) # based on the estimated best number of trees

f.predict_v <- predict.TDboost(td_train,v,best.iter)
f.predict_test <- predict.TDboost(td_train,test,best.iter)
f.predict_h <- predict.TDboost(td_train,h,best.iter)

#Creating the ensemble

en_v = (lm_v*16+ rf_v + f.predict_v*17)/34
en_test = (lm_te*16+ rf_te + f.predict_test*17)/34
en_h = (lm_h*16+ rf_h + f.predict_h*17)/34

tw_v = as.data.frame(cbind(en_v, v$claimcst0))
tw_test = as.data.frame(cbind(en_te, test$claimcst0))
colnames(tw_v) <- c("Predicted","Actual")
colnames(tw_test) <- c("Predicted","Actual")

#Getting the Gini Index - Validation
tw_v<-data.frame(tw_v$Predicted,tw_v$Actual,split="Valid")
get.GINI(input=tw_v, py='tw_v.Predicted', y='tw_v.Actual', filter='Valid', split_ind='split')

#Getting the Gini Index - Test
tw_t<-data.frame(tw_test$Predicted,tw_test$Actual,split="Test")
get.GINI(input=tw_t, py='tw_test.Predicted', y='tw_test.Actual', filter='Test', split_ind='split')

#writing the output to file
op <- as.data.frame(cbind(h$id, en_h))
write.csv(op, "hold_out_claims.csv", row.names = FALSE)
