library(party)
library(tree)
library(rpart)
library(rpart.plot)
library(randomForest)

cw <- read.csv("creditworthiness.csv")

#Taking the dataset of known ratings in the dataframe cw.k
cw.k <- subset(cw, cw$credit.rating != 0)

cw.k	<- lapply(cw.k,	factor)

cw.k<-as.data.frame(cw.k)

#dividing training and test data
cw.train <- cw.k[1:(nrow(cw.k)/2), ] 

cw.test <- cw.k[-(1:(nrow(cw.k)/2)), ]

# 2. Using default settings,
#???t a decision tree to the training set predict the 
#credit ratings of customers using all of the other variables 
#in the dataset.

#Fitting the decision tree with rpart
cw_train.rpart <- rpart(credit.rating	~	.,data	=	cw.train,
                        method="class",control	=	rpart.control(minsplit=1))

png("images/cw_train_rpart2.png",res=120,height=1200,width=2000)


rpart.plot(cw_train.rpart,tweak = 0.8,space=0)
dev.off()

#display the tree produced
print(cw_train.rpart)

#Reading data of a median customer
med_cust	<- read.csv(file="medCust.csv",	header=TRUE,	sep=",")
med_cust	<- lapply(med_cust,	factor)
#converting from factor to DataFrame to re-arrange the data
med_cust  <- as.data.frame(med_cust)
cw_train.rpart	<- rpart(credit.rating	~	.,	data	=	cw.train,
                 method="class",
                 control	=	rpart.control(minsplit=1))
Cw_med_predict <- predict(cw_train.rpart,med_cust,type="class")
Cw_med_predict
## median - train --- end---

cw_test_predict <- predict(cw_train.rpart, cw.test, type = "class")
table(cw.test$credit.rating,
      cw_test_predict)

# To find the accuracy
confusionMatrix(cw.test$credit.rating,
                cw_test_predict)





p1 <- length(which(cw.train$credit.rating == 1))/nrow(cw.train)
p1
p2 <- length(which(cw.train$credit.rating == 2))/nrow(cw.train)
p2
p3 <- length(which(cw.train$credit.rating == 3))/nrow(cw.train)
p3


p1_f <- length(which(cw.train$functionary == 0))/nrow(cw.train)
p1_f
p2_f <- length(which(cw.train$functionary == 1))/nrow(cw.train)
p2_f

### Random forest
# fitting with default values
RF_cw_train <- randomForest(credit.rating ~ ., cw.train)

print(RF_cw_train)

# Random forest after testing with different values
RF_cw_train <- randomForest(formula = credit.rating ~ .,
                            data = cw.train,
                            ntree=300,nodesize = 5, mtry = 26)

print(RF_cw_train)

RF_cw_pred <- predict(RF_cw_train, cw.test, type = "class")

table(cw.test$credit.rating,
      RF_cw_pred)

confusionMatrix(cw.test$credit.rating,
                RF_cw_pred)

## SVM
library(e1071)
svm_cw <- svm(credit.rating ~ ., data = cw.train)
print(svm_cw)


cw_test <- read.csv("creditworthiness.csv")

median_cust <- newdf[982,]

median_cust <- lapply(median_cust,factor)

median_cust <- as.data.frame(median_cust)




newdf <- rbind(cw.train,med_cust)

medcust_data <- newdf[-c(1:981), ]

medcust_data <- lapply(medcust_data,factor)
medcust_data <- as.data.frame(medcust_data)



svm_pred <- predict(svm_cw,medcust_data, decision.values = TRUE,
                    Type = "class")

print(svm_pred)

svm_pred_test <- predict(svm_cw,cw.test, decision.values = TRUE)

table(cw.test$credit.rating,svm_pred_test)

confusionMatrix(cw.test$credit.rating,svm_pred_test)

## Manual_tuning

svm_man_tuned <- svm(credit.rating ~ ., data = cw.train,
              gamma = 0.01 , cost = 1)

svm_pred_man_tuned <- predict(svm_man_tuned,cw.test, decision.values = TRUE)

confusionMatrix(cw.test$credit.rating,svm_pred_man_tuned )

## Naive Bayes

NB_cw <- naiveBayes(credit.rating ~ ., data = cw.train)

NB_pred <- predict(NB_cw,med_cust,type = "raw")

NB_pred

NB_pred <- predict(NB_cw,med_cust)

NB_pred

NB_cw


NB_pred_test <- predict(NB_cw,cw.test)

confusionMatrix(cw.test$credit.rating,NB_pred_test)

# logistic regression
#pre-processing as per requirement

cw.k <- subset(cw, cw$credit.rating != 0)

cw.k_log <- cw.k[,]

  
cw.k_log$credit.rating[cw.k$credit.rating == 2] <- 0
cw.k_log$credit.rating [cw.k$credit.rating == 3] <- 0
cw.k_log <- lapply (cw.k, factor)
cw.k_log <- as.data.frame(cw.k)

cw.k_log_train <- cw.k_log[1:(nrow(cw.k_log)/2),]
cw.k_log_train <- lapply (cw.k_log_train, factor)
cw.k_log_train <- as.data.frame(cw.k_log_train)

cw.k_log_test <- cw.k_log[-(1:(nrow(cw.k_log)/2)),]
cw.k_log_test <- lapply (cw.k_log_test, factor)
cw.k_log_test <- as.data.frame(cw.k_log_test)


logistic_result <- glm((credit.rating) ~ .,family=binomial("logit"),
                       data = cw.k_log_train)
summary(logistic_result)



chisq.test(cw.k_log_train$credit.rating, 
           cw.k_log_train$years.employed)

library(pROC)


svm_new <- svm(credit.rating ~ ., data = cw.k_log_train,
                    gamma = 0.02 , cost = 1)



test_prob = predict(logistic_result,cw.k_log_test,
                    type = "response")

test_roc = roc(cw.k_log_test$credit.rating ~ svm_new$, 
               plot = TRUE, print.auc = TRUE)



plot(logistic_result,col="darkred")
plot(svm_new,col="darkgreen",add=TRUE)


plot(logistic_result,col="orange",add=TRUE)

legend(0.6,0.45, c('glm','svm'),lty=c(1,1),
       lwd=c(2,2),col=c('darkred','darkgreen','orange'))



