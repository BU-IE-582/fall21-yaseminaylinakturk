#2020802000 yasemin aylin akturk
#ie 582 hw5
#27.01.2022


klasor =  "C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/HW5/"

setwd(klasor)


library("data.table")
library("xlsx")
library("dplyr")
library("tidyverse")
library("mice")
library("tidyr")
library("stringr")
library("ggplot2")
library("caret")
library("glmnet")
library("smotefamily")
library(dplyr) 
library(caret) 
library(DMwR) 
library(purrr)
library(pROC) 
library("gbm")
library("corrplot")
library("ROSE")
library(randomForest)
library(caret)
library(e1071)
library("kernlab")
library("smotefamily")
library("fastAdaboost")
library("class")
library("kknn")
library(caretEnsemble)
library(naivebayes)
library("resample")
library("klaR")
library("VIM")
library('skimr')
library("rpart")

memory.limit()

## To increase the storage capacity
memory.limit(size=56000)

#read----
#data skim ----

datatrain <- read.csv("train.csv", header = T)

head(datatrain)

dim(datatrain)

#too many features
dim(datatrain)   #1715   63


#duplicated?
sum(duplicated(datatrain))
#no duplicated 

#save the org data, do manipulations to the datatrain
orginaltrain = datatrain 
head(orginaltrain)
dim(orginaltrain) #1715   63
#any missing value?

datatrain [datatrain  == " "] <- NA
datatrain[datatrain  == ""] <- NA

#check NA #how many NA's
nalist = cbind(
  lapply(
    lapply(datatrain, is.na)
    , sum)
)

nalist

#none, fully observed.

#pattern NA

mice_plot <- aggr(datatrain, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(datatrain), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

#

str(datatrain)

#$ Var_39 and  $ Var_53  : char
#converto to numeric
unique(datatrain$Var_39)
unique(datatrain$Var_53)

datatrain$Var_39=ifelse(datatrain$Var_39== "Y",1,0)
datatrain$Var_53=ifelse(datatrain$Var_53== "Y",1,0)

#now all numeric
str(datatrain)
summary(datatrain)
skim(datatrain)


#show class dist.
#imbalanced data set
prop.table(table(datatrain$default))

barplot(prop.table(table(datatrain$default)),
        col = rainbow(2),
        ylim = c(0, 1),
        main = "Class Distribution")



#check correlation
#id is not needed
Cormat = cor(datatrain[,-1])

corrplot(Cormat, method= "color") 
#some are correlated, deal with feature selection by RF vaImportance



head(datatrain)
#dont need app_id 


datatrain =  datatrain [,-1]
head(datatrain)




#Spliting training set into two parts based on outcome: 75% and 25%----
#internal train and test
set.seed(123)
index <- createDataPartition(datatrain$default, p=0.75, list=FALSE)
trainSet <- datatrain[ index,]
testSet <- datatrain[-index,]
head(trainSet)



sum(trainSet$default) #123 in train subset out of 1287
length(trainSet$default)  #1287
length(testSet$default) #428
sum(testSet$default) #49 1s in test set


#imbalanced in the subset for train
prop.table(table(trainSet$default))

barplot(prop.table(table(trainSet$default)),
        col = rainbow(2),
        ylim = c(0, 1),
        main = "Class Distribution-Trainsubset")


#scale without target column (default) remove loan
alltrainscl <- as.data.table(scale(trainSet[,-c(1,2)]))
head(alltrainscl)
tail(alltrainscl)
#add target col
alltrainsclall= cbind(alltrainscl, trainSet$default)
dim(alltrainsclall)
colnames(alltrainsclall)[61] <-  "default" 
head(alltrainsclall)
dim(alltrainsclall) #1287   63-2=61 #no id no loan

#same for test

#scale without target column (default)
alltestscl <- as.data.table(scale(testSet[,-c(1,2)]))
head(alltestscl)
tail(alltestscl)
#add target col
alltestsclall= cbind(alltestscl, testSet$default)
dim(alltestsclall)
colnames(alltestsclall)[61] <-  "default" 
head(alltestsclall)
dim(alltestsclall) #428   61 #no id no loan


#handle perfomence& imbalance with cost function.
#wieght the misclassification of each instance 
#and minizmize cost

#build a custom performance metric
#we have the following cost for each instance 
#for each misclassification

#          pred=1	               pred=0
#obs=1	   0	                  loan_amount
#obs=0	   0,15*loan_amount    	0





#customized performance metric for "Total money lost" ----
#multiply with the actual loan amount, not the scaled version
#index fix due to folding

totalcost = function(data, lev = NULL , model= NULL ) 
  
{
  #conversion, othwerwise they are 1 and 2 
  data$pred <- as.numeric(data$pred)-1
  data$obs <- as.numeric(data$obs)-1 
  
  #keep loan amount here only for those which are in the fold
  loanlist = trainSet$loan_amount[data$rowIndex]
  
  cost= 0
  for (i in 1:length(data$obs)) 
  {
    
    
    if  ( data$obs [i] == 1  & data$pred[i] == 0  )
    {
      #cost = cost +  trainSet$loan_amount [i] 
      cost = cost +  loanlist [i] #now
    }
    
    if  ( data$obs [i] == 0  & data$pred[i] == 1 )
    {
      #cost = cost +  trainSet$loan_amount [i] *0.15
      cost = cost +   loanlist [i] *0.15 #now
    }
    
  } #end for
  
  
  names(cost) <- c('Total money lost')
  cost
  
  
} #end function



#customized performance metric for "Total money lost"
#multiply with the actual loan amount, not the scaled version



#build the function for test set as well
#to measure performance of the test set



calculate_cost_test = function(pred, true ) 
  
{
  testcost= 0
  for (i in 1:length(pred)) 
  {
    
    if  ( true [i] == 1  & pred[i] == 0  )
    {
      testcost = testcost +  testSet$loan_amount [i] 
      
    }
    
    if  ( true [i] == 0  & pred [i] == 1 )
    {
      testcost  = testcost  +  testSet$loan_amount [i] *0.15
      
    }
    
 
                   
    } #end for
  
  
  names(testcost ) <- c('Total money lost  TEST')
  testcost
  

} #end function

#

#train with alltrainsclall
#test on alltestsclall


#caret selects the parameters for which the cost is minimized
#this is supplied as a performance metric

# RF models ----
#model1
set.seed(123)

TC <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE)



RF1 <- train(as.factor(default) ~., data = alltrainsclall,
                  method = "ranger",  metric = "Total money lost" ,
                  tuneLength = 3,
                  trControl = TC,
                  maximize = FALSE)
                 



predicted_rf1  <-  predict(RF1   ,alltestsclall[,-61])



table(predicted_rf1 ,  as.factor(alltestsclall$default))
#class(predicted_rf1)

cm1 = confusionMatrix(predicted_rf1 , as.factor(alltestsclall$default))
cm1

testcost1= calculate_cost_test (predicted_rf1 ,  alltestsclall$default)
testcost1




#a sample calculation check on excel

write.csv(predicted_rf1   ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/final/predicted_rf1.csv", row.names = FALSE)
write.csv(alltestsclall$default  ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/final/testSetdefault.csv", row.names = FALSE)
write.csv(testSet$loan_amount  ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/final/testSetloan_amount.csv", row.names = FALSE)

#same totalcost is reache by excel and R-function OK.

# model2.RF
set.seed(123)
TC2 <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE)

RF2_grid <- expand.grid(mtry = c(1:15) ,min.node.size=c(1,5), splitrule = c("extratrees","gini"))


RF2 <- train(as.factor(default) ~., data = alltrainsclall,
             method = "ranger",  metric = "Total money lost" ,
             tuneGrid = RF2_grid,
             trControl = TC2,
             maximize = FALSE)




predicted_rf2  <-  predict(RF2   ,alltestsclall[,-61])

table(predicted_rf2 ,  as.factor(alltestsclall$default))

cm2 = confusionMatrix(predicted_rf2 , as.factor(alltestsclall$default))
cm2

testcost2= calculate_cost_test (predicted_rf2 ,  alltestsclall$default)
testcost2



#####cannot learn due to imbalance

#option1 : down sampling

# Build down-sampled model

set.seed(123)
TC3 <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE)


TC3$sampling <- "down"

RF3_grid <- expand.grid(mtry = c(1:15) ,min.node.size=c(1,5), splitrule = c("extratrees","gini"))


down_RF3 <- train(as.factor(default) ~., data = alltrainsclall,
             method = "ranger",  metric = "Total money lost" ,
             tuneGrid = RF3_grid,
             trControl = TC3,
             maximize = FALSE) 


predicted_rf3  <-  predict(down_RF3  ,alltestsclall[,-61])

table(predicted_rf3 ,  as.factor(alltestsclall$default))

cm3 = confusionMatrix(predicted_rf3 , as.factor(alltestsclall$default))
cm3

testcost3= calculate_cost_test (predicted_rf3 ,  alltestsclall$default)
testcost3


#option2 : "up" sampling

# Buil up-sampled model

set.seed(123)
TC4 <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE)


TC4$sampling <- "up"

RF4_grid <- expand.grid(mtry = c(1:15) ,min.node.size=c(1,5), splitrule = c("extratrees","gini"))


up_RF4 <- train(as.factor(default) ~., data = alltrainsclall,
                  method = "ranger",  metric = "Total money lost" ,
                  tuneGrid = RF4_grid,
                  trControl = TC4,
                  maximize = FALSE) 


predicted_rf4  <-  predict(up_RF4  ,alltestsclall[,-61])

table(predicted_rf4 ,  as.factor(alltestsclall$default))

cm4 = confusionMatrix(predicted_rf4 , as.factor(alltestsclall$default))
cm4

testcost4= calculate_cost_test (predicted_rf4 ,  alltestsclall$default)
testcost4
#up is not as good as down


#option:3 create a synthetic data with smote to find the 
#first smote to balance the data out

#smote
#8 times larger 
#K=2 negihborhood for generation

newtrain <- SMOTE(X=alltrainsclall , target= alltrainsclall$default, K=2, dup_size = 8 )

verynewtrain2 <- newtrain [["data"]]
dim(verynewtrain2)
head(verynewtrain2)
#drop class
verynewtrain <- verynewtrain2 [,-62]
dim(verynewtrain)
head(verynewtrain)
classnow <- verynewtrain  %>% group_by(default) %>% summarize(count=n()) 
classnow #balanced
head(verynewtrain)

#train on this dataset

set.seed(123)
TC5 <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE)


RF5_grid <- expand.grid(mtry = c(1:15) ,min.node.size=c(1,5), splitrule = c("extratrees","gini"))


smote_RF5 <- train(as.factor(default) ~., data = verynewtrain,
                method = "ranger",  metric = "Total money lost" ,
                tuneGrid = RF5_grid,
                trControl = TC5,
                maximize = FALSE) 


predicted_rf5  <-  predict(smote_RF5  ,alltestsclall[,-61])

table(predicted_rf5 ,  as.factor(alltestsclall$default))

cm5 = confusionMatrix(predicted_rf5 , as.factor(alltestsclall$default))
cm5

testcost5= calculate_cost_test (predicted_rf5 ,  alltestsclall$default)
testcost5

#best we got is down sampling RF

#model#3
down_RF3$bestTune
#check variable importance 



my_RF3_down=randomForest(as.factor(default) ~., data = alltrainsclall,
                    min.node.size=1, splitrule = "gini",
                    mtry = 10, 
                    importance=TRUE, localImp=TRUE) 



varImpPlot(my_RF3_down)

#only use the first 10 variables based on importance (dec. in acc.)

##("Var_3", "Var_29","Var_27","Var_4","Var_5","Var_22","Var_2","Var_31","Var_26","Var_8")


head(alltrainsclall)
reducedtrain = alltrainsclall[,c("Var_3", "Var_29","Var_27","Var_4","Var_5","Var_22","Var_2","Var_31","Var_26","Var_8","default")]
head(reducedtrain)
dim(reducedtrain)
reducedtest = alltestsclall[,c("Var_3", "Var_29","Var_27","Var_4","Var_5","Var_22","Var_2","Var_31","Var_26","Var_8","default")]
head(reducedtest)
dim(reducedtest)


#redo model RF with down sampling

#model#6

set.seed(123)
TC6 <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE)


TC6$sampling <- "down"

RF6_grid <- expand.grid(mtry = c(1:10) ,min.node.size=c(1,5), splitrule = c("extratrees","gini"))


down_RF6 <- train(as.factor(default) ~., data = reducedtrain,
                  method = "ranger",  metric = "Total money lost" ,
                  tuneGrid = RF6_grid,
                  trControl = TC6,
                  maximize = FALSE) 


predicted_rf6  <-  predict(down_RF6  ,reducedtest[,-11])

table(predicted_rf6 ,  as.factor(reducedtest$default))

cm6 = confusionMatrix(predicted_rf6 , as.factor(reducedtest$default))
cm6

testcost6= calculate_cost_test (predicted_rf6 , reducedtest$default)
testcost6

#reduced features provides better results.


#try decision tree ----

#model#7
#option1 : down sampling
#full data all features
# Build down-sampled DC model

set.seed(123)
TC7 <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE)


TC7$sampling <- "down"

Tr_grid7 <- expand.grid(cp=c(0.005,0.007,0.01,0.03,0.05,0.1))

down_tr7 <- train(as.factor(default) ~., data = alltrainsclall,
                  method = "rpart",  metric = "Total money lost" ,
                  tuneGrid = Tr_grid7,
                  control = rpart.control(minbucket=c(1)),
                  trControl = TC7,
                  maximize = FALSE) 


predicted_tr7  <-  predict(down_tr7  ,alltestsclall[,-61])

table(predicted_tr7 ,  as.factor(alltestsclall$default))

cm7 = confusionMatrix(predicted_tr7 , as.factor(alltestsclall$default))
cm7

testcost7= calculate_cost_test (predicted_tr7 ,  alltestsclall$default)
testcost7

#model8 
#different minbucket

down_tr8 <- train(as.factor(default) ~., data = alltrainsclall,
                  method = "rpart",  metric = "Total money lost" ,
                  tuneGrid = Tr_grid7,
                  control = rpart.control(minbucket=c(3)),
                  trControl = TC7,
                  maximize = FALSE) 


predicted_tr8  <-  predict(down_tr8  ,alltestsclall[,-61])

table(predicted_tr8 ,  as.factor(alltestsclall$default))

cm8 = confusionMatrix(predicted_tr8 , as.factor(alltestsclall$default))
cm8

testcost8= calculate_cost_test (predicted_tr8 ,  alltestsclall$default)
testcost8


#model9 
#different minbucket
set.seed(123)
down_tr9 <- train(as.factor(default) ~., data = alltrainsclall,
                  method = "rpart",  metric = "Total money lost" ,
                  tuneGrid = Tr_grid7,
                  control = rpart.control(minbucket=c(10)),
                  trControl = TC7,
                  maximize = FALSE) 


predicted_tr9  <-  predict(down_tr8  ,alltestsclall[,-61])

table(predicted_tr9 ,  as.factor(alltestsclall$default))

cm9 = confusionMatrix(predicted_tr9 , as.factor(alltestsclall$default))
cm9

testcost9= calculate_cost_test (predicted_tr9,  alltestsclall$default)
testcost9


#option2 : "up" sampling

# Buil up-sampled model

#model10

set.seed(123)
TC10 <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE)

TC10$sampling <- "up"

Tr_grid10 <- expand.grid(cp=c(0.005,0.007,0.01,0.03,0.05,0.1))


up_tr10 <- train(as.factor(default) ~., data = alltrainsclall,
                  method = "rpart",  metric = "Total money lost" ,
                  tuneGrid = Tr_grid10 ,
                  control = rpart.control(minbucket=c(1)),
                  trControl = TC10,
                  maximize = FALSE) 


predicted_up_tr10   <-  predict(up_tr10   ,alltestsclall[,-61])

table(predicted_up_tr10  ,  as.factor(alltestsclall$default))

cm10 = confusionMatrix(predicted_up_tr10 , as.factor(testSet$default))
cm10

testcost10= calculate_cost_test (predicted_up_tr10  ,  alltestsclall$default)
testcost10


#model11
#another minbucket

set.seed(123)
TC10 <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE)

TC10$sampling <- "up"

Tr_grid10 <- expand.grid(cp=c(0.005,0.007,0.01,0.03,0.05,0.1))


up_tr11 <- train(as.factor(default) ~., data = alltrainsclall,
                 method = "rpart",  metric = "Total money lost" ,
                 tuneGrid = Tr_grid10 ,
                 control = rpart.control(minbucket=c(10)),
                 trControl = TC10,
                 maximize = FALSE) 


predicted_up_tr11   <-  predict(up_tr11   ,alltestsclall[,-61])

table(predicted_up_tr11  ,  as.factor(alltestsclall$default))

cm11 = confusionMatrix(predicted_up_tr11 , as.factor(testSet$default))
cm11

testcost11= calculate_cost_test (predicted_up_tr11  ,  alltestsclall$default)
testcost11



#option:3 create a synthetic data with smote to find the 
#first smote to balance the data out

#model12
#smote
#8 times larger 
#K=2 negihborhood for generation

#train on the smote generated dataset

set.seed(123)
TC12 <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE)
TC12$sampling = NULL



smote_tr12 <- train(y =  as.factor(verynewtrain$default), x = as.matrix(verynewtrain [,-61], with = F),
                   method = "rpart",  metric = "Total money lost" ,
                   trControl = TC12,
                   tuneLength = 6,
                   maximize = FALSE) 



table(verynewtrain$default)


predicted_smote_tr12  <-  predict(smote_tr12  ,alltestsclall[,-61])

table(predicted_smote_tr12 ,  as.factor(alltestsclall$default))

cm12 = confusionMatrix(predicted_smote_tr12 , as.factor(alltestsclall$default))
cm12

testcost12= calculate_cost_test (predicted_smote_tr12 ,  alltestsclall$default)
testcost12


#DC with reduced features


#model13 
# fewer variables
set.seed(123)
Tr_grid13 <- expand.grid(cp=c(0.0001, 0.001,0.005,0.007,0.01,0.03,0.05,0.1))
TC7$sampling = "down"

down_tr13 <- train(as.factor(default) ~., data = reducedtrain,
                  method = "rpart",  metric = "Total money lost" ,
                  tuneGrid = Tr_grid13,
                  control = rpart.control(minbucket=c(3)),
                  trControl = TC7,
                  maximize = FALSE) 


predicted_tr13  <-  predict(down_tr13  ,reducedtest[,-11])

table(predicted_tr13 ,  as.factor(reducedtest$default))

cm13 = confusionMatrix(predicted_tr13 , as.factor(reducedtest$default))
cm13

testcost13= calculate_cost_test (predicted_tr13  ,  reducedtest$default)
testcost13

#not getting better with reduced features


#GBM models ----

#up
#model14
set.seed(123)
TC14 <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE)
TC14$sampling <- "up"

gbm_Grid14=expand.grid(interaction.depth = c( 3, 5), 
                    n.trees = c(1:5)*100, 
                    shrinkage = c(0.05,0.1),
                    n.minobsinnode = c(1, 3, 5))



up_gbm14 <- train(as.factor(default) ~., data = alltrainsclall,
                 method = "gbm",  metric = "Total money lost" ,
                 tuneGrid = gbm_Grid14 ,
                 trControl = TC14,
                 maximize = FALSE) 



predicted_up_gbm14   <-  predict(up_gbm14   ,alltestsclall[,-61])

table(predicted_up_gbm14 ,  as.factor(alltestsclall$default))

cm14 = confusionMatrix(predicted_up_gbm14 , as.factor(testSet$default))
cm14

testcost14= calculate_cost_test (predicted_up_gbm14  ,  alltestsclall$default)
testcost14


#got a better one
#up with reduced feature set
#model15

up_gbm15 <- train(as.factor(default) ~., data = reducedtrain,
                  method = "gbm",  metric = "Total money lost" ,
                  tuneGrid = gbm_Grid14 ,
                  trControl = TC14,
                  maximize = FALSE) 


predicted_up_gbm15   <-  predict(up_gbm15,reducedtest[,-11])

table(predicted_up_gbm15 ,  as.factor(reducedtest$default))

cm15 = confusionMatrix(predicted_up_gbm15 , as.factor(reducedtest$default))
cm15

testcost15= calculate_cost_test (predicted_up_gbm15 ,  reducedtest$default)
testcost15

#gbm down
#model16
TC16 <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE)
TC16$sampling <- "down"

gbm_Grid16=expand.grid(interaction.depth = c( 3, 5), 
                       n.trees = c(1:5)*100, 
                       shrinkage = c(0.05,0.1),
                       n.minobsinnode = c(1, 3, 5))



down_gbm16 <- train(as.factor(default) ~., data = alltrainsclall,
                  method = "gbm",  metric = "Total money lost" ,
                  tuneGrid = gbm_Grid16 ,
                  trControl = TC16,
                  maximize = FALSE) 



predicted_down_gbm16  <-  predict(down_gbm16   ,alltestsclall[,-61])

table(predicted_down_gbm16 ,  as.factor(alltestsclall$default))

cm16 = confusionMatrix(predicted_down_gbm16 , as.factor(testSet$default))
cm16

testcost16= calculate_cost_test (predicted_down_gbm16  ,  alltestsclall$default)
testcost16


#model17 gbm
#down and reduced

down_gbm17 <- train(as.factor(default) ~., data = reducedtrain,
                  method = "gbm",  metric = "Total money lost" ,
                  tuneGrid = gbm_Grid16 ,
                  trControl = TC16,
                  maximize = FALSE) 


predicted_down_gbm17    <-  predict(down_gbm17,reducedtest[,-11])

table(predicted_down_gbm17,  as.factor(reducedtest$default))

cm17 = confusionMatrix(predicted_down_gbm17 , as.factor(reducedtest$default))
cm17

testcost17= calculate_cost_test (predicted_down_gbm17 ,  reducedtest$default)
testcost17


#gbm smote



#train on the smote generated dataset

set.seed(123)
TC18 <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE)

gbm_Grid18=expand.grid(interaction.depth = c( 3, 5), 
                       n.trees = c(1:5)*100, 
                       shrinkage = c(0.05,0.1),
                       n.minobsinnode = c(1, 3, 5))



smote_gbm18 <- train(as.factor(default) ~., data = verynewtrain,
                    method = "gbm",  metric = "Total money lost" ,
                    tuneGrid = gbm_Grid18,
                    trControl = TC18,
                    maximize = FALSE) 


predicted_smote_gbm18  <-  predict(smote_gbm18  ,alltestsclall[,-61])

table(predicted_smote_gbm18 ,  as.factor(alltestsclall$default))

cm18 = confusionMatrix(predicted_smote_gbm18 , as.factor(alltestsclall$default))
cm18

testcost18= calculate_cost_test (predicted_smote_gbm18,  alltestsclall$default)
testcost18

#gbm smote different tuning

set.seed(123)
TC19 <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE)



smote_gbm19 <- train(as.factor(default) ~., data = verynewtrain,
                     method = "gbm",  metric = "Total money lost" ,
                     tuneLength = 6,
                     trControl = TC19,
                     maximize = FALSE) 


predicted_smote_gbm19  <-  predict(smote_gbm19  ,alltestsclall[,-61])

table(predicted_smote_gbm19 ,  as.factor(alltestsclall$default))

cm19 = confusionMatrix(predicted_smote_gbm19 , as.factor(alltestsclall$default))
cm19

testcost19= calculate_cost_test (predicted_smote_gbm19,  alltestsclall$default)
testcost19



#log-reg----
#down
set.seed(123)
TC20 <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE)
TC20$sampling <- "down"

log_grid20 <- expand.grid(alpha = 1, lambda= seq( from = 0.001, to =0.003, by = 0.00003))



log_reg20 <- train(as.factor(default) ~., data = alltrainsclall,
                    method = "glmnet",  metric = "Total money lost" ,
                    tuneGrid = log_grid20 ,
                    trControl = TC20,
                    maximize = FALSE) 

predicted_log_reg20  <-  predict(log_reg20  ,alltestsclall[,-61])

table(predicted_log_reg20 ,  as.factor(alltestsclall$default))

cm20 = confusionMatrix(predicted_log_reg20 , as.factor(alltestsclall$default))
cm20

testcost20= calculate_cost_test (predicted_log_reg20,  alltestsclall$default)
testcost20


#down with reduced 
set.seed(123)
TC21 <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE)
TC21$sampling <- "down"

log_grid21 <- expand.grid(alpha = 1, lambda= seq( from = 0.001, to =0.003, by = 0.00003))



log_reg21 <- train(as.factor(default) ~., data = reducedtrain,
                   method = "glmnet",  metric = "Total money lost" ,
                   tuneGrid = log_grid21 ,
                   trControl = TC21,
                   maximize = FALSE) 

predicted_log_reg21  <-  predict(log_reg21  ,reducedtest[,-11])

table(predicted_log_reg21 ,  as.factor(reducedtest$default))

cm21 = confusionMatrix(predicted_log_reg21 , as.factor(reducedtest$default))
cm21

testcost21= calculate_cost_test (predicted_log_reg21,  reducedtest$default)
testcost21


#better without elimnation of features

#down with tuneleght
set.seed(123)
TC22 <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE)
TC22$sampling <- "down"



log_reg22 <- train(as.factor(default) ~., data = alltrainsclall,
                   method = "glmnet",  metric = "Total money lost" ,
                   tuneLength = 22 ,
                   trControl = TC22,
                   maximize = FALSE) 

predicted_log_reg22  <-  predict(log_reg22  ,alltestsclall[,-61])

table(predicted_log_reg22 ,  as.factor(alltestsclall$default))

cm22 = confusionMatrix(predicted_log_reg22 , as.factor(alltestsclall$default))
cm22

testcost22= calculate_cost_test (predicted_log_reg22,  alltestsclall$default)
testcost22


#logreg up
set.seed(123)
TC23 <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE)
TC23$sampling <- "up"

log_grid23 <- expand.grid(alpha = 1, lambda= seq( from = 0.001, to =0.003, by = 0.00003))



log_reg23 <- train(as.factor(default) ~., data = alltrainsclall,
                   method = "glmnet",  metric = "Total money lost" ,
                   tuneGrid = log_grid23 ,
                   trControl = TC23,
                   maximize = FALSE) 

predicted_log_reg23  <-  predict(log_reg23  ,alltestsclall[,-61])

table(predicted_log_reg23 ,  as.factor(alltestsclall$default))

cm23 = confusionMatrix(predicted_log_reg23 , as.factor(alltestsclall$default))
cm23

testcost23= calculate_cost_test (predicted_log_reg23,  alltestsclall$default)
testcost23


#logreg smote data
set.seed(123)
TC24 <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE)


log_grid24 <- expand.grid(alpha = 1, lambda= seq( from = 0.001, to =0.003, by = 0.00003))


###
all  = alltrainsclall

all$default = ifelse (all$default == 1 , "y", "n")

alltest  = alltestsclall

alltest$default = ifelse (alltest$default == 1 , "y", "n")
###


log_reg24 <- train(as.factor(default) ~., data = all,
                   method = "glmnet",  metric = "Total money lost" ,
                   tuneGrid = log_grid24 ,
                   trControl = TC24,
                   maximize = FALSE) 

predicted_log_reg24  <-  predict(log_reg24  ,alltestsclall[,-61])

table(predicted_log_reg24 ,  as.factor(alltestsclall$default))

cm24 = confusionMatrix(predicted_log_reg24 , as.factor(alltest$default))
cm24

predicted_log_reg24 = ifelse (predicted_log_reg24 == "y" , 1, 0)
alltest$default = ifelse (alltest$default == "y" , 1, 0)

testcost24= calculate_cost_test (as.factor(predicted_log_reg24),  as.factor(alltest$default))
testcost24



#SVM ----


#linear svm and down
set.seed(123)
TC25 <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE)
TC25$sampling <- "down"


Lin_svm_down25 <- train(as.factor(default) ~., data = alltrainsclall,
                   method ="svmLinear",  metric = "Total money lost" ,
                   tuneLength = 10,
                   trControl = TC25,
                   maximize = FALSE) 

predicted_Lin_svm_down25  <-  predict(Lin_svm_down25  ,alltestsclall[,-61])

table(predicted_Lin_svm_down25,  as.factor(alltestsclall$default))

cm25 = confusionMatrix(predicted_Lin_svm_down25 , as.factor(alltestsclall$default))
cm25

testcost25= calculate_cost_test (predicted_Lin_svm_down25,  alltestsclall$default)
testcost25

#polysvm 
set.seed(123)
TC26 <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE)
TC26$sampling <- "down"


Poly_svm_down26 <- train(as.factor(default) ~., data = alltrainsclall,
                        method ="svmPoly",  metric = "Total money lost" ,
                        tuneLength = 10,
                        trControl = TC26,
                        maximize = FALSE) 

predicted_Lin_svm_down26  <-  predict(Lin_svm_down26  ,alltestsclall[,-61])

table(predicted_Lin_svm_down26,  as.factor(alltestsclall$default))

cm26 = confusionMatrix(predicted_Lin_svm_down26 , as.factor(alltestsclall$default))
cm26

testcost26= calculate_cost_test (predicted_Lin_svm_down26,  alltestsclall$default)
testcost26


#radialsvm 
set.seed(123)
TC27 <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE)
TC27$sampling <- "down"


Rad_svm_down27 <- train(as.factor(default) ~., data = alltrainsclall,
                         method ="svmRadial",  metric = "Total money lost" ,
                         tuneLength = 10,
                         trControl = TC27,
                         maximize = FALSE) 

predicted_Rad_svm_down27 <-  predict(Rad_svm_down27  ,alltestsclall[,-61])

table(predicted_Rad_svm_down27,  as.factor(alltestsclall$default))

cm27 = confusionMatrix(predicted_Rad_svm_down27 , as.factor(alltestsclall$default))
cm27

testcost27= calculate_cost_test (predicted_Rad_svm_down27,  alltestsclall$default)
testcost27

#radial down and less features

set.seed(123)

Rad_svm_red_down28 <- train(as.factor(default) ~., data = reducedtrain,
                        method ="svmRadial",  metric = "Total money lost" ,
                        tuneLength = 10,
                        trControl = TC27,
                        maximize = FALSE) 

predicted_Rad_svm_red_down28 <-  predict(Rad_svm_red_down28  ,reducedtest[,-11])

table(predicted_Rad_svm_red_down28,  as.factor(reducedtest$default))

cm28 = confusionMatrix(predicted_Rad_svm_red_down28 , as.factor(reducedtest$default))
cm28

testcost28= calculate_cost_test (predicted_Rad_svm_red_down28,  reducedtest$default)
testcost28

#rad svm up


set.seed(123)
TC29 <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE)
TC29$sampling <- "up"


Rad_svm_up29 <- train(as.factor(default) ~., data = alltrainsclall,
                        method ="svmRadial",  metric = "Total money lost" ,
                        tuneLength = 10,
                        trControl = TC29,
                        maximize = FALSE) 

predicted_Rad_svm_up29 <-  predict(Rad_svm_up29  ,alltestsclall[,-61])

table(predicted_Rad_svm_up29,  as.factor(alltestsclall$default))

cm29 = confusionMatrix(predicted_Rad_svm_up29 , as.factor(alltestsclall$default))
cm29

testcost29= calculate_cost_test (predicted_Rad_svm_up29,  alltestsclall$default)
testcost29


#rad svm smote

set.seed(123)
TC30 <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE)



Rad_svm_smote30 <- train(as.factor(default) ~., data = verynewtrain,
                      method ="svmRadial",  metric = "Total money lost" ,
                      tuneLength = 10,
                      trControl = TC29,
                      maximize = FALSE) 

predicted_Rad_svm_smote30<-  predict(Rad_svm_smote30  ,alltestsclall[,-61])

table(predicted_Rad_svm_smote30,  as.factor(alltestsclall$default))

cm30 = confusionMatrix(predicted_Rad_svm_smote30 , as.factor(alltestsclall$default))
cm30

testcost30= calculate_cost_test (predicted_Rad_svm_smote30,  alltestsclall$default)
testcost30


#try to put additional class weights ----
#add inverse wieghts on the classes
#rebuild these models
#the best models so far
#down_RF6
#log_reg20
#log_reg23
#log_reg24
#Rad_svm_down27

#modelweight 

prop.table(table(alltrainsclall$default))

model_weights <- ifelse (alltrainsclall$default == "1",
                        (1/table(alltrainsclall$default)[1]) * 0.9044289,
                        (1/table(alltrainsclall$default)[2]) * 0.0955711)


sum(model_weights)

#setting of  down_RF6,but add inverse weights

set.seed(123)
TC31 <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE)

RF31_grid <- expand.grid(mtry = c(7) ,min.node.size=c(5), splitrule = c("extratrees"))


w_RF31 <- train(as.factor(default) ~., data = reducedtrain,
                  method = "ranger",  metric = "Total money lost" ,
                  tuneGrid = RF31_grid,
                  trControl = TC31,
                  weights= model_weights ,
                  maximize = FALSE) 



predicted_w_RF31<-  predict(w_RF31  ,reducedtest[,-11])

table(predicted_w_RF31,  as.factor(reducedtest$default))

cm31 = confusionMatrix(predicted_w_RF31 , as.factor(reducedtest$default))
cm31

testcost31= calculate_cost_test (predicted_w_RF31,  reducedtest$default)
testcost31

#model32
#logreg20 with class weights

set.seed(123)
TC32 <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE)


log_grid32 <- expand.grid(alpha = 1, lambda= c(0.00229))



log_reg32 <- train(as.factor(default) ~., data = alltrainsclall,
                   method = "glmnet",  metric = "Total money lost" ,
                   tuneGrid = log_grid32,
                   trControl = TC32,
                   class.weights = "inverse",
                   maximize = FALSE) 


predicted_log_reg32 <-  predict(log_reg32  ,alltestsclall[,-61])

table(predicted_log_reg32 ,  as.factor(alltestsclall$default))

cm32 = confusionMatrix(predicted_log_reg32 , as.factor(alltestsclall$default))
cm32

testcost32= calculate_cost_test (predicted_log_reg32,  alltestsclall$default)
testcost32



#radialsvm with class weights
set.seed(123)
TC33 <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE)



w_Rad_svm33 <- train(as.factor(default) ~., data = alltrainsclall,
                        method ="svmRadial",  metric = "Total money lost" ,
                        tuneLength = 10,
                        trControl = TC33,
                        class.weights = "inverse",
                        maximize = FALSE) 

predicted_w_Rad_svm33 <-  predict(w_Rad_svm33 ,alltestsclall[,-61])

table(predicted_w_Rad_svm33,  as.factor(alltestsclall$default))

cm33 = confusionMatrix(predicted_w_Rad_svm33 , as.factor(alltestsclall$default))
cm33

testcost33= calculate_cost_test (predicted_w_Rad_svm33,  alltestsclall$default)
testcost33



#radialsvm with  weights

set.seed(123)
TC34 <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE)


wm_Rad_svm34 <- train(as.factor(default) ~., data = alltrainsclall,
                     method ="svmRadial",  metric = "Total money lost" ,
                     tuneLength = 10,
                     trControl = TC34,
                     weigths = model.weights,
                     maximize = FALSE) 

predicted_wm_Rad_svm34 <-  predict(wm_Rad_svm34 ,alltestsclall[,-61])

table(predicted_wm_Rad_svm34,  as.factor(alltestsclall$default))

cm34 = confusionMatrix(predicted_wm_Rad_svm34 , as.factor(alltestsclall$default))
cm34

testcost34= calculate_cost_test (predicted_wm_Rad_svm34,  alltestsclall$default)
testcost34

#knn---
#reduced set
set.seed(123)
TC35 <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE)

knn35 <- train(as.factor(default) ~., data = reducedtrain,
                   method     = "kknn",
                   tuneGrid   = expand.grid(kmax = 1:5, distance = c(1,2) , kernel= c("rectangular", "optimal", "inv")),
                   trControl  = TC35,
                   metric     = "Total money lost",
                   maximize = FALSE       )


predicted_knn35 <-  predict(knn35 ,reducedtest[,-11])

table(predicted_knn35,  as.factor(reducedtest$default))

cm35 = confusionMatrix(predicted_knn35 , as.factor(reducedtest$default))
cm35

testcost35= calculate_cost_test (predicted_knn35,  reducedtest$default)
testcost35


#knn reduced down sampling
set.seed(123)
TC36 <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE)

TC36$sampling <- "down"

knn36 <- train(as.factor(default) ~., data = reducedtrain,
               method     = "kknn",
               tuneGrid   = expand.grid(kmax = 1:5, distance = c(1,2) , kernel= c("rectangular", "optimal", "inv")),
               trControl  = TC36,
               metric     = "Total money lost",
               maximize = FALSE       )


predicted_knn36  <-  predict(knn36  ,reducedtest[,-11])

table(predicted_knn36,  as.factor(reducedtest$default))

cm36 = confusionMatrix(predicted_knn36 , as.factor(reducedtest$default))
cm36

testcost36= calculate_cost_test (predicted_knn36,  reducedtest$default)
testcost36


#ensemble models ---- 

#down ensemble 
#parameters of best downs

set.seed(123)
TC37 <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE, classProbs = TRUE)
TC37$sampling <- "down"
algorithms_to_use37 <- c('glmnet', 'ranger', 'gbm', 'svmRadial')

all  = alltrainsclall

all$default = ifelse (all$default == 1 , "y", "n")

stacked_models37 <- caretList(
  as.factor(default) ~., 
  data = all,
  trControl=TC37, 
  methodList=algorithms_to_use37,
  
  tuneList=list(
    caretModelSpec("gbm", tuneGrid = expand.grid(  interaction.depth = c(3, 5), 
                                                   n.trees = c(3:5)*100, 
                                                   shrinkage = c(0.05,0.1),
                                                   n.minobsinnode = 1)),
    caretModelSpec("ranger", tuneGrid = expand.grid(mtry = c(7) ,min.node.size=5, splitrule = "extratrees")),
    caretModelSpec("glmnet", tuneGrid = expand.grid(alpha = 1, lambda= c(0.00229))),
    caretModelSpec("svmRadial", tuneGrid = expand.grid(C = 1, sigma =  c(0.02224858)))
    )
)

alltest  = alltestsclall

alltest$default = ifelse (alltest$default == 1 , "y", "n")

#glmstack
set.seed(123)
glm_stack37 <- caretStack(stacked_models37, method="glmnet", trControl = TC37, metric     = "Total money lost")


predicted_glm_stack37 <-  predict(glm_stack37  ,alltest[,-61])

table(predicted_glm_stack37,  as.factor(alltest$default))

cm37 = confusionMatrix(predicted_glm_stack37 , as.factor(alltest$default))
cm37

predicted_glm_stack37 = ifelse (predicted_glm_stack37 == "y" , "1", "0")

testcost37= calculate_cost_test (predicted_glm_stack37,  alltestsclall$default)
testcost37

#gbm stack
set.seed(123)
gbm_stack38 <- caretStack(stacked_models37, method="gbm", trControl = TC37, metric     = "Total money lost")


predicted_gbm_stack38  <-  predict(gbm_stack38   ,alltest[,-61])

table(predicted_gbm_stack38 ,  as.factor(alltest$default))

cm38 = confusionMatrix(predicted_gbm_stack38  , as.factor(alltest$default))
cm38

predicted_gbm_stack38  = ifelse (predicted_gbm_stack38  == "y" , "1", "0")

testcost38= calculate_cost_test (predicted_gbm_stack38 ,  alltestsclall$default)
testcost38


#rf stack
set.seed(123)
rf_stack39 <- caretStack(stacked_models37, method="ranger", trControl = TC37, metric     = "Total money lost")


predicted_rf_stack39  <-  predict(rf_stack39   ,alltest[,-61])

table(predicted_rf_stack39,  as.factor(alltest$default))

cm39 = confusionMatrix(predicted_rf_stack39  , as.factor(alltest$default))
cm39

predicted_rf_stack39  = ifelse (predicted_rf_stack39  == "y" , "1", "0")

testcost39= calculate_cost_test (predicted_rf_stack39 ,  alltestsclall$default)
testcost39

#svm stack
set.seed(123)
svm_stack40 <- caretStack(stacked_models37, method="svmRadial", trControl = TC37, metric     = "Total money lost")


predicted_svm_stack40  <-  predict(svm_stack40    ,alltest[,-61])

table(predicted_svm_stack40 ,  as.factor(alltest$default))

cm40 = confusionMatrix(predicted_svm_stack40   , as.factor(alltest$default))
cm40

predicted_svm_stack40   = ifelse (predicted_svm_stack40   == "y" , "1", "0")

testcost40= calculate_cost_test (predicted_svm_stack40  ,  alltestsclall$default)
testcost40


#ensemble with less features
#reduced dataset

set.seed(123)
TC38 <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE, classProbs = TRUE)
TC38$sampling <- "down"
algorithms_to_use38 <- c('glmnet', 'ranger', 'gbm', 'svmRadial')

allred  = reducedtrain

allred$default = ifelse (allred$default == 1 , "y", "n")

stacked_models38 <- caretList(
  as.factor(default) ~., 
  data = allred,
  trControl=TC38, 
  methodList=algorithms_to_use38
   )



allredtest  = reducedtest

allredtest$default = ifelse (allredtest$default == 1 , "y", "n")

#glmstack with less features
set.seed(123)
glm_stack41 <- caretStack(stacked_models38, method="glmnet", trControl = TC38, metric     = "Total money lost")


predicted_glm_stack41  <-  predict(glm_stack41   ,allredtest[,-11])

table(predicted_glm_stack41 ,  as.factor(allredtest$default))

cm41 = confusionMatrix(predicted_glm_stack41  , as.factor(allredtest$default))
cm41

predicted_glm_stack41 = ifelse (predicted_glm_stack41 == "y" , "1", "0")

testcost41= calculate_cost_test (predicted_glm_stack41,  alltestsclall$default)
testcost41


#comparision
#for best 5

finalreport1 = resamples(list(
  Rad_svm27_down=Rad_svm_down27,
  LR22_down=log_reg22,
  LR23_up=log_reg23,
  LR20_down=log_reg20,
  knn36_down_reduced_features=knn36))


finalreport1$values
summary(finalreport1)
bwplot(finalreport1 , horizontal = T)

#select LR22_down= out of these
log_reg22

#get actual test data
  
datatest <- read.csv("test.csv", header = T)
head(datatest) #ids are sorted

  

str(datatest)

#$ Var_39 and  $ Var_53  : char
#converto to numeric
unique(datatest$Var_39)
unique(datatest$Var_53)

datatest$Var_39=ifelse(datatest$Var_39== "Y",1,0)
datatest$Var_53=ifelse(datatest$Var_53== "Y",1,0)

#now all numeric
str(datatest)
summary(datatest)
head(datatest)

#test data
#scale without target column (default) and ids

actualtestscl <- as.data.table(scale(datatest[,-c(1,3)]))
head(actualtestscl)
tail(actualtestscl)
dim(actualtestscl) #847

#check train, same col order?
head(alltrainscl) #847 uniq_id
#yes
  
  
#use the model to predict (FINAL)
  

#retrain on full train
head(datatrain)
dim(datatrain)
#scale without target column (default) [2], drop loan [1]
scldatatrain <- as.data.table(scale(datatrain[,-c(1,2)]))
#add target col
allscldatatrain = cbind(scldatatrain, datatrain$default)
colnames(allscldatatrain)[61] <-  "default" 
head(allscldatatrain)
dim(allscldatatrain) #1715  61 #no id and no loan amount (63-2=61)



##
#log_reg22
###
#	alpha	lambda
#	0.4	0.03856635

#
##


###

set.seed(123)
TC22 <- trainControl(method="cv", number=10, summaryFunction = totalcost, savePredictions=TRUE)
TC22$sampling <- "down"


log_reg22_full <- train(as.factor(default) ~., data = alltrainsclall,
                        method = "glmnet",  metric = "Total money lost" ,
                        tuneGrid = expand.grid(alpha = c(0.4), lambda= c(0.03856635)) ,
                        trControl = TC22,
                        maximize = FALSE) 
log_reg22_full

##

  

predicted_test_log_reg22  <-  predict(log_reg22   ,actualtestscl)
length(predicted_test_log_reg22) #847

head(predicted_test_log_reg22)
result1 =cbind(datatest$loan_application_id, predicted_test_log_reg22)
head(result1)
#need to convert 
result = cbind(datatest$loan_application_id,as.numeric( predicted_test_log_reg22 )-1)
colnames(result) = c("loan_application_id", "prediction")
head(result)
tail(result)



write.csv2(result,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/HW5/testpredictions.csv ", row.names = FALSE)

