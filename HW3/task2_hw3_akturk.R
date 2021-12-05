##IE 585 - hw#3 -2020802000 yasemin aylin akturk
#hw3 task2
klasor =  "C:/Users/y.akturk/Documents/"
setwd(klasor)


x_acc <- read.csv2("uWaveGestureLibrary_X_TRAIN.csv", header = F, fileEncoding = 'UTF-8-BOM')
y_acc <- read.csv2("uWaveGestureLibrary_Y_TRAIN.csv", header = F, fileEncoding = 'UTF-8-BOM')
z_acc <- read.csv2("uWaveGestureLibrary_Z_TRAIN.csv", header = F, fileEncoding = 'UTF-8-BOM')

test_x_acc <- read.csv2("uWaveGestureLibrary_X_TEST.csv", header = F, fileEncoding = 'UTF-8-BOM')
test_y_acc <- read.csv2("uWaveGestureLibrary_Y_TEST.csv", header = F, fileEncoding = 'UTF-8-BOM')
test_z_acc <- read.csv2("uWaveGestureLibrary_Z_TEST.csv", header = F, fileEncoding = 'UTF-8-BOM')


#rename col names

colnames (x_acc)[1] = c("Ges_Class")
colnames (y_acc)[1] = c("Ges_Class")
colnames (z_acc)[1] = c("Ges_Class")


colnames (test_x_acc)[1] = c("Ges_Class")
colnames (test_y_acc)[1] = c("Ges_Class")
colnames (test_z_acc)[1] = c("Ges_Class")

#sample data

x_acc[1:8,1:3]
y_acc[1:8,1:3]
z_acc[1:8,1:3]

#concatenate train set and test set

cop_x_acc = x_acc
cop_y_acc = y_acc[,-1]
cop_z_acc = z_acc[,-1]

test_cop_x_acc = test_x_acc
test_cop_y_acc = test_y_acc[,-1]
test_cop_z_acc = test_z_acc[,-1]

#rename columns

colnames(cop_x_acc)[2:316] <-paste("X.", 1:315)
colnames(cop_y_acc)[1:315] <-paste("Y.", 1:315)
colnames(cop_z_acc)[1:315] <-paste("Z.", 1:315)

colnames(test_cop_x_acc)[2:316] <-paste("X.", 1:315)
colnames(test_cop_y_acc)[1:315] <-paste("Y.", 1:315)
colnames(test_cop_z_acc)[1:315] <-paste("Z.", 1:315)


#combine data
train_Data_Conc <-as.data.frame(cbind(cop_x_acc[,],cop_y_acc[,],cop_z_acc[,]))
test_Data_Conc <-as.data.frame(cbind(test_cop_x_acc[,],test_cop_y_acc[,],test_cop_z_acc[,]))

#sample train
train_Data_Conc[1:3, c(1,2,320,850)]
train_Data_Conc[,1] = as.factor(train_Data_Conc[,1])
train_Data_Conc[1:3, c(1,2,320,850)]

#sample test
test_Data_Conc[1:3, c(1,2,320,850)]
test_Data_Conc[,1] = as.factor (test_Data_Conc[,1])
test_Data_Conc[1:3, c(1,2,320,850)]

#show dimension
dim(train_Data_Conc)
dim(test_Data_Conc)



#scale
#doing something distance based then scaled data should be used
Scaled_Train_Data_Conc <- train_Data_Conc
Scaled_Train_Data_Conc[,-1] <- scale(train_Data_Conc[,-1])
#Scaled_Train_Data_Conc[1:3, c(1,2,320,850)]



Scaled_Test_Data_Conc <- test_Data_Conc
Scaled_Test_Data_Conc[,-1] <- scale(test_Data_Conc[,-1])
Scaled_Test_Data_Conc[1:3, c(1,2,320,850)]


#In the first column we have the class ID, the stadandardized x1-x2---y1-y2--z1-z2-- in the rest of the columns
#same until here

install.packages("glmnet")

library("glmnet")

#make the class info binary
#put 1 if class3 else put 0
#transform both training and test data

c_train=Scaled_Train_Data_Conc
c_train[1:40,1:2]

c_train$Ges_Class=ifelse(c_train$Ges_Class==3,1,0)
c_train[1:40,1:2]


c_test=Scaled_Test_Data_Conc
c_test[1:20,1:2]
c_test$Ges_Class=ifelse(c_test$Ges_Class==3,1,0)
c_test[1:20,1:2]

##logistic regression


log_reg=glm(Ges_Class~.,c_train,family='binomial')
summary(log_reg)


predicted=predict(log_reg,c_train)
head(predicted)

#This warning checks if the rank of the data matrix 
#is at least equal to the number of parameters 

predicted=predict(log_reg,c_train,type='response')
head(predicted)

head(coef(log_reg))
tail(coef(log_reg))

predicted_prob <-  predict(log_reg,c_train,type='response')
##ratio calculation
ratio= sum(c_train[,1])/ length(c_train[,1])

prediction <- as.integer(predicted_prob > ratio)

confusion_mat <- addmargins(table(c_train$Ges_Class, prediction))

# Labeling
names(dimnames(confusion_mat)) <- c("True status", "Prediction")
confusion_mat

#training accuracy
trainerr =  1- (sum(diag((confusion_mat[1:2,1:2]))) /  confusion_mat[3,3])
trainerr

acc_train= 1- trainerr
acc_train

#accuracy is 1. Overfitting is suspectible. It probably picks up many noisy variables.

#on test data
predicted_prob_test <-  predict(log_reg,c_test,type='response')
predicted_prob_test
prediction_test <- as.integer(predicted_prob_test > ratio)

confusion_mat_test <- addmargins(table(c_test$Ges_Class, prediction_test))

# Labeling
names(dimnames(confusion_mat_test)) <- c("True status", "Prediction")
confusion_mat_test

#testing accuracy
testerr =  1- (sum(diag((confusion_mat_test[1:2,1:2]))) /  confusion_mat_test[3,3])
testerr

acc_test= 1- testerr
acc_test

#50% accuracy is not sufficiently good. The model performed poorly on the test data.

###do with glmnet lambda = 0####

log_reg1=glmnet(as.matrix(c_train[,-1]),c_train$Ges_Class,family='binomial')
log_reg1

predicted_prob_test1 <-  predict(log_reg1,as.matrix(c_test[,-1]),type='response', s= 0)
prediction_test1 <- as.integer(predicted_prob_test1 > ratio)

conf_mat_test1 <- addmargins(table(c_test$Ges_Class, prediction_test1))


conf_mat_test1

head(coef(log_reg1, s=0))
tail(coef(log_reg1, s=0))

testerr1 =  1- (sum(diag((conf_mat_test1[1:2,1:2]))) /  conf_mat_test1[3,3])
testerr1
acc1 = 1- testerr1
acc1

#total non zero coeff + 1 intercept
howmanylog= sum(ifelse(coef(log_reg1,s=0) != 0 , 1,0))


howmanylog

#######penalize###########
#alpha=1 is lasso regression (default)
library("glmnet")
library("dplyr")
class(c_train)
class_mat_train = as.matrix(c_train[,-1])
cvlogfit=cv.glmnet(class_mat_train,c_train$Ges_Class,family='binomial',nfolds=10, type.measure = "deviance")
cvlogfit
plot(cvlogfit)
#show coefficients 
coef(cvlogfit,s="lambda.min")[c(1,15,16,18,327,328,340,632,880,888),1]

###


dim(coef(cvlogfit,s="lambda.min"))
howmanyx= sum(ifelse(coef(cvlogfit,s="lambda.min")[2:316,1] != 0 , 1,0))
howmanyy= sum(ifelse(coef(cvlogfit,s="lambda.min")[317:631,1] != 0 , 1,0))
howmanyz= sum(ifelse(coef(cvlogfit,s="lambda.min")[632:946,1] != 0 , 1,0))
howmanyx
howmanyy
howmanyz
#total non zero coeff + 1 intercept
howmany = howmanyx +howmanyy + howmanyz +1 
howmany
#before we had 3*315 = 945 features, we reduced them. Now we have 36 features and 1 intercept.
#now see, in which indexes the non-zero coefficients occur
#plot one instance from each class as time series, and try to catch where class#3 is distinct the most.
#in terms of distance from one the other, we may see the part (in index) where the class#3 is the most distinct from the others.

#########plot

#the acceleration data for classes in x1-x2----y1-y2----z1-z2 format
#get a random instance of ech class
#use Scaled_Train_Data_Conc data
Scaled_Train_Data_Conc[c(1,2,4,7,8,11,20,21),1:2]
#index 1-gesture6
#index 2-gesture5
#index 4-gesture3##
#index 7-gesture7
#index 8-gesture4
#index 11-gesture1
#index 20-gesture2
#index 21-gesture8


#this is class#3
plot( c(1:945),Scaled_Train_Data_Conc[4,-1],type="l", xlab="Index", col="red",ylab="", main ="Training data in timeseries")
#plot the others
for (i in c(1,2,7,8,11,20,21))   
{
  points ( c(1:945),Scaled_Train_Data_Conc[i,-1],type="l", xlab="Index", col="beige") 
 
}

#add a vertical line for which the coeffieicients a nonzero, for instance
coef(cvlogfit,s="lambda.min")[c(1,15,16,18,327,328,340,632,880,888),1]

no=0
for (i in 2:946) {
if (coef(cvlogfit,s="lambda.min")[i,1] != 0) 
{#print ( coef(cvlogfit,s="lambda.min")[i,1]) #show which
 print(i)
  no = no+1
  abline ( v  =  i-1 , col = "black")} 
  # minus 1st index intercept,indexofcoord=i-1, where intercept(i=1),x1... x(index)=i-1,...x315, y1,...z1,...z315
}
no # number of nonzero coeff (excluding intercept, only for x1,x2,,z314,z315)
#first 315 for x, next 315 for y, the last 315 for z
#nonzero ones are in between x235-x336 mostly, a few nonzero for y and z has even less has affect
#coordinate values has the most affect on the results, the class#3 behavior differs from the other class in terms of the xcord values the most


####################
#test on test data, use lambda min
class_mat_test = as.matrix(c_test[,-1])
#on test data
pen_prob_test <-  predict(cvlogfit,class_mat_test,type='response', s='lambda.min')
pen_prob_test
pen_pred_test <- as.integer(pen_prob_test > ratio)

pen_confusion_mat_test <- addmargins(table(c_test$Ges_Class, pen_pred_test))

# Labeling
names(dimnames(pen_confusion_mat_test)) <- c("True status", "Prediction")
pen_confusion_mat_test

#testing accuracy
pentesterr =  1- (sum(diag((pen_confusion_mat_test[1:2,1:2]))) /  pen_confusion_mat_test[3,3])
pentesterr 

pen_acc_test= 1- pentesterr
pen_acc_test

#accuracy is increased compared to the model without penalties. 
#overfitting is prevented since not all of the features were employed in the model


###distance matrix based approach
#we already have
class_mat_train[1:5,1:5]
class_mat_test [1:5,1:5]
#get the transpose, put in vector from
tr_train = t(class_mat_train)
tr_test = t(class_mat_test)
tr_train [1:5,1:5]
tr_test [1:5,1:5]
#check dim
dim(tr_train)
dim(tr_test)
#calculate distance between training of each instance and each instance of trainig 
#compute on train data

dismat_train = matrix( nrow = ncol(tr_train), ncol = ncol(tr_train))
dismat_tt = matrix( nrow = ncol(tr_test), ncol = ncol(tr_train))

for (i in 1:ncol(tr_train))
{
for (j in 1:ncol(tr_train))
  {
    
dismat_train [i,j]  = sqrt(sum( (tr_train[,i] -  tr_train[,j])^2 ) )

  } #end j
} #end i


dismat_train [1:5,1:5]

#distance between 1st instance and 4th is 50.69
dim(dismat_train)
#it is 896*896 N*N

#calculate distance between  each instance of test and each instance of training o

for (i in 1:ncol(tr_test))
{
  for (j in 1:ncol(tr_train))
  {
    
    dismat_tt [i,j]  = sqrt(sum( (tr_train[,j] -  tr_test[,i])^2 ) )
    
  } #end j
} #end 

#sample train & test distance matrix
dismat_tt [1:5,1:5]

#distance between 1st test instance and 4th trainin instance is 41.30
dim(dismat_tt)
#it is 3582*896 Ntest*N


#use this distance info to build a model 
#use training distance matrix to build model with 10 fold

cvfitds=cv.glmnet(dismat_train,c_train$Ges_Class,family='binomial',nfolds=10, type.measure = "deviance")
cvfitds
plot(cvfitds)


#show coefficients
coef(cvfitds,s="lambda.min")[c(1,5,6,113,190,200),1]

###


dim(coef(cvfitds,s="lambda.min"))
howmanyds= sum(ifelse(coef(cvfitds,s="lambda.min") != 0 , 1,0))

#total non zero coeff + 1 intercept

howmanyds

# predict on test


ds_prob <-  predict(cvfitds,dismat_tt,type='response', s="lambda.min")
ds_pred <- as.integer(ds_prob > ratio)

ds_confusion_mat <- addmargins(table(c_test$Ges_Class, ds_pred ))

ds_confusion_mat

#testing accuracy
dserr =  1- (sum(diag((ds_confusion_mat[1:2,1:2]))) /  ds_confusion_mat[3,3])
dserr

dsacc= 1- dserr
dsacc