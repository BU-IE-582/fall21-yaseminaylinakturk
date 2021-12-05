#IE 585 - hw#3 -2020802000 yasemin aylin akturk
#task1
klasor =  "C:/Users/y.akturk/Documents/"
setwd(klasor)

install.packages("caret", dependencies= TRUE)
install.packages("kknn")
install.packages("class")

library("caret")

library("kknn")

library("class")

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


#train using crossvalidation 10 folds
trControl <- trainControl(method  = "cv", number  = 10)


#train model
#"rectangular" (which is standard unweighted knn)
#try k 1 to 5
#try distance euclidean  (minkowski parameter = 2)
#try distance manhattan  (minkowski parameter = 1)
#Using the Manhattan metric, a distance between two time series is the area between them.


fit1 <- train(Ges_Class ~ .,
             method     = "kknn",
             tuneGrid   = expand.grid(kmax = 1:5, distance = 1 , kernel= "rectangular"),
             trControl  = trControl,
             metric     = "Accuracy",
             data       = Scaled_Train_Data_Conc)

fit1
#best K=2 when dist=1


fit2 <- train(Ges_Class ~ .,
              method     = "kknn",
              tuneGrid   = expand.grid(kmax = 1:5, distance = 2 , kernel= "rectangular"),
              trControl  = trControl,
              metric     = "Accuracy",
              data       = Scaled_Train_Data_Conc)

fit2

#distance=2 Euclidean, best K=2
#distance = 1 Manhattan, best K=2




####manhattan k3
start.time <- Sys.time()

Gesture.kknn1 <- kknn(Ges_Class ~ ., Scaled_Train_Data_Conc, Scaled_Test_Data_Conc,distance = 1, k=3, kernel ="rectangular")

table(Scaled_Test_Data_Conc$Ges_Class, Gesture.kknn1$fit)

error_man = 1 - sum (Scaled_Test_Data_Conc$Ges_Class ==  Gesture.kknn1$fit)/nrow(Scaled_Test_Data_Conc)
error_man

accur_man = 1 - error_man
accur_man


end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

####euclidean k2
start.time <- Sys.time()

Gesture.kknn2 <- kknn(Ges_Class ~ ., Scaled_Train_Data_Conc, Scaled_Test_Data_Conc,distance = 2, k=2, kernel ="rectangular")

table(Scaled_Test_Data_Conc$Ges_Class, Gesture.kknn2$fit)

error_euc = 1 - sum (Scaled_Test_Data_Conc$Ges_Class ==  Gesture.kknn2$fit)/nrow(Scaled_Test_Data_Conc)
error_euc

accur_euc = 1 - error_euc

accur_euc


end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


###overall
fit <- train(Ges_Class ~ .,
             method     = "kknn",
             tuneGrid   = expand.grid(kmax = 1:5, distance = 1:2 , kernel= "rectangular"),
             trControl  = trControl,
             metric     = "Accuracy",
             data       = Scaled_Train_Data_Conc)

fit

###overall end


#####try in another way

(fit.train1 <- train.kknn(Ges_Class ~ ., Scaled_Train_Data_Conc, kmax = 5, distance = 1, kernel ="rectangular", kcv=10))

table(predict(fit.train1, Scaled_Test_Data_Conc), Scaled_Test_Data_Conc$Ges_Class)



(fit.train2 <- train.kknn(Ges_Class ~ ., Scaled_Train_Data_Conc, kmax = 5, distance = 2,kernel ="rectangular",  kcv=10))

table(predict(fit.train2, Scaled_Test_Data_Conc), Scaled_Test_Data_Conc$Ges_Class)


###

