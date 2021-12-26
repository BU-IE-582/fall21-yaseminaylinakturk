#yaseminaylinakturk
#hw4


install.packages("randomForest")
library("randomForest")
install.packages("dplyr")
library("dplyr")
install.packages("mice")
library(mice) 
install.packages('skimr')
install.packages("fastmap")
library('skimr')
install.packages("ggplot2")
library('ggplot2')
install.packages("data.table")
library("data.table")
install.packages("cluster")
install.packages("factoextra")
library("factoextra")
library("cluster")
library("caret")
install.packages ("rpart")
library(rpart)
install.packages ("rattle")
install.packages ("bitops")
library("rattle")
library("glmnet")

klasor =  "C:/Users/y.akturk/Documents/"
setwd(klasor)

mymusk <- read.csv("Musk1.csv", header = F)

head(mymusk)
colnames(mymusk)[1:2] <- c("Bag_Class","Bag_Id")

#show structure
str(mymusk)



nrow(mymusk)
ncol(mymusk)

#check NA
md.pattern(mymusk)

#non of them has N/A values.


#quick look
skim(mymusk)
summary(mymusk)

#duplicated?
sum(duplicated(mymusk))
#no dublicated instances


#balance check
#no of class1 bags
hist(mymusk$Bag_Class)
class1 = sum(mymusk$Bag_Class)
class1 
#total
nrow(mymusk)
#class0= total-class1
class0=  nrow(mymusk) - class1 
class0
ratio = class1/nrow(mymusk)
ratio


#plot 
 

plot(mymusk$Bag_Id,mymusk$Bag_Class)

ggplot(mymusk, aes(x = as.factor(Bag_Class), y = as.factor (Bag_Id), color = Bag_Class)) +
   geom_jitter() +
   theme_minimal() +
   theme(legend.position = "none") +
   labs(title = " Bag Class vs Bag Id",
        x = "Bag Class",
        y = "Bag Id")

 
 #  The features should be scaled 

sclmusk <- scale(mymusk[,-c(1,2)])
head(sclmusk)

#k-medoids will be used with euclidean and manhattan distance for representing the problem.
#As distance metrics; euclidean and manhattan distances are going to be used.
#The number of clusters need to be determined and tuned accordingly.
#Where the biggest drop happens in the Within Sum of Squares(WSS) happens considered as the optimal number of clusters. 

#does not have the first two columns
euc_dist <- sclmusk %>% dist("euclidean") 
man_dist <- sclmusk %>% dist("manhattan") 


euc_dist<-as.data.table(as.matrix(euc_dist))
man_dist<-as.data.table(as.matrix(man_dist))
#sample for dist matrix
euc_dist[1:6, c(1,2,3,120,124,146)]

#Find the Optimal Number of Clusters
#To perform k-medoids clustering in R we can use the pam() function,
#which stands for “partitioning around medians” and uses the following
#https://www.statology.org/k-medoids-in-r/

#we had it scaled


#Since we don’t know beforehand how many clusters is optimal, we’ll create a plot that can help us decide:
  
 # 1. Number of Clusters vs. the Total Within Sum of Squares

#First, we’ll use the fviz_nbclust() function to create a plot of the number of clusters vs. the total within sum of squares:


graphE= fviz_nbclust(sclmusk, pam, method = "wss")
graphE$data

fviz_nbclust(sclmusk, pam, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)
  
#

#The total within sum of squares will typically always increase as we increase the number of clusters, so when we create this type of plot we look for an “elbow” where the sum of squares begins to “bend” or level off.

#The point where the plot bends is typically the optimal number of clusters. Beyond this number, overfitting is likely to occur.

#For this plot it appear that there is a bit of an elbow or “bend” at k = 4 clusters

# Perform K-Medoids Clustering with Optimal K
#Lastly, we can perform k-medoids clustering on the dataset using the optimal value for k of 4:

#perform k-medoids clustering with k = 4 clusters
kmedoidsE <- pam(sclmusk, k = 4,metric = "euclidean")

#view results
kmedoidsE
#We can visualize the clusters on a scatterplot that displays the first two principal components on the axes using the fivz_cluster() function:

#plot results of final k-medoids model
fviz_cluster(kmedoidsE, data = sclmusk)



##########
#man


graphM=fviz_nbclust(sclmusk, pam, metric = "manhattan", method = "wss")+
  geom_vline(xintercept = 3, linetype = 2)
graphM
graphM$data
kmedoidsM <- pam(sclmusk, k = 3,metric = "manhattan")
kmedoidsM
fviz_cluster(kmedoidsM, data = sclmusk)
###

#representation#
####


#euclidean
# represent the data so that the features show the average distances to the center of the clusters

kmedoidsE$id.med

mdis1 = c() #empty dist 
k= 4  #4 clusters for euclidean
#generate 4 features with the distance info

mdis1<-cbind(euc_dist[,get(as.character(as.numeric(kmedoidsE$id.med[1])))],euc_dist[,get(as.character(as.numeric(kmedoidsE$id.med[2])))],euc_dist[,get(as.character(as.numeric(kmedoidsE$id.med[3])))],euc_dist[,get(as.character(as.numeric(kmedoidsE$id.med[4])))])


mdis1

mdis1 <- mdis1 %>% as.data.table(mdis1)
setnames(mdis1, old=colnames(mdis1), new=c("Cls1","Cls2","Cls3","Cls4"))

bag1 <- cbind(mymusk[,1:2], mdis1) %>% as.data.table()
bagmean1 <- bag1 %>% aggregate(., list(bag1$Bag_Id), mean) %>% select(-Group.1) 
head(bagmean1)
tail(bagmean1)


#manhattan

# represent the data so that the features show the average distances to the center of the clusters

kmedoidsM$id.med

mdis2 = c() #empty dist 
kl= 3  #3 clusters for manhattan
#generate 3 feautures with the distance info

mdis2<-cbind(man_dist[,get(as.character(as.numeric(kmedoidsM$id.med[1])))],euc_dist[,get(as.character(as.numeric(kmedoidsM$id.med[2])))],euc_dist[,get(as.character(as.numeric(kmedoidsM$id.med[3])))])


mdis2

mdis2 <- mdis2 %>% as.data.table(mdis2)
setnames(mdis2, old=colnames(mdis2), new=c("Cls1","Cls2","Cls3"))

bag2 <- cbind(mymusk[,1:2], mdis2) %>% as.data.table()
bagmean2 <- bag2 %>% aggregate(., list(bag2$Bag_Id), mean) %>% select(-Group.1) 
head(bagmean2)
tail(bagmean2)


#transform 0-1 to factors
bagmean1$Bag_Class = ifelse(bagmean1$Bag_Class == 1, "onebag", "zerobag")
head(bagmean1)
bagmean2$Bag_Class = ifelse(bagmean2$Bag_Class == 1, "onebag", "zerobag")
head(bagmean2)

#Decision tree


#with Euclidean rep
Euc_grid <- expand.grid(cp=c(0.005,0.007,0.01,0.03,0.05,0.1))
#it would not take minbucket as a grid value, I tried manually
Traincontrol_Euc <-trainControl(method="cv", number=10,  allowParallel=TRUE, classProbs = TRUE)


#the decision tree with euclidean rep.

set.seed(123)
tree_Euc5 <- train(x=as.matrix(bagmean1[,3:6],with = F ),y=as.matrix(bagmean1$Bag_Class,with = F), method ="rpart", tuneGrid = Euc_grid ,trControl=Traincontrol_Euc, control = rpart.control(minbucket=c(5)))
tree_Euc5
set.seed(123)
tree_Euc7 <- train(x=as.matrix(bagmean1[,3:6],with = F ),y=as.matrix(bagmean1$Bag_Class,with = F), method ="rpart", tuneGrid = Euc_grid ,trControl=Traincontrol_Euc, control = rpart.control(minbucket=c(7)))
tree_Euc7
set.seed(123)
tree_Euc10 <- train(x=as.matrix(bagmean1[,3:6],with = F ),y=as.matrix(bagmean1$Bag_Class,with = F), method ="rpart", tuneGrid = Euc_grid ,trControl=Traincontrol_Euc, control = rpart.control(minbucket=c(10)))
tree_Euc10


##final tree Euc

mybag1=as.data.table(bagmean1)
my_tree_Euc <- rpart(Bag_Class~Cls1+Cls2+Cls3+Cls4 , mybag1, minbucket = c(5), cp = 0.05)

fancyRpartPlot(my_tree_Euc)

###



logbag1= bagmean1
logbag1$Bag_Class=ifelse(logbag1$Bag_Class== "onebag",1,0)
head(logbag1)
logbag2= bagmean2
logbag2$Bag_Class=ifelse(logbag2$Bag_Class== "onebag",1,0)
head(logbag2)

k1 =  sum(logbag1$Bag_Class)
tot = length(logbag1$Bag_Class)
k0 = tot - k1
k1
k0
kratio= k1/tot
kratio 

k1m =  sum(logbag2$Bag_Class)
totm = length(logbag2$Bag_Class)
k0m = totm - k1m
k1m
k0m
kratiom= k1m/totm
kratiom 

#classratio is 0.51 not imbalanced, however this ratio can still be used for prediction


predicted_prob1 <-  predict(my_tree_Euc,bagmean1,type= "prob")
predicted_class1  <- ifelse(predicted_prob1 [,1] > ratio, "onebag", "zerobag")
confusion_mat1 <- addmargins(table(bagmean1$Bag_Class, predicted_class1 ))
confusion_mat1
#training accuracy
trainerr1 =  1- (sum(diag((confusion_mat1[1:2,1:2]))) /  confusion_mat1[3,3])
trainerr1
acc_train1= 1- trainerr1
acc_train1



#with Manhattan rep
set.seed(123)
Man_grid <- expand.grid(cp=c(0.005,0.007,0.01,0.03,0.05,0.1))
#it would not take minbucket as a grid value, I tried manually
Traincontrol_Man <-trainControl(method="cv", number=10,  allowParallel=TRUE, classProbs = TRUE)




#the decision tree with manh rep
set.seed(123)
tree_Man1 <- train(x=as.matrix(bagmean2[,3:5],with = F ),y=as.matrix(bagmean2$Bag_Class,with = F), method ="rpart", tuneGrid = Man_grid ,trControl=Traincontrol_Man, control = rpart.control(minbucket=c(1)))
tree_Man1
tree_Man5 <- train(x=as.matrix(bagmean2[,3:5],with = F ),y=as.matrix(bagmean2$Bag_Class,with = F), method ="rpart", tuneGrid = Man_grid ,trControl=Traincontrol_Man, control = rpart.control(minbucket=c(5)))
tree_Man5
tree_Man7 <- train(x=as.matrix(bagmean2[,3:5],with = F ),y=as.matrix(bagmean2$Bag_Class,with = F), method ="rpart", tuneGrid = Man_grid ,trControl=Traincontrol_Man, control = rpart.control(minbucket=c(7)))
tree_Man7
tree_Man10 <- train(x=as.matrix(bagmean2[,3:5],with = F ),y=as.matrix(bagmean2$Bag_Class,with = F), method ="rpart", tuneGrid = Man_grid ,trControl=Traincontrol_Man, control = rpart.control(minbucket=c(10)))
tree_Man10


##final tree man

mybag2=as.data.table(bagmean2)
my_tree_Man <- rpart(Bag_Class~Cls1+Cls2+Cls3, mybag2, minbucket = c(5), cp = 0.05)

fancyRpartPlot(my_tree_Man)

predicted_prob2<-  predict(my_tree_Man, bagmean2,type= "prob")
predicted_class2 <- ifelse(predicted_prob2[,1] > ratio, "onebag", "zerobag")
confusion_mat2 <- addmargins(table(bagmean2$Bag_Class, predicted_class2 ))
confusion_mat2
#training accuracy
trainerr2 =  1- (sum(diag((confusion_mat2[1:2,1:2]))) /  confusion_mat2[3,3])
trainerr2
acc_train2= 1- trainerr2
acc_train2


         

#Random forest
set.seed(123)

#euclidean

Euc_grid_RF <- expand.grid(mtry = c(1:4) ,min.node.size=5, splitrule = "gini")
Traincontrol_Euc_RF <-trainControl(method="cv", number=10,  allowParallel=TRUE, classProbs = TRUE)
RF_euc <- train(x=as.matrix(bagmean1[,3:6],with = F ),y=as.matrix(bagmean1$Bag_Class), method="ranger", tuneGrid= Euc_grid_RF, trControl= Traincontrol_Euc_RF)
RF_euc

#plot

my_RF_euc =randomForest(bagmean1[,-c(1,2)],as.factor(bagmean1$Bag_Class),mtyr=2, min.node.size = 5, splitrule = "gini")
print(my_RF_euc)
plot(my_RF_euc)
my_RF_euc$confusion


err =  1- (sum(diag((my_RF_euc$confusion[1:2,1:2]))) /  sum((my_RF_euc$confusion[1:2,1:2])))
err

acc= 1- err
acc

#manhattan
set.seed(123)
Man_grid_RF <- expand.grid(mtry = c(1:3) ,min.node.size=5, splitrule = "gini")
Traincontrol_Man_RF <-trainControl(method="cv", number=10,  allowParallel=TRUE, classProbs = TRUE)
RF_man <- train(x=as.matrix(bagmean2[,3:5],with = F ),y=as.matrix(bagmean2$Bag_Class), method="ranger", tuneGrid= Man_grid_RF, trControl= Traincontrol_Man_RF)
RF_man

#plot

my_RF_man =randomForest(bagmean2[,-c(1,2)],as.factor(bagmean2$Bag_Class),mtyr=1, min.node.size = 5, splitrule = "gini")
print(my_RF_man)
plot(my_RF_man)
my_RF_man$confusion

errm =  1- (sum(diag((my_RF_man$confusion[1:2,1:2]))) /  sum((my_RF_man$confusion[1:2,1:2])))
errm

accm= 1- errm
accm





#log reg with caret

set.seed(123)
log_euc_grid <- expand.grid(alpha = 1, lambda= seq( from = 0.001, to =0.1, by = 0.0001))
Traincontrol_euc_log <-trainControl(method="cv", number=10,  allowParallel=TRUE, classProbs = TRUE)
log_euclid <- train(x=as.matrix(bagmean1[,3:6],with = F ),y=as.matrix(bagmean1$Bag_Class), method="glmnet",family='binomial',type.measure = "deviance",tuneGrid= log_euc_grid , trControl= Traincontrol_euc_log )
log_euclid

set.seed(123)
log_man_grid <- expand.grid(alpha = 1, lambda= seq( from = 0.001, to =0.1, by = 0.001))
Traincontrol_Man_log <-trainControl(method="cv", number=10,  allowParallel=TRUE, classProbs = TRUE)
log_manha <- train(x=as.matrix(bagmean2[,3:5],with = F ),y=as.matrix(bagmean2$Bag_Class), method="glmnet",family='binomial',type.measure = "deviance", tuneGrid= log_man_grid , trControl= Traincontrol_Man_log )
log_manha


#use the distance info  to build a logistic regression model with penalty



k1 =  sum(logbag1$Bag_Class)
tot = length(logbag1$Bag_Class)
k0 = tot - k1
k0
kratio= k1/tot

###

logbag1= bagmean1
logbag1$Bag_Class=ifelse(logbag1$Bag_Class== "onebag",1,0)
head(logbag1)
logbag2= bagmean2
logbag2$Bag_Class=ifelse(logbag2$Bag_Class== "onebag",1,0)
head(logbag2)

#use this distance info to build a model 
#use training distance matrix to build model with 10 folds

#euc

cvfitds=cv.glmnet(as.matrix(logbag1[,3:6],with = F ),logbag1$Bag_Class,family='binomial',nfolds=10, type.measure = "deviance")
cvfitds

cvfitdsa=cv.glmnet(as.matrix(logbag1[,3:6],with = F ),logbag1$Bag_Class,family='binomial',nfolds=10, type.measure = "class")
cvfitdsa

#predict on training data (deviance)
prob <-  predict(cvfitds,as.matrix(logbag1[,3:6],with = F ),type='response', s="lambda.min")
pred <- as.integer(prob > ratio)
euc_confusion_mat <- addmargins(table(logbag1$Bag_Class, pred ))
euc_confusion_mat

euc_err =  1- (sum(diag((euc_confusion_mat[1:2,1:2]))) /  euc_confusion_mat[3,3])
euc_err

euc_acc= 1- euc_err
euc_acc



#predict on training data (accuracy)
proba <-  predict(cvfitdsa,as.matrix(logbag1[,3:6],with = F ),type='response', s="lambda.min")
preda <- as.integer(proba > ratio)
euc_confusion_mata <- addmargins(table(logbag1$Bag_Class, preda ))
euc_confusion_mata

euc_erra =  1- (sum(diag((euc_confusion_mata[1:2,1:2]))) /  euc_confusion_mata[3,3])
euc_erra

euc_acca= 1- euc_erra
euc_acca



#man
cvfitdsman=cv.glmnet(as.matrix(logbag2[,3:5],with = F ),logbag2$Bag_Class,family='binomial',nfolds=10, type.measure = "deviance")
cvfitdsman
cvfitdsmana=cv.glmnet(as.matrix(logbag2[,3:5],with = F ),logbag2$Bag_Class,family='binomial',nfolds=10, type.measure = "class")
cvfitdsmana


#predict on training data(deviance)
probman <-  predict(cvfitdsman,as.matrix(logbag2[,3:5],with = F ),type='response', s="lambda.min")
predman <- as.integer(probman > ratio)
man_confusion_mat <- addmargins(table(logbag2$Bag_Class, predman ))
man_confusion_mat

man_err =  1- (sum(diag((man_confusion_mat[1:2,1:2]))) / man_confusion_mat[3,3])
man_err

man_acc= 1- man_err
man_acc

#predict on training data (accuracy)
probmana <-  predict(cvfitdsmana,as.matrix(logbag2[,3:5],with = F ),type='response', s="lambda.min")
predmana <- as.integer(probmana > ratio)
man_confusion_mata <- addmargins(table(logbag2$Bag_Class, predmana ))
man_confusion_mata

man_erra =  1- (sum(diag((man_confusion_mata[1:2,1:2]))) / man_confusion_mata[3,3])
man_erra

man_acca= 1- man_erra
man_acca



#report results

myreport = resamples(list(DecTree_Euc=tree_Euc5, DecTree_Man=tree_Man5,RF_Euc =RF_euc,    RF_Man =RF_man))
myreport
myreport$values
summary(myreport)
bwplot(myreport , metric = "Accuracy", horizontal = T)

myreport$methods

#now with log reg also


myreport2 = resamples(list(DecTree_Euc=tree_Euc5, DecTree_Man=tree_Man5,RF_Euc =RF_euc,    RF_Man =RF_man, Log_Man =log_manha , Log_Euc =log_euclid))
myreport2
myreport2$values
summary(myreport2)
bwplot(myreport2 , metric = "Accuracy", horizontal = T)


