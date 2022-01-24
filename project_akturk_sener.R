#IE582 project 
#group 3
#yasemin aylin akturk
#melike nur sener

#library ----
klasor =  "C:/Users/y.akturk/Documents/"
setwd(klasor)

library("data.table")
library("xlsx")
#install.packages("tidyverse")
library("dplyr")
library("tidyverse")
library("mice")
library("tidyr")
library("stringr")
library("ggplot2")
library("caret")
library("glmnet")
#install.packages("smotefamily")
library("smotefamily")
library(dplyr) # for data manipulation
library(caret) # for model-building
#install.packages("DMwR")
#library(DMwR) # for smote implementation
library(purrr) # for functional programming (map)
library(pROC) # for AUC calculations
#install.packages("gbm")
library("gbm")
library("corrplot")
#install.packages("ROSE")
library("ROSE")
library(randomForest)
library(caret)
#install.packages("e1071")
library(e1071)
#install.packages("kernlab")
library("kernlab")
library("smotefamily")
library("fastAdaboost")
library("adabag")
library("class")
library("kknn")
#install.packages("caretEnsemble")
library(caretEnsemble)

#install.packages("doParallel")
#install.packages("combinat")
install.packages("questionr")
install.packages("naivebayes")
library(naivebayes)
install.packages("forcats")
library("resample")
library("klaR")

memory.limit()

## To increase the storage capacity
memory.limit(size=56000)




#read ----

traindata <- read.csv("train.csv", header = T, fileEncoding = 'UTF-8-BOM')



head(traindata)


dim(traindata) #5493268      19

#duplicated ----
orgtrain =traindata

a=as.data.table(orgtrain)

a[time_stamp=="2020-10-14T10:42:23Z" & unique_id == "714",] 

#there are many duplicated 

sum(duplicated(traindata))



traindata= unique(orgtrain)   #only with distinct values.
traindata= distinct(orgtrain)  
utrain = orgtrain%>%group_by_all%>%count  #yes there are duplicated
dim(utrain)

dim(traindata)  #the dimension diminihes #2077356      19
length(unique(traindata$unique_id)) #5618

write.csv(traindata,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/trainunique.csv", row.names = FALSE)

#fill empty ones with NA 

traindata [2750:2755,]
traindata [traindata  == " "] <- NA
traindata [traindata  == ""] <- NA
traindata [2750:2755,]

#check NA #how many NA's
mynalist = cbind(
  lapply(
    lapply(traindata, is.na)
    , sum)
)

mynalist

#pattern NA ----
library("VIM")
mice_plot <- aggr(traindata, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(traindata), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

#

#try to fill the data for product gender with the help of business unit ----

missgender = traindata[,c(8,9)]


missgender$product_gender = ifelse(is.na(missgender$product_gender), "notavailable", missgender$product_gender )


table(missgender$businessunit, missgender$product_gender)

#decided to replace all missing product genders with "unisex"
#it seems appropirate given the above table

filltrain = traindata
#filltrain[1,9]

filltrain[is.na(filltrain$product_gender), 9] = "Unisex"
table(filltrain$businessunit, filltrain$product_gender)

#now all missing product gender info is turned to unisex


#check NA #how many NA's now
mynalist2 = cbind(
  lapply(
    lapply(filltrain, is.na)
    , sum)
)

mynalist2

#pattern NA
library("VIM")
mice_plot <- aggr(filltrain, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(filltrain), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

#
#check what to do with other NAs
#dont do anything
filltrain[is.na(filltrain$contentid ),]
head(filltrain[is.na(filltrain$category_id  ),])


nrow(filltrain[is.na(filltrain$category_id ),]) ##the number of instances missing almost all info

alllength = nrow(filltrain)
alllength #all the instances
#2184 of these of 2million , we wont gather info from here, only the counts maybe


#we have NAs for sale price now ----

head(filltrain[is.na(filltrain$sellingprice ),])

noprice =  filltrain[is.na(filltrain$sellingprice ),]

#impute price of the product based on some info
#put the mean value of the corresponding category3 level
# for instance if a salesprice is missing and its Level3_Category_Name = "cuzdan" then get the mean for all "cuzdan" and put it there for the missing part.

#work on the salesprice and category3 only
#remove price NAs first
removeprc =  na.omit(filltrain)
priceimpute = removeprc [, c("sellingprice", "Level3_Category_Name")]
head(priceimpute)
priceimpute = as.data.table(priceimpute)

summarized.price = priceimpute[, list(meanpriceforcat3 = mean(sellingprice)), by ="Level3_Category_Name"]
summarized.price [ order (Level3_Category_Name), ]

write.csv(summarized.price,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/summarized.price.csv", row.names = FALSE)


#use this table for imputing the salesprice
#for 3d yazici, we should get 495 as mean price
#this is imputed for all the missing salesprice within cat3 = 3d yazici

filledprice = merge(filltrain, summarized.price, by="Level3_Category_Name", all = TRUE)
head(filledprice)

#reconstruct the dataframe with full price data
#if price is NA then put mean price of category3, otherwise keep the actual price

filledprice$sellingprice  = ifelse(is.na(filledprice$sellingprice), filledprice$meanpriceforcat3, filledprice$sellingprice )


head(filledprice)

#example:
#anything with missing price and cat3 = "cuzdan" should be 127 tl
summarized.price [ Level3_Category_Name =="CÃ¼zdan", ]
#before we had NA for price
traindata[ traindata$time_stamp =="2020-10-31T22:45:36Z", ][1,]
#after, we have the mean price within that category3
filledprice[filledprice$time_stamp =="2020-10-31T22:45:36Z",][1,]
#ok.done


#reconstruct the dataframe
#drop the last column since, its info is copied for the ones with missing price values,
head(filledprice)
ftrain1 =  filledprice[,-20]
head(ftrain1)
#reorder, put cat3 back in place
ftrain2 = ftrain1 [, c(2:14,1,15:19)]
head(ftrain2)
finaltrain  = ftrain2 #done
head(finaltrain)

write.csv(finaltrain,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/finaltrain.csv", row.names = FALSE)

dim(finaltrain)

finaltrain <- read.csv("finaltrain.csv")





tail(finaltrain)

length(unique(finaltrain$unique_id))
head(unique(finaltrain$unique_id))
tail(unique(finaltrain$unique_id))
order(unique(finaltrain$unique_id))
#check NA now
mynalist4 = cbind(
  lapply(
    lapply(finaltrain, is.na)
    , sum)
)

mynalist4
head(finaltrain[is.na(finaltrain$sellingprice ),])

#done with NAs.


#now comes the feature engineering part ----

mytraintable = as.data.table(finaltrain)
mytraintable

length(unique(mytraintable$unique_id))

#date and time modification
#seperste to date and time
dt_traintable = mytraintable %>% separate(time_stamp, c("date", 'time'), sep="T")
head(dt_traintable)
tail(dt_traintable)


#remove Z, and miliseconds
dt_traintable$time = str_sub(dt_traintable$time,1,nchar(dt_traintable$time)-1)
dt_traintable$time = str_sub(dt_traintable$time,1,8)

head(dt_traintable)
dt_traintable$date =  as.Date(dt_traintable$date)
#create new columns
day <- c()
workhour <- c()
weekday <- c()
#
dt_traintable$day = format(dt_traintable$date, format="%a")
head(dt_traintable)

######## 1: weekday, 0 :weekend
dt_traintable$weekday = ifelse (dt_traintable$day == "Paz" | dt_traintable$day == "Cmt" , 0 ,1  )

#time
dt_traintable$time =  strptime(dt_traintable$time, format = "%H:%M:%OS")

#workhours? 
######## 1: workhour, 0 :off
dt_traintable$workhour = ifelse (dt_traintable$weekday == "1" & (dt_traintable$time > "08:00:00"  & dt_traintable$time < "18:00:00" ), 1 ,0  ) ######## 1: workhour, 0 :off

head(dt_traintable)
tail(dt_traintable)



#use this final data-set to structure the data in a nicer/structured format

ids= as.data.table(unique(dt_traintable$unique_id))

colnames(ids) <-  "unique_id" 
ids[order(unique_id)] #5618

#followings are going to be our new features

#one user how many times clicks female prd_gender
summarized.prdgenderfm = dt_traintable[  product_gender == "KadÄ±n", list( countfm = length (product_gender ) ), by =c("unique_id")]
summarized.prdgenderfm [order(unique_id),]

#merge
mergedtrain= merge(summarized.prdgenderfm , ids, by="unique_id", all = TRUE)
mergedtrain 


#one user how many times clicks  male prd_gender
summarized.prdgenderml = dt_traintable[ product_gender == "Erkek", list( countml = length (product_gender ) ), by =c("unique_id")]
summarized.prdgenderml[order(unique_id),]

#merge
mergedtrain= merge(summarized.prdgenderml , mergedtrain, by="unique_id", all = TRUE)
mergedtrain


#one user how many times clicks  unisex prd_gender
summarized.prdgenderuni = dt_traintable[ product_gender == "Unisex", list( countuni = length (product_gender ) ), by =c("unique_id")]
summarized.prdgenderuni[order(unique_id),]

#merge
mergedtrain= merge(summarized.prdgenderuni , mergedtrain, by="unique_id", all = TRUE)
mergedtrain



#one user how many times orders female prd_gender / may be correlated with click number
summarized.prdgenderfmorder = dt_traintable[ product_gender == "KadÄ±n" &  user_action == "order", list( countfmorder = length (product_gender ) ), by =c("unique_id")]
summarized.prdgenderfmorder[order(unique_id),]

#merge
mergedtrain= merge(summarized.prdgenderfmorder , mergedtrain, by="unique_id", all = TRUE)
mergedtrain



#one user how many times orders male prd_gender / may be coorelated with click number
summarized.prdgendermlorder = dt_traintable[ product_gender == "Erkek" &  user_action == "order", list( countmlorder = length (product_gender ) ), by =c("unique_id")]
summarized.prdgendermlorder[order(unique_id),]

#merge
mergedtrain= merge(summarized.prdgendermlorder , mergedtrain, by="unique_id", all = TRUE)
mergedtrain


#one user how many times clicks within this given timeframe [acitivity count)] at total
summarized.click = dt_traintable[  , list( countclick = length (user_action ) ), by =c("unique_id")]
summarized.click [order(unique_id),]

#merge
mergedtrain= merge(summarized.click , mergedtrain, by="unique_id", all = TRUE)
mergedtrain

#one user how many times order within this timeframe [buy count)] at total
summarized.buy = dt_traintable[  user_action == "order" , list( countbuy = length (user_action ) ), by =c("unique_id")]
summarized.buy [order(unique_id),]

#merge
mergedtrain= merge(summarized.buy , mergedtrain, by="unique_id", all = TRUE)
mergedtrain


#one user how many times favorites within this timeframe [fav count)] at total
summarized.fav = dt_traintable[  user_action == "favorite" , list( countfav = length (user_action ) ), by =c("unique_id")]
summarized.fav [order(unique_id),]

#merge
mergedtrain= merge(summarized.fav  , mergedtrain, by="unique_id", all = TRUE)
mergedtrain


#one user how many times clicks within one category (search habbits indicator)
summarized.search = dt_traintable[   , list( search = length (user_action ) ), by =c("unique_id", "Level3_Category_Id" )]
summarized.search [unique_id == "1",]
#use the above one to get
#one user how many times clicks within one category (search habbits indicator) on average 
# ~~like search length for one purchase
summarized.shabbit = summarized.search[   , list( searchhabbit = mean(search ) ), by =c("unique_id" )]
summarized.shabbit [unique_id == "1",] #example

#merge
mergedtrain= merge(summarized.shabbit  , mergedtrain, by="unique_id", all = TRUE)
mergedtrain



#one user how many times has actvity on the weekends 
######## 1: weekday, 0 :weekend
summarized.weekend = dt_traintable[  weekday == 0, list( weekendcount = length (weekday ) ), by =c("unique_id")]
summarized.weekend  [order(unique_id),]

#merge
mergedtrain= merge(summarized.weekend  , mergedtrain, by="unique_id", all = TRUE)
mergedtrain




#one user how many times has actvity during workhours
######## 1: workhour, 0 :off
summarized.work = dt_traintable[ workhour == 1 , list( countwork = length (workhour ) ), by =c("unique_id")]
summarized.work[order(unique_id),]

#merge
mergedtrain= merge(summarized.work  , mergedtrain, by="unique_id", all = TRUE)
mergedtrain

####

#########max Level3_Category_Id
#most active category3 for a user
summarized.search3 = dt_traintable[   , list( search3 = length (user_action ) ), by =c("unique_id", "Level3_Category_Id" )]
summarized.search3 [unique_id == "1",][order(search3),]
#use the above one to get
#most active category3 for a user
summarized.smode3 = summarized.search3[   ,list (search3 = max(search3 )) , by =c("unique_id" )]

summarized.maxcat3= merge(summarized.search3 ,summarized.smode3  ,by= c("unique_id" , "search3") )
#if there are equallly liked categories just pick the one with higher cat_id
summarized.maxcat3= summarized.maxcat3[,list (mostloved3 = max(Level3_Category_Id )) ,by= c("unique_id" , "search3") ]
nrow(summarized.maxcat3)
summarized.maxcat3[unique_id == "1",]
summarized.likedcat3 = summarized.maxcat3[,-2]
summarized.likedcat3[unique_id == "1",]


#merge
mergedtrain= merge(summarized.likedcat3  , mergedtrain, by="unique_id", all = TRUE)
mergedtrain




####Level2_Category_Id
#most active category2 for a user
summarized.search2 = dt_traintable[   , list( search2 = length (user_action ) ), by =c("unique_id", "Level2_Category_Id" )]
summarized.search2 [unique_id == "1",][order(search2),]
#use the above one to get
#most active category2 for a user
summarized.smode2 = summarized.search2[   ,list (search2 = max(search2 )) , by =c("unique_id" )]
summarized.maxcat2= merge(summarized.smode2 ,summarized.search2  ,by= c("unique_id" , "search2") )
#if there are equallly liked categories just pick the one with higher cat_id
summarized.maxcat2= summarized.maxcat2[,list (mostloved2 = max(Level2_Category_Id )) ,by= c("unique_id" , "search2") ]

summarized.maxcat2[unique_id == "1",]
summarized.likedcat2 = summarized.maxcat2[,-2]
summarized.likedcat2[unique_id == "1",]

#merge
mergedtrain= merge(summarized.likedcat2  , mergedtrain, by="unique_id", all = TRUE)
mergedtrain





####Level1_Category_Id
#most active category1 for a user
summarized.search1 = dt_traintable[   , list( searchone = length (user_action ) ), by =c("unique_id", "Level1_Category_Id" )]
summarized.search1 [unique_id == "1",][order(searchone),]
#use the above one to get
#most active category1 for a user
summarized.smode1 = summarized.search1[   ,list (searchone = max(searchone )) , by =c("unique_id" )]
summarized.maxcat1= merge(summarized.smode1 ,summarized.search1  ,by= c("unique_id" , "searchone") )
#if there are equallly liked categories just pick the one with higher cat_id
summarized.maxcat1= summarized.maxcat1[,list (mostloved1 = max(Level1_Category_Id )) ,by= c("unique_id" , "searchone") ]

summarized.maxcat1[unique_id == "1",]
summarized.likedcat1 = summarized.maxcat1[,-2]
summarized.likedcat1[unique_id == "1",]

#merge
mergedtrain= merge(summarized.likedcat1  , mergedtrain, by="unique_id", all = TRUE)
mergedtrain
nrow(mergedtrain)



####business id
#most active business unt = category_id for a user
summarized.categ = dt_traintable[   , list( cat = length (user_action ) ), by =c("unique_id", "category_id" )]
summarized.categ [unique_id == "1",][order(cat),]
#use the above one to get
#most active business category for a user
summarized.maxcateg = summarized.categ[   ,list (cat = max(cat )) , by =c("unique_id" )]
summarized.maxcategory= merge(summarized.maxcateg ,summarized.categ  ,by= c("unique_id" , "cat") )
#if there are equallly liked categories just pick the one with higher cat_id
summarized.maxcategory= summarized.maxcategory[,list (mostlovedbusi = max(category_id )) ,by= c("unique_id" , "cat") ]
summarized.maxcategory[unique_id == "1",]
summarized.likedbus = summarized.maxcategory[,-2]
summarized.likedbus[unique_id == "1",]

#merge
mergedtrain= merge(summarized.likedbus  , mergedtrain, by="unique_id", all = TRUE)
mergedtrain
nrow(mergedtrain)




####mostly loved brand
#most active brand for a user
summarized.brand = dt_traintable[   , list( br = length (user_action ) ), by =c("unique_id", "brand_id" )]
summarized.brand [unique_id == "1",][order(br),]
#use the above one to get
#most active business category for a user
summarized.br = summarized.brand[   ,list (br = max(br )) , by =c("unique_id" )]
summarized.brandlove= merge(summarized.br ,summarized.brand ,by= c("unique_id" , "br") )
#if there are equallly liked brands just pick the one with higher brand_id
summarized.brandlove= summarized.brandlove[ ,list (mostlovedbrand = max(brand_id )) ,by= c("unique_id" , "br") ]

summarized.brandlove[unique_id == "1",]
summarized.likedbrand = summarized.brandlove[,-2]
summarized.likedbrand[unique_id == "1",]


#merge
mergedtrain= merge(summarized.likedbrand  , mergedtrain, by="unique_id", all = TRUE)
mergedtrain
nrow(mergedtrain)




#avg price of bought prod

summarized.buyprice = dt_traintable[user_action == "order", list(meanprice = mean(sellingprice)), by ="unique_id"]
summarized.buyprice[unique_id == "1",]

#merge
mergedtrain= merge(summarized.buyprice  , mergedtrain, by="unique_id", all = TRUE)
mergedtrain
nrow(mergedtrain)


#gender 
summarized.gender= dt_traintable[, length(gender), by =c("gender","unique_id")]

summarized.gender=summarized.gender[,-3]
summarized.gender

totnumberofperson = nrow(summarized.gender) #5618
length(unique(dt_traintable$unique_id)) #5618

#merge
mergedtrain= merge(summarized.gender , mergedtrain, by="unique_id", all = TRUE)
mergedtrain
nrow(mergedtrain)

#turn NAs to 0.
#for example if a countbuy= NA then it means that the person  has never bought something, therefore it got an NA during the data table operations,
#logical to put a zero there
#for example if a weekendcount = NA then it means that the person never was active on a weekend therefore logical to put a zero


 #####final training data set
finaltrain1 = mergedtrain
finaltrain1
finaltrain1[is.na(finaltrain1)] <- 0
finaltrain1 #ordered by id -min-to-max
dim(finaltrain1)
#remove ids
finaltrain = finaltrain1 [, -c(1)]
dim(finaltrain)

head(finaltrain)
#here for reusage
write.csv(finaltrain,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/finaltrainfeautures.csv", row.names = FALSE)


finaltrain =   read.csv("C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/finaltrainfeautures.csv", header = T, fileEncoding = 'UTF-8-BOM')

#correlation ----
CM = cor(finaltrain[,-1])
corrplot(CM, method= "number") 
#mostly correlated
#  check var of each col
colVars(finaltrain)

#finaltrain full ----

finaltrain$gender = ifelse(mergedtrain$gender == "F", 1, 0 ) 

finaltrainscl <- as.data.table(scale(finaltrain [,-c(1)]))
head(finaltrainscl )
finaltrainsclall= cbind(finaltrainscl, finaltrain$gender)

colnames(finaltrainsclall)[18] <-  "genderfm" 

finaltrainsclall
##need to do all for test data as well







#gender imbalance issue ----
class = summarized.gender[ , list (countgender= length(unique_id)), by= "gender"]
class
class[1,2]+class[2,2]   #5618 checks out
ml = class[1,2] 
fm =class[2,2]   #5618 checks out
tot = fm+ml
tot
#more female than male
#female twice as large as male
fm/tot
#65% is female
#needs some sort of oversampling/undersampling treatment


#show 

barplot(prop.table(table(finaltrainsclall$genderfm)),
        col = rainbow(2),
        ylim = c(0, 0.75),
        main = "Class Distribution")




#Spliting training set into two parts based on outcome: 75% and 25%----
#internal train and test
set.seed(1234)
index <- createDataPartition(finaltrain$gender , p=0.75, list=FALSE)
inttrainSet <- finaltrain[ index,]
inttestSet <- finaltrain[-index,]
head(inttrainSet)

#show 

barplot(prop.table(table(inttrainSet$gender)),
        col = rainbow(2),
        ylim = c(0, 0.75),
        main = "Subtrain Class Distribution")

#scale

#internal train ----

sclinttrainSet <- as.data.table(scale(inttrainSet[,-c(1)]))
head(sclinttrainSet)
sclinttrainSetall = cbind(sclinttrainSet , inttrainSet$gender)

colnames(sclinttrainSetall)[18] <-  "genderfm" 
sum(sclinttrainSetall$genderfm)
sclinttrainSetall


#need to do all for test partition data as well ----
sclintestSet <- as.data.table(scale(inttestSet[,-c(1)]))
head(inttestSet)
sclinttestSetall = cbind(sclintestSet , inttestSet$gender)

colnames(sclinttestSetall )[18] <-  "genderfm" 

sclinttestSetall 

###end split

#model build and predict on internal train &test

#internal train and test----


head(sclinttrainSetall)
dim(sclinttrainSetall)
head(sclinttestSetall)
dim(sclinttestSetall)

#smote for internal training data ----
####train data set
#more female than male
#female twice as large as male
#dup_size	:The maximum times of synthetic minority instances over original majority instances in the oversampling
#duplicate once in order to equalize
sclinttrainSetall$genderfm = ifelse(sclinttrainSetall$genderfm == "F", 1, 0 ) 
sclinttrainSetall
set.seed(1234)
newDataint <- SMOTE(X=sclinttrainSetall , target= sclinttrainSetall$genderfm, K=1, dup_size = 1 )
library("dplyr")
#verynew <- newData[["data"]] %>%select(-class)
verynewint <- newDataint[["data"]] 
dim(verynewint)
verynewint  = verynewint [,-19]
dim(newDataint[["data"]] )
dim(verynewint)
classnowint <- verynewint  %>% group_by(genderfm) %>% summarize(count=n()) 
classnowint #balanced

usefinalint = verynewint

usefinalint$genderfm = ifelse(usefinalint$genderfm == "1", "F", "M" ) 

#usefinal$gender = ifelse(usefinal$gender == "F", "1", "0" ) 
usefinalint

#already scaled

trainsclint = usefinalint


trainsclint #final smote internal train
#x part

trainsclint = trainsclint[,-18]

#another smote
set.seed(1234)

newDataint2 <- SMOTE(X=sclinttrainSetall , target= sclinttrainSetall$gender, K=2, dup_size = 1 )

verynewint2 <- newDataint2[["data"]]
verynewint2 <- verynewint2 [,-19]
dim(newDataint2[["data"]] )
dim(verynewint2)
classnowint2 <- verynewint2  %>% group_by(genderfm) %>% summarize(count=n()) 
classnowint2 #balanced

usefinalint2 = verynewint2

usefinalint2$genderfm = ifelse(usefinalint2$genderfm == "1", "F", "M" ) 
#usefinal$gender = ifelse(usefinal$gender == "F", "1", "0" ) 
usefinalint2
#scale wo id and gender, all are cont. var.
trainsclint2  =  usefinalint2  #another final smote internal train
trainsclint2
#x part
trainsclint2 = trainsclint2[,-18]
#already scaled
#



#not sampled data
head(sclinttrainSetall)
dim(sclinttrainSetall)
head(sclinttestSetall)
dim(sclinttestSetall)



#test
#only x
testint = sclinttestSetall[,-18]


#target
#sclinttestSetall$genderfm = ifelse(sclinttestSetall$genderfm == "1", "F", "M" ) 
sclinttestSetall

#x part

#models----
#model1
#logreg
#smote1
set.seed(1234)
intlog_reg_grid <- expand.grid(alpha = 1, lambda= seq( from = 0.001, to =0.003, by = 0.00003))

TC2 <-trainControl(method="cv", number=10, classProbs = TRUE,summaryFunction=twoClassSummary)
log_reg2 <- train(x=as.matrix(trainsclint ,with = F ),y=as.matrix(usefinalint$genderfm, with = F),metric= c("ROC") ,method="glmnet",family='binomial',tuneGrid= log_reg_grid, trControl= TC2 )
log_reg2 


#BAC
confusionMatrix(predict(log_reg2, testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(log_reg2, testint, type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted[, "F"])
roc

###
#model2
#logreg 
#smote1
set.seed(1234)
log_reg_grid3 <- expand.grid(alpha = c(0,0.5,1), lambda= seq( from = 0.001, to =0.003, by = 0.00003))

TC2 <-trainControl(method="cv", number=10, classProbs = TRUE,summaryFunction=twoClassSummary)
log_reg3 <- train(x=as.matrix(trainsclint ,with = F ),y=as.matrix(usefinalint$genderfm, with = F),metric= c("ROC") ,method="glmnet",family='binomial',tuneGrid= log_reg_grid3, trControl= TC2 )
log_reg3 


#BAC
confusionMatrix(predict(log_reg3, testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(log_reg3, testint, type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted[, "F"])
roc



#model3
#rf
library("randomForest")

set.seed(1234)

grid_RF <- expand.grid(mtry = c(4:17) ,min.node.size=5, splitrule = "gini")
Traincontrol_RF <-trainControl(method="cv", number=10,  allowParallel=TRUE, classProbs = TRUE, summaryFunction = twoClassSummary)
RF_mod <- train(x=as.matrix(trainsclint ,with = F ),y=as.matrix(usefinalint$genderfm, with = F) ,method="ranger",metric="ROC", tuneGrid= grid_RF, trControl= Traincontrol_RF)
RF_mod


#varImpPlot(RF_mod$bestTune)

RF_try=randomForest(x=as.matrix(trainsclint ,with = F ),y=as.factor(as.matrix(usefinalint$genderfm, with = F)),
                    min.node.size=5, splitrule = "gini",
                    mtry = 4, 
                    importance=TRUE, localImp=TRUE) 


varImpPlot(RF_try)



#BAC
confusionMatrix(predict(RF_mod, testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(RF_mod, testint, type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted[, "F"])
roc




#model4
set.seed(1234)

grid_RF_node <- expand.grid(mtry = c(4:17) ,min.node.size=1, splitrule = "gini")
Traincontrol_RF <-trainControl(method="cv", number=10,  allowParallel=TRUE, classProbs = TRUE, summaryFunction = twoClassSummary)
RF_node <- train(x=as.matrix(trainsclint ,with = F ),y=as.matrix(usefinalint$genderfm, with = F) ,method="ranger",metric="ROC", tuneGrid= grid_RF_node, trControl= Traincontrol_RF)
RF_node

#BAC
confusionMatrix(predict(RF_node, testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(RF_node, testint, type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted[, "F"])
roc






set.seed(1234)
#newRF
grid_RF_node1 <- expand.grid(mtry = c(1:8) ,min.node.size=1, splitrule = "gini")
Traincontrol_RF <-trainControl(method="cv", number=10,  allowParallel=TRUE, classProbs = TRUE, summaryFunction = twoClassSummary)
RF_node1 <- train(x=as.matrix(trainsclint[,c("countml","countfm","countclick","mostlovedbrand","countfav" ,"countwork", "countuni", "weekendcount" )] ,with = F ),
                  y=as.matrix(usefinalint$genderfm, with = F),
                  method="ranger",metric="ROC", tuneGrid= grid_RF_node1, trControl= Traincontrol_RF)
RF_node1

#BAC
confusionMatrix(predict(RF_node1, testint[,c("countml","countfm","countclick","mostlovedbrand","countfav" ,"countwork", "countuni", "weekendcount" )]),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(RF_node1, testint[,c("countml","countfm","countclick","mostlovedbrand","countfav" ,"countwork", "countuni", "weekendcount" )], type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted[, "F"])
roc





#newgbm
set.seed(1234)
gbmGrid=expand.grid(interaction.depth = c( 3, 5), 
                    n.trees = c(1:5)*100, 
                    shrinkage = c(0.05,0.1),
                    n.minobsinnode = c(1, 3, 5))
TC_gbm <-trainControl(method="cv", number=10,  allowParallel=TRUE, classProbs = TRUE,summaryFunction = twoClassSummary)


gbm_node1 <- train(x=as.matrix(trainsclint[,c("countml","countfm","countclick","mostlovedbrand","countfav" ,"countwork", "countuni", "weekendcount" )] ,with = F ),
                   y=as.matrix(usefinalint$genderfm, with = F),
                   method="gbm",metric="ROC", tuneGrid= gbmGrid, trControl= TC_gbm)
gbm_node1



#BAC
confusionMatrix(predict(gbm_node1, testint[,c("countml","countfm","countclick","mostlovedbrand","countfav" ,"countwork", "countuni", "weekendcount" )]),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(gbm_node1, testint[,c("countml","countfm","countclick","mostlovedbrand","countfav" ,"countwork", "countuni", "weekendcount" )], type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted[, "F"])
roc








#fewrf

set.seed(1234)
#RF
grid_RF_node1 <- expand.grid(mtry = c(1:8) ,min.node.size=1, splitrule = "gini")
Traincontrol_RF <-trainControl(method="cv", number=10,  allowParallel=TRUE, classProbs = TRUE, summaryFunction = twoClassSummary)
RF_node11 <- train(x=as.matrix(trainsclint[,c("countml","countfm","countclick","mostlovedbrand","countfav" ,"countwork", "countuni", "weekendcount" )] ,with = F ),
                   y=as.matrix(usefinalint$genderfm, with = F),
                   method="ranger",metric="ROC", tuneGrid= grid_RF_node1, trControl= Traincontrol_RF)
RF_node11




#BAC
confusionMatrix(predict(RF_node11, testint[,c("countml","countfm","countclick","mostlovedbrand","countfav" ,"countwork", "countuni", "weekendcount" )]),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(RF_node11, testint[,c("countml","countfm","countclick","mostlovedbrand","countfav" ,"countwork", "countuni", "weekendcount" )], type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted[, "F"])
roc




set.seed(1234)

#RF remove corr
grid_RF_node1 <- expand.grid(mtry = c(1:8) ,min.node.size=1, splitrule = "gini")
Traincontrol_RF <-trainControl(method="cv", number=10,  allowParallel=TRUE, classProbs = TRUE, summaryFunction = twoClassSummary)
RF_nodefew <- train(x=as.matrix(trainsclint[,c("meanprice" , "mostlovedbrand", "mostlovedbusi","mostloved1", "mostloved2","countclick",
                                               "countmlorder","countfmorder", "countuni", "countml","countfm" )] ,with = F ),
                    y=as.matrix(usefinalint$genderfm, with = F),
                    method="ranger",metric="ROC", tuneGrid= grid_RF_node1, trControl= Traincontrol_RF)
RF_nodefew


#BAC
confusionMatrix(predict(RF_nodefew, testint[,c("meanprice" , "mostlovedbrand", "mostlovedbusi","mostloved1", "mostloved2","countclick",
                                               "countmlorder","countfmorder", "countuni", "countml","countfm" )]),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(RF_nodefew, testint[,c("meanprice" , "mostlovedbrand", "mostlovedbusi","mostloved1", "mostloved2","countclick",
                                           "countmlorder","countfmorder", "countuni", "countml","countfm" )], type = "prob")


roc = auc(response=sclinttestSetall$genderfm, predictor= predicted[, "F"])
roc




print(RF_mod)
plot(RF_mod)

#90 roc

#gbm
set.seed(1234)
TC_gbm <-trainControl(method="cv", number=10,  allowParallel=TRUE, classProbs = TRUE,summaryFunction = twoClassSummary)

gbmGrid=expand.grid(interaction.depth = c( 3, 5), 
                    n.trees = c(1:5)*100, 
                    shrinkage = c(0.05,0.1),
                    n.minobsinnode = 10)

gboost <- train(x=as.matrix(trainsclint ,with = F ),
                y=as.matrix(usefinalint$genderfm, with = F),
                method = "gbm",
                verbose = FALSE,
                metric = "ROC",
                tuneGrid=gbmGrid,
                trControl = TC_gbm)
gboost 
gboost$bestTune

#90 roc

#BAC
confusionMatrix(predict(gboost , testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(gboost , testint, type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted[, "F"])
roc





#svm
set.seed(1234)
TC_svm <-trainControl(method="cv", number=10,  allowParallel=TRUE, classProbs = TRUE,summaryFunction = twoClassSummary)


svm_radial_mod <- train(x=as.matrix(trainsclint ,with = F ), y=as.matrix(usefinalint$genderfm, with = F), method = "svmRadial", trControl = TC_svm , tuneLength = 5,  metric = "ROC")
# Print the best tuning parameter sigma and C that maximizes model accuracy
svm_radial_mod 
svm_radial_mod$bestTune


#BAC
confusionMatrix(predict(svm_radial_mod, testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(svm_radial_mod , testint, type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted[, "F"])
roc





#rad svm with only most imp 2 variables

svm_radial_mod_imp <- train(x=as.matrix(trainsclint[,c("countml","countfm")] ,with = F ), y=as.matrix(usefinalint$genderfm, with = F), method = "svmRadial", trControl = TC_svm , tuneLength = 5,  metric = "ROC" )
# Print the best tuning parameter sigma and C that maximizes model accuracy
svm_radial_mod_imp 

#BAC
confusionMatrix(predict(svm_radial_mod_imp , testint[,c("countml","countfm")]),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(svm_radial_mod_imp , testint[,c("countml","countfm")], type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted[, "F"])
roc








#less feature

svm_radial_mod_impmore <- train(x=as.matrix(trainsclint[,c("countml","countfm","countclick","mostlovedbrand","countfav" ,"countwork", "countuni", "weekendcount" )] ,with = F ),
                                y=as.matrix(usefinalint$genderfm, with = F),
                                method = "svmRadial", 
                                trControl = TC_svm , tuneLength = 5,
                                metric = "ROC")
# Print the best tuning parameter sigma and C that maximizes model accuracy
svm_radial_mod_impmore



#BAC
confusionMatrix(predict(svm_radial_mod_impmore , testint[,c("countml","countfm","countclick","mostlovedbrand","countfav" ,"countwork", "countuni", "weekendcount" )]),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(svm_radial_mod_impmore , testint[,c("countml","countfm","countclick","mostlovedbrand","countfav" ,"countwork", "countuni", "weekendcount" )], type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted[, "F"])
roc











#svm few var based on varýmp

svm_radial_few <- train(x=as.matrix(trainsclint[,c("meanprice" , "mostlovedbrand", "mostlovedbusi","mostloved1", "mostloved2","countclick",
                                                   "countmlorder","countfmorder", "countuni", "countml","countfm" )] ,with = F ),
                        y=as.matrix(usefinalint$genderfm, with = F),
                        method="svmRadial",metric="ROC", tuneLength = 6, trControl= TC_svm)
svm_radial_few 


#BAC
confusionMatrix(predict(svm_radial_few , testint[,c("meanprice" , "mostlovedbrand", "mostlovedbusi","mostloved1", "mostloved2","countclick",
                                                    "countmlorder","countfmorder", "countuni", "countml","countfm" )] ),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(svm_radial_few  , testint[,c("meanprice" , "mostlovedbrand", "mostlovedbusi","mostloved1", "mostloved2","countclick",
                                                 "countmlorder","countfmorder", "countuni", "countml","countfm" )] , type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted[, "F"])
roc









#rf with K=2 smote
set.seed(1234)

grid_RF_n1 <- expand.grid(mtry = c(4:17) ,min.node.size=1, splitrule = "gini")
Traincontrol_RF_n1 <-trainControl(method="repeatedcv", repeats = 5, number=10,  allowParallel=TRUE, classProbs = TRUE, summaryFunction = twoClassSummary)
RF_mod_n1 <- train(x=as.matrix(trainsclint2 ,with = F ),y=as.matrix(usefinalint2$genderfm, with = F) ,method="ranger",metric="ROC", tuneGrid= grid_RF_n1, trControl= Traincontrol_RF_n1)
RF_mod_n1




#BAC
confusionMatrix(predict(RF_mod_n1 , testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(RF_mod_n1 , testint, type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted[, "F"])
roc










#with class wieghts



weighted_svm_rad <- train(genderfm ~ .,
                          data = sclinttrainSetall ,
                          method="svmRadial",
                          metric="ROC", 
                          tuneLength = 5,
                          class.weights= "inverse",
                          trControl= TC_svm)




weighted_svm_rad


#BAC
confusionMatrix(predict(weighted_svm_rad , testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(weighted_svm_rad , testint, type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted[, "F"])
roc









#%83
#svm poly
set.seed(1234)
svm_pol <- train(x=as.matrix(trainsclint ,with = F ), y=as.matrix(usefinalint$genderfm, with = F), method = "svmPoly", trControl = TC_svm , tuneLength = 3,  metric = "ROC",)
# Print the best tuning parameter sigma and C that maximizes model accuracy
svm_pol
svm_pol$bestTune


#BAC
confusionMatrix(predict(svm_pol , testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(svm_pol , testint, type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted[, "F"])
roc







#svm linear
set.seed(1234)
svm_linear_mod <- train(x=as.matrix(trainsclint ,with = F ), y=as.matrix(usefinalint$genderfm, with = F), method = "svmLinear", trControl = TC_svm , tuneLength = 10,metric = "ROC",)
svm_linear_mod$bestTune
#




#BAC
confusionMatrix(predict(svm_linear_mod, testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(svm_linear_mod , testint, type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted[, "F"])
roc








#
# Build weighted model

table(sclinttrainSetall$genderfm)
prop.table(table(sclinttrainSetall$genderfm))


model_weights <- ifelse(sclinttrainSetall$genderfm == "1",
                        (1/table(sclinttrainSetall$genderfm)[1]) * 0.3459896,
                        (1/table(sclinttrainSetall$genderfm)[2]) * 0.6540104)

sum(model_weights)


#weighted rf
set.seed(1234)
sclinttrainSetall$genderfm= ifelse(sclinttrainSetall$genderfm== "1", "F", "M" ) 

weighted_fit_rf <- train(genderfm ~ .,
                         data = sclinttrainSetall ,
                         method="ranger",
                         metric="ROC", 
                         weights = model_weights,
                         tuneGrid= grid_RF,
                         trControl= Traincontrol_RF)
weighted_fit_rf 


#BAC
confusionMatrix(predict(weighted_fit_rf, testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(weighted_fit_rf, testint, type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted[, "F"])
roc







#weighted boost
set.seed(1234)
weighted_fit_gboost <- train(genderfm ~ .,
                             data = sclinttrainSetall,
                             method="gbm",
                             metric="ROC", 
                             weights = model_weights,
                             tuneGrid=gbmGrid,
                             trControl = TC_gbm)

weighted_fit_gboost 





#BAC
confusionMatrix(predict(weighted_fit_gboost, testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(weighted_fit_gboost, testint, type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted[, "F"])
roc






# Build down-sampled model
set.seed(1234)

TC_gbm$sampling <- "down"
Traincontrol_RF$sampling <- "down"

down_fit_RF <- train(genderfm ~ .,
                     data = sclinttrainSetall ,
                     method="ranger",
                     metric="ROC", 
                     tuneGrid= grid_RF,
                     trControl= Traincontrol_RF)

down_fit_RF 


#BAC
confusionMatrix(predict(down_fit_RF, testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(down_fit_RF, testint, type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted[, "F"])
roc








#
set.seed(1234)
Traincontrol_RF2 <-trainControl(method="repeatedcv", number=10, repeats = 5, allowParallel=TRUE, classProbs = TRUE, summaryFunction = twoClassSummary)
Traincontrol_RF2$sampling <- "down"

grid_RF2 <- expand.grid(mtry = c(4:15) ,min.node.size=1, splitrule = "gini")


#
down_fit_RF2 <- train(genderfm ~ .,
                      data = sclinttrainSetall ,
                      method="ranger",
                      metric="ROC", 
                      tuneGrid= grid_RF2,
                      trControl= Traincontrol_RF2)

down_fit_RF2




#BAC
confusionMatrix(predict(down_fit_RF2, testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(down_fit_RF2, testint, type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted[, "F"])
roc







#
#
set.seed(1234)
grid_RFpre2 <- expand.grid(mtry = c(4:8) ,min.node.size=1, splitrule = "gini")

down_fit_preRF2 <- train(genderfm ~ .,
                         data = sclinttrainSetall ,
                         method="ranger",
                         metric="ROC", 
                         tuneGrid= grid_RFpre2,
                         preProc=c("pca"),
                         trControl= Traincontrol_RF2)

down_fit_preRF2


#BAC
confusionMatrix(predict(down_fit_preRF2, testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(down_fit_preRF2, testint, type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted[, "F"])
roc




#
set.seed(1234)

down_fit_gboost <- train(genderfm ~ .,
                         data = sclinttrainSetall  ,
                         method="gbm",
                         metric="ROC", 
                         tuneGrid=gbmGrid,
                         trControl = TC_gbm)


down_fit_gboost 



#BAC
confusionMatrix(predict(down_fit_gboost , testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(down_fit_gboost , testint, type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted[, "F"])
roc





# Build up-sampled model
set.seed(1234)
Traincontrol_RF$sampling <- "up"

up_fit_RF <- train(genderfm ~ .,
                   data = sclinttrainSetall ,
                   method="ranger",
                   metric="ROC", 
                   tuneGrid= grid_RF,
                   trControl= Traincontrol_RF)

up_fit_RF 



#BAC
confusionMatrix(predict(up_fit_RF , testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(up_fit_RF , testint, type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted[, "F"])
roc





#rose rf
set.seed(1234)
Traincontrol_RF$sampling <- NULL

table(sclinttrainSetall$genderfm)

rose_dataint <- ROSE(genderfm~., data = sclinttrainSetall, N = 5512, seed=123)$data

table(rose_dataint$genderfm)  #balanced

rose_fit_RF <- train(genderfm ~ .,
                     data = rose_dataint ,
                     method="ranger",
                     metric="ROC", 
                     tuneGrid= grid_RF,
                     trControl= Traincontrol_RF)
rose_fit_RF




#BAC
confusionMatrix(predict(rose_fit_RF, testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(rose_fit_RF, testint, type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted[, "F"])
roc






#the model with boosting function and train it with train data. 
#The 'boosting' function applies the AdaBoost.M1 and SAMME algorithms using classification trees. 
#A 'boos' is a bootstrap uses the weights for each observation in an iteration 
#if it is TRUE. Otherwise, each observation is used with its weight.
#A 'mfinal' is the number of iterations or trees to use.


set.seed(1234)

TC_ada <-trainControl(method="cv", number=10,  allowParallel=TRUE, classProbs = TRUE,summaryFunction = twoClassSummary)

fitGrid <- expand.grid(mfinal = (1:3)*3,         
                       maxdepth = c(1, 3),       
                       coeflearn = c("Breiman")) 

adaboost <- train(x=as.matrix(trainsclint ,with = F ),
                  y=as.matrix(usefinalint$genderfm, with = F),
                  method = "AdaBoost.M1",
                  verbose = FALSE,
                  metric = "ROC",
                  tuneGrid = fitGrid,
                  trControl = TC_ada)

adaboost


#BAC
confusionMatrix(predict(adaboost, testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(adaboost, testint, type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted[, "F"])
roc



#
set.seed(1234)
fitGrid2<- expand.grid(mfinal = (4:7)*3,         
                       maxdepth = c( 5,7,10),       
                       coeflearn = c("Breiman")) 

adaboost2 <- train(x=as.matrix(trainsclint ,with = F ),
                   y=as.matrix(usefinalint$genderfm, with = F),
                   method = "AdaBoost.M1",
                   verbose = FALSE,
                   metric = "ROC",
                   tuneGrid = fitGrid2,
                   trControl = TC_ada)

adaboost2


#BAC
confusionMatrix(predict(adaboost2, testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(adaboost2, testint, type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted[, "F"])
roc





#knn
#no sampling
set.seed(1234)
TC_knn <-trainControl(method="cv", number=10,  allowParallel=TRUE, classProbs = TRUE,summaryFunction = twoClassSummary)

knn_model <- train(genderfm ~ .,
                   data = sclinttrainSetall,
                   weights = model_weights,
                   method     = "kknn",
                   tuneGrid   = expand.grid(kmax = 1:5, distance = 1 , kernel= c("rectangular", "optimal")),
                   trControl  = TC_knn,
                   metric     = "ROC")



#BAC
confusionMatrix(predict(knn_model, testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(knn_model, testint, type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted[, "F"])
roc








#ensemble with down sampling


set.seed(1234)
#set.seed(18121984)

control_stacking <- trainControl(method="repeatedcv", number=10, repeats=5, savePredictions=TRUE, classProbs=TRUE,summaryFunction = twoClassSummary)


control_stacking$sampling <- "down"

algorithms_to_use <- c('rpart', 'glm', 'ranger', 'svmRadial')



stacked_models <- caretList(
  genderfm ~ .,
  data = sclinttrainSetall ,
  trControl=control_stacking, 
  methodList=algorithms_to_use)

stacking_results <- resamples(stacked_models)



summary(stacking_results)

glm_stack <- caretStack(stacked_models, method="glm", metric="ROC", trControl=control_stacking )



print(glm_stack)

#BAC
confusionMatrix(predict(glm_stack, testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(glm_stack, testint,type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted)
roc




#

rf_stack <- caretStack(stacked_models, method="ranger", metric="ROC", trControl=control_stacking )

print(rf_stack)


#BAC
confusionMatrix(predict(rf_stack, testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(rf_stack, testint,type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted)
roc



#

svm_stack <- caretStack(stacked_models, method="svmRadial", metric="ROC", trControl=control_stacking )

print(svm_stack)

#BAC
confusionMatrix(predict(svm_stack, testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(svm_stack, testint,type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted)
roc










#ensemble with rose
set.seed(1234)

control_stacking3 <- trainControl(method="repeatedcv", number=10, repeats=5, savePredictions=TRUE, classProbs=TRUE,summaryFunction = twoClassSummary)


control_stacking3$sampling <- NULL


rose_dataint<- ROSE(genderfm~., data = sclinttrainSetall, N = 5516, seed=1234)$data


algorithms_to_use3 <- c('gbm', 'glm', 'ranger', 'svmRadial')

stacked_models3 <- caretList(
  genderfm ~ .,
  data = rose_dataint ,
  trControl=control_stacking3, 
  methodList=algorithms_to_use3)

stacking_results3 <- resamples(stacked_models3)



summary(stacking_results3)

glm_stack3 <- caretStack(stacked_models3, method="glm", metric="ROC", trControl=control_stacking )

print(glm_stack3)




#BAC
confusionMatrix(predict(glm_stack3, testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(glm_stack3, testint,type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted)
roc








#ensemble glm with down sampling

set.seed(1234)

control_stacking2 <- trainControl(method="repeatedcv", number=10, repeats=5, savePredictions=TRUE, classProbs=TRUE,summaryFunction = twoClassSummary)


control_stacking2$sampling <- "down"

algorithms_to_use2 <- c('glmnet', 'ranger', 'gbm', 'svmRadial')


stacked_models2 <- caretList(
  genderfm ~ .,
  data = sclinttrainSetall,
  trControl=control_stacking2, 
  methodList=algorithms_to_use2)

stacking_results2 <- resamples(stacked_models2)


summary(stacking_results2)

glm_stack2 <- caretStack(stacked_models2, method="glmnet", metric="ROC", trControl=control_stacking )

print(glm_stack2)



#BAC
confusionMatrix(predict(glm_stack2, testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(glm_stack2, testint,type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted)
roc







svm_stack2 <- caretStack(stacked_models2, method="svmRadial", metric="ROC", trControl=control_stacking )

print(svm_stack2)



#BAC
confusionMatrix(predict(glm_stack2, testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(glm_stack2, testint,type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted)
roc






gbm_stack2 <- caretStack(stacked_models2, method="gbm", metric="ROC", trControl=control_stacking )

print(gbm_stack2)


#BAC
confusionMatrix(predict(gbm_stack2, testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(gbm_stack2, testint,type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted)
roc







rf_stack2 <- caretStack(stacked_models2, method="ranger", metric="ROC", trControl=control_stacking )

print(rf_stack2)



#BAC
confusionMatrix(predict(rf_stack2, testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(rf_stack2, testint,type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted)
roc





#down ensemble another
set.seed(1234)
control_stacking2 <- trainControl(method="repeatedcv", number=10, repeats=5, savePredictions=TRUE, classProbs=TRUE,summaryFunction = twoClassSummary)
algorithms_to_use2 <- c('glmnet', 'ranger', 'gbm', 'svmRadial')

control_stacking2$sampling <- "down"

stacked_models9 <- caretList(
  genderfm ~ .,
  data = sclinttrainSetall ,
  trControl=control_stacking2, 
  methodList=algorithms_to_use2,
  
  tuneList=list(
    caretModelSpec("gbm", tuneGrid = expand.grid(  interaction.depth = c(3, 5), 
                                                   n.trees = c(3:5)*100, 
                                                   shrinkage = c(0.05,0.1),
                                                   n.minobsinnode = 1)),
    caretModelSpec("ranger", tuneGrid = expand.grid(mtry = c(1:6) ,min.node.size=1, splitrule = "gini")),
    caretModelSpec("glmnet", tuneGrid = expand.grid(alpha = 1, lambda= seq( from = 0.001, to =0.003, by = 0.0003)))
  )
)

glm_stack9 <- caretStack(stacked_models9, method="glm", metric="ROC", trControl=control_stacking2 )



#BAC
confusionMatrix(predict(glm_stack9, testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(glm_stack9, testint,type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted)
roc







#down ensemble another
set.seed(1234)
control_stacking10 <- trainControl(method="cv", number=10,  savePredictions=TRUE, classProbs=TRUE,summaryFunction = twoClassSummary)
algorithms_to_use10 <- c('glm', 'ranger', 'rpart' ,'svmRadial')

control_stacking10$sampling <- "down"

stacked_models10 <- caretList(
  genderfm ~ .,
  data = sclinttrainSetall ,
  trControl=control_stacking10, 
  methodList=algorithms_to_use10,
  tuneList=list(
    caretModelSpec("ranger", tuneGrid = expand.grid(mtry = c(1:6) ,min.node.size=1, splitrule = "gini"))
  )
)

glm_stack10 <- caretStack(stacked_models10, method="glm", metric="ROC", trControl=control_stacking10 )





#BAC
confusionMatrix(predict(glm_stack10, testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(glm_stack10, testint,type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted)
roc






#down ensemble another many
set.seed(1234)
control_stacking11 <- trainControl(method="cv", number=10,  savePredictions=TRUE, classProbs=TRUE,summaryFunction = twoClassSummary)
algorithms_to_use11 <- c('glmnet', 'ranger', 'rpart' ,'svmRadial', "gbm")

control_stacking11$sampling <- "down"

stacked_models11 <- caretList(
  genderfm ~ .,
  data = sclinttrainSetall ,
  trControl=control_stacking11, 
  methodList=algorithms_to_use11,
  tuneList=list(
    caretModelSpec("gbm", tuneGrid = expand.grid(  interaction.depth = c(3, 5), 
                                                   n.trees = 500, 
                                                   shrinkage = c(0.05,0.1),
                                                   n.minobsinnode = 1)),
    caretModelSpec("ranger", tuneGrid = expand.grid(mtry = c(1:6) ,min.node.size=1, splitrule = "gini")),
    caretModelSpec("glmnet", tuneGrid = expand.grid(alpha = 1, lambda= seq( from = 0.0001, to =0.0005, by = 0.0001)))
    
  )
)

glm_stack11 <- caretStack(stacked_models11, method="glmnet", metric="ROC", trControl=control_stacking10 )




#BAC
confusionMatrix(predict(glm_stack11, testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(glm_stack11, testint,type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted)
roc






#ensemble with smote K=2


set.seed(1234)

control_stacking4 <- trainControl(method="repeatedcv", number=10, repeats=5, savePredictions=TRUE, classProbs=TRUE,summaryFunction = twoClassSummary)


control_stacking4$sampling <- NULL

algorithms_to_use4 <- c('rpart', 'glm', 'ranger', 'svmRadial')


stacked_models4 <- caretList(
  x=as.matrix(trainsclint2 ,with = F ),
  y=as.matrix(usefinalint2$genderfm, with = F),
  trControl=control_stacking4, 
  methodList=algorithms_to_use4)


stacking_results4 <- resamples(stacked_models4)


glm_stack4 <- caretStack(stacked_models4, method="glm", metric="ROC", trControl=control_stacking4 )

print(glm_stack4)




#BAC
confusionMatrix(predict(glm_stack4, testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(glm_stack4, testint,type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted)
roc





#ensemble with smote K=2


set.seed(1234)

control_stacking5 <- trainControl(method="repeatedcv", number=10, repeats=5, savePredictions=TRUE, classProbs=TRUE,summaryFunction = twoClassSummary)


control_stacking5$sampling <- NULL

algorithms_to_use5 <- c("glmnet", "ranger", "gbm")


stacked_models5 <- caretList(
  x=as.matrix(trainsclint2 ,with = F ),
  y=as.matrix(usefinalint2$genderfm, with = F),
  trControl=control_stacking5, 
  methodList=algorithms_to_use5)


stacking_results5 <- resamples(stacked_models5)


glm_stack5 <- caretStack(stacked_models5, method="glmnet", metric="ROC", trControl=control_stacking5 )

print(glm_stack5)


#
#BAC
confusionMatrix(predict(glm_stack5, testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(glm_stack5, testint,type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted)
roc









rf_stack5 <- caretStack(stacked_models4, method="ranger", metric="ROC", trControl=control_stacking5 )

print(rf_stack5)



#BAC
confusionMatrix(predict(rf_stack5, testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(rf_stack5, testint,type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted)
roc







#


set.seed(1234)

control_stacking6 <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE,summaryFunction = twoClassSummary)


control_stacking6$sampling <- NULL

algorithms_to_use6 <- c( "ranger", "gbm")


gbmGrids=expand.grid(interaction.depth = c( 3, 5), 
                     n.trees = c(3:5)*100, 
                     shrinkage = c(0.05,0.1),
                     n.minobsinnode = 10)

# verbose = FALSE

grid_RFs <- expand.grid(mtry = c(4:10) ,min.node.size=1, splitrule = "gini")


stacked_models6 <- caretList(
  x=as.matrix(trainsclint2 ,with = F ),
  y=as.matrix(usefinalint2$genderfm, with = F),
  trControl=control_stacking6, 
  methodList=algorithms_to_use6,
  tuneList=list(
    caretModelSpec("ranger", tuneLength=5),
    caretModelSpec("gbm", tuneLength=5)
  )
)


#gmb2=caretModelSpec(method="gbm", tuneGrid=gbmGrids, preProcess="pca")

gbm_stack6 <- caretStack(stacked_models6, method="gbm", metric="ROC", trControl=control_stacking6 )

print(gbm_stack6)



#BAC
confusionMatrix(predict(gbm_stack6, testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(gbm_stack6, testint,type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted)
roc





#new ensmble

set.seed(1234)

control_stacking6 <- trainControl(method="repeatedcv", number=10, repeats=5, savePredictions=TRUE, classProbs=TRUE,summaryFunction = twoClassSummary)


control_stacking6$sampling <- NULL


grid_RFs <- expand.grid(mtry = c(4:10) ,min.node.size=1, splitrule = "gini")


stacked_models7 <- caretList(
  x=as.matrix(trainsclint2 ,with = F ),
  y=as.matrix(usefinalint2$genderfm, with = F),
  trControl=control_stacking6, 
  methodList=algorithms_to_use6,
  
  tuneList=list(
    caretModelSpec("gbm", tuneGrid = expand.grid(  interaction.depth = c( 3, 6), 
                                                   n.trees = c(3:5)*100, 
                                                   shrinkage = c(0.05,0.1),
                                                   n.minobsinnode = 1)),
    caretModelSpec("ranger", tuneGrid = expand.grid(mtry = c(4:6) ,min.node.size=1, splitrule = "gini"))
  )
)




rf_stack7 <- caretStack(stacked_models7, method="ranger", metric="ROC", trControl=control_stacking6 )






#BAC
confusionMatrix(predict(rf_stack7, testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(rf_stack7, testint,type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted)
roc






#new ensemble
control_stacking8 <- trainControl(method="repeatedcv", number=10, repeats=2, savePredictions=TRUE, classProbs=TRUE,summaryFunction = twoClassSummary)
algorithms_to_use8 <- c( "ranger", "gbm", "glmnet")

stacked_models8 <- caretList(
  x=as.matrix(trainsclint[,c("meanprice" , "mostlovedbrand", "mostlovedbusi","mostloved1", "mostloved2","countclick",
                             "countmlorder","countfmorder", "countuni", "countml","countfm" )] ,with = F ),
  y=as.matrix(usefinalint$genderfm, with = F),
  trControl=control_stacking8, 
  methodList=algorithms_to_use8,
  verbose= FALSE,
  tuneList=list(
    caretModelSpec("gbm", tuneGrid = expand.grid(  interaction.depth = c( 3, 6), 
                                                   n.trees = c(3:5)*100, 
                                                   shrinkage = c(0.05,0.1),
                                                   n.minobsinnode = 1)),
    caretModelSpec("ranger", tuneGrid = expand.grid(mtry = c(4:6) ,min.node.size=1, splitrule = "gini")),
    caretModelSpec("glmnet", tuneGrid = expand.grid(alpha = 1, lambda= seq( from = 0.001, to =0.003, by = 0.0003)))
  )
)







glm_stack8 <- caretStack(stacked_models8, method="glmnet", metric="ROC",trControl=control_stacking8)




#BAC
confusionMatrix(predict(glm_stack8, testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(glm_stack8, testint,type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted)
roc


#

rf_stack8 <- caretStack(stacked_models8, method="ranger", metric="ROC",trControl=control_stacking8)



#

#new ensemble21 #smote
control_stacking21<- trainControl(method="repeatedcv", number=10, repeats=2, savePredictions=TRUE, classProbs=TRUE,summaryFunction = twoClassSummary)
algorithms_to_use21 <- c( "ranger", "gbm", "glmnet")

stacked_models21 <- caretList(
  x=as.matrix(trainsclint[,c("countclick",
                             "countmlorder","countfmorder", "countuni", "countml","countfm" )] ,with = F ),
  y=as.matrix(usefinalint$genderfm, with = F),
  trControl=control_stacking21, 
  #methodList=algorithms_to_use21,
  verbose= FALSE,
  tuneList=list(
    caretModelSpec("gbm", tuneGrid = expand.grid(  interaction.depth = c( 3, 6), 
                                                   n.trees = c(3:5)*100, 
                                                   shrinkage = c(0.05,0.1),
                                                   n.minobsinnode = 1)),
    caretModelSpec("ranger", tuneGrid = expand.grid(mtry = c(4:6) ,min.node.size=1, splitrule = "gini")),
    caretModelSpec("glmnet", tuneGrid = expand.grid(alpha = 1, lambda= seq( from = 0.001, to =0.003, by = 0.0003)))
  )
)

gbm_stack21 <- caretStack(stacked_models21, method="gbm", metric="ROC",trControl=control_stacking21)


#BAC
confusionMatrix(predict(gbm_stack21, testint[,c("countclick","countmlorder","countfmorder", "countuni", "countml","countfm" )]),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(gbm_stack21, testint[,c("countclick",
                                            "countmlorder","countfmorder", "countuni", "countml","countfm" )],type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted)
roc




#


RF_nodefew <- train(x=as.matrix(trainsclint[,c("meanprice" , "mostlovedbrand", "mostlovedbusi","mostloved1", "mostloved2","countclick",
                                               "countmlorder","countfmorder", "countuni", "countml","countfm" )] ,with = F ),
                    y=as.matrix(usefinalint$genderfm, with = F),
                    method="ranger",metric="ROC", tuneGrid= grid_RF_node1, trControl= Traincontrol_RF)



RF_nodefew


#BAC
confusionMatrix(predict(RF_nodefew, testint[,c("meanprice" , "mostlovedbrand", "mostlovedbusi","mostloved1", "mostloved2","countclick",
                                               "countmlorder","countfmorder", "countuni", "countml","countfm" )]),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(RF_nodefew, testint[,c("meanprice" , "mostlovedbrand", "mostlovedbusi","mostloved1", "mostloved2","countclick",
                                           "countmlorder","countfmorder", "countuni", "countml","countfm" )],type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted[,"F"])
roc




#naive bayes with smote


set.seed(1234)
TC_naive <-trainControl(method="repeatedcv", repeats = 5, number=10,  allowParallel=TRUE, classProbs = TRUE,summaryFunction = twoClassSummary)


naive_cl <- train(x=as.matrix(trainsclint ,with = F ),
                  y=as.matrix(usefinalint$genderfm, with = F),
                  method = "naive_bayes",
                  tuneLength = 6,
                  metric = "ROC",
                  trControl = TC_naive)

#BAC
confusionMatrix(predict(naive_cl , testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(naive_cl , testint,type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted[,"F"])
roc



#nnt
#with smote

set.seed(1234)
TC_nnt <-trainControl(method="cv", number=10,  allowParallel=TRUE, verboseIter = TRUE, classProbs = TRUE,summaryFunction = twoClassSummary)


nnet <- train(x=as.matrix(trainsclint ,with = F ),
              y=as.matrix(usefinalint$genderfm, with = F),
              method = "nnet",
              metric = "ROC",
              tuneLength = 6,
              trControl = TC_nnt)

#BAC
confusionMatrix(predict(nnet  , testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(nnet  , testint,type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted[,"F"])
roc



#rda with smote


set.seed(1234)
TC_rda <-trainControl(method="cv", number=10,  allowParallel=TRUE, verboseIter = TRUE, classProbs = TRUE,summaryFunction = twoClassSummary)


rda <- train(x=as.matrix(trainsclint ,with = F ),
             y=as.matrix(usefinalint$genderfm, with = F),
             method = "rda",
             metric = "ROC",
             tuneLength = 6,
             trControl = TC_rda)

#BAC
confusionMatrix(predict(rda  , testint),as.factor ( sclinttestSetall$genderfm), positive = 'F')


#"ROC curve and AUC"
predicted = predict(rda  , testint,type = "prob")

roc = auc(response=sclinttestSetall$genderfm, predictor= predicted[,"F"])
roc




###end internal train and test








#all the models with full trainnig data----


#smote for full training data ----
####train data set
#more female than male
#female twice as large as male
#dup_size	:The maximum times of synthetic minority instances over original majority instances in the oversampling
#duplicate once in order to equalize
finaltrain$gender = ifelse(finaltrain$gender == "F", 1, 0 ) 

newData <- SMOTE(X=finaltrain , target= finaltrain$gender, K=1, dup_size = 1 )
library("dplyr")
#verynew <- newData[["data"]] %>%select(-class)
verynew <- newData[["data"]] 
dim(verynew)
verynew  = verynew [,-19]
dim(newData[["data"]] )
dim(verynew)
classnow <- verynew  %>% group_by(gender) %>% summarize(count=n()) 
classnow #balanced

usefinal = verynew

usefinal$gender = ifelse(usefinal$gender == "1", "F", "M" ) 
#usefinal$gender = ifelse(usefinal$gender == "F", "1", "0" ) 
usefinal 
#scale wo id and gender, all are cont. var.
trainscl <- as.data.table(scale(usefinal [,-c(1)]))
#with gender
trainsclgn = cbind(trainscl , usefinal$gender)

colnames(trainsclgn)[18] <-  "gender" 
head(trainsclgn )

#another smote


newData2 <- SMOTE(X=finaltrain , target= finaltrain$gender, K=2, dup_size = 1 )

verynew2 <- newData2[["data"]]
verynew2 <- verynew2 [,-19]
dim(newData2[["data"]] )
dim(verynew2)
classnow2 <- verynew2  %>% group_by(gender) %>% summarize(count=n()) 
classnow2 #balanced

usefinal2 = verynew2

usefinal2$gender = ifelse(usefinal2$gender == "1", "F", "M" ) 
#usefinal$gender = ifelse(usefinal$gender == "F", "1", "0" ) 
usefinal 
#scale wo id and gender, all are cont. var.
trainscl2 <- as.data.table(scale(usefinal2 [,-c(1)]))
#with gender
trainsclgn2 = cbind(trainscl2 , usefinal2$gender)

colnames(trainsclgn2)[18] <-  "gender" 
head(trainsclgn2 )

#



#not sampled data
head(trainscl)
dim(trainscl)
finaltrainsclall

#models----

#logreg

set.seed(1234)
log_reg_grid <- expand.grid(alpha = 1, lambda= seq( from = 0.001, to =0.003, by = 0.00003))

TC2 <-trainControl(method="cv", number=10, classProbs = TRUE,summaryFunction=twoClassSummary)
log_reg2 <- train(x=as.matrix(trainscl ,with = F ),y=as.matrix(usefinal$gender, with = F),metric= c("ROC") ,method="glmnet",family='binomial',tuneGrid= log_reg_grid, trControl= TC2 )
log_reg2 

set.seed(1234)
log_reg_grid3 <- expand.grid(alpha = c(0,0.5,1), lambda= seq( from = 0.001, to =0.003, by = 0.00003))

TC2 <-trainControl(method="cv", number=10, classProbs = TRUE,summaryFunction=twoClassSummary)
log_reg3 <- train(x=as.matrix(trainscl ,with = F ),y=as.matrix(usefinal$gender, with = F),metric= c("ROC") ,method="glmnet",family='binomial',tuneGrid= log_reg_grid3, trControl= TC2 )
log_reg3 




#rf
library("randomForest")

set.seed(1234)

grid_RF <- expand.grid(mtry = c(4:17) ,min.node.size=5, splitrule = "gini")
Traincontrol_RF <-trainControl(method="cv", number=10,  allowParallel=TRUE, classProbs = TRUE, summaryFunction = twoClassSummary)
RF_mod <- train(x=as.matrix(trainscl ,with = F ),y=as.matrix(usefinal$gender, with = F) ,method="ranger",metric="ROC", tuneGrid= grid_RF, trControl= Traincontrol_RF)
RF_mod


varImpPlot(RF_mod$bestTune)

RF_try=randomForest(x=as.matrix(trainscl ,with = F ),y=as.factor(as.matrix(usefinal$gender, with = F)),
                    min.node.size=5, splitrule = "gini",
                    mtry = 4, 
                    importance=TRUE, localImp=TRUE) 


varImpPlot(RF_try)


set.seed(1234)

grid_RF_node <- expand.grid(mtry = c(4:17) ,min.node.size=1, splitrule = "gini")
Traincontrol_RF <-trainControl(method="cv", number=10,  allowParallel=TRUE, classProbs = TRUE, summaryFunction = twoClassSummary)
RF_node <- train(x=as.matrix(trainscl ,with = F ),y=as.matrix(usefinal$gender, with = F) ,method="ranger",metric="ROC", tuneGrid= grid_RF_node, trControl= Traincontrol_RF)
RF_node



set.seed(1234)
#newRF
grid_RF_node1 <- expand.grid(mtry = c(1:8) ,min.node.size=1, splitrule = "gini")
Traincontrol_RF <-trainControl(method="cv", number=10,  allowParallel=TRUE, classProbs = TRUE, summaryFunction = twoClassSummary)
RF_node1 <- train(x=as.matrix(trainscl[,c("countml","countfm","countclick","mostlovedbrand","countfav" ,"countwork", "countuni", "weekendcount" )] ,with = F ),
                 y=as.matrix(usefinal$gender, with = F),
                 method="ranger",metric="ROC", tuneGrid= grid_RF_node1, trControl= Traincontrol_RF)
RF_node1


set.seed(1234)
#newgbm
gbmGrid=expand.grid(interaction.depth = c( 3, 5), 
                    n.trees = c(1:5)*100, 
                    shrinkage = c(0.05,0.1),
                    n.minobsinnode = c(1, 3, 5))
TC_gbm <-trainControl(method="cv", number=10,  allowParallel=TRUE, classProbs = TRUE,summaryFunction = twoClassSummary)


gbm_node1 <- train(x=as.matrix(trainscl[,c("countml","countfm","countclick","mostlovedbrand","countfav" ,"countwork", "countuni", "weekendcount" )] ,with = F ),
                  y=as.matrix(usefinal$gender, with = F),
                  method="gbm",metric="ROC", tuneGrid= gbmGrid, trControl= TC_gbm)
gbm_node1


#fewrf

set.seed(1234)
#RF
grid_RF_node1 <- expand.grid(mtry = c(1:8) ,min.node.size=1, splitrule = "gini")
Traincontrol_RF <-trainControl(method="cv", number=10,  allowParallel=TRUE, classProbs = TRUE, summaryFunction = twoClassSummary)
RF_node1 <- train(x=as.matrix(trainscl[,c("countml","countfm","countclick","mostlovedbrand","countfav" ,"countwork", "countuni", "weekendcount" )] ,with = F ),
                  y=as.matrix(usefinal$gender, with = F),
                  method="ranger",metric="ROC", tuneGrid= grid_RF_node1, trControl= Traincontrol_RF)
RF_node1

set.seed(1234)
#RF remove corr
grid_RF_node1 <- expand.grid(mtry = c(1:8) ,min.node.size=1, splitrule = "gini")
Traincontrol_RF <-trainControl(method="cv", number=10,  allowParallel=TRUE, classProbs = TRUE, summaryFunction = twoClassSummary)
RF_nodefew <- train(x=as.matrix(trainscl[,c("meanprice" , "mostlovedbrand", "mostlovedbusi","mostloved1", "mostloved2","countclick",
                                          "countmlorder","countfmorder", "countuni", "countml","countfm" )] ,with = F ),
                  y=as.matrix(usefinal$gender, with = F),
                  method="ranger",metric="ROC", tuneGrid= grid_RF_node1, trControl= Traincontrol_RF)
RF_nodefew


                 
    
                       


##countfm , countml,  

print(RF_mod)
plot(RF_mod)

#90 roc

#gbm
set.seed(1234)
TC_gbm <-trainControl(method="cv", number=10,  allowParallel=TRUE, classProbs = TRUE,summaryFunction = twoClassSummary)

gbmGrid=expand.grid(interaction.depth = c( 3, 5), 
                    n.trees = c(1:5)*100, 
                    shrinkage = c(0.05,0.1),
                    n.minobsinnode = 10)

gboost <- train(x=as.matrix(trainscl ,with = F ),
                  y=as.matrix(usefinal$gender, with = F),
                  method = "gbm",
                  verbose = FALSE,
                  metric = "ROC",
                  tuneGrid=gbmGrid,
                  trControl = TC_gbm)
gboost 
gboost$bestTune

#90 roc

#svm
set.seed(1234)
TC_svm <-trainControl(method="cv", number=10,  allowParallel=TRUE, classProbs = TRUE,summaryFunction = twoClassSummary)


svm_radial_mod <- train(x=as.matrix(trainscl ,with = F ), y=as.matrix(usefinal$gender, with = F), method = "svmRadial", trControl = TC_svm , tuneLength = 5,  metric = "ROC",)
# Print the best tuning parameter sigma and C that maximizes model accuracy
svm_radial_mod 
svm_radial_mod$bestTune


#rad svm with only most imp variables

svm_radial_mod_imp <- train(x=as.matrix(trainscl[,c("countml","countfm")] ,with = F ), y=as.matrix(usefinal$gender, with = F), method = "svmRadial", trControl = TC_svm , tuneLength = 5,  metric = "ROC",)
# Print the best tuning parameter sigma and C that maximizes model accuracy
svm_radial_mod_imp 


svm_radial_mod_impmore <- train(x=as.matrix(trainscl[,c("countml","countfm","countclick","mostlovedbrand","countfav" ,"countwork", "countuni", "weekendcount" )] ,with = F ),
                             y=as.matrix(usefinal$gender, with = F),
                             method = "svmRadial", 
                             trControl = TC_svm , tuneLength = 5,
                             metric = "ROC",)
# Print the best tuning parameter sigma and C that maximizes model accuracy
svm_radial_mod_impmore


#svm few var

svm_radial_few <- train(x=as.matrix(trainscl[,c("meanprice" , "mostlovedbrand", "mostlovedbusi","mostloved1", "mostloved2","countclick",
                                            "countmlorder","countfmorder", "countuni", "countml","countfm" )] ,with = F ),
                    y=as.matrix(usefinal$gender, with = F),
                    method="svmRadial",metric="ROC", tuneLength = 6, trControl= TC_svm)
svm_radial_few 





 #rf with K=2 smote
set.seed(1234)

grid_RF_n1 <- expand.grid(mtry = c(4:17) ,min.node.size=1, splitrule = "gini")
Traincontrol_RF_n1 <-trainControl(method="repeatedcv", repeats = 5, number=10,  allowParallel=TRUE, classProbs = TRUE, summaryFunction = twoClassSummary)
RF_mod_n1 <- train(x=as.matrix(trainscl2 ,with = F ),y=as.matrix(usefinal2$gender, with = F) ,method="ranger",metric="ROC", tuneGrid= grid_RF_n1, trControl= Traincontrol_RF_n1)
RF_mod_n1














#with class wieghts




                 

weighted_svm_rad <- train(genderfm ~ .,
                         data = finaltrainsclall ,
                         method="svmRadial",
                         metric="ROC", 
                         tuneLength = 5,
                         class.weights= "inverse",
                         trControl= TC_svm)


weighted_svm_rad



#try with sigma == 

 #%83
#svm poly
set.seed(1234)
svm_pol <- train(x=as.matrix(trainscl ,with = F ), y=as.matrix(usefinal$gender, with = F), method = "svmPoly", trControl = TC_svm , tuneLength = 3,  metric = "ROC",)
# Print the best tuning parameter sigma and C that maximizes model accuracy
svm_pol
svm_pol$bestTune


#svm linear
set.seed(1234)
svm_linear_mod <- train(x=as.matrix(trainscl ,with = F ), y=as.matrix(usefinal$gender, with = F), method = "svmLinear", trControl = TC_svm , tuneLength = 10,metric = "ROC",)
svm_linear_mod$bestTune
#

#
# Build weighted model

table(finaltrainsclall$genderfm)
prop.table(table(finaltrainsclall$genderfm))


model_weights <- ifelse(finaltrainsclall$gender == "1",
                        (1/table(finaltrainsclall$genderfm)[1]) * 0.3451406,
                        (1/table(finaltrainsclall$genderfm)[2]) * 0.6548594)

sum(model_weights)


#weighted rf
finaltrainsclall$genderfm = ifelse(finaltrainsclall$genderfm == "1", "F", "M" ) 

weighted_fit_rf <- train(genderfm ~ .,
                         data = finaltrainsclall ,
                         method="ranger",
                         metric="ROC", 
                         weights = model_weights,
                         tuneGrid= grid_RF,
                         trControl= Traincontrol_RF)

weighted_fit_rf 


#weighted boost
weighted_fit_gboost <- train(genderfm ~ .,
                         data = finaltrainsclall ,
                         method="gbm",
                         metric="ROC", 
                         weights = model_weights,
                         tuneGrid=gbmGrid,
                         trControl = TC_gbm)

weighted_fit_gboost 


# Build down-sampled model

TC_gbm$sampling <- "down"
Traincontrol_RF$sampling <- "down"

down_fit_RF <- train(genderfm ~ .,
             data = finaltrainsclall ,
             method="ranger",
             metric="ROC", 
            tuneGrid= grid_RF,
            trControl= Traincontrol_RF)

down_fit_RF 

#
Traincontrol_RF2 <-trainControl(method="repeatedcv", number=10, repeats = 5, allowParallel=TRUE, classProbs = TRUE, summaryFunction = twoClassSummary)
Traincontrol_RF2$sampling <- "down"

grid_RF2 <- expand.grid(mtry = c(4:15) ,min.node.size=1, splitrule = "gini")


#
down_fit_RF2 <- train(genderfm ~ .,
                     data = finaltrainsclall ,
                     method="ranger",
                     metric="ROC", 
                     tuneGrid= grid_RF2,
                     trControl= Traincontrol_RF2)

down_fit_RF2

grid_RFpre2 <- expand.grid(mtry = c(4:8) ,min.node.size=1, splitrule = "gini")

down_fit_preRF2 <- train(genderfm ~ .,
                      data = finaltrainsclall ,
                      method="ranger",
                      metric="ROC", 
                      tuneGrid= grid_RFpre2,
                      preProc=c("pca"),
                      trControl= Traincontrol_RF2)

down_fit_preRF2





down_fit_gboost <- train(genderfm ~ .,
                             data = finaltrainsclall ,
                             method="gbm",
                             metric="ROC", 
                             tuneGrid=gbmGrid,
                             trControl = TC_gbm)


down_fit_gboost 







# Build up-sampled model

Traincontrol_RF$sampling <- "up"

up_fit_RF <- train(genderfm ~ .,
                     data = finaltrainsclall ,
                     method="ranger",
                     metric="ROC", 
                     tuneGrid= grid_RF,
                     trControl= Traincontrol_RF)

up_fit_RF 


#rose rf

Traincontrol_RF$sampling <- NULL

rose_data <- ROSE(genderfm~., data = finaltrainsclall, N = 5518, seed=123)$data
table(rose_data$genderfm)

rose_fit_RF <- train(genderfm ~ .,
                     data = finaltrainsclall ,
                     method="ranger",
                     metric="ROC", 
                     tuneGrid= grid_RF,
                     trControl= Traincontrol_RF)
rose_fit_RF




#the model with boosting function and train it with train data. 
#The 'boosting' function applies the AdaBoost.M1 and SAMME algorithms using classification trees. 
#A 'boos' is a bootstrap uses the weights for each observation in an iteration 
#if it is TRUE. Otherwise, each observation is used with its weight.
#A 'mfinal' is the number of iterations or trees to use.




TC_ada <-trainControl(method="cv", number=10,  allowParallel=TRUE, classProbs = TRUE,summaryFunction = twoClassSummary)

fitGrid <- expand.grid(mfinal = (1:3)*3,         
                         maxdepth = c(1, 3),       
                         coeflearn = c("Breiman")) 

adaboost <- train(x=as.matrix(trainscl ,with = F ),
                y=as.matrix(usefinal$gender, with = F),
                method = "AdaBoost.M1",
                verbose = FALSE,
                metric = "ROC",
                tuneGrid = fitGrid,
                trControl = TC_ada,
                )
adaboost


fitGrid2<- expand.grid(mfinal = (4:7)*3,         
                       maxdepth = c( 5,7,10),       
                       coeflearn = c("Breiman")) 

adaboost2 <- train(x=as.matrix(trainscl ,with = F ),
                  y=as.matrix(usefinal$gender, with = F),
                  method = "AdaBoost.M1",
                  verbose = FALSE,
                  metric = "ROC",
                  tuneGrid = fitGrid2,
                  trControl = TC_ada,
)

adaboost2


#knn

TC_knn <-trainControl(method="cv", number=10,  allowParallel=TRUE, classProbs = TRUE,summaryFunction = twoClassSummary)

knn_model <- train(genderfm ~ .,
                   data = finaltrainsclall ,
                  weights = model_weights,
                 method     = "kknn",
                tuneGrid   = expand.grid(kmax = 1:5, distance = 1 , kernel= c("rectangular", "optimal")),
                 trControl  = TC_knn,
                 metric     = "ROC")

#ensemble with down sampling


set.seed(1234)
set.seed(18121984)

control_stacking <- trainControl(method="repeatedcv", number=10, repeats=5, savePredictions=TRUE, classProbs=TRUE,summaryFunction = twoClassSummary)


control_stacking$sampling <- "down"

algorithms_to_use <- c('rpart', 'glm', 'ranger', 'svmRadial')



stacked_models <- caretList(
                   genderfm ~ .,
                   data = finaltrainsclall ,
                   trControl=control_stacking, 
                   methodList=algorithms_to_use)

stacking_results <- resamples(stacked_models)



summary(stacking_results)

glm_stack <- caretStack(stacked_models, method="glm", metric="ROC", trControl=control_stacking )



print(glm_stack)

rf_stack <- caretStack(stacked_models, method="ranger", metric="ROC", trControl=control_stacking )

print(rf_stack)

svm_stack <- caretStack(stacked_models, method="svmRadial", metric="ROC", trControl=control_stacking )

print(svm_stack)

#ensemble with rose

control_stacking3 <- trainControl(method="repeatedcv", number=10, repeats=5, savePredictions=TRUE, classProbs=TRUE,summaryFunction = twoClassSummary)


control_stacking3$sampling <- NULL
 

rose_data<- ROSE(genderfm~., data = finaltrainsclall, N = 5518, seed=123)$data


algorithms_to_use3 <- c('gbm', 'glm', 'ranger', 'svmRadial')

stacked_models3 <- caretList(
  genderfm ~ .,
  data = rose_data ,
  trControl=control_stacking3, 
  methodList=algorithms_to_use3)

stacking_results3 <- resamples(stacked_models3)



summary(stacking_results3)

glm_stack3 <- caretStack(stacked_models3, method="glm", metric="ROC", trControl=control_stacking )

print(glm_stack3)




#ensemble glm with down sampling

set.seed(1234)

control_stacking2 <- trainControl(method="repeatedcv", number=10, repeats=5, savePredictions=TRUE, classProbs=TRUE,summaryFunction = twoClassSummary)


control_stacking2$sampling <- "down"

algorithms_to_use2 <- c('glmnet', 'ranger', 'gbm', 'svmRadial')


stacked_models2 <- caretList(
  genderfm ~ .,
  data = finaltrainsclall ,
  trControl=control_stacking2, 
  methodList=algorithms_to_use2)

stacking_results2 <- resamples(stacked_models2)


summary(stacking_results2)

glm_stack2 <- caretStack(stacked_models2, method="glmnet", metric="ROC", trControl=control_stacking )

print(glm_stack)

svm_stack2 <- caretStack(stacked_models2, method="svmRadial", metric="ROC", trControl=control_stacking )

print(svm_stack2)

gbm_stack2 <- caretStack(stacked_models2, method="gbm", metric="ROC", trControl=control_stacking )

print(gbm_stack2)

rf_stack2 <- caretStack(stacked_models2, method="ranger", metric="ROC", trControl=control_stacking )

print(rf_stack2)


#down ensemble another

control_stacking2 <- trainControl(method="repeatedcv", number=10, repeats=5, savePredictions=TRUE, classProbs=TRUE,summaryFunction = twoClassSummary)
algorithms_to_use2 <- c('glmnet', 'ranger', 'gbm', 'svmRadial')

control_stacking2$sampling <- "down"

stacked_models9 <- caretList(
  genderfm ~ .,
  data = finaltrainsclall ,
  trControl=control_stacking2, 
  methodList=algorithms_to_use2,
  
  tuneList=list(
    caretModelSpec("gbm", tuneGrid = expand.grid(  interaction.depth = c(3, 5), 
                                                   n.trees = c(3:5)*100, 
                                                   shrinkage = c(0.05,0.1),
                                                   n.minobsinnode = 1)),
    caretModelSpec("ranger", tuneGrid = expand.grid(mtry = c(1:6) ,min.node.size=1, splitrule = "gini")),
    caretModelSpec("glmnet", tuneGrid = expand.grid(alpha = 1, lambda= seq( from = 0.001, to =0.003, by = 0.0003)))
  )
)

glm_stack9 <- caretStack(stacked_models9, method="glm", metric="ROC", trControl=control_stacking2 )



#down ensemble another

control_stacking10 <- trainControl(method="cv", number=10,  savePredictions=TRUE, classProbs=TRUE,summaryFunction = twoClassSummary)
algorithms_to_use10 <- c('glm', 'ranger', 'rpart' ,'svmRadial')

control_stacking10$sampling <- "down"

stacked_models10 <- caretList(
  genderfm ~ .,
  data = finaltrainsclall ,
  trControl=control_stacking10, 
  methodList=algorithms_to_use10,
    tuneList=list(
    caretModelSpec("ranger", tuneGrid = expand.grid(mtry = c(1:6) ,min.node.size=1, splitrule = "gini"))
      )
)

glm_stack10 <- caretStack(stacked_models10, method="glm", metric="ROC", trControl=control_stacking10 )



#down ensemble another many

control_stacking11 <- trainControl(method="cv", number=10,  savePredictions=TRUE, classProbs=TRUE,summaryFunction = twoClassSummary)
algorithms_to_use11 <- c('glmnet', 'ranger', 'rpart' ,'svmRadial', "gbm")

control_stacking11$sampling <- "down"

stacked_models11 <- caretList(
  genderfm ~ .,
  data = finaltrainsclall ,
  trControl=control_stacking11, 
  methodList=algorithms_to_use11,
  tuneList=list(
    caretModelSpec("gbm", tuneGrid = expand.grid(  interaction.depth = c(3, 5), 
                                                   n.trees = 500, 
                                                   shrinkage = c(0.05,0.1),
                                                   n.minobsinnode = 1)),
    caretModelSpec("ranger", tuneGrid = expand.grid(mtry = c(1:6) ,min.node.size=1, splitrule = "gini")),
    caretModelSpec("glmnet", tuneGrid = expand.grid(alpha = 1, lambda= seq( from = 0.0001, to =0.0005, by = 0.0001)))
   
        )
)

glm_stack11 <- caretStack(stacked_models11, method="glmnet", metric="ROC", trControl=control_stacking10 )




#ensemble with smote K=2


set.seed(1234)

control_stacking4 <- trainControl(method="repeatedcv", number=10, repeats=5, savePredictions=TRUE, classProbs=TRUE,summaryFunction = twoClassSummary)


control_stacking4$sampling <- NULL
  
algorithms_to_use4 <- c('rpart', 'glm', 'ranger', 'svmRadial')


stacked_models4 <- caretList(
  x=as.matrix(trainscl2 ,with = F ),
  y=as.matrix(usefinal2$gender, with = F),
  trControl=control_stacking4, 
  methodList=algorithms_to_use4)


stacking_results4 <- resamples(stacked_models4)


glm_stack4 <- caretStack(stacked_models4, method="glm", metric="ROC", trControl=control_stacking4 )

print(glm_stack4)




#ensemble with smote K=2


set.seed(1234)

control_stacking5 <- trainControl(method="repeatedcv", number=10, repeats=5, savePredictions=TRUE, classProbs=TRUE,summaryFunction = twoClassSummary)


control_stacking5$sampling <- NULL

algorithms_to_use5 <- c("glmnet", "ranger", "gbm")


stacked_models5 <- caretList(
  x=as.matrix(trainscl2 ,with = F ),
  y=as.matrix(usefinal2$gender, with = F),
  trControl=control_stacking5, 
  methodList=algorithms_to_use5)


stacking_results5 <- resamples(stacked_models5)


glm_stack5 <- caretStack(stacked_models5, method="glmnet", metric="ROC", trControl=control_stacking5 )

print(glm_stack5)

rf_stack5 <- caretStack(stacked_models4, method="ranger", metric="ROC", trControl=control_stacking5 )

print(rf_stack5)

#


set.seed(1234)

control_stacking6 <- trainControl(method="repeatedcv", number=10, repeats=5, savePredictions=TRUE, classProbs=TRUE,summaryFunction = twoClassSummary)


control_stacking6$sampling <- NULL

algorithms_to_use6 <- c( "ranger", "gbm")


gbmGrids=expand.grid(interaction.depth = c( 3, 5), 
                     n.trees = c(3:5)*100, 
                     shrinkage = c(0.05,0.1),
                     n.minobsinnode = 10)

# verbose = FALSE

grid_RFs <- expand.grid(mtry = c(4:10) ,min.node.size=1, splitrule = "gini")


stacked_models6 <- caretList(
  x=as.matrix(trainscl2 ,with = F ),
  y=as.matrix(usefinal2$gender, with = F),
  trControl=control_stacking6, 
  methodList=algorithms_to_use6,
  tuneList=list(
    caretModelSpec("ranger", tuneLength=5),
    caretModelSpec("gbm", tuneLength=5)
                  )
)


 #gmb2=caretModelSpec(method="gbm", tuneGrid=gbmGrids, preProcess="pca")

gbm_stack6 <- caretStack(stacked_models6, method="gbm", metric="ROC", trControl=control_stacking6 )

print(gbm_stack6)

#new ensmble

set.seed(1234)

control_stacking6 <- trainControl(method="repeatedcv", number=10, repeats=5, savePredictions=TRUE, classProbs=TRUE,summaryFunction = twoClassSummary)


control_stacking6$sampling <- NULL


grid_RFs <- expand.grid(mtry = c(4:10) ,min.node.size=1, splitrule = "gini")


stacked_models7 <- caretList(
  x=as.matrix(trainscl2 ,with = F ),
  y=as.matrix(usefinal2$gender, with = F),
  trControl=control_stacking6, 
  methodList=algorithms_to_use6,
  
  tuneList=list(
    caretModelSpec("gbm", tuneGrid = expand.grid(  interaction.depth = c( 3, 6), 
                                                    n.trees = c(3:5)*100, 
                                                    shrinkage = c(0.05,0.1),
                                                    n.minobsinnode = 1)),
    caretModelSpec("ranger", tuneGrid = expand.grid(mtry = c(4:6) ,min.node.size=1, splitrule = "gini"))
  )
)




rf_stack7 <- caretStack(stacked_models7, method="ranger", metric="ROC", trControl=control_stacking6 )


#new ensemble
control_stacking8 <- trainControl(method="repeatedcv", number=10, repeats=2, savePredictions=TRUE, classProbs=TRUE,summaryFunction = twoClassSummary)
algorithms_to_use8 <- c( "ranger", "gbm", "glmnet")

stacked_models8 <- caretList(
  x=as.matrix(trainscl[,c("meanprice" , "mostlovedbrand", "mostlovedbusi","mostloved1", "mostloved2","countclick",
                            "countmlorder","countfmorder", "countuni", "countml","countfm" )] ,with = F ),
  y=as.matrix(usefinal2$gender, with = F),
  trControl=control_stacking8, 
  methodList=algorithms_to_use8,
  verbose= FALSE,
  tuneList=list(
    caretModelSpec("gbm", tuneGrid = expand.grid(  interaction.depth = c( 3, 6), 
                                                   n.trees = c(3:5)*100, 
                                                   shrinkage = c(0.05,0.1),
                                                   n.minobsinnode = 1)),
    caretModelSpec("ranger", tuneGrid = expand.grid(mtry = c(4:6) ,min.node.size=1, splitrule = "gini")),
    caretModelSpec("glmnet", tuneGrid = expand.grid(alpha = 1, lambda= seq( from = 0.001, to =0.003, by = 0.0003)))
  )
)







glm_stack8 <- caretStack(stacked_models8, method="glmnet", metric="ROC",trControl=control_stacking8)

rf_stack8 <- caretStack(stacked_models8, method="ranger", metric="ROC",trControl=control_stacking8)


print(rf_stack7)

#

#new ensemble21 #smote
control_stacking21<- trainControl(method="repeatedcv", number=10, repeats=2, savePredictions=TRUE, classProbs=TRUE,summaryFunction = twoClassSummary)
algorithms_to_use21 <- c( "ranger", "gbm", "glmnet")

stacked_models21 <- caretList(
  x=as.matrix(trainscl[,c("countclick",
                          "countmlorder","countfmorder", "countuni", "countml","countfm" )] ,with = F ),
  y=as.matrix(usefinal$gender, with = F),
  trControl=control_stacking21, 
  #methodList=algorithms_to_use21,
  verbose= FALSE,
  tuneList=list(
    caretModelSpec("gbm", tuneGrid = expand.grid(  interaction.depth = c( 3, 6), 
                                                   n.trees = c(3:5)*100, 
                                                   shrinkage = c(0.05,0.1),
                                                   n.minobsinnode = 1)),
    caretModelSpec("ranger", tuneGrid = expand.grid(mtry = c(4:6) ,min.node.size=1, splitrule = "gini")),
    caretModelSpec("glmnet", tuneGrid = expand.grid(alpha = 1, lambda= seq( from = 0.001, to =0.003, by = 0.0003)))
  )
)

gbm_stack21 <- caretStack(stacked_models21, method="gbm", metric="ROC",trControl=control_stacking21)



#


RF_nodefew <- train(x=as.matrix(trainscl[,c("meanprice" , "mostlovedbrand", "mostlovedbusi","mostloved1", "mostloved2","countclick",
                                            "countmlorder","countfmorder", "countuni", "countml","countfm" )] ,with = F ),
                    y=as.matrix(usefinal$gender, with = F),
                    method="ranger",metric="ROC", tuneGrid= grid_RF_node1, trControl= Traincontrol_RF)
RF_nodefew
#naive bayes with smote


set.seed(1234)
TC_naive <-trainControl(method="repeatedcv", repeats = 5, number=10,  allowParallel=TRUE, classProbs = TRUE,summaryFunction = twoClassSummary)


naive_cl <- train(x=as.matrix(trainscl ,with = F ),
                y=as.matrix(usefinal$gender, with = F),
                method = "naive_bayes",
                tuneLength = 6,
                metric = "ROC",
                trControl = TC_naive)



#nnt
#with smote

set.seed(1234)
TC_nnt <-trainControl(method="cv", number=10,  allowParallel=TRUE, verboseIter = TRUE, classProbs = TRUE,summaryFunction = twoClassSummary)


nnet <- train(x=as.matrix(trainscl ,with = F ),
                y=as.matrix(usefinal$gender, with = F),
                method = "nnet",
                metric = "ROC",
                tuneLength = 6,
                trControl = TC_nnt)

#rda with smote


set.seed(1234)
TC_rda <-trainControl(method="cv", number=10,  allowParallel=TRUE, verboseIter = TRUE, classProbs = TRUE,summaryFunction = twoClassSummary)


rda <- train(x=as.matrix(trainscl ,with = F ),
              y=as.matrix(usefinal$gender, with = F),
              method = "rda",
              metric = "ROC",
              tuneLength = 6,
              trControl = TC_rda)





#test data manuplation ----


#do the same procedure for test set

####TEST###data

#read


testdata <- read.csv("test.csv", header = T, fileEncoding = 'UTF-8-BOM')


head(testdata)

dim(testdata) #2324814      19

#duplicated?
orgtest = testdata

b=as.data.table(orgtest)

b[time_stamp=="2020-10-14T10:42:17Z" & unique_id == "1039",] #exactly the same

b[unique_id == "4501",] #NA

#there are many duplicated 

sum(duplicated(testdata))



testdata= unique(orgtest)   #only with distinct values.
testdata= distinct(orgtest) 
write.csv(testdata,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/testuniqe.csv", row.names = FALSE)

dim(testdata)#the dimension diminihes 877989
utest = orgtest%>%group_by_all%>%count  #yes there are duplicated
utest [1:8,c(1,2,3,20)]

dim(utest) #877989


length(unique(testdata$unique_id)) #2380 ok



testdata
#fill empty ones with NA

testdata [testdata  == " "] <- NA
testdata [testdata  == ""] <- NA
testdata [2750:2755,]



#remove type and gender, no info there
testdata = testdata[, -c(17, 19)]
head(testdata )


#check NA #how many NA's
testmynalist = cbind(
  lapply(
    lapply(testdata, is.na)
    , sum)
)

testmynalist



#try to fill the data for product gender with the help of business unit


#decided to replace all missing product genders with "unisex"
#it seems appropirate given the above table
filltest = testdata

filltest[is.na(filltest$product_gender), 9] = "Unisex"
table(filltest$businessunit, filltest$product_gender)

#now all missing product gender info is turned to unisex


#check NA #how many NA's now
testmynalist2 = cbind(
  lapply(
    lapply(filltest, is.na)
    , sum)
)

testmynalist2

#vae filltest here


# NAs for sale price 

head(testdata[is.na(testdata$sellingprice ),])

head(testdata[is.na(testdata$unique_id ),])

testnoprice =  testdata[is.na(testdata$sellingprice ),]

#impute price of the product based on some info
#put the mean value of the corresponding category3 level

#here,summarized.price comes from train set

testfilledprice = merge( summarized.price,filltest, by="Level3_Category_Name", all.y = TRUE )
head(testfilledprice)
dim(testfilledprice)
length(unique(testfilledprice$unique_id))
#reconstruct the dataframe with full price data
#if price is NA then put mean price of category3, otherwise keep the actual price

testfilledprice$sellingprice  = ifelse(is.na(testfilledprice$sellingprice), testfilledprice$meanpriceforcat3, testfilledprice$sellingprice )


head(testfilledprice)

testfilledprice[1000:1020,]


#reconstruct the dataframe
#drop the last column since, its info is copied for the ones with missing price values,
head(testfilledprice)
dim(testfilledprice)
ftest1 =  testfilledprice[,-2]
head(ftest1)
#reorder, put cat3 back in place
ftest2 = ftest1 [, c(2:14,1,15:17)]
head(ftest2)
finaltest  = ftest2 #done
head(finaltest)
dim(finaltest)
#check NA now
testmynalist5 = cbind(
  lapply(
    lapply(finaltest, is.na)
    , sum)
)

testmynalist5

write.csv(finaltest,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/finaltestend.csv", row.names = FALSE)

#done with NAs.


#now comes the feature engineering part

mytesttable = as.data.table(finaltest)
mytesttable
length(unique(mytesttable$unique_id))

#date and time modification
#seperste to date and time
dt_testtable = mytesttable %>% separate(time_stamp, c("date", 'time'), sep="T")
head(dt_testtable)
tail(dt_testtable)


#remove Z, and miliseconds
dt_testtable$time = str_sub(dt_testtable$time,1,nchar(dt_testtable$time)-1)
dt_testtable$time = str_sub(dt_testtable$time,1,8)

head(dt_testtable)
dt_testtable$date =  as.Date(dt_testtable$date)
#create new columns
day <- c()
workhour <- c()
weekday <- c()
#
dt_testtable$day = format(dt_testtable$date, format="%a")
head(dt_testtable)

######## 1: weekday, 0 :weekend
dt_testtable$weekday = ifelse (dt_testtable$day == "Paz" | dt_testtable$day == "Cmt" , 0 ,1  )

#time
dt_testtable$time =  strptime(dt_testtable$time, format = "%H:%M:%OS")

#workhours? 
######## 1: workhour, 0 :off
dt_testtable$workhour = ifelse (dt_testtable$weekday == "1" & (dt_testtable$time > "08:00:00"  & dt_testtable$time < "18:00:00" ), 1 ,0  ) ######## 1: workhour, 0 :off

head(dt_testtable)
tail(dt_testtable)
dim(dt_testtable)
write.csv(dt_testtable,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/dt_testtable.csv", row.names = FALSE)


#use this final data-set to structure the data in a nicer/structured format

testids= as.data.table(unique(dt_testtable$unique_id))

colnames(testids) <-  "unique_id" 
testids[order(unique_id)]

write.csv(testids,"C:/Users/y.akturk/Desktop/t", row.names = FALSE)


#followings are going to be our new features

#one user how many times clicks female prd_gender
tsummarized.prdgenderfm = dt_testtable[  product_gender == "Kadýn", list( countfm = length (product_gender ) ), by =c("unique_id")]
tsummarized.prdgenderfm [order(unique_id),]

#merge
mergedtest= merge(tsummarized.prdgenderfm , testids, by="unique_id", all = TRUE)
mergedtest 
nrow(mergedtest)

#one user how many times clicks  male prd_gender
tsummarized.prdgenderml = dt_testtable[ product_gender == "Erkek", list( countml = length (product_gender ) ), by =c("unique_id")]
tsummarized.prdgenderml[order(unique_id),]

#merge
mergedtest= merge(tsummarized.prdgenderml , mergedtest, by="unique_id", all = TRUE)
mergedtest
nrow(mergedtest)

#one user how many times clicks  unisex prd_gender
tsummarized.prdgenderuni = dt_testtable[ product_gender == "Unisex", list( countuni = length (product_gender ) ), by =c("unique_id")]
tsummarized.prdgenderuni[order(unique_id),]

#merge
mergedtest= merge(tsummarized.prdgenderuni , mergedtest, by="unique_id", all = TRUE)
mergedtest

nrow(mergedtest)

#one user how many times orders female prd_gender / may be correlated with click number
tsummarized.prdgenderfmorder = dt_testtable[ product_gender == "Kadýn" &  user_action == "order", list( countfmorder = length (product_gender ) ), by =c("unique_id")]
tsummarized.prdgenderfmorder[order(unique_id),]

#merge
mergedtest= merge(tsummarized.prdgenderfmorder , mergedtest, by="unique_id", all = TRUE)
mergedtest

nrow(mergedtest)

#one user how many times orders male prd_gender / may be coorelated with click number
tsummarized.prdgendermlorder = dt_testtable[ product_gender == "Erkek" &  user_action == "order", list( countmlorder = length (product_gender ) ), by =c("unique_id")]
tsummarized.prdgendermlorder[order(unique_id),]

#merge
mergedtest= merge(tsummarized.prdgendermlorder , mergedtest, by="unique_id", all = TRUE)
mergedtest

nrow(mergedtest)
#one user how many times clicks within this given timeframe [acitivity count)] at total
tsummarized.click = dt_testtable[  , list( countclick = length (user_action ) ), by =c("unique_id")]
tsummarized.click [order(unique_id),]

#merge
mergedtest= merge(tsummarized.click , mergedtest, by="unique_id", all = TRUE)
mergedtest
nrow(mergedtest)
#one user how many times order within this timeframe [buy count)] at total
tsummarized.buy = dt_testtable[  user_action == "order" , list( countbuy = length (user_action ) ), by =c("unique_id")]
tsummarized.buy [order(unique_id),]

#merge
mergedtest= merge(tsummarized.buy , mergedtest, by="unique_id", all = TRUE)
mergedtest
nrow(mergedtest)

#one user how many times favorites within this timeframe [fav count)] at total
tsummarized.fav = dt_testtable[  user_action == "favorite" , list( countfav = length (user_action ) ), by =c("unique_id")]
tsummarized.fav [order(unique_id),]

#merge
mergedtest= merge(tsummarized.fav  , mergedtest, by="unique_id", all = TRUE)
mergedtest
nrow(mergedtest)

#one user how many times clicks within one category (search habbits indicator)
tsummarized.search = dt_testtable[   , list( search = length (user_action ) ), by =c("unique_id", "Level3_Category_Id" )]
tsummarized.search [unique_id == "1",]
#use the above one to get
#one user how many times clicks within one category (search habbits indicator) on average 
# ~~like search length for one purchase
tsummarized.shabbit = tsummarized.search[   , list( searchhabbit = mean(search ) ), by =c("unique_id" )]
tsummarized.shabbit [unique_id == "1",] #example

#merge
mergedtest= merge(tsummarized.shabbit  , mergedtest, by="unique_id", all = TRUE)
mergedtest

nrow(mergedtest)

#one user how many times has actvity on the weekends 
######## 1: weekday, 0 :weekend
tsummarized.weekend = dt_testtable[  weekday == 0, list( weekendcount = length (weekday ) ), by =c("unique_id")]
tsummarized.weekend  [order(unique_id),]

#merge
mergedtest= merge(tsummarized.weekend  , mergedtest, by="unique_id", all = TRUE)
mergedtest


nrow(mergedtest)

#one user how many times has actvity during workhours
######## 1: workhour, 0 :off
tsummarized.work = dt_testtable[ workhour == 1 , list( countwork = length (workhour ) ), by =c("unique_id")]
tsummarized.work[order(unique_id),]

#merge
mergedtest= merge(tsummarized.work  , mergedtest, by="unique_id", all = TRUE)
mergedtest 
nrow(mergedtest)
####

#########max Level3_Category_Id
#most active category3 for a user
tsummarized.search3 = dt_testtable[   , list( search3 = length (user_action ) ), by =c("unique_id", "Level3_Category_Id" )]
tsummarized.search3 [unique_id == "1",][order(search3),]
#use the above one to get
#most active category3 for a user
tsummarized.smode3 = tsummarized.search3[   ,list (search3 = max(search3 )) , by =c("unique_id" )]

tsummarized.maxcat3= merge(tsummarized.search3 ,tsummarized.smode3  ,by= c("unique_id" , "search3") )
#if there are equallly liked categories just pick the one with higher cat_id
tsummarized.maxcat3= tsummarized.maxcat3[,list (mostloved3 = max(Level3_Category_Id )) ,by= c("unique_id" , "search3") ]
nrow(tsummarized.maxcat3)
tsummarized.maxcat3[unique_id == "1",]
tsummarized.likedcat3 = tsummarized.maxcat3[,-2]
tsummarized.likedcat3[unique_id == "1",]


#merge
mergedtest= merge(tsummarized.likedcat3  , mergedtest, by="unique_id", all = TRUE)
mergedtest
nrow(mergedtest)


####Level2_Category_Id
#most active category2 for a user
tsummarized.search2 = dt_testtable[   , list( search2 = length (user_action ) ), by =c("unique_id", "Level2_Category_Id" )]
tsummarized.search2 [unique_id == "1",][order(search2),]
#use the above one to get
#most active category2 for a user
tsummarized.smode2 = tsummarized.search2[   ,list (search2 = max(search2 )) , by =c("unique_id" )]
tsummarized.maxcat2= merge(tsummarized.smode2 ,tsummarized.search2  ,by= c("unique_id" , "search2") )
#if there are equallly liked categories just pick the one with higher cat_id
tsummarized.maxcat2= tsummarized.maxcat2[,list (mostloved2 = max(Level2_Category_Id )) ,by= c("unique_id" , "search2") ]

tsummarized.maxcat2[unique_id == "1",]
tsummarized.likedcat2 = tsummarized.maxcat2[,-2]
tsummarized.likedcat2[unique_id == "1",]

#merge
mergedtest= merge(tsummarized.likedcat2  , mergedtest, by="unique_id", all = TRUE)
mergedtest
nrow(mergedtest)




####Level1_Category_Id
#most active category1 for a user
tsummarized.search1 = dt_testtable[   , list( searchone = length (user_action ) ), by =c("unique_id", "Level1_Category_Id" )]
tsummarized.search1 [unique_id == "1",][order(searchone),]
#use the above one to get
#most active category1 for a user
tsummarized.smode1 = tsummarized.search1[   ,list (searchone = max(searchone )) , by =c("unique_id" )]
tsummarized.maxcat1= merge(tsummarized.smode1 ,tsummarized.search1  ,by= c("unique_id" , "searchone") )
#if there are equallly liked categories just pick the one with higher cat_id
tsummarized.maxcat1= tsummarized.maxcat1[,list (mostloved1 = max(Level1_Category_Id )) ,by= c("unique_id" , "searchone") ]

tsummarized.maxcat1[unique_id == "1",]
tsummarized.likedcat1 = tsummarized.maxcat1[,-2]
tsummarized.likedcat1[unique_id == "1",]

#merge
mergedtest= merge(tsummarized.likedcat1  , mergedtest, by="unique_id", all = TRUE)
mergedtest
nrow(mergedtest)
nrow(mergedtest)


####business ýd
#most active business unt = category_id for a user
tsummarized.categ = dt_testtable[   , list( cat = length (user_action ) ), by =c("unique_id", "category_id" )]
tsummarized.categ [unique_id == "1",][order(cat),]
#use the above one to get
#most active business category for a user
tsummarized.maxcateg = tsummarized.categ[   ,list (cat = max(cat )) , by =c("unique_id" )]
tsummarized.maxcategory= merge(tsummarized.maxcateg ,tsummarized.categ  ,by= c("unique_id" , "cat") )
#if there are equallly liked categories just pick the one with higher cat_id
tsummarized.maxcategory= tsummarized.maxcategory[,list (mostlovedbusi = max(category_id )) ,by= c("unique_id" , "cat") ]
tsummarized.maxcategory[unique_id == "1",]
tsummarized.likedbus = tsummarized.maxcategory[,-2]
tsummarized.likedbus[unique_id == "1",]

#merge
mergedtest= merge(tsummarized.likedbus  , mergedtest, by="unique_id", all = TRUE)
mergedtest 
nrow(mergedtest)




####mostly loved brand
#most active brand for a user
tsummarized.brand = dt_testtable[   , list( br = length (user_action ) ), by =c("unique_id", "brand_id" )]
tsummarized.brand [unique_id == "1",][order(br),]
#use the above one to get
#most active business category for a user
tsummarized.br = tsummarized.brand[   ,list (br = max(br )) , by =c("unique_id" )]
tsummarized.brandlove= merge(tsummarized.br ,tsummarized.brand ,by= c("unique_id" , "br") )
#if there are equallly liked brands just pick the one with higher brand_id
tsummarized.brandlove= tsummarized.brandlove[ ,list (mostlovedbrand = max(brand_id )) ,by= c("unique_id" , "br") ]

tsummarized.brandlove[unique_id == "1",]
tsummarized.likedbrand = tsummarized.brandlove[,-2]
tsummarized.likedbrand[unique_id == "1",]


#merge
mergedtest= merge(tsummarized.likedbrand  , mergedtest, by="unique_id", all = TRUE)
mergedtest
nrow(mergedtest)




#avg price of bought prod

tsummarized.buyprice = dt_testtable[user_action == "order", list(meanprice = mean(sellingprice)), by ="unique_id"]
tsummarized.buyprice[unique_id == "1",]

#merge
mergedtest= merge(tsummarized.buyprice  , mergedtest, by="unique_id", all = TRUE)
mergedtest
nrow(mergedtest)


#unq 

length(unique(dt_testtable$unique_id)) #2380

#merge

nrow(mergedtest)

#turn NAs to 0.
#for example if a countbuy= NA then it means that the person  has never bought something, therefore it got an NA during the data table operations,
#logical to put a zero there
#for example if a weekendcount = NA then it means that the person never was active on a weekend therefore logical to put a zero


#####final testing data set
finaltest1 = mergedtest
finaltest1
finaltest1[is.na(finaltest1)] <- 0
finaltest1 #ordered by id -min-to-max
dim(finaltest1)
#remove ids
finaltest = finaltest1 
#finaltest = finaltest1 [, -c(1)]
dim(finaltest)


write.csv(finaltest,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/featuretest.csv", row.names = FALSE)



finaltest =   read.csv("C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/featuretest.csv", header = T, fileEncoding = 'UTF-8-BOM')



#end test data manup




#test

testscl <- as.data.table(scale(finaltest [,-c(1)])) #noids






#predicton on actual test ----

predicted_prob_rf <-  predict(RF_mod,testscl ,type= "prob")

write.csv(predicted_prob_rf ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/predicted_prob_rf .csv", row.names = FALSE)

 
predicted_prob_gboost<-  predict(gboost,testscl ,type= "prob")
write.csv(predicted_prob_gboost ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/predicted_prob_gboost .csv", row.names = FALSE)



predicted_prob_svmrad<-  predict(svm_radial_mod ,testscl ,type= "prob")
write.csv(predicted_prob_svmrad ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/predicted_prob_svmrad.csv", row.names = FALSE)



predicted_prob_svmpol<-  predict(svm_radial_pol ,testscl ,type= "prob")
write.csv(predicted_prob_svmpol ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/predicted_prob_svmpol.csv", row.names = FALSE)



predicted_prob_weighted_fit_rf <-  predict(weighted_fit_rf  ,testscl ,type= "prob")
write.csv(predicted_prob_weighted_fit_rf  ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/predicted_prob_weighted_fit_rf.csv", row.names = FALSE)


predicted_prob_weighted_fit_gboost  <-  predict(weighted_fit_gboost   ,testscl ,type= "prob")
write.csv(predicted_prob_weighted_fit_gboost ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/predicted_prob_weighted_fit_gboost2", row.names = FALSE)


predicted_prob_down_fit_gboost  <-  predict(down_fit_gboost   ,testscl ,type= "prob")
write.csv(predicted_prob_down_fit_gboost  ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/down_fit_gboost", row.names = FALSE)


predicted_prob_down_fit_RF <-  predict(down_fit_RF   ,testscl ,type= "prob")
write.csv(predicted_prob_down_fit_RF  ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/down_fit_RF", row.names = FALSE)


predicted_svm_radial_mod_imp  <-  predict(svm_radial_mod_imp    ,testscl[,c("countml","countfm")], type = "prob" )
write.csv(predicted_svm_radial_mod_imp   ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/predicted_svm_radial_mod_imp.csv", row.names = FALSE)



predicted_up_fit_RF <-  predict(up_fit_RF  ,testscl ,type= "prob")
write.csv(predicted_up_fit_RF ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/predicted_up_fit_RF.csv", row.names = FALSE)



predicted_rose_fit_RF <-  predict(rose_fit_RF  ,testscl ,type= "prob")
write.csv(predicted_rose_fit_RF  ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/predicted_rose_fit_RF.csv", row.names = FALSE)


predicted_svm_pol <-  predict(svm_pol  ,testscl ,type= "prob")
write.csv(predicted_svm_pol   ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/predicted_svm_pol.csv", row.names = FALSE)


predicted_knn_model  <-  predict(knn_model  ,testscl ,type= "prob")
write.csv(predicted_knn_model  ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/predicted_knn_model.csv", row.names = FALSE)


predicted_glm_stack  <-  predict(glm_stack  ,testscl ,type= "prob")
write.csv(predicted_glm_stack  ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/predicted_glm_stack.csv", row.names = FALSE)


predicted_rf_stack  <-  predict(rf_stack  ,testscl ,type= "prob")
write.csv(predicted_rf_stack  ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/predicted_rf_stack.csv", row.names = FALSE)


predicted_svm_stack  <-  predict(rf_stack  ,testscl ,type= "prob")
write.csv(predicted_svm_stack  ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/predicted_svm_stack.csv", row.names = FALSE)


predicted_svm_radial_mod_impmore  <-  predict(svm_radial_mod_impmore  ,testscl[,c("countml","countfm","countclick","mostlovedbrand","countfav" ,"countwork", "countuni", "weekendcount" )] ,type= "prob")
write.csv(predicted_svm_radial_mod_impmore  ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/predicted_svm_radial_mod_impmore.csv", row.names = FALSE)



predicted_weighted_svm_rad  <-  predict(weighted_svm_rad  ,testscl ,type= "prob")
write.csv(predicted_weighted_svm_rad  ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/weighted_svm_rad.csv", row.names = FALSE)


predicted_down_fit_RF2 <-  predict(down_fit_RF2  ,testscl ,type= "prob")
write.csv(predicted_down_fit_RF2  ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/down_fit_RF2.csv", row.names = FALSE)


predicted_glm_stack2 <-  predict(glm_stack2  ,testscl ,type= "prob")
write.csv(predicted_glm_stack2  ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/glm_stack2.csv", row.names = FALSE)

predicted_svm_stack2 <-  predict(svm_stack2  ,testscl ,type= "prob")
write.csv(predicted_svm_stack2  ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/svm_stack2.csv", row.names = FALSE)


predicted_gbm_stack2 <-  predict(gbm_stack2  ,testscl ,type= "prob")
write.csv(predicted_gbm_stack2  ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/gbm_stack2.csv", row.names = FALSE)


predicted_rf_stack2 <-  predict(rf_stack2  ,testscl ,type= "prob")
write.csv(predicted_rf_stack2  ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/rf_stack2.csv", row.names = FALSE)


predicted_glm_stack3 <-  predict(glm_stack3  ,testscl ,type= "prob")
write.csv(predicted_glm_stack3  ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/glm_stack3.csv", row.names = FALSE)


predicted_RF_node <-  predict(RF_node  ,testscl ,type= "prob")
write.csv(predicted_RF_node  ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/RF_node.csv", row.names = FALSE)


predicted_naive_cl <-  predict(naive_cl  ,testscl ,type= "prob")
write.csv(predicted_naive_cl  ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/naive_cl.csv", row.names = FALSE)


predicted_RF_mod_n1 <-  predict(RF_mod_n1  ,testscl ,type= "prob")
write.csv(predicted_RF_mod_n1  ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/RF_mod_n1.csv", row.names = FALSE)


predicted_glm_stack4 <-  predict(glm_stack4  ,testscl ,type= "prob")
write.csv(predicted_glm_stack4  ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/glm_stack4.csv", row.names = FALSE)


predicted_rf_stack4 <-  predict(rf_stack4  ,testscl ,type= "prob")
write.csv(rf_stack4  ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/rf_stack4.csv", row.names = FALSE)

predicted_glm_stack5 <-  predict(glm_stack5  ,testscl ,type= "prob")
write.csv(predicted_glm_stack5  ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/glm_stack5.csv", row.names = FALSE)


predicted_rf_stack5 <-  predict(rf_stack5  ,testscl ,type= "prob")
write.csv(predicted_rf_stack5  ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/rf_stack5.csv", row.names = FALSE)


predicted_gbm_stack6 <-  predict(gbm_stack6  ,testscl ,type= "prob")
write.csv(predicted_gbm_stack6  ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/gbm_stack6.csv", row.names = FALSE)


predicted_rf_stack7 <-  predict(rf_stack7  ,testscl ,type= "prob")
write.csv(predicted_rf_stack7  ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/rf_stack7.csv", row.names = FALSE)


predicted_RF_node1  <-  predict(RF_node1  ,testscl[,c("countml","countfm","countclick","mostlovedbrand","countfav" ,"countwork", "countuni", "weekendcount" )] ,type= "prob")
write.csv(predicted_RF_node1  ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/RF_node1.csv", row.names = FALSE)


predicted_gbm_node1 <-  predict(gbm_node1  ,testscl[,c("countml","countfm","countclick","mostlovedbrand","countfav" ,"countwork", "countuni", "weekendcount" )] ,type= "prob")
write.csv(predicted_gbm_node1  ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/gbm_node1.csv", row.names = FALSE)



predicted_RF_nodefew  <-  predict(RF_nodefew   ,testscl[,c("meanprice" , "mostlovedbrand", "mostlovedbusi","mostloved1", "mostloved2","countclick",
                                                       "countmlorder","countfmorder", "countuni", "countml","countfm" )] , type= "prob")

write.csv(predicted_RF_nodefew  ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/RF_nodefew.csv", row.names = FALSE)



predicted_svm_radial_few   <-  predict(svm_radial_few ,testscl[,c("meanprice" , "mostlovedbrand", "mostlovedbusi","mostloved1", "mostloved2","countclick",
                                                           "countmlorder","countfmorder", "countuni", "countml","countfm" )] , type= "prob")

write.csv(predicted_svm_radial_few  ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/svm_radial_few.csv", row.names = FALSE)



predicted_glm_stack8   <-  predict(glm_stack8 ,testscl[,c("meanprice" , "mostlovedbrand", "mostlovedbusi","mostloved1", "mostloved2","countclick",
                                                                  "countmlorder","countfmorder", "countuni", "countml","countfm" )] , type= "prob")

write.csv(predicted_glm_stack8   ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/glm_stack8.csv", row.names = FALSE)



predicted_rf_stack8   <-  predict(rf_stack8 ,testscl[,c("meanprice" , "mostlovedbrand", "mostlovedbusi","mostloved1", "mostloved2","countclick",
                                                          "countmlorder","countfmorder", "countuni", "countml","countfm" )] , type= "prob")

write.csv(predicted_rf_stack8   ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/rf_stack8.csv", row.names = FALSE)


predicted_down_fit_preRF2 <-  predict(down_fit_preRF2  ,testscl ,type= "prob")
write.csv(predicted_down_fit_preRF2  ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/down_fit_preRF2.csv", row.names = FALSE)



predicted_glm_stack9  <-  predict(glm_stack9  ,testscl ,type= "prob")
write.csv(predicted_glm_stack9   ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/glm_stack9.csv", row.names = FALSE)



predicted_glm_stack10  <-  predict(glm_stack10   ,testscl ,type= "prob")
write.csv(predicted_glm_stack10    ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/glm_stack10.csv", row.names = FALSE)

predicted_glm_stack11  <-  predict(glm_stack11   ,testscl ,type= "prob")
write.csv(predicted_glm_stack11    ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/glm_stack11.csv", row.names = FALSE)

#with rs chnge
predicted_glm_stacknew  <-  predict(glm_stack   ,testscl ,type= "prob")
write.csv(predicted_glm_stacknew    ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/glm_stacknew.csv", row.names = FALSE)

#nnet

predicted_nnet <-  predict(nnet   ,testscl ,type= "prob")
write.csv(predicted_nnet   ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/nnet.csv", row.names = FALSE)



#adaboost

predicted_adaboost <-  predict(adaboost   ,testscl ,type= "prob")
write.csv(predicted_adaboost   ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/adaboost.csv", row.names = FALSE)



predicted_adaboost2 <-  predict(adaboost2   ,testscl ,type= "prob")
write.csv(predicted_adaboost2   ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/adaboost2.csv", row.names = FALSE)


predicted_log_reg2 <-  predict(log_reg2   ,testscl ,type= "prob")
write.csv(predicted_log_reg2    ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/log_reg2 .csv", row.names = FALSE)

predicted_log_reg3 <-  predict(log_reg3   ,testscl ,type= "prob")
write.csv(predicted_log_reg3    ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/log_reg3 .csv", row.names = FALSE)

#newens21

predicted_gbm_stack21   <-  predict(gbm_stack21 ,testscl[,c("countclick","countmlorder","countfmorder", "countuni", "countml","countfm" )] , type= "prob")

write.csv(predicted_gbm_stack21   ,"C:/Users/y.akturk/Desktop/BOUN/BOUN/IE582/project/gbm_stack21.csv", row.names = FALSE)







