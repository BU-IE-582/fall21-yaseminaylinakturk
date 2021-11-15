#IE 585 - hw#2 -2020802000 yasemin aylin akturk
#task3
klasor =  "C:/Users/y.akturk/Documents/"
setwd(klasor)
install.packages("plot3D")
library(plot3D)
install.packages("plotly")
library(plotly)

x_acc <- read.csv2("uWaveGestureLibrary_X_TRAIN.csv", header = F, fileEncoding = 'UTF-8-BOM')
y_acc <- read.csv2("uWaveGestureLibrary_Y_TRAIN.csv", header = F, fileEncoding = 'UTF-8-BOM')
z_acc <- read.csv2("uWaveGestureLibrary_Z_TRAIN.csv", header = F, fileEncoding = 'UTF-8-BOM')



colnames (x_acc)[1] = c("Ges_Class")
colnames (y_acc)[1] = c("Ges_Class")
colnames (z_acc)[1] = c("Ges_Class")

head(x_acc)
dim(x_acc)

x_acc[1:8,1:3]
y_acc[1:8,1:3]
z_acc[1:8,1:3]




Get_Acc <-function(ID)
{
  #get the first row no where matches the ID 
  #filter and get the first
  rowID <- dplyr:::nth (which(x_acc[,1]==ID),1) 
  #combine data X,Y,z
  comb_acc_data<-as.data.frame(cbind(t(x_acc[rowID,]),t(y_acc[rowID,]),t(z_acc[rowID,]))) 
  #rename cols
  colnames(comb_acc_data)<-c("corX","corY","corZ")
  #drop first
  #donot name the rows
  comb_acc_data<-comb_acc_data[-1,] 
  rownames(comb_acc_data)<-NULL
  return(comb_acc_data)
}

Get_Velo <-function(ID)
{
  #get the first row no where matches the ID 
  #filter and get the first
  rowID <- dplyr:::nth (which(x_acc[,1]==ID),1) 
  #combine data X,Y,z
  comb_acc_data<-as.data.frame(cbind(t(x_acc[rowID,]),t(y_acc[rowID,]),t(z_acc[rowID,]))) 
  #rename cols
  colnames(comb_acc_data)<-c("corX","corY","corZ")
  #drop first
  #donot name the rows
  comb_acc_data<-comb_acc_data[-1,] 
  rownames(comb_acc_data)<-NULL
  #acceleration to velocity, col. cum. sum
  comb_velo_data<-apply(comb_acc_data,2,cumsum) 
  return(comb_velo_data)
}


Get_Pos <-function(ID)  
  {
  #get the first row no where matches the ID 
  #filter and get the first
  rowID <- dplyr:::nth (which(x_acc[,1]==ID),1) 
  #combine data X,Y,z
  comb_acc_data<-as.data.frame(cbind(t(x_acc[rowID,]),t(y_acc[rowID,]),t(z_acc[rowID,]))) 
  #rename cols
  colnames(comb_acc_data)<-c("corX","corY","corZ")
  #drop first
  #donot name the rows
  comb_acc_data<-comb_acc_data[-1,] 
  rownames(comb_acc_data)<-NULL
  #acceleration to velocity, col. cum. sum
  comb_velo_data<-apply(comb_acc_data,2,cumsum) 
  #acceleration to position, col. cum. sum
  comb_pos_data<-apply(comb_velo_data,2,cumsum) 
  
  return(  comb_pos_data)
  }

#sample
Get_Acc(1)[1:8,]
Get_Velo(1)[1:8,]
Get_Pos(1)[1:8,]

#plot 3d 
#first col has the gesture ID
gestID <- sort(unique(x_acc[,1]), decreasing = F)

par(mfrow = c(2,4))

for (a in gestID)
  
{
  
  accel <- Get_Acc(a)

  plot3D::scatter3D(x = accel[,1],y = accel[,2],z = accel[,3],main = paste("Acceleration_", a) ,xlab = "x",ylab = "y",zlab = "z", col = "blue",type="l") 
  
}


par(mfrow = c(2,4))

for (v in gestID)
  
{
  
  velocity <- Get_Velo(v)
  
  plot3D::scatter3D(x = velocity[,1],y = velocity[,2],z = velocity[,3],main = paste("Velocity_", v) ,xlab = "x",ylab = "y",zlab = "z", col = "magenta",type="l") 
  
}


par(mfrow = c(2,4))

for (p in gestID)
  
{
  
  position <- Get_Pos(p)
  
  plot3D::scatter3D(x = position[,1],y = position[,2],z = position[,3],main = paste("Position_", p) ,xlab = "x",ylab = "y",zlab = "z", col = "red",type="l") 
  
}

#rotatable plots
position_1 <- Get_Pos(1)
plot_ly(x = position_1[,1], y = position_1[,2], z = position_1[,3], type="scatter3d", mode="markers") %>% layout(title = "Gesture #1")
position_2 <- Get_Pos(2)
plot_ly(x = position_2[,1], y = position_2[,2], z = position_2[,3], type="scatter3d", mode="markers") %>% layout(title = "Gesture #2")
position_3 <- Get_Pos(3)
plot_ly(x = position_3[,1], y = position_3[,2], z = position_3[,3], type="scatter3d", mode="markers") %>% layout(title = "Gesture #3")
position_4 <- Get_Pos(4)
plot_ly(x = position_4[,1], y = position_4[,2], z = position_4[,3], type="scatter3d", mode="markers") %>% layout(title = "Gesture #4")
position_5 <- Get_Pos(5)
plot_ly(x = position_5[,1], y = position_5[,2], z = position_5[,3], type="scatter3d", mode="markers") %>% layout(title = "Gesture #5")
position_6 <- Get_Pos(6)
plot_ly(x = position_6[,1], y = position_6[,2], z = position_6[,3], type="scatter3d", mode="markers") %>% layout(title = "Gesture #6")
position_7 <- Get_Pos(7)
plot_ly(x = position_7[,1], y = position_7[,2], z = position_7[,3], type="scatter3d", mode="markers") %>% layout(title = "Gesture #7")
position_8 <- Get_Pos(8)
plot_ly(x = position_8[,1], y = position_8[,2], z = position_8[,3], type="scatter3d", mode="markers") %>% layout(title = "Gesture #8")


#PCA###########

cop_x_acc = x_acc
cop_y_acc = y_acc[,-1]
cop_z_acc = z_acc[,-1]


colnames(cop_x_acc)[2:316] <-paste("X.", 1:315)
colnames(cop_y_acc)[1:315] <-paste("Y.", 1:315)
colnames(cop_z_acc)[1:315] <-paste("Z.", 1:315)


Get_Conc <-function(ID)  
{
  #get the rows class no  matches the ID 
  #filter
  rowlist <-  which(cop_x_acc[,1]==ID) 
    #combine data X,Y,z
  Data_Conc <-as.data.frame(cbind(cop_x_acc[rowlist,],cop_y_acc[rowlist,],cop_z_acc[rowlist,])) 
 
  return(Data_Conc)
}

#par(mfrow= c(2,4))
###Class1
Get_Conc(1)[1:6, c(1,2,317,632)]
dim(Get_Conc(1))
#Conc_Data1 = t(Get_Conc(1))
Conc_Data1 = Get_Conc(1)
dim(Conc_Data1)
Conc_Data1 [c(1,2),1:6]
Conc_Data1 = Conc_Data1[,-1]
Conc_Data1 [c(1,2),1:6]


pca1 <- prcomp(Conc_Data1,scale =T)
summary(pca1)
pca1$rotation


##plot

plot(pca1$rotation[,2], type="l",col="blue", main= "Gesture#1", ylab= "Eigenvector")
points(pca1$rotation[,1],type="l",col="red")
legend("bottomleft",paste("Eigenvector",c("1","2")),col=c("red", "blue"), pch= 16,cex= 0.6)





##get explained variation prop for first 2 comp

vars =  pca1$sdev * pca1$sdev
props = vars / sum(vars)
cumsum(props) [1:2]



###Class2
Conc_Data2 = Get_Conc(2)
Conc_Data2 = Conc_Data2[,-1]
Conc_Data2 [c(1,2),1:6]


pca2 <- prcomp(Conc_Data2,  scale =T)

##plot

plot(pca2$rotation[,2], type="l",col="blue", main= "Gesture#2",ylab= "Eigenvector")
points(pca2$rotation[,1], type="l",col="red")
legend("bottomleft",paste("Eigenvector",c("1","2")),col=c("red", "blue"), pch= 16,cex= 0.6)

##get explained variation prop for first 2 comp

vars =  pca2$sdev * pca2$sdev
props = vars / sum(vars)
cumsum(props) [1:2]


###Class3
Conc_Data3 = Get_Conc(3)
Conc_Data3 = Conc_Data3[,-1]
Conc_Data3 [c(1,2),1:6]

pca3 <- prcomp(Conc_Data3,  scale =T)

##plot

plot(pca3$rotation[,2], type="l",col="blue", main= "Gesture#3",ylab= "Eigenvector")
points(pca3$rotation[,1], type="l",col="red")
legend("bottomleft",paste("Eigenvector",c("1","2")),col=c("red", "blue"), pch= 16,cex= 0.6)

##get explained variation prop for first 2 comp

vars =  pca3$sdev * pca3$sdev
props = vars / sum(vars)
cumsum(props) [1:2]


###Class4
Conc_Data4 = Get_Conc(4)
Conc_Data4 = Conc_Data4[,-1]
Conc_Data4 [c(1,2),1:6]

pca4 <- prcomp(Conc_Data4,  scale =T)

##plot

plot(pca4$rotation[,2], type="l",col="blue", main= "Gesture#4",ylab= "Eigenvector")
points(pca4$rotation[,1], type="l",col="red")

legend("bottomleft",paste("Eigenvector",c("1","2")),col=c("red", "blue"), pch= 16,cex= 0.6)

##get explained variation prop for first 2 comp

vars =  pca4$sdev * pca4$sdev
props = vars / sum(vars)
cumsum(props) [1:2]




###Class5
Conc_Data5 = Get_Conc(5)
Conc_Data5 = Conc_Data5[,-1]
Conc_Data5 [c(1,2),1:6]

pca5 <- prcomp(Conc_Data5,  scale =T)

##plot

plot(pca5$rotation[,2], type="l",col="blue", main= "Gesture#5",ylab= "Eigenvector")
points(pca5$rotation[,1], type="l",col="red")
legend("bottomleft",paste("Eigenvector",c("1","2")),col=c("red", "blue"), pch= 16,cex= 0.6)

##get explained variation prop for first 2 comp

vars =  pca5$sdev * pca5$sdev
props = vars / sum(vars)
cumsum(props) [1:2]


###Class6
Conc_Data6 = Get_Conc(6)
Conc_Data6 = Conc_Data6[,-1]
Conc_Data6 [c(1,2),1:6]

pca6 <- prcomp(Conc_Data6,  scale =T)

##plot

plot(pca6$rotation[,2], type="l",col="blue", main= "Gesture#6",ylab= "Eigenvector")
points(pca6$rotation[,1], type="l",col="red")
legend("bottomleft",paste("Eigenvector",c("1","2")),col=c("red", "blue"), pch= 16,cex= 0.6)

##get explained variation prop for first 2 comp

vars =  pca6$sdev * pca6$sdev
props = vars / sum(vars)
cumsum(props) [1:2]


###Class7
Conc_Data7 = Get_Conc(7)
Conc_Data7 = Conc_Data7[,-1]
Conc_Data7 [c(1,2),1:6]

pca7 <- prcomp(Conc_Data7,  scale =T)

##plot

plot(pca7$rotation[,2], type="l",col="blue", main= "Gesture#7",ylab= "Eigenvector")
points(pca7$rotation[,1], type="l",col="red")
legend("bottomleft",paste("Eigenvector",c("1","2")),col=c("red", "blue"), pch= 16,cex= 0.6)

##get explained variation prop for first 2 comp

vars =  pca7$sdev * pca7$sdev
props = vars / sum(vars)
cumsum(props) [1:2]


###Class8
Conc_Data8 = Get_Conc(8)
Conc_Data8 = Conc_Data8[,-1]
Conc_Data8 [c(1,2),1:6]

pca8 <- prcomp(Conc_Data8,  scale =T)

##plot

plot(pca8$rotation[,2], type="l",col="blue", main= "Gesture#8",ylab= "Eigenvector")
points(pca8$rotation[,1], type="l",col="red")
legend("bottomleft",paste("Eigenvector",c("1","2")),col=c("red", "blue"), pch= 16,cex= 0.6)

##get explained variation prop for first 2 comp

vars =  pca8$sdev * pca8$sdev
props = vars / sum(vars)
cumsum(props) [1:2]

#try to see pattern
par(mfrow=c(4,1))
#the acceleration data for class#8 in x1-x2----y1-y2----z1-z2 format
#get a random instance, say 10th, of the class
plot( c(1:945),Conc_Data8[10,],type="l", xlab="Index", main ="Raw")  
#the eigenvector1
plot(pca8$rotation[,1], type="l",col="red", main ="Eigenvector#1")
#the eigenvector2
plot(pca8$rotation[,2], type="l",col="blue",main ="Eigenvector#2")
#linear combination of the eigenvector1 and #the eigenvector2
plot(pca8$rotation[,1]+pca8$rotation[,2], type="l",col="green", main ="Eigenvector#1+#2")










